-module(wd_download_manager_srv).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_new_downloader/2, add_downloader_workers/1, download/2]).

-record(local_state, {table_id}).

start_link() ->
  gen_server:start_link({local, download_manager_srv}, ?MODULE, [], []).

init(_Args) ->
  TableId = ets:new(download_manager, [set, public]),
  {ok, #local_state{table_id = TableId}}.

handle_call({start_wd_downloader_srv, WebsiteHostnameBin, DepthMaximumSetting, SupervisorPid}, _From, State) ->
  #local_state{table_id = TableId} = State,
  Id = iolist_to_binary([WebsiteHostnameBin, <<"_srv">>]),
  ChildSpecs = #{
    id => binary_to_atom(Id, utf8),
    start => {wd_downloader_srv, start_link, [WebsiteHostnameBin, DepthMaximumSetting, SupervisorPid]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [wd_downloader_srv]
  },
  {ok, Pid} = supervisor:start_child(idea_execute_sup, ChildSpecs),
  Ref = erlang:monitor(process, Pid),

  ets:insert(TableId, {WebsiteHostnameBin, {Pid, Ref}}),
  {reply, ok, State};

handle_call({start_wd_downloader_wrk_sup, WebsiteHostnameBin, MFA}, _From, State) ->
  Id = iolist_to_binary([WebsiteHostnameBin, <<"_sup">>]),
  IdAtom = binary_to_atom(Id, utf8),
  ChildSpecs = #{
    id => IdAtom,
    start => {wd_downloader_wrk_sup, start_link, [IdAtom, MFA]},
    restart => temporary,
    shutdown => 10000,
    type => supervisor,
    modules => [wd_downloader_wrk_sup]
  },
  {ok, _Pid} = supervisor:start_child(idea_execute_sup, ChildSpecs),
  {reply, {ok, IdAtom},  State};

handle_call({get_downloader_srv_pid, WebsiteHostnameBin}, _From, State) ->
  #local_state{table_id = TableId} = State,
  [{_, {DownloaderSrvPid, _MonitorRef}}] = ets:lookup(TableId, WebsiteHostnameBin),
  {reply, {ok, DownloaderSrvPid}, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, process, _Pid, _}, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

start_new_downloader(WebsiteHostname, DepthMaximumSetting) ->
  WebsiteHostnameBin = list_to_binary(WebsiteHostname),
  MFA = {wd_downloader_wrk, start_link, []},

  %% Create the new supervisor of downloader worker
  {ok, SupervisorPid} = gen_server:call(
    download_manager_srv,
    {start_wd_downloader_wrk_sup, WebsiteHostnameBin, MFA}
  ),

  %% Create new downloader server
  ok = gen_server:call(
    download_manager_srv,
    {start_wd_downloader_srv, WebsiteHostnameBin, DepthMaximumSetting, SupervisorPid}
  ),
  ok.

add_downloader_workers(WebsiteHostname) ->
  WebsiteHostnameBin = list_to_binary(WebsiteHostname),
  Reply = gen_server:call(downloader_srv_pid(WebsiteHostnameBin), add_downloader_workers),
  case Reply of
    ok -> ok;
    workers_already_added -> io:format("Workers already added.~n")
  end.

download(WebsiteHostname, WebsiteHttpType) ->
  WebsiteHostnameBin = list_to_binary(WebsiteHostname),
  WebsiteHttpTypeBin = list_to_binary(WebsiteHttpType),
  DownloaderSrvPid = downloader_srv_pid(WebsiteHostnameBin),
  Reply = gen_server:call(DownloaderSrvPid, {initial_download, WebsiteHttpTypeBin}),

  case Reply of
    ok ->
      gen_server:cast(DownloaderSrvPid, coordinate_all_workers);

    {timeout, InitialDownloadTimeoutCount} ->
      if
        InitialDownloadTimeoutCount =< 3 ->
          io:format("Timeout for initial download. Now retrying...~n"),
          download(WebsiteHostname, WebsiteHttpType);

        true ->
          io:format("It's been 3 times timeout when doing initial download.~n"),
          io:format("It seems the target website is in problem. Or you have internet connection problem.~n"),
          io:format("Cannot finish initial download. The downloader cannot continue.~n")
      end;

    already_started ->
      io:format("Download already started.~n")
  end.

downloader_srv_pid(WebsiteHostnameBin) ->
  {ok, DownloaderSrvPid} = gen_server:call(
    download_manager_srv,
    {get_downloader_srv_pid, WebsiteHostnameBin}
  ),
  DownloaderSrvPid.
