-module(wd_download_manager_srv).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_new_downloader/1, add_downloader_worker/1, download/1]).

-record(local_state, {table_id}).

start_link() ->
  gen_server:start_link({local, download_manager_srv}, ?MODULE, [], []).

init(_Args) ->
  TableId = ets:new(download_manager, [set, public]),
  {ok, #local_state{table_id = TableId}}.

handle_call({start_wd_downloader_srv, WebsiteHostname, SupervisorPid}, _From, State) ->
  Id = WebsiteHostname ++ "_srv",
  ChildSpecs = #{
    id => list_to_atom(Id),
    start => {wd_downloader_srv, start_link, [SupervisorPid]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [wd_downloader_srv]
  },
  {ok, Pid} = supervisor:start_child(idea_execute_sup, ChildSpecs),

  #local_state{table_id = TableId} = State,
  ets:insert(TableId, {list_to_binary(WebsiteHostname), Pid}),

  {reply, ok, State};

handle_call({start_wd_downloader_wrk_sup, WebsiteHostname, MFA}, _From, State) ->
  Id = list_to_atom(WebsiteHostname ++ "_sup"),
  ChildSpecs = #{
    id => Id,
    start => {wd_downloader_wrk_sup, start_link, [Id, MFA]},
    restart => temporary,
    shutdown => 10000,
    type => supervisor,
    modules => [wd_downloader_wrk_sup]
  },
  {ok, _Pid} = supervisor:start_child(idea_execute_sup, ChildSpecs),

  {reply, {ok, Id},  State};

handle_call({get_downloader_srv_pid, WebsiteHostnameBin}, _From, State) ->
  #local_state{table_id = TableId} = State,
  [{_, DownloaderSrvPid}] = ets:lookup(TableId, WebsiteHostnameBin),

  {reply, {ok, DownloaderSrvPid}, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, process, _Pid, _}, State) ->
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

start_new_downloader(WebsiteHostname) ->
  MFA = {wd_downloader_wrk, start_link, []},
  %% Create new downloader worker supervisor
  {ok, SupervisorPid} = gen_server:call(
    download_manager_srv,
    {start_wd_downloader_wrk_sup, WebsiteHostname, MFA}
  ),
  %% Create new downloader server
  ok = gen_server:call(
    download_manager_srv,
    {start_wd_downloader_srv, WebsiteHostname, SupervisorPid}
  ),
  ok.

add_downloader_worker(WebsiteHostname) ->
  {ok, DownloaderSrvPid} = gen_server:call(
    download_manager_srv,
    {get_downloader_srv_pid, list_to_binary(WebsiteHostname)}
  ),
  ok = gen_server:call(DownloaderSrvPid, add_downloader_worker),
  ok.

download(WebsiteHostname) ->
  {ok, DownloaderSrvPid} = gen_server:call(
    download_manager_srv,
    {get_downloader_srv_pid, list_to_binary(WebsiteHostname)}
  ),
  Reply = gen_server:call(DownloaderSrvPid, initial_download),
  case Reply of
    ok ->
      gen_server:cast(DownloaderSrvPid, coordinate_all_workers);

    already_started ->
      io:format("Download already started.~n")
  end.
