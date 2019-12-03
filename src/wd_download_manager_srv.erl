-module(wd_download_manager_srv).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_new_downloader/3, add_downloader_workers/1, download/1]).

-record(local_state, {table_id}).

start_link() ->
  gen_server:start_link({local, download_manager_srv}, ?MODULE, [], []).

init(_Args) ->
  TableId = ets:new(download_manager, [set, public]),
  {ok, #local_state{table_id = TableId}}.

handle_call({start_wd_downloader_srv, Args}, _From, #local_state{table_id = TableId} = State) ->
  [WebsiteHostnameBin, _WebsiteHttpTypeBin, _DepthMaximumSetting, _SupervisorPid] = Args,
  Id = iolist_to_binary([WebsiteHostnameBin, <<"_srv">>]),

  ChildSpecs = #{
    id => binary_to_atom(Id, utf8),
    start => {wd_downloader_srv, start_link, Args},
    restart => transient,
    shutdown => 5000,
    type => worker,
    modules => [wd_downloader_srv]
  },

  {ok, DownloaderSrvPid} = supervisor:start_child(idea_execute_sup, ChildSpecs),
  DownloaderSrvRef = erlang:monitor(process, DownloaderSrvPid),
  ets:insert(TableId, {WebsiteHostnameBin, {DownloaderSrvPid, DownloaderSrvRef}}),
  {reply, {ok, DownloaderSrvPid}, State};

handle_call({start_wd_downloader_wrk_sup, SupervisorPid, MFA}, _From, State) ->
  ChildSpecs = #{
    id => SupervisorPid,
    start => {wd_downloader_wrk_sup, start_link, [SupervisorPid, MFA]},
    restart => temporary,
    shutdown => 10000,
    type => supervisor,
    modules => [wd_downloader_wrk_sup]
  },

  {ok, _Pid} = supervisor:start_child(idea_execute_sup, ChildSpecs),
  {reply, ok, State};

handle_call({get_downloader_srv_pid, WebsiteHostnameBin}, _From, State) ->
  #local_state{table_id = TableId} = State,
  [{_, {DownloaderSrvPid, _MonitorRef}}] = ets:lookup(TableId, WebsiteHostnameBin),
  {reply, {ok, DownloaderSrvPid}, State}.

handle_cast({downloader_srv_confirm, DownloaderSrvPid, WebsiteHostnameBin}, State) ->
  #local_state{table_id = TableId} = State,
  {monitored_by, MonitoredList} = process_info(DownloaderSrvPid, monitored_by),

  if
    length(MonitoredList) < 1 ->
      MonitorRef = erlang:monitor(process, DownloaderSrvPid),
      ets:insert(TableId, {WebsiteHostnameBin, {DownloaderSrvPid, MonitorRef}}),
      gen_server:cast(DownloaderSrvPid, recollect_workers),
      {noreply, State};

    true ->
      {noreply, State}
  end;

handle_cast({shutdown_downloader, WebsiteHostnameBin}, State) ->
  DownloaderSrvPid = downloader_srv_pid(WebsiteHostnameBin),
  % supervisor:terminate_child(idea_execute_sup, ),
  supervisor:delete_child(idea_execute_sup),
  supervisor:terminate_child(idea_execute_sup);

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, process, _Pid, _}, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

start_new_downloader(WebsiteHostname, WebsiteHttpType, DepthMaximumSetting) ->
  WebsiteHostnameBin = list_to_binary(WebsiteHostname),
  WebsiteHttpTypeBin = list_to_binary(WebsiteHttpType),
  SupervisorPid = binary_to_atom(iolist_to_binary([WebsiteHostnameBin, <<"_sup">>]), utf8),

  %% Create new downloader server
  DownloaderSrvArgs = [WebsiteHostnameBin, WebsiteHttpTypeBin, DepthMaximumSetting, SupervisorPid],
  {ok, DownloaderSrvPid} = gen_server:call(download_manager_srv, {start_wd_downloader_srv, DownloaderSrvArgs}),
  DownloaderWrkArgs = [DownloaderSrvPid, WebsiteHostnameBin, WebsiteHttpTypeBin, DepthMaximumSetting],
  MFA = {wd_downloader_wrk, start_link, DownloaderWrkArgs},

  %% Create the new supervisor of downloader worker
  ok = gen_server:call(download_manager_srv, {start_wd_downloader_wrk_sup, SupervisorPid, MFA}),
  ok.

add_downloader_workers(WebsiteHostname) ->
  WebsiteHostnameBin = list_to_binary(WebsiteHostname),
  Reply = gen_server:call(downloader_srv_pid(WebsiteHostnameBin), add_downloader_workers),

  case Reply of
    ok -> ok;
    workers_already_added -> 'WORKERS_ALREADY_ADDED'
  end.

download(WebsiteHostname) ->
  download(WebsiteHostname, 0).
download(WebsiteHostname, GenServerCallTimeoutCount) ->
  WebsiteHostnameBin = list_to_binary(WebsiteHostname),
  DownloaderSrvPid = downloader_srv_pid(WebsiteHostnameBin),

  if
    GenServerCallTimeoutCount < 1 ->
      io:format("Starting download for: ~p~n", [WebsiteHostname]);
    true ->
      io:format("[~p] Retrying download for: ~p~n", [GenServerCallTimeoutCount,WebsiteHostname])
  end,

  case catch gen_server:call(DownloaderSrvPid, initial_download) of
    {'EXIT', {timeout, _TheRest}} ->
      if
        GenServerCallTimeoutCount =< 3 ->
          download(WebsiteHostname, GenServerCallTimeoutCount + 1);
        true ->
          'INITIAL_DOWNLOAD_MAX_RETRY_REACHED'
      end;

    ok ->
      gen_server:cast(DownloaderSrvPid, coordinate_all_workers);

    nomatch ->
      'INITIAL_DOWNLOAD_FINISH_INDEX_PAGE_URLS_NOMATCH';

    already_started ->
      'DOWNLOAD_ALREADY_STARTED'
  end.

downloader_srv_pid(WebsiteHostnameBin) ->
  {ok, DownloaderSrvPid} = gen_server:call(download_manager_srv, {get_downloader_srv_pid, WebsiteHostnameBin}),
  DownloaderSrvPid.
