-module(wd_download_manager_srv).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_new_downloader/3, add_downloader_workers/1, download/1]).

start_link() ->
  gen_server:start_link({local, download_manager_srv}, ?MODULE, [], []).

init(_Args) ->
  {ok, undefined}.

handle_call({start_wd_downloader_srv, Args}, _From, State) ->
  [_, _, WebsiteHostnameBin, _, _] = Args,

  ChildSpecs = #{
    id => get_server_name(WebsiteHostnameBin, <<"_srv">>),
    start => {wd_downloader_srv, start_link, Args},
    restart => transient,
    shutdown => 5000,
    type => worker,
    modules => [wd_downloader_srv]
  },

  {ok, DownloaderSrvPid} = supervisor:start_child(idea_execute_sup, ChildSpecs),
  _MonitorRef = erlang:monitor(process, DownloaderSrvPid),
  {reply, ok, State};

handle_call({start_wd_downloader_wrk_sup, DownloaderWrkSupServerName, MFA}, _From, State) ->
  ChildSpecs = #{
    id => DownloaderWrkSupServerName,
    start => {wd_downloader_wrk_sup, start_link, [DownloaderWrkSupServerName, MFA]},
    restart => temporary,
    shutdown => 10000,
    type => supervisor,
    modules => [wd_downloader_wrk_sup]
  },

  {ok, _Pid} = supervisor:start_child(idea_execute_sup, ChildSpecs),
  {reply, ok, State}.

handle_cast({downloader_srv_confirm, DownloaderSrvPid}, State) ->
  {monitored_by, MonitoredList} = process_info(DownloaderSrvPid, monitored_by),

  if
    length(MonitoredList) < 1 ->
      _MonitorRef = erlang:monitor(process, DownloaderSrvPid),
      gen_server:cast(DownloaderSrvPid, recollect_workers),
      {noreply, State};

    true ->
      {noreply, State}
  end;

handle_cast({shutdown_downloader, WebsiteHostnameBin}, State) ->
  io:format("Shutting down the downloader for: ~p~n", [WebsiteHostnameBin]),

  DownloaderSrvId = get_server_name(WebsiteHostnameBin, <<"_srv">>),
  DownloaderWrkSup = get_server_name(WebsiteHostnameBin, <<"_sup">>),
  ok = supervisor:terminate_child(idea_execute_sup, DownloaderWrkSup),
  ok = supervisor:terminate_child(idea_execute_sup, DownloaderSrvId),
  ok = supervisor:delete_child(idea_execute_sup, DownloaderSrvId),

  io:format("Shutdown success~n"),
  {noreply, State};

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
  DownloaderWrkSupServerName = get_server_name(WebsiteHostnameBin, <<"_sup">>),
  DownloaderSrvServerName = get_server_name(WebsiteHostnameBin, <<"_srv">>),

  %% Create new downloader server
  DownloaderSrvArgs = [
    DownloaderSrvServerName,
    DownloaderWrkSupServerName,
    WebsiteHostnameBin,
    WebsiteHttpTypeBin,
    DepthMaximumSetting
  ],
  ok = gen_server:call(download_manager_srv, {start_wd_downloader_srv, DownloaderSrvArgs}),
  DownloaderWrkArgs = [DownloaderSrvServerName, WebsiteHostnameBin, WebsiteHttpTypeBin, DepthMaximumSetting],
  MFA = {wd_downloader_wrk, start_link, DownloaderWrkArgs},

  %% Create the new supervisor of downloader worker
  ok = gen_server:call(download_manager_srv, {start_wd_downloader_wrk_sup, DownloaderWrkSupServerName, MFA}),
  ok.

add_downloader_workers(WebsiteHostname) ->
  WebsiteHostnameBin = list_to_binary(WebsiteHostname),
  DownloaderSrvServerName = get_server_name(WebsiteHostnameBin, <<"_srv">>),
  Reply = gen_server:call(DownloaderSrvServerName, add_downloader_workers),

  case Reply of
    ok -> ok;
    workers_already_added -> 'WORKERS_ALREADY_ADDED'
  end.

download(WebsiteHostname) ->
  download(WebsiteHostname, 0).
download(WebsiteHostname, GenServerCallTimeoutCount) ->
  WebsiteHostnameBin = list_to_binary(WebsiteHostname),
  DownloaderSrvServerName = get_server_name(WebsiteHostnameBin, <<"_srv">>),

  if
    GenServerCallTimeoutCount < 1 ->
      io:format("Starting download for: ~p~n", [WebsiteHostname]);
    true ->
      io:format("[~p] Retrying download for: ~p~n", [GenServerCallTimeoutCount,WebsiteHostname])
  end,

  case catch gen_server:call(DownloaderSrvServerName, initial_download) of
    {'EXIT', {timeout, _TheRest}} ->
      GenServerCallTimeoutCountNextState = GenServerCallTimeoutCount + 1,

      if
        GenServerCallTimeoutCountNextState =< 3 ->
          download(WebsiteHostname, GenServerCallTimeoutCountNextState);
        true ->
          gen_server:cast(download_manager_srv, {shutdown_downloader, WebsiteHostnameBin}),
          'INITIAL_DOWNLOAD_MAX_RETRY_REACHED'
      end;

    ok ->
      gen_server:cast(DownloaderSrvServerName, coordinate_all_workers);

    nomatch ->
      'INITIAL_DOWNLOAD_FINISH_INDEX_PAGE_URLS_NOMATCH';

    already_started ->
      'DOWNLOAD_ALREADY_STARTED'
  end.

get_server_name(WebsiteHostnameBin, TypeBin) ->
  ListToBin = iolist_to_binary([WebsiteHostnameBin, TypeBin]),
  binary_to_atom(ListToBin, utf8).
