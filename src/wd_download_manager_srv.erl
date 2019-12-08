-module(wd_download_manager_srv).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_new_downloader/3, download/2]).

-define(MAX_CRASH, 2).
-define(MAX_RETRY, 3).

-record(local_state, {sequences, downloaders, downloaders_fails_count}).

start_link() ->
  gen_server:start_link({local, download_manager_srv}, ?MODULE, [], []).

init(_Args) ->
  {ok, #local_state{sequences = 0, downloaders = dict:new(), downloaders_fails_count = dict:new()}}.

handle_call({start_wd_downloader_srv, Args}, _From, State) ->
  #local_state{downloaders = Downloaders, downloaders_fails_count = DownloadersFailsCount} = State,
  DownloaderSrvServerName = lists:nth(2, Args),

  ChildSpecs = #{
    id => DownloaderSrvServerName,
    start => {wd_downloader_srv, start_link, Args},
    restart => transient,
    shutdown => 5000,
    type => worker,
    modules => [wd_downloader_srv]
  },

  {ok, DownloaderSrvPid} = supervisor:start_child(idea_execute_sup, ChildSpecs),
  _MonitorRef = erlang:monitor(process, DownloaderSrvPid),
  DownloadersNextState = dict:store(DownloaderSrvPid, DownloaderSrvServerName, Downloaders),
  DownloadersFailsCountNextState = dict:store(DownloaderSrvServerName, 0, DownloadersFailsCount),

  {reply, ok, State#local_state{
    downloaders = DownloadersNextState,
    downloaders_fails_count = DownloadersFailsCountNextState
  }};

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
  {reply, ok, State};

handle_call(next_sequence, _From, #local_state{sequences = Sequences} = State) ->
  SequencesNextState = Sequences + 1,
  {reply, {ok, SequencesNextState}, State#local_state{sequences = SequencesNextState}}.

handle_cast({downloader_srv_confirm, DownloaderSrvPid, Sequence, WebsiteHostnameBin}, State) ->
  #local_state{downloaders = Downloaders, downloaders_fails_count = DownloadersFailsCount} = State,
  DownloaderSrvServerName = get_server_name(WebsiteHostnameBin, Sequence, <<"_srv">>),
  {ok, CounterValue} = dict:find(DownloaderSrvServerName, DownloadersFailsCount),

  if
    CounterValue =< ?MAX_CRASH ->
      {monitored_by, MonitoredList} = process_info(DownloaderSrvPid, monitored_by),

      if
        length(MonitoredList) < 1 ->
          _MonitorRef = erlang:monitor(process, DownloaderSrvPid),
          DownloadersNextState = dict:store(DownloaderSrvPid, DownloaderSrvServerName, Downloaders),
          ok = gen_server:call(DownloaderSrvPid, recollect_workers),
          {noreply, State#local_state{downloaders = DownloadersNextState}};

        true ->
          {noreply, State}
      end;

    true ->
      DownloadersFailsCountNextState = dict:erase(DownloaderSrvServerName, DownloadersFailsCount),
      gen_server:cast(download_manager_srv, {shutdown_downloader, WebsiteHostnameBin}),
      {noreply, State#local_state{downloaders_fails_count = DownloadersFailsCountNextState}}
  end;

handle_cast({shutdown_downloader, Sequence, WebsiteHostnameBin}, State) ->
  #local_state{downloaders_fails_count = DownloadersFailsCount} = State,
  io:format("Shutting down the downloader for: ~p~n", [WebsiteHostnameBin]),

  DownloaderSrvServerName = get_server_name(WebsiteHostnameBin, Sequence, <<"_srv">>),
  DownloaderWrkSup = get_server_name(WebsiteHostnameBin, Sequence, <<"_sup">>),

  ok = supervisor:terminate_child(idea_execute_sup, DownloaderWrkSup),
  ok = supervisor:terminate_child(idea_execute_sup, DownloaderSrvServerName),
  ok = supervisor:delete_child(idea_execute_sup, DownloaderSrvServerName),
  DownloadersFailsCountNextState = dict:erase(DownloaderSrvServerName, DownloadersFailsCount),

  io:format("Shutdown ok~n"),
  {noreply, State#local_state{downloaders_fails_count = DownloadersFailsCountNextState}};

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _}, State) ->
  #local_state{downloaders = Downloaders, downloaders_fails_count = DownloadersFailsCount} = State,
  {ok, DownloaderSrvServerName} = dict:find(Pid, Downloaders),

  case dict:find(DownloaderSrvServerName, DownloadersFailsCount) of
    error ->
      DownloadersNextState = dict:erase(Pid, Downloaders),
      {noreply, State#local_state{downloaders = DownloadersNextState}};

    {ok, CounterValue} ->
      MaxCrash = ?MAX_CRASH + 1,

      if
        CounterValue =< MaxCrash ->
          DownloadersFailsCountNextState = dict:update_counter(DownloaderSrvServerName, 1, DownloadersFailsCount),
          DownloadersNextState = dict:erase(Pid, Downloaders),

          {noreply, State#local_state{
            downloaders = DownloadersNextState,
            downloaders_fails_count = DownloadersFailsCountNextState
          }};

        true ->
          {noreply, State}
      end
  end;

handle_info(_AnyMessage, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

start_new_downloader(WebsiteHostname, WebsiteHttpType, DepthMaximumSetting) ->
  {ok, Sequence} = gen_server:call(download_manager_srv, next_sequence),
  WebsiteHostnameBin = list_to_binary(WebsiteHostname),
  WebsiteHttpTypeBin = list_to_binary(WebsiteHttpType),
  DownloaderWrkSupServerName = get_server_name(WebsiteHostnameBin, Sequence, <<"_sup">>),
  DownloaderSrvServerName = get_server_name(WebsiteHostnameBin, Sequence, <<"_srv">>),

  %% Create new downloader server
  DownloaderSrvArgs = [
    Sequence,
    DownloaderSrvServerName,
    DownloaderWrkSupServerName,
    WebsiteHostnameBin,
    WebsiteHttpTypeBin,
    DepthMaximumSetting
  ],
  ok = gen_server:call(download_manager_srv, {start_wd_downloader_srv, DownloaderSrvArgs}),

  %% Create the new supervisor of downloader worker
  DownloaderWrkArgs = [DownloaderSrvServerName, WebsiteHostnameBin, WebsiteHttpTypeBin, DepthMaximumSetting],
  MFA = {wd_downloader_wrk, start_link, DownloaderWrkArgs},
  ok = gen_server:call(download_manager_srv, {start_wd_downloader_wrk_sup, DownloaderWrkSupServerName, MFA}),

  %% Add workers
  ok = gen_server:call(DownloaderSrvServerName, add_downloader_workers),
  {ok, Sequence}.

download(WebsiteHostname, Sequence) ->
  download(WebsiteHostname, Sequence, 0).
download(WebsiteHostname, Sequence, GenServerCallTimeoutCount) ->
  WebsiteHostnameBin = list_to_binary(WebsiteHostname),
  DownloaderSrvServerName = get_server_name(WebsiteHostnameBin, Sequence, <<"_srv">>),

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
        GenServerCallTimeoutCountNextState =< ?MAX_RETRY ->
          download(WebsiteHostname, Sequence, GenServerCallTimeoutCountNextState);
        true ->
          gen_server:cast(download_manager_srv, {shutdown_downloader, Sequence, WebsiteHostnameBin}),
          'INITIAL_DOWNLOAD_MAX_RETRY_REACHED'
      end;

    ok ->
      gen_server:cast(DownloaderSrvServerName, coordinate_all_workers);

    nomatch ->
      'INITIAL_DOWNLOAD_FINISH_INDEX_PAGE_URLS_NOMATCH';

    timeout ->
      'REQUEST_TIMEOUT';

    already_started ->
      'DOWNLOAD_ALREADY_STARTED'
  end.

get_server_name(WebsiteHostnameBin, Sequence, TypeBin) ->
  ListToBin = iolist_to_binary([WebsiteHostnameBin, integer_to_binary(Sequence), TypeBin]),
  binary_to_atom(ListToBin, utf8).
