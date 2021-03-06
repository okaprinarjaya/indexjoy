-module(wd_downloader_srv).

-behaviour(gen_server).

-export([start_link/6]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MAX_WORKERS, 5).
-record(local_state, {
  sequence,
  initial_download,
  depth_maximum_setting,
  depth_reach,
  website_hostname,
  website_http_type,
  urls_queue,
  workers,
  supervisor_pid,
  table_id_downloader_srv
}).

start_link(Sequence, DownloaderSrvServerName, DownloaderWrkSupServerName, WebsiteHostnameBin, WebsiteHttpTypeBin, DepthMaximumSetting) ->
  gen_server:start_link(
    {local, DownloaderSrvServerName},
    ?MODULE,
    [Sequence, DownloaderWrkSupServerName, WebsiteHostnameBin, WebsiteHttpTypeBin, DepthMaximumSetting],
    []
  ).

init([Sequence, DownloaderWrkSupServerName, WebsiteHostnameBin, WebsiteHttpTypeBin, DepthMaximumSetting]) ->
  gen_server:cast(download_manager_srv, {downloader_srv_confirm, self(), Sequence, WebsiteHostnameBin}),

  TableId = ets:new(downloader_srv, [set, public]),

  {ok, #local_state{
    sequence = Sequence,
    initial_download = true,
    depth_maximum_setting = DepthMaximumSetting,
    depth_reach = 0,
    website_hostname = WebsiteHostnameBin,
    website_http_type = WebsiteHttpTypeBin,
    workers = [],
    supervisor_pid = DownloaderWrkSupServerName,
    table_id_downloader_srv = TableId
  }}.

handle_call(add_downloader_workers, _From, State) ->
  #local_state{workers = Workers, supervisor_pid = DownloaderWrkSupServerName} = State,

  if
    length(Workers) > 0 ->
      {reply, ok, State};

    true ->
      WorkersNextState = [worker_starter(DownloaderWrkSupServerName) || _ <- lists:seq(1, ?MAX_WORKERS)],
      {reply, ok, State#local_state{workers = lists:append(WorkersNextState, Workers)}}
  end;

handle_call(initial_download, _From, State) ->
  #local_state{
    initial_download = InitialDownload,
    website_hostname = WebsiteHostnameBin,
    website_http_type = WebsiteHttpTypeBin,
    workers = Workers
  } = State,

  case InitialDownload of
    true ->
      {WorkerPid, _Ref} = lists:nth(1, Workers),
      IndexPage = [WebsiteHttpTypeBin, <<"://">>, WebsiteHostnameBin, <<"/">>],

      case catch gen_server:call(WorkerPid, {initial_download, iolist_to_binary(IndexPage)}) of
        {'EXIT', {timeout, _TheRest}} ->
          {reply, timeout, State};

        timeout ->
          {reply, timeout, State};

        nomatch ->
          {reply, nomatch, State};

        {ok, {UrlsList, FinishedDownloadProcessDepthStateNew}} ->
          {reply, ok, State#local_state{
            initial_download = false,
            depth_reach = FinishedDownloadProcessDepthStateNew,
            urls_queue = queue:from_list(UrlsList)
          }}
      end;

    false ->
      {reply, already_started, State}
  end;

handle_call(recollect_workers, _From, #local_state{supervisor_pid = DownloaderWrkSupServerName} = State) ->
  WorkersList = supervisor:which_children(DownloaderWrkSupServerName),

  if
    length(WorkersList) > 0 ->
      WorkersNextState = lists:map(
        fun({undefined, Pid, worker, _}) ->
          MonitorRef = erlang:monitor(process, Pid),
          {Pid, MonitorRef}
        end,
        WorkersList
      ),
      {reply, ok, State#local_state{workers = WorkersNextState}};

    true ->
      {reply, ok, State}
  end.

handle_cast(coordinate_all_workers, #local_state{workers = Workers} = State) ->
  gogogo(Workers),
  {noreply, State};

handle_cast({gimme_next_page, {[], undefined}, WorkerPid}, State) ->
  #local_state{urls_queue = UrlsQueue, table_id_downloader_srv = TableIdDownloaderSrv} = State,

  case pick_task_for_worker(TableIdDownloaderSrv, UrlsQueue, WorkerPid) of
    {{value, {UrlPath, CurrentProcessedUrlDepthState}}, UrlsQueueNextState} ->
      gen_server:cast(WorkerPid, {download, UrlPath, CurrentProcessedUrlDepthState}),
      {noreply, State#local_state{urls_queue = UrlsQueueNextState}};

    {empty, UrlsQueueNextState} ->
      gen_server:cast(WorkerPid, retry),
      {noreply, State#local_state{urls_queue = UrlsQueueNextState}};

    {retry, UrlsQueueNextState} ->
      gen_server:cast(WorkerPid, retry),
      {noreply, State#local_state{urls_queue = UrlsQueueNextState}}
  end;

handle_cast({gimme_next_page, {UrlsList, FinishedDownloadProcessDepthStateNew}, WorkerPid}, State)
  when length(UrlsList) > 0 ->
    #local_state{
      depth_reach = DepthReach,
      urls_queue = UrlsQueue
    } = State,

    UrlsQueueNew = queue:from_list(UrlsList),
    UrlsQueueNextState = queue:join(UrlsQueue, UrlsQueueNew),

    gen_server:cast(WorkerPid, retry),

    if
      FinishedDownloadProcessDepthStateNew < DepthReach ->
        {noreply, State#local_state{urls_queue = UrlsQueueNextState}};

      true ->
        {noreply, State#local_state{
          depth_reach = FinishedDownloadProcessDepthStateNew,
          urls_queue = UrlsQueueNextState
        }}
    end;

handle_cast({requeue, UrlPath, CurrentProcessedUrlDepthState, Sender}, State) ->
  #local_state{urls_queue = UrlsQueue} = State,
  UrlsQueueNextState = queue:in({UrlPath, CurrentProcessedUrlDepthState}, UrlsQueue),
  gen_server:cast(Sender, retry),
  {noreply, State#local_state{urls_queue = UrlsQueueNextState}};

handle_cast({downloader_wrk_confirm, WorkerPid}, #local_state{workers = Workers} = State) ->
  {monitored_by, MonitoredList} = process_info(WorkerPid, monitored_by),

  if
    length(MonitoredList) < 1 ->
      MonitorRef = erlang:monitor(process, WorkerPid),
      Member = {WorkerPid, MonitorRef},
      WorkersNextState = [Member | Workers],
      gen_server:cast(WorkerPid, gogogo),
      {noreply, State#local_state{workers = WorkersNextState}};

    true ->
      {noreply, State}
  end.

handle_info({gimme_next_page_info, WorkerPid}, State) ->
  #local_state{
    sequence = Sequence,
    depth_maximum_setting = DepthMaximumSetting,
    depth_reach = DepthReach,
    urls_queue = UrlsQueue,
    website_hostname = WebsiteHostnameBin,
    table_id_downloader_srv = TableIdDownloaderSrv
  } = State,

  UrlsQueueLen = queue:len(UrlsQueue),

  if
    DepthReach =:= DepthMaximumSetting andalso UrlsQueueLen < 1 ->
      gen_server:cast(download_manager_srv, {shutdown_downloader, Sequence, WebsiteHostnameBin}),
      {noreply, State};

    true ->
      case pick_task_for_worker(TableIdDownloaderSrv, UrlsQueue, WorkerPid) of
        {{value, {UrlPath, CurrentProcessedUrlDepthState}}, UrlsQueueNextState} ->
          gen_server:cast(WorkerPid, {download, UrlPath, CurrentProcessedUrlDepthState}),
          {noreply, State#local_state{urls_queue = UrlsQueueNextState}};

        {empty, UrlsQueueNextState} ->
          gen_server:cast(WorkerPid, retry),
          {noreply, State#local_state{urls_queue = UrlsQueueNextState}};

        {retry, UrlsQueueNextState} ->
          gen_server:cast(WorkerPid, retry),
          {noreply, State#local_state{urls_queue = UrlsQueueNextState}}
      end
  end;

handle_info({'DOWN', Ref, process, Pid, _}, #local_state{workers = Workers} = State) ->
  Member = {Pid, Ref},
  WorkersNextState = lists:delete(Member, Workers),
  {noreply, State#local_state{workers = WorkersNextState}};

handle_info(_AnyMessage, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

worker_starter(SupervisorPid) ->
  {ok, Pid} = supervisor:start_child(SupervisorPid, []),
  Ref = erlang:monitor(process, Pid),
  {Pid, Ref}.

gogogo([]) -> ok;
gogogo(Workers) ->
  [Worker | WorkersTail] = Workers,
  {WorkerPid, _Ref} = Worker,
  gen_server:cast(WorkerPid, gogogo),
  gogogo(WorkersTail).

pick_task_for_worker(TableIdDownloaderSrv, UrlsQueue, Sender) ->
  case queue:out(UrlsQueue) of
    {{value, {UrlPath, CurrentProcessedUrlDepthState}}, UrlsQueueNextState} ->
      if
        UrlPath =/= <<"/">> andalso UrlPath =/= <<>> ->
          case ets:member(TableIdDownloaderSrv, UrlPath) of
            false ->
              ets:insert(TableIdDownloaderSrv, {UrlPath}),
              {{value, {UrlPath, CurrentProcessedUrlDepthState}}, UrlsQueueNextState};
            true ->
              pick_task_for_worker(TableIdDownloaderSrv, UrlsQueueNextState, Sender)
          end;

        true ->
          {retry, UrlsQueueNextState}
      end;

    {empty, UrlsQueueNextState} ->
      {empty, UrlsQueueNextState}
  end.
