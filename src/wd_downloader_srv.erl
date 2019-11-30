-module(wd_downloader_srv).

-behaviour(gen_server).

-export([start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MAX_WORKERS, 5).
-record(local_state, {
  initial_download,
  depth_maximum_setting,
  depth_reach,
  website_hostname,
  website_http_type,
  urls_queue,
  workers,
  supervisor_pid,
  table_id_downloader_srv,
  initial_download_timeout_count
}).

start_link(WebsiteHostnameBin, WebsiteHttpTypeBin, DepthMaximumSetting, SupervisorPid) ->
  gen_server:start_link(
    ?MODULE,
    [WebsiteHostnameBin, WebsiteHttpTypeBin, DepthMaximumSetting, SupervisorPid],
    []
  ).

init([WebsiteHostnameBin, WebsiteHttpTypeBin, DepthMaximumSetting, SupervisorPid]) ->
  TableId = ets:new(downloader_srv, [set, public]),

  {ok, #local_state{
    initial_download = true,
    depth_maximum_setting = DepthMaximumSetting,
    depth_reach = 0,
    website_hostname = WebsiteHostnameBin,
    website_http_type = WebsiteHttpTypeBin,
    workers = [],
    supervisor_pid = SupervisorPid,
    table_id_downloader_srv = TableId,
    initial_download_timeout_count = 0
  }}.

handle_call(add_downloader_workers, _From, State) ->
  #local_state{
    workers = Workers,
    supervisor_pid = SupervisorPid
  } = State,

  if
    length(Workers) > 0 ->
      {reply, workers_already_added, State};

    true ->
      WorkersNew = [worker_starter(SupervisorPid) || _ <- lists:seq(1, ?MAX_WORKERS)],
      {reply, ok, State#local_state{workers = lists:append(WorkersNew, Workers)}}
  end;

handle_call(initial_download, _From, State) ->
  #local_state{
    initial_download = InitialDownload,
    website_hostname = WebsiteHostnameBin,
    website_http_type = WebsiteHttpTypeBin,
    workers = Workers,
    initial_download_timeout_count = InitialDownloadTimeoutCount
  } = State,

  case InitialDownload of
    true ->
      {WorkerPid, _Ref} = lists:nth(1, Workers),
      IndexPage = [WebsiteHttpTypeBin, <<"://">>, WebsiteHostnameBin, <<"/">>],
      Reply = gen_server:call(
        WorkerPid,
        {initial_download, iolist_to_binary(IndexPage)}
      ),

      case Reply of
        {ok, {UrlsList, FinishedDownloadProcessDepthStateNew}} ->
          {reply, ok, State#local_state{
            initial_download = false,
            website_http_type = WebsiteHttpTypeBin,
            depth_reach = FinishedDownloadProcessDepthStateNew,
            urls_queue = queue:from_list(UrlsList)
          }};

        timeout ->
          {reply, {timeout, InitialDownloadTimeoutCount}, State#local_state{
            initial_download_timeout_count = InitialDownloadTimeoutCount + 1
          }}
      end;

    false ->
      {reply, already_started, State}
  end.

handle_cast(coordinate_all_workers, #local_state{workers = Workers} = State) ->
  gogogo(Workers),
  {noreply, State};

handle_cast({gimme_next_page, {[], undefined}, Sender}, State) ->
  #local_state{urls_queue = UrlsQueue, table_id_downloader_srv = TableIdDownloaderSrv} = State,

  case pick_task_for_worker(TableIdDownloaderSrv, UrlsQueue, Sender) of
    {{value, {UrlPath, CurrentProcessedUrlDepthState}}, UrlsQueueNextState} ->
      gen_server:cast(Sender, {download, UrlPath, CurrentProcessedUrlDepthState}),
      {noreply, State#local_state{urls_queue = UrlsQueueNextState}};

    {empty, UrlsQueueNextState} ->
      gen_server:cast(Sender, retry),
      {noreply, State#local_state{urls_queue = UrlsQueueNextState}};

    {retry, UrlsQueueNextState} ->
      gen_server:cast(Sender, retry),
      {noreply, State#local_state{urls_queue = UrlsQueueNextState}}
  end;

handle_cast({gimme_next_page, {UrlsList, FinishedDownloadProcessDepthStateNew}, Sender}, State)
  when length(UrlsList) > 0 ->
    #local_state{
      depth_reach = DepthReach,
      urls_queue = UrlsQueue
    } = State,

    UrlsQueueNew = queue:from_list(UrlsList),
    UrlsQueueNextState = queue:join(UrlsQueue, UrlsQueueNew),

    gen_server:cast(Sender, retry),

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
  {noreply, State#local_state{urls_queue = UrlsQueueNextState}}.

handle_info({gimme_next_page_info, Sender}, State) ->
  #local_state{
    depth_maximum_setting = DepthMaximumSetting,
    depth_reach = DepthReach,
    urls_queue = UrlsQueue,
    table_id_downloader_srv = TableIdDownloaderSrv
  } = State,

  UrlsQueueLen = queue:len(UrlsQueue),

  if
    DepthReach =:= DepthMaximumSetting andalso UrlsQueueLen < 1 ->
      gen_server:cast(Sender, complete),
      {noreply, State};

    true ->
      case pick_task_for_worker(TableIdDownloaderSrv, UrlsQueue, Sender) of
        {{value, {UrlPath, CurrentProcessedUrlDepthState}}, UrlsQueueNextState} ->
          gen_server:cast(Sender, {download, UrlPath, CurrentProcessedUrlDepthState}),
          {noreply, State#local_state{urls_queue = UrlsQueueNextState}};

        {empty, UrlsQueueNextState} ->
          gen_server:cast(Sender, retry),
          {noreply, State#local_state{urls_queue = UrlsQueueNextState}};

        {retry, UrlsQueueNextState} ->
          gen_server:cast(Sender, retry),
          {noreply, State#local_state{urls_queue = UrlsQueueNextState}}
      end
  end;

handle_info({'DOWN', _Ref, process, Pid, _}, State) ->
  io:format("~p IS DOWN~n", [Pid]),
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
