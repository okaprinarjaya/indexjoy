-module(wd_downloader_srv).

-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MAX_WORKERS, 5).
-record(local_state, {
  initial_download,
  depth_maximum_setting,
  depth_reach,
  website_hostname,
  urls_queue,
  workers,
  supervisor_pid
}).

start_link(WebsiteHostname, DepthMaximumSetting, SupervisorPid) ->
  gen_server:start_link(?MODULE, [WebsiteHostname, DepthMaximumSetting, SupervisorPid], []).

init([WebsiteHostname, DepthMaximumSetting, SupervisorPid]) ->
  {ok, #local_state{
    initial_download = true,
    depth_maximum_setting = DepthMaximumSetting,
    depth_reach = 0,
    website_hostname = WebsiteHostname,
    workers = [],
    supervisor_pid = SupervisorPid
  }}.

handle_call(add_downloader_workers, _From, State) ->
  #local_state{
    depth_maximum_setting = DepthMaximumSetting,
    workers = Workers,
    supervisor_pid = SupervisorPid
  } = State,

  if
    length(Workers) > 0 ->
      {reply, workers_already_added, State};

    true ->
      WorkersNew = [worker_starter(SupervisorPid, DepthMaximumSetting) || _ <- lists:seq(1, ?MAX_WORKERS)],
      {reply, ok, State#local_state{workers = lists:append(WorkersNew, Workers)}}
  end;

handle_call(initial_download, _From, State) ->
  #local_state{
    initial_download = InitialDownload,
    website_hostname = WebsiteHostname,
    workers = Workers
  } = State,

  case InitialDownload of
    true ->
      {WorkerPid, _Ref} = lists:nth(1, Workers),
      IndexPage = "http://" ++ WebsiteHostname ++ "/",
      {ok, {UrlsList, FinishedDownloadProcessDepthStateNew}} = gen_server:call(
        WorkerPid,
        {initial_download, IndexPage}
      ),

      {reply, ok, State#local_state{
        initial_download = false,
        depth_reach = FinishedDownloadProcessDepthStateNew,
        urls_queue = queue:from_list(UrlsList)
      }};

    false ->
      {reply, already_started, State}
  end.

handle_cast(coordinate_all_workers, #local_state{workers = Workers} = State) ->
  gogogo(Workers),
  {noreply, State};

handle_cast({gimme_next_page, {[], undefined}, Sender}, #local_state{urls_queue = UrlsQueue} = State) ->
  UrlsQueueNextState = give_task_to_worker(UrlsQueue, Sender),
  {noreply, State#local_state{urls_queue = UrlsQueueNextState}};

handle_cast({gimme_next_page, {UrlsList, FinishedDownloadProcessDepthStateNew}, Sender}, State)
  when length(UrlsList) > 0 ->
    #local_state{
      depth_maximum_setting = DepthMaximumSetting,
      depth_reach = DepthReach,
      urls_queue = UrlsQueue
    } = State,

    if
      FinishedDownloadProcessDepthStateNew =< DepthMaximumSetting ->
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

      true ->
        gen_server:cast(Sender, retry),
        {noreply, State}
    end.

handle_info({gimme_next_page_info, Sender}, State) ->
  io:format("~p Retrying to ask a task to downloader srv after X secs~n", [Sender]),
  #local_state{
    depth_maximum_setting = DepthMaximumSetting,
    depth_reach = DepthReach,
    urls_queue = UrlsQueue
  } = State,
  UrlsQueueLen = queue:len(UrlsQueue),

  if
    DepthReach =:= DepthMaximumSetting andalso UrlsQueueLen < 1 ->
      gen_server:cast(Sender, complete),
      {noreply, State};

    true ->
      UrlsQueueNextState = give_task_to_worker(UrlsQueue, Sender),
      {noreply, State#local_state{urls_queue = UrlsQueueNextState}}
  end;

handle_info({'DOWN', _Ref, process, Pid, _}, State) ->
  io:format("~p IS DOWN~n", [Pid]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

worker_starter(SupervisorPid, DepthMaximumSetting) ->
  {ok, Pid} = supervisor:start_child(SupervisorPid, [self(), DepthMaximumSetting]),
  Ref = erlang:monitor(process, Pid),
  {Pid, Ref}.

gogogo([]) -> ok;
gogogo(Workers) ->
  [Worker | WorkersTail] = Workers,
  {WorkerPid, _Ref} = Worker,
  gen_server:cast(WorkerPid, gogogo),
  gogogo(WorkersTail).

give_task_to_worker(UrlsQueue, Sender) ->
  case queue:out(UrlsQueue) of
    {{value, {Url, CurrentProcessedUrlDepthState}}, UrlsQueueNextState} ->
      gen_server:cast(Sender, {download, Url, CurrentProcessedUrlDepthState}),
      UrlsQueueNextState;

    {empty, UrlsQueueNextState} ->
      gen_server:cast(Sender, retry),
      UrlsQueueNextState
  end.
