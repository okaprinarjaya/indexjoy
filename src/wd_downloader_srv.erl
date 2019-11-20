-module(wd_downloader_srv).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MAX_WORKERS, 5).
-record(local_state, {initial_download, website_hostname, urls_queue, workers, supervisor_pid}).

start_link(SupervisorPid, WebsiteHostname) ->
  gen_server:start_link(?MODULE, [SupervisorPid, WebsiteHostname], []).

init([SupervisorPid, WebsiteHostname]) ->
  {ok, #local_state{
    initial_download = true,
    website_hostname = WebsiteHostname,
    workers = [],
    supervisor_pid = SupervisorPid
  }}.

handle_call(add_downloader_workers, _From, State) ->
  #local_state{workers = Workers, supervisor_pid = SupervisorPid} = State,
  if
    length(Workers) > 0 ->
      {reply, workers_already_added, State};
    true ->
      WorkersNew = [worker_starter(SupervisorPid) || _ <- lists:seq(1, ?MAX_WORKERS)],
      {reply, ok, State#local_state{workers = lists:append(WorkersNew, Workers)}}
  end;

handle_call(initial_download, _From, State) ->
  #local_state{initial_download = InitialDownload, website_hostname = WebsiteHostname, workers = Workers} = State,
  case InitialDownload of
    true ->
      {WorkerPid, _Ref} = lists:nth(1, Workers),
      IndexPage = "http://" ++ WebsiteHostname ++ "/",
      {ok, UrlsList} = gen_server:call(WorkerPid, {initial_download, IndexPage}),
      {reply, ok, State#local_state{initial_download = false, urls_queue = queue:from_list(UrlsList)}};

    false ->
      {reply, already_started, State}
  end;

handle_call(coordinate_all_workers, _From, #local_state{workers = Workers} = State) ->
  gogogo(Workers),
  {reply, ok, State}.

handle_cast({gimme_next_page, _UrlsList, Sender}, #local_state{urls_queue = UrlsQueue} = State) ->
  case queue:out(UrlsQueue) of
    {{value, Url}, UrlsQueueNew} ->
      gen_server:cast(Sender, {download, Url}),
      {noreply, State#local_state{urls_queue = UrlsQueueNew}};

    {empty, UrlsQueueNew} ->
      {noreply, State#local_state{urls_queue = UrlsQueueNew}}
  end.

handle_info({'DOWN', _Ref, process, _Pid, _}, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

worker_starter(SupervisorPid) ->
  {ok, Pid} = supervisor:start_child(SupervisorPid, [self()]),
  Ref = erlang:monitor(process, Pid),
  {Pid, Ref}.

gogogo([]) -> ok;
gogogo(Workers) ->
  [Worker | WorkersTail] = Workers,
  {WorkerPid, _Ref} = Worker,
  gen_server:cast(WorkerPid, gogogo),
  gogogo(WorkersTail).
