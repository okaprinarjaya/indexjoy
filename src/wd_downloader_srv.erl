-module(wd_downloader_srv).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(local_state, {initial_download, urls_queue, workers, supervisor_pid}).

start_link(SupervisorPid) ->
  gen_server:start_link(?MODULE, [SupervisorPid], []).

init([SupervisorPid]) ->
  {ok, #local_state{initial_download = true, workers = [], supervisor_pid = SupervisorPid}}.

handle_call(add_downloader_worker, _From, State) ->
  #local_state{workers = Workers, supervisor_pid = SupervisorPid} = State,
  {ok, Pid} = supervisor:start_child(SupervisorPid, [self()]),
  Ref = erlang:monitor(process, Pid),
  NewWorker = [{Pid, Ref}],

  {reply, ok, State#local_state{workers = lists:append(NewWorker, Workers)}};

handle_call(initial_download, _From, #local_state{initial_download = InitialDownload, workers = Workers} = State) ->
  case InitialDownload of
    true ->
      WorkerPid = lists:nth(1, Workers),
      {ok, UrlsList} = gen_server:call(WorkerPid, {initial_download, "http://indonesia-kompeten.com/"}),

      {reply, ok, State#local_state{initial_download = true, urls_queue = queue:from_list(UrlsList)}};

    false ->
      {reply, already_started, State}
  end.

handle_cast(coordinate_all_workers, State) ->
  {noreply, State};

handle_cast({done_downloading_a_page, _Sender}, State) ->
  {noreply, State};

handle_cast(_Any, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, process, _Pid, _}, State) ->
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
