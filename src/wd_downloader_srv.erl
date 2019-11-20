-module(wd_downloader_srv).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(local_state, {urls_queue, supervisor_pid}).

start_link(SupervisorPid) ->
  gen_server:start_link(?MODULE, [SupervisorPid], []).

init([SupervisorPid]) ->
  {ok, #local_state{urls_queue = queue:new(), supervisor_pid = SupervisorPid}}.

handle_call(add_downloader_worker, _From, #local_state{supervisor_pid = SupervisorPid} = State) ->
  supervisor:start_child(SupervisorPid, [self()]),
  {reply, ok, State}.

handle_cast(message, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, process, _Pid, _}, State) ->
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
