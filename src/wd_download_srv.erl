-module(wd_download_srv).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(local_state, {urls_queue}).

start_link(ServerName) ->
  gen_server:start_link({local, list_to_atom(ServerName)}, ?MODULE, [], []).

init(_Args) ->
  {ok, #local_state{urls_queue = queue:new()}}.

handle_call(message, _From, State) ->
  {reply, message, State}.

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
