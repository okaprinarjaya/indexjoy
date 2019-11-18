-module(wd_download_manager_srv).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_new_downloader/1, ets_lookup/1]).

-record(local_state, {table_id}).

start_link() ->
  gen_server:start_link({local, wd_download_manager_server}, ?MODULE, [], []).

init(_Args) ->
  TableId = ets:new(download_manager, [set, public]),
  {ok, #local_state{table_id = TableId}}.

handle_call({start_wd_downloader_srv, ServerName, SupervisorPid}, _From, State) ->
  Id = "wd_downloader_srv_" ++ ServerName,
  ChildSpecs = #{
    id => list_to_atom(Id),
    start => {wd_downloader_srv, start_link, [SupervisorPid]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [wd_downloader_srv]
  },
  {ok, Pid} = supervisor:start_child(idea_execute_sup, ChildSpecs),

  #local_state{table_id = TableId} = State,
  ets:insert(TableId, {list_to_binary(ServerName), Pid}),

  {reply, ok, State};

handle_call({start_wd_downloader_sup, MFA}, _From, State) ->
  ChildSpecs = #{
    id => wd_downloader_sup_id,
    start => {wd_downloader_sup, start_link, [MFA]},
    restart => temporary,
    shutdown => 10000,
    type => supervisor,
    modules => [wd_downloader_sup]
  },
  {ok, Pid} = supervisor:start_child(idea_execute_sup, ChildSpecs),

  {reply, {ok, Pid},  State};

handle_call({ets_lookup, ServerNameBinary}, _From, State) ->
  #local_state{table_id = TableId} = State,
  Result = ets:lookup(TableId, ServerNameBinary),

  {reply, {ok, Result}, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, process, _Pid, _}, State) ->
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

start_new_downloader(ServerName) ->
  MFA = {wd_downloader_wrk, start_link, []},
  {ok, SupervisorPid} = gen_server:call(wd_download_manager_server, {start_wd_downloader_sup, MFA}),
  gen_server:call(wd_download_manager_server, {start_wd_downloader_srv, ServerName, SupervisorPid}),
  ok.

ets_lookup(ServerName) ->
  ServerNameBinary = list_to_binary(ServerName),
  {ok, Result} = gen_server:call(wd_download_manager_server, {ets_lookup, ServerNameBinary}),
  Result.
