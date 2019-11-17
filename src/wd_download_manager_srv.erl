-module(wd_download_manager_srv).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_new_download/1]).

-record(local_state, {number_of_website_downloader = 0}).

start_link() ->
  gen_server:start_link({local, wd_download_manager_server}, ?MODULE, [], []).

init(_Args) ->
  {ok, #local_state{number_of_website_downloader = 0}}.

handle_call({start_wd_download_srv, ServerName}, _From, State) ->
  ChildSpecs = {
    wd_download_srv_id,
    {wd_download_srv, start_link, [ServerName]},
    permanent,
    5000,
    worker,
    [wd_download_srv]
  },
  {ok, _Pid} = supervisor:start_child(idea_execute_sup, ChildSpecs),

  {reply, {ok, <<"wd_download_srv - started">>}, State};

handle_call({start_wd_download_sup, {M, F, A}}, _From, State) ->
  ChildSpecs = #{
    id => wd_download_sup_id,
    start => {wd_download_sup, start_link, [{M,F,A}]},
    restart => temporary,
    shutdown => 10000,
    type => supervisor,
    modules => [wd_download_sup]
  },
  {ok, _Pid} = supervisor:start_child(idea_execute_sup, ChildSpecs),

  {reply, message,  State}.

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

start_new_download(ServerName) ->
  {ok, Message} = gen_server:call(wd_download_manager_server, {start_wd_download_srv, ServerName}),
  io:format("RESULT: ~p~n", [Message]).
