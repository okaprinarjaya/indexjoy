-module(wd_downloader_single_path_wrk).

-behaviour(gen_server).

-export([start_link/1, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(UrlPath) ->
  gen_server:start_link(?MODULE, [UrlPath], []).

init([UrlPath]) ->
  gen_server:cast(self(), {download, UrlPath}),
  {ok, undefined}.

handle_call(_AnyMessage, _From, State) ->
  {reply, State}.

handle_cast({download, UrlPath}, State) ->
  io:format("Downloading: ~p~n", [UrlPath]),
  timer:sleep(5000),
  {stop, normal, State};

handle_cast(_AnyMessage, State) ->
  {noreply, State}.

handle_info(_AnyMessage, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
