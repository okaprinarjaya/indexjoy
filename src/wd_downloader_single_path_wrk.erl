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

  Headers = [
    {<<"User-Agent">>, <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:70.0) Gecko/20100101 Firefox/70.0">>}
  ],

  case hackney:request(get, UrlPath, Headers, <<>>, []) of
    {ok, 200, _RespHeaders, ClientRef} ->
      {ok, Body} = hackney:body(ClientRef),
      UrlSplit = string:split(UrlPath, "//"),
      myhelpers:save_page(Body, lists:nth(2, UrlSplit)),

      io:format("Done~n"),
      {stop, normal, State};

    {error, connect_timeout} ->
      io:format("Connect timeout~n"),
      {stop, normal, State};

    {error, timeout} ->
      io:format("Just timeout~n"),
      {stop, normal, State}
  end;

handle_cast(_AnyMessage, State) ->
  {noreply, State}.

handle_info(_AnyMessage, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
