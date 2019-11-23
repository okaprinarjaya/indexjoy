-module(wd_downloader_wrk).

-behaviour(gen_server).

-export([start_link/1]).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(local_state, {downloader_srv}).

start_link(DownloaderServerPid) ->
  gen_server:start_link(?MODULE, [DownloaderServerPid], []).

init([DownloaderServerPid]) ->
  {ok, #local_state{downloader_srv = DownloaderServerPid}}.

handle_call({initial_download, IndexPage}, _From, State) ->
  UrlsListMock = [
    <<"http://indonesia-kompeten.com/a">>,
    <<"http://indonesia-kompeten.com/b">>,
    <<"http://indonesia-kompeten.com/c">>,
    <<"http://indonesia-kompeten.com/d">>,
    <<"http://indonesia-kompeten.com/e">>
  ],
  io:format("Downloading: ~p~n", [IndexPage]),
  {reply, {ok, UrlsListMock}, State}.

handle_cast(gogogo, #local_state{downloader_srv = DownloaderServerPid} = State) ->
  gen_server:cast(DownloaderServerPid, {gimme_next_page, [], self()}),
  {noreply, State};

handle_cast(complete, State) ->
  io:format("Bye! I am dying~n"),
  {stop, shutdown, State};

handle_cast(retry, #local_state{downloader_srv = DownloaderServerPid} = State) ->
  erlang:send_after(5000, DownloaderServerPid, {gimme_next_page_info, self()}),
  {noreply, State};

handle_cast({download, Url}, #local_state{downloader_srv = DownloaderServerPid} = State) ->
  io:format("~p downloading: ~p...~n", [self(), Url]),
  gen_server:cast(DownloaderServerPid, {gimme_next_page, [], self()}),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
