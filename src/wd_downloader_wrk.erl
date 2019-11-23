-module(wd_downloader_wrk).

-behaviour(gen_server).

-export([start_link/2]).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(local_state, {downloader_srv, depth_maximum_setting}).

start_link(DownloaderServerPid, DepthMaximumSetting) ->
  gen_server:start_link(?MODULE, [DownloaderServerPid, DepthMaximumSetting], []).

init([DownloaderServerPid, DepthMaximumSetting]) ->
  {ok, #local_state{downloader_srv = DownloaderServerPid, depth_maximum_setting = DepthMaximumSetting}}.

handle_call({initial_download, IndexPage}, _From, State) ->
  UrlsListMock = [
    {<<"http://indonesia-kompeten.com/a">>, 1},
    {<<"http://indonesia-kompeten.com/b">>, 1},
    {<<"http://indonesia-kompeten.com/c">>, 1},
    {<<"http://indonesia-kompeten.com/d">>, 1},
    {<<"http://indonesia-kompeten.com/e">>, 1}
  ],
  io:format("Downloading: ~p~n", [IndexPage]),
  {reply, {ok, {UrlsListMock, 1}}, State}.

handle_cast(gogogo, #local_state{downloader_srv = DownloaderServerPid} = State) ->
  gen_server:cast(DownloaderServerPid, {gimme_next_page, {[], undefined}, self()}),
  {noreply, State};

handle_cast(complete, State) ->
  io:format("Bye! I am dying~n"),
  {stop, shutdown, State};

handle_cast(retry, #local_state{downloader_srv = DownloaderServerPid} = State) ->
  erlang:send_after(5000, DownloaderServerPid, {gimme_next_page_info, self()}),
  {noreply, State};

handle_cast({download, Url, CurrentProcessedUrlDepthState}, State) ->
  io:format("~p downloading: ~p, depth_state = ~p~n", [self(), Url, CurrentProcessedUrlDepthState]),
  #local_state{downloader_srv = DownloaderServerPid, depth_maximum_setting = DepthMaximumSetting} = State,

  if
    CurrentProcessedUrlDepthState < DepthMaximumSetting ->
      %% Parse the current downloaded page to get new urls if any
      %% For simulation purpose, let's say current downloaded page contain new urls,
      %% then we have to increase the depth value by one
      FinishedDownloadProcessDepthStateNew = CurrentProcessedUrlDepthState + 1,
      UrlsListMock = [
        {<<"http://indonesia-kompeten.com/a/a1">>, FinishedDownloadProcessDepthStateNew},
        {<<"http://indonesia-kompeten.com/b/b1">>, FinishedDownloadProcessDepthStateNew},
        {<<"http://indonesia-kompeten.com/c/c1">>, FinishedDownloadProcessDepthStateNew},
        {<<"http://indonesia-kompeten.com/d/d1">>, FinishedDownloadProcessDepthStateNew},
        {<<"http://indonesia-kompeten.com/e/e1">>, FinishedDownloadProcessDepthStateNew}
      ],
      Message = {gimme_next_page, {UrlsListMock, FinishedDownloadProcessDepthStateNew}, self()},
      gen_server:cast(DownloaderServerPid, Message),
      {noreply, State};

    true ->
      gen_server:cast(DownloaderServerPid, {gimme_next_page, {[], undefined}, self()}),
      {noreply, State}
  end.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
