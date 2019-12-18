-module(wd_downloader_wrk).

-behaviour(gen_server).

-export([start_link/4]).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(local_state, {downloader_srv, depth_maximum_setting, website_hostname, website_http_type}).

start_link(DownloaderSrvPid, WebsiteHostnameBin, WebsiteHttpTypeBin, DepthMaximumSetting) ->
  gen_server:start_link(
    ?MODULE,
    [DownloaderSrvPid, WebsiteHostnameBin, WebsiteHttpTypeBin, DepthMaximumSetting],
    []
  ).

init([DownloaderSrvPid, WebsiteHostnameBin, WebsiteHttpTypeBin, DepthMaximumSetting]) ->
  gen_server:cast(DownloaderSrvPid, {downloader_wrk_confirm, self()}),

  {ok, #local_state{
    downloader_srv = DownloaderSrvPid,
    depth_maximum_setting = DepthMaximumSetting,
    website_hostname = WebsiteHostnameBin,
    website_http_type = WebsiteHttpTypeBin
  }}.

handle_call({initial_download, IndexPage}, _From, State) ->
  #local_state{website_hostname = WebsiteHostnameBin, website_http_type = WebsiteHttpTypeBin} = State,
  Headers = [
    {<<"User-Agent">>, <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:70.0) Gecko/20100101 Firefox/70.0">>}
  ],

  case hackney:request(get, IndexPage, Headers, <<>>, []) of
    {ok, 200, _RespHeaders, ClientRef} ->
      {ok, Body} = hackney:body(ClientRef),
      myhelpers:save_page(Body, <<"/index">>),

      case myhelpers:extract_urls(Body, WebsiteHostnameBin, WebsiteHttpTypeBin) of
        nomatch ->
          {reply, nomatch, State};

        UrlsList ->
          UrlsListWithDepthLevel = lists:map(fun(UrlPath) -> {UrlPath, 1} end, UrlsList),
          {reply, {ok, {UrlsListWithDepthLevel, 1}}, State}
      end;

    {error, timeout} ->
      {reply, timeout, State};

    {error, connect_timeout} ->
      {reply, timeout, State}
  end.

handle_cast({download, UrlPath, CurrentProcessedUrlDepthState}, State) ->
  #local_state{
    downloader_srv = DownloaderSrvPid,
    depth_maximum_setting = DepthMaximumSetting,
    website_hostname = WebsiteHostnameBin,
    website_http_type = WebsiteHttpTypeBin
  } = State,
  Page = iolist_to_binary([WebsiteHttpTypeBin, <<"://">>, WebsiteHostnameBin, UrlPath]),

  io:format("~p downloading: ~p, depth_state = ~p~n", [self(), Page, CurrentProcessedUrlDepthState]),

  Headers = [
    {<<"User-Agent">>, <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:70.0) Gecko/20100101 Firefox/70.0">>}
  ],

  case hackney:request(get, Page, Headers, <<>>, []) of
    {ok, 200, _RespHeaders, ClientRef} ->
      {ok, Body} = hackney:body(ClientRef),
      myhelpers:save_page(Body, UrlPath),

      if
        CurrentProcessedUrlDepthState < DepthMaximumSetting ->
          UrlsList = myhelpers:extract_urls(Body, WebsiteHostnameBin, WebsiteHttpTypeBin),

          if
            length(UrlsList) > 0 ->
              FinishedDownloadProcessDepthStateNew = CurrentProcessedUrlDepthState + 1,
              UrlsListWithDepthLevel = lists:map(
                fun(Url) -> {Url, FinishedDownloadProcessDepthStateNew} end,
                UrlsList
              ),
              Message = {gimme_next_page, {UrlsListWithDepthLevel, FinishedDownloadProcessDepthStateNew}, self()},
              gen_server:cast(DownloaderSrvPid, Message),
              {noreply, State};

            true ->
              %% Because the current page doesn't contain new urls so the urls extractor return empty list.
              %% For the rest we only need to process next url at the queue until the queue become empty.
              gen_server:cast(DownloaderSrvPid, {gimme_next_page, {[], undefined}, self()}),
              {noreply, State}
          end;

        true ->
          %% We don't need to get new urls anymore because we're already reach the max depth.
          %% For the rest we only need to process next url at the queue until the queue become empty.
          gen_server:cast(DownloaderSrvPid, {gimme_next_page, {[], undefined}, self()}),
          {noreply, State}
      end;

    {error, timeout} ->
      gen_server:cast(DownloaderSrvPid, {requeue, UrlPath, CurrentProcessedUrlDepthState, self()}),
      {noreply, State};

    {error, connect_timeout} ->
      gen_server:cast(DownloaderSrvPid, {requeue, UrlPath, CurrentProcessedUrlDepthState, self()}),
      {noreply, State}
  end;

handle_cast(gogogo, #local_state{downloader_srv = DownloaderSrvPid} = State) ->
  gen_server:cast(DownloaderSrvPid, {gimme_next_page, {[], undefined}, self()}),
  {noreply, State};

handle_cast(retry, #local_state{downloader_srv = DownloaderSrvPid} = State) ->
  erlang:send_after(5000, DownloaderSrvPid, {gimme_next_page_info, self()}),
  {noreply, State}.

handle_info(_AnyMessage, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
