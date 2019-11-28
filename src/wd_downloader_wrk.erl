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

-record(local_state, {downloader_srv, depth_maximum_setting, website_hostname, website_http_type}).

start_link(DownloaderServerPid, DepthMaximumSetting) ->
  gen_server:start_link(?MODULE, [DownloaderServerPid, DepthMaximumSetting], []).

init([DownloaderServerPid, DepthMaximumSetting]) ->
  {ok, #local_state{downloader_srv = DownloaderServerPid, depth_maximum_setting = DepthMaximumSetting}}.

handle_call({initial_download, IndexPage, WebsiteHostnameBin, WebsiteHttpTypeBin}, _From, State) ->
  Headers = [
    {<<"User-Agent">>, <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:70.0) Gecko/20100101 Firefox/70.0">>}
  ],
  case hackney:request(get, IndexPage, Headers, <<>>, []) of
    {ok, _StatusCode, _RespHeaders, ClientRef} ->
      {ok, Body} = hackney:body(ClientRef),
      myhelpers:save_page(Body, <<"/index">>),

      UrlsList = myhelpers:extract_urls(Body, WebsiteHostnameBin, WebsiteHttpTypeBin),
      UrlsListWithDepthLevel = lists:map(fun(Url) -> {Url, 1} end, UrlsList),

      {reply, {ok, {UrlsListWithDepthLevel, 1}}, State#local_state{
        website_hostname = WebsiteHostnameBin,
        website_http_type = WebsiteHttpTypeBin
      }};

    {error,timeout} ->
      io:format("Timeout ~n"),
      {reply, timeout, State#local_state{
        website_hostname = WebsiteHostnameBin,
        website_http_type = WebsiteHttpTypeBin
      }}
  end.

handle_cast({gogogo, WebsiteHostnameBin, WebsiteHttpTypeBin}, State) ->
  #local_state{downloader_srv = DownloaderServerPid} = State,
  gen_server:cast(DownloaderServerPid, {gimme_next_page, {[], undefined}, self()}),
  {noreply, State#local_state{website_hostname = WebsiteHostnameBin, website_http_type = WebsiteHttpTypeBin}};

handle_cast(complete, State) ->
  {stop, shutdown, State};

handle_cast(retry, #local_state{downloader_srv = DownloaderServerPid} = State) ->
  erlang:send_after(5000, DownloaderServerPid, {gimme_next_page_info, self()}),
  {noreply, State};

handle_cast({download, UrlPath, CurrentProcessedUrlDepthState}, State) ->
  #local_state{
    downloader_srv = DownloaderServerPid,
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
    {ok, _StatusCode, _RespHeaders, ClientRef} ->
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
              gen_server:cast(DownloaderServerPid, Message),
              {noreply, State};

            true ->
              %% We can't reply with message containing new urls list. Yes because the current page doesn't
              %% contain new urls anymore so the urls extractor return empty list.
              %% For the rest we only need to process next url at the queue until the queue become empty.
              gen_server:cast(DownloaderServerPid, {gimme_next_page, {[], undefined}, self()}),
              {noreply, State}
          end;

        true ->
          %% We don't need to get new urls anymore because we're already reach the max depth.
          %% For the rest we only need to process next url at the queue until the queue become empty.
          gen_server:cast(DownloaderServerPid, {gimme_next_page, {[], undefined}, self()}),
          {noreply, State}
      end;

    {error,timeout} ->
      %% It should do requeue the timed out url here
      io:format("Timeout ~n"),
      {noreply, State}
  end.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
