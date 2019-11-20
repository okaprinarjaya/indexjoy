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

init(DownloaderServerPid) ->
  {ok, #local_state{downloader_srv = DownloaderServerPid}}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.