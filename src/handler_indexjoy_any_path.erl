-module(handler_indexjoy_any_path).

-export([init/2]).

-include_lib("hrl_common.hrl").

init(Request, State) ->
  PathBin = cowboy_req:path(Request),

  case proplists:get_value(PathBin, ?ROUTE_PATH_LIST_INDEXJOY) of
    undefined ->
      Html = <<"<html><head><title>404 Not Found</title></head><body><h1>404 Not Found</h1></body></html>">>,
      Reply = cowboy_req:reply(404, #{<<"content-type">> => <<"text/html">>}, Html, Request),
      {ok, Reply, State};

    Handler ->
      Handler:init(Request, State)
  end.
