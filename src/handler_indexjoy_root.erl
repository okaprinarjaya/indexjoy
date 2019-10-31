-module(handler_indexjoy_root).

-export([init/2]).

init(Request, State) ->
  Reply = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/html">>},
    <<"<html><head><title>Welcome to IndexJoy!</title></head><body><h1>Halo Customer! Halo Good Bot! Welcome to IndexJoy!</h1></body></html>">>,
    Request
  ),
  {ok, Reply, State}.
