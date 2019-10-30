-module(handler_test_test_test).

-export([init/2]).

init(RequestFacilitator, State) ->
  ReplyFacilitator = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/html">>},
    <<"<html><head><title>Halo Customer!</title></head><body><h1>Halo Customer! Halo Good Bot!</h1></body></html>">>,
    RequestFacilitator
  ),
  {ok, ReplyFacilitator, State}.
