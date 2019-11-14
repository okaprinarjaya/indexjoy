-module(handler_any_host_any_path_redirect).

-export([init/2]).

init(Req0, State) ->
  URI = cowboy_req:uri(Req0, #{port => 443, scheme => "https"}),
  Req = cowboy_req:reply(301, #{<<"location">> => URI}, Req0),
  {ok, Req, State}.
