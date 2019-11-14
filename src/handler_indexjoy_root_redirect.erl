-module(handler_indexjoy_root_redirect).

-export([init/2]).

-include_lib("hrl_common.hrl").

init(Req0, State) ->
  case ?INDEXJOY_JOY_AUTO_HTTPS_STATUS of
    enable_auto_https ->
      URI = cowboy_req:uri(Req0, #{port => 443, scheme => "https"}),
      Req = cowboy_req:reply(301, #{<<"location">> => URI}, Req0),

      {ok, Req, State};

    disable_auto_https ->
      handler_indexjoy_root:init(Req0, State)
  end.
