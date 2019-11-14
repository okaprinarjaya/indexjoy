-module(handler_any_host_any_path_redirect).

-export([init/2]).

-include_lib("hrl_common.hrl").

init(Req0, State) ->
  HostCustomer = cowboy_req:host(Req0),

  case catch maps:get(HostCustomer, ?STATIC_CUSTOMER_HOSTNAME_TO_IPADDR) of
    {badkey, _} ->
      handler_any_host_any_path:handle_404(Req0, State);
    {'EXIT',{{badkey, _}, _}} ->
      handler_any_host_any_path:handle_404(Req0, State);

    {_, AutoHttpsStatus} ->
      case AutoHttpsStatus of
        enable_auto_https ->
          URI = cowboy_req:uri(Req0, #{port => 443, scheme => "https"}),
          Req = cowboy_req:reply(301, #{<<"location">> => URI}, Req0),

          {ok, Req, State};

        disable_auto_https ->
          handler_any_host_any_path:init(Req0, State)
      end
  end.
