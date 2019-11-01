-module(handler_any_host_any_path).

-export([init/2]).

-define(STATIC_CUSTOMER_HOSTNAME_TO_IPADDR, #{
  <<"dananutama.com">> => <<"119.81.88.232">>,
  <<"insite.co.id">> => <<"103.247.9.42">>
}).

init(RequestFacilitator, State) ->
  case whos_coming_detection_by_user_agent(RequestFacilitator) of
    clientbrowser ->
      handle_clientbrowser(RequestFacilitator, State);
    googlebot ->
      handle_googlebot(RequestFacilitator, State)
    end.

whos_coming_detection_by_user_agent(RequestFacilitator) ->
  GoogleBotList = [
    <<"Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)">>,
    <<"Mozilla/5.0 AppleWebKit/537.36 (KHTML, like Gecko; compatible; Googlebot/2.1; +http://www.google.com/bot.html) Safari/537.36">>,
    <<"Mozilla/5.0 (Linux; Android 6.0.1; Nexus 5X Build/MMB29P) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.96 Mobile Safari/537.36 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)">>,
    <<"Googlebot/2.1 (+http://www.google.com/bot.html)">>,
    <<"Googlebot-Image/1.0">>,
    <<"Googlebot-News">>,
    <<"Googlebot-Video/1.0">>
  ],
  UserAgent = cowboy_req:header(<<"user-agent">>, RequestFacilitator),

  case lists:member(UserAgent, GoogleBotList) of
    true -> googlebot;
    false -> clientbrowser
  end.

handle_clientbrowser(RequestFacilitator, State) ->
  HostCustomer = cowboy_req:host(RequestFacilitator),

  case catch maps:get(HostCustomer, ?STATIC_CUSTOMER_HOSTNAME_TO_IPADDR) of
    {badkey, _} ->
      handle_clientbrowser_undefined_hostname(RequestFacilitator, State);
    {'EXIT',{{badkey, _}, _}} ->
      handle_clientbrowser_undefined_hostname(RequestFacilitator, State);

    MachineCustomer ->
      UserAgentFacilitator = cowboy_req:header(<<"user-agent">>, RequestFacilitator),
      PathRetrieveCustomer = cowboy_req:path(RequestFacilitator),
      RequestHeadersRetrieveCustomer = [
        {<<"Host">>, HostCustomer},
        {<<"User-Agent">>, UserAgentFacilitator},
        {<<"Pragma">>, <<"no-cache">>},
        {<<"Connection">>, <<"keep-alive">>}
      ],
      RequestCustomer = {get, PathRetrieveCustomer, RequestHeadersRetrieveCustomer, <<>>},

      {ok, ConnectionToCustomer} = hackney:connect(hackney_tcp, MachineCustomer, 80, []),
      {ok, StatusCodeCustomer, ResponseHeadersCustomer, ConnectionReferenceCustomer} =
        hackney:send_request(ConnectionToCustomer, RequestCustomer),
      {ok, BodyCustomer} = hackney:body(ConnectionReferenceCustomer),

      HeadersCustomer = hackney_headers:new(ResponseHeadersCustomer),
      ContentTypeCustomer = hackney_headers:get_value(<<"Content-Type">>, HeadersCustomer),

      ReplyFacilitator = cowboy_req:reply(
        StatusCodeCustomer,
        #{<<"content-type">> => ContentTypeCustomer},
        BodyCustomer,
        RequestFacilitator
      ),
      {ok, ReplyFacilitator, State}
  end.

handle_clientbrowser_undefined_hostname(RequestFacilitator, State) ->
  ReplyFacilitator = cowboy_req:reply(
    204,
    #{<<"content-type">> => <<"text/plain">>},
    <<>>,
    RequestFacilitator
  ),
  {ok, ReplyFacilitator, State}.

handle_googlebot(RequestFacilitator, State) ->
  ReplyFacilitator = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/html">>},
    <<"<html><head><title>Halo Search Engine Crawler!</title></head><body><h1>Halo Search Engine Crawler!</h1></body></html>">>,
    RequestFacilitator
  ),
  {ok, ReplyFacilitator, State}.
