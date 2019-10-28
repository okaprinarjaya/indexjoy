-module(handler_any_host_any_path).

-export([init/2]).

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
  MachineCustomer = <<"185.199.110.153">>,
  HostCustomer = <<"okaprinarjaya.github.io">>,
  PathRetrieveCustomer = cowboy_req:path(RequestFacilitator),
  RequestHeadersRetrieveCustomer = [
    {<<"Host">>, HostCustomer},
    {<<"User-Agent">>, <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/77.0.3865.120 Safari/537.36">>},
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
  {ok, ReplyFacilitator, State}.

handle_googlebot(RequestFacilitator, State) ->
  ReplyFacilitator = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/html">>},
    <<"<html><head><title>Halo Search Engine Crawler!</title></head><body><h1>Halo Search Engine Crawler!</h1></body></html>">>,
    RequestFacilitator
  ),
  {ok, ReplyFacilitator, State}.
