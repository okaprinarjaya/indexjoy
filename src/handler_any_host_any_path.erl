-module(handler_any_host_any_path).

-export([init/2]).

-include_lib("hrl_common.hrl").

init(RequestFacilitator, State) ->
  case whos_coming_detection_by_user_agent(RequestFacilitator) of
    'client-browser' ->
      handle_client_browser(RequestFacilitator, State);
    'search-engine-crawler' ->
      handle_search_engine_crawler_bot(RequestFacilitator, State)
    end.

whos_coming_detection_by_user_agent(RequestFacilitator) ->
  case cowboy_req:header(<<"user-agent">>, RequestFacilitator) of
    undefined ->
      'search-engine-crawler';
    UserAgent ->
      io:format("USER AGENT: ~p~n", [UserAgent]),
      case lists:member(UserAgent, ?SEARCH_ENGINE_CRAWLER_BOT_LIST) of
        true -> 'search-engine-crawler';
        false -> 'client-browser'
      end
  end.

handle_client_browser(RequestFacilitator, State) ->
  HostCustomer = cowboy_req:host(RequestFacilitator),

  case catch maps:get(HostCustomer, ?STATIC_CUSTOMER_HOSTNAME_TO_IPADDR) of
    {badkey, _} ->
      handle_client_browser_undefined_hostname(RequestFacilitator, State);
    {'EXIT',{{badkey, _}, _}} ->
      handle_client_browser_undefined_hostname(RequestFacilitator, State);

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

handle_client_browser_undefined_hostname(RequestFacilitator, State) ->
  ReplyFacilitator = cowboy_req:reply(
    204,
    #{<<"content-type">> => <<"text/plain">>},
    <<>>,
    RequestFacilitator
  ),
  {ok, ReplyFacilitator, State}.

handle_search_engine_crawler_bot(RequestFacilitator, State) ->
  CustomerHost = cowboy_req:host(RequestFacilitator),
  HostCustomerAndPath = [CustomerHost, cowboy_req:path(RequestFacilitator)],

  case db_customers_seo_pages:select_page_by(iolist_to_binary(HostCustomerAndPath)) of
    {ok, BinPageFilename} ->
      {ok, CustomersSeoPagesBasePath} = application:get_env(idea_execute, index_joy_customers_seo_pages_path),
      FullPathFile = [CustomersSeoPagesBasePath, <<"/">>, CustomerHost, <<"/">>, BinPageFilename],

      case file:read_file(iolist_to_binary(FullPathFile)) of
        {ok, BinFileContent} ->
          ReplyFacilitator = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, BinFileContent, RequestFacilitator),
          {ok, ReplyFacilitator, State};

        {error, _} ->
          handle_search_engine_crawler_bot_404(RequestFacilitator, State)
      end;

    notfound ->
      %% It should request and render the original page.
      %% But for now, temporarily will response 404.
      handle_search_engine_crawler_bot_404(RequestFacilitator, State)
  end.

handle_search_engine_crawler_bot_404(Request, State) ->
  Html = <<"<html><head><title>404 Not Found</title></head><body><h1>404 Not Found</h1></body></html>">>,
  Reply = cowboy_req:reply(404, #{<<"content-type">> => <<"text/html">>}, Html, Request),

  {ok, Reply, State}.
