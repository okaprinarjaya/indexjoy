-module(handler_test_test_test).

-export([init/2, fetch_page/3, create_any_tables_types/0, test_ets/1]).

init(Request, State) ->
  QS = cowboy_req:parse_qs(Request),
  CustomerHostAndPath = proplists:get_value(<<"chp">>, QS),
  PageFilename = proplists:get_value(<<"pfn">>, QS),

  if
    length(QS) > 0 ->
      db_customers_seo_pages:save_page(CustomerHostAndPath, PageFilename);
    true -> pass
  end,

  HTML = [
    <<"<html><head><title>Halo Customer - Test</title></head><body>">>,
    <<"<h1>Halo Customer! Halo Good Bot! Welcome to IndexJoy! This is a test.</h1>">>,
    <<"</body></html>">>
  ],

  Reply = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/html">>},
    HTML,
    Request
  ),
  {ok, Reply, State}.

fetch_page(Website, WebsiteHostname, WebsiteHttpType) ->
  case hackney:request(get, Website, [{<<"User-Agent">>, <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:70.0) Gecko/20100101 Firefox/70.0">>}], <<>>, []) of
    {ok, _StatusCode, _RespHeaders, ClientRef} ->
      {ok, Body} = hackney:body(ClientRef),
      List = myhelpers:extract_urls(Body, WebsiteHostname, WebsiteHttpType),

      % io:format("~p~n", [List]);
      lists:foreach(fun(Url) -> io:format("~s~n", [Url]) end, List);

    {error,timeout} ->
      io:format("yaaaaaahhhhhh ssiit timeout! ~n")
  end.

create_any_tables_types() ->
  lists:foreach(fun test_ets/1, [set, ordered_set, bag, duplicate_bag]).

test_ets(Mode) ->
  TableId = ets:new(test, [Mode]),
  ets:insert(TableId, {a, 1}),
  ets:insert(TableId, {b, 2}),
  ets:insert(TableId, {a, 1}),
  ets:insert(TableId, {a, 3}),
  List = ets:tab2list(TableId),
  io:format("~-13w => ~p~n", [Mode, List]),
  ets:delete(TableId).
