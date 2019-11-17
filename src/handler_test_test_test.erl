-module(handler_test_test_test).

-export([init/2, fetch_page/0, create_any_tables_types/0, test_ets/1]).

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

fetch_page() ->
  {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(get, <<"http://indonesia-kompeten.com">>, [], <<>>, []),
  {ok, Body} = hackney:body(ClientRef),

  {match, List} = re:run(Body, <<"(<a .+>.+<\/a>)">>, [global, dotall, ungreedy, {capture, all_but_first, binary}]),
  lists:foreach(fun(Item) -> [Content] = Item, io:format("~s~n", [Content]) end, List).

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
