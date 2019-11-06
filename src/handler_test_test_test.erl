-module(handler_test_test_test).

-export([init/2]).

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
