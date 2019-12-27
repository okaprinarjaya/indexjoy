-module(handler_indexjoy_path_root).

-export([init/2]).

init(Request, State) ->
  {atomic, Rows} = db_customers_seo_pages:select_all(),
  Fn = fun(Row) ->
    {CustomerHostAndPath, _HostOnly, PageFilename} = Row,
    [
      <<"<li>">>,
      CustomerHostAndPath,
      <<", ">>,
      PageFilename,
      <<"</li>">>
    ]
  end,

  HtmlUList = [Fn(Row) || Row <- Rows],
  HTML = [
    <<"<html><head><title>Welcome to IndexJoy!</title></head><body>">>,
    <<"<h1>Halo Customer! Halo Good Bot! Welcome to IndexJoy!</h1>">>,
    <<"<ul>">>,
    HtmlUList,
    <<"</ul>">>,
    <<"</body></html>">>
  ],

  Reply = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/html">>},
    HTML,
    Request
  ),
  {ok, Reply, State}.
