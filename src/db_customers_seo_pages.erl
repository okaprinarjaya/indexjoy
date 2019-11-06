-module(db_customers_seo_pages).

-export([init/0]).

-export([
  save_page/2,
  select_page_by/1,
  select_all_by/1,
  select_all/0
]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("hrl_tables_structures.hrl").

init() ->
  case catch mnesia:table_info(customers_seo_pages, version) of
    {{_, _}, []} ->
      io:format("Table: customers_seo_pages - Already exists~n");

    Fail when Fail =:= {'EXIT',{aborted,{no_exists,customers_seo_pages,version}}}; Fail =:= {aborted,{no_exists,customers_seo_pages,version}} ->
      TabDefs = [
        {attributes, record_info(fields, customers_seo_pages)},
        {disc_copies, [node()]},
        {type, set}
      ],
      mnesia:create_table(customers_seo_pages, TabDefs),
      io:format("Table: customers_seo_pages - Created~n")
  end.

save_page(CustomerHostAndPath, PageFilename) ->
  FunTrx = fun() ->
    CustomerHostOnly = lists:nth(1, string:split(CustomerHostAndPath, "/")),
    PageRecord = #customers_seo_pages{
      customer_host_and_path = CustomerHostAndPath,
      customer_host_only = CustomerHostOnly,
      page_filename = PageFilename
    },
    mnesia:write(PageRecord)
  end,
  mnesia:transaction(FunTrx).

select_page_by(CustomerHostAndPath) ->
  {atomic, List} = mnesia:transaction(fun() -> mnesia:read(customers_seo_pages, CustomerHostAndPath) end),
  if
    List =/= [] ->
      {customers_seo_pages, _CustomerHostAndPath, _CustomerHost, BinPageFilename } = lists:nth(1, List),
      {ok, BinPageFilename};
    true -> notfound
  end.

select_all_by(CustomerHostOnly) ->
  FunTrx = fun() ->
    QH = qlc:q(
      [{A, B, C} || #customers_seo_pages{customer_host_and_path = A, customer_host_only = B, page_filename = C} <- mnesia:table(customers_seo_pages), B =:= CustomerHostOnly]
    ),
    qlc:eval(QH)
  end,
  mnesia:transaction(FunTrx).

select_all() ->
  FunTrx = fun() ->
    qlc:eval(
      qlc:q([{A, B, C} || #customers_seo_pages{customer_host_and_path = A, customer_host_only = B, page_filename = C} <- mnesia:table(customers_seo_pages)])
    )
  end,
  mnesia:transaction(FunTrx).
