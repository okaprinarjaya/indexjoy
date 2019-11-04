-module(db_customers_pages).

-export([init/0]).

-export([
  save_page/2,
  select_page_by/1,
  select_all_by/1,
  select_all/0
]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("hrl_customers_pages.hrl").

init() ->
  case catch mnesia:table_info(customers_pages, version) of
    {{_, _}, []} ->
      io:format("Table: customers_pages - Already exists~n");

    Fail when Fail =:= {'EXIT',{aborted,{no_exists,customers_pages,version}}}; Fail =:= {aborted,{no_exists,customers_pages,version}} ->
      TabDefs = [
        {attributes, record_info(fields, customers_pages)},
        {disc_copies, [node()]},
        {type, set}
      ],
      mnesia:create_table(customers_pages, TabDefs),
      io:format("Table: customers_pages - Created~n")
  end.

save_page(CustomerHostAndPath, PageFilename) ->
  FunTrx = fun() ->
    CustomerHostOnly = lists:nth(1, string:split(CustomerHostAndPath, "/")),
    PageRecord = #customers_pages{
      customer_host_and_path = CustomerHostAndPath,
      customer_host_only = CustomerHostOnly,
      page_filename = PageFilename
    },
    mnesia:write(PageRecord)
  end,
  mnesia:transaction(FunTrx).

select_page_by(CustomerHostAndPath) ->
  mnesia:transaction(fun() -> mnesia:read(customers_pages, CustomerHostAndPath) end).

select_all_by(CustomerHostOnly) ->
  FunTrx = fun() ->
    QH = qlc:q(
      [{A, B, C} || #customers_pages{customer_host_and_path = A, customer_host_only = B, page_filename = C} <- mnesia:table(customers_pages), B =:= CustomerHostOnly]
    ),
    qlc:eval(QH)
  end,
  mnesia:transaction(FunTrx).

select_all() ->
  FunTrx = fun() ->
    qlc:eval(
      qlc:q([{A, B, C} || #customers_pages{customer_host_and_path = A, customer_host_only = B, page_filename = C} <- mnesia:table(customers_pages)])
    )
  end,
  mnesia:transaction(FunTrx).
