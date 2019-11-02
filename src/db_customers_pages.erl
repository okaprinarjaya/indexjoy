-module(db_customers_pages).

-export([init/0]).

-export([
  insert_page/3,
  select_page_filename_by/2,
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
        {type, bag}
      ],
      mnesia:create_table(customers_pages, TabDefs),
      io:format("Table: customers_pages - Created~n")
  end.

insert_page(CustomerHost, PagePath, PageFilename) ->
  FunTrx = fun() ->
    PageRecord = #customers_pages{
      customer_host = CustomerHost,
      page_path = PagePath,
      page_filename = PageFilename
    },
    mnesia:write(PageRecord)
  end,

  mnesia:transaction(FunTrx).

select_page_filename_by(CustomerHost, PagePath) ->
  FunTrx = fun() ->
    MatchHead = #customers_pages{
      customer_host = CustomerHost,
      page_path = PagePath,
      page_filename = '$3',
      _ = '_'
    },
    % Guard = {'>', '$3', 10},
    MatchFunction = {MatchHead, [], ['$3']},
    MatchSpec = [MatchFunction],

    mnesia:select(customers_pages, MatchSpec)
  end,

  mnesia:transaction(FunTrx).

select_all() ->
  FunTrx = fun() ->
    qlc:eval(
      qlc:q([X || X <- mnesia:table(customers_pages)])
    )
  end,

  mnesia:transaction(FunTrx).
