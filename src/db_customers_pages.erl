-module(db_customers_pages).

-export([init/0]).

-export([
  insert_page/3,
  select_file_by/2,
  select_all/0
]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("hrl_customers_pages.hrl").

init() ->
  case catch mnesia:table_info(customers_pages, version) of
    {{_, _}, []} ->
      io:format("customers_pages - already exists~n");

    Any when Any =:= {'EXIT',{aborted,{no_exists,customers_pages,version}}}; Any =:= {aborted,{no_exists,customers_pages,version}} ->
      TabDefs = [
        {attributes, record_info(fields, customers_pages)},
        {disc_copies, [node()]},
        {type, bag}
      ],
      mnesia:create_table(customers_pages, TabDefs),

      io:format("customers_pages - created~n")
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

select_file_by(CustomerHost, PagePath) ->
  FunTrx = fun() ->
    MatchHead = #customers_pages{customer_host = CustomerHost, page_path = PagePath, page_filename = '$3', _ = '_'},
    % Guard = {},
    Result = '$3',
    MatchFunction = {MatchHead, [], [Result]},
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
