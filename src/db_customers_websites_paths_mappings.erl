-module(db_customers_websites_paths_mappings).

-export([init/0]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("hrl_tables_structures.hrl").

init() ->
  case catch mnesia:table_info(customers_websites_paths_mappings, version) of
    {{_, _}, []} ->
      io:format("Table: customers_websites_paths_mappings - Already exists~n");

    Fail when Fail =:= {'EXIT',{aborted,{no_exists,customers_websites_paths_mappings,version}}}; Fail =:= {aborted,{no_exists,customers_websites_paths_mappings,version}} ->
      TabDefs = [
        {attributes, record_info(fields, customers_websites_paths_mappings)},
        {disc_copies, [node()]},
        {type, set}
      ],
      mnesia:create_table(customers_websites_paths_mappings, TabDefs),
      io:format("Table: customers_websites_paths_mappings - Created~n")
  end.
