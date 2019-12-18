-module(idea_execute_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("hrl_common.hrl").

start(_StartType, _StartArgs) ->
  %% Routes Setup
  RoutesForRedirect = [?ROUTE_INDEXJOY_HOST_REDIRECT, {'_', [{'_', handler_any_host_any_path_redirect, []}]}],
  RoutesForHttps = [{'_', [{'_', handler_any_host_any_path, []}]}],

  DispatchForRedirect = cowboy_router:compile(RoutesForRedirect),
  DispatchForHttps = cowboy_router:compile(RoutesForHttps),

  %% SSL HTTPS Setup
  PrivDir = code:priv_dir(idea_execute),
  ConfigTls = [{port, 443}, {sni_hosts, ?SNI_HOSTS(PrivDir)}],

  %% Start Web Server
  {ok, _} = cowboy:start_clear(http, [{port, 80}], #{env => #{dispatch => DispatchForRedirect}}),
  {ok, _} = cowboy:start_tls(https, ConfigTls, #{env => #{dispatch => DispatchForHttps}}),

  init_db(),
  idea_execute_sup:start_link().

stop(_State) ->
  ok.

init_db() ->
  io:format("Preparing schema~n"),

  case mnesia:create_schema([node()]) of
    {error,{_,{already_exists,_}}} ->
      io:format("Schema: IndexJoyDb - Already exists~n");
    ok ->
      io:format("Schema: IndexJoyDb - Created~n")
  end,

  io:format("Starting Mnesia~n"),
  application:ensure_started(mnesia),

  io:format("Preparing tables~n"),
  db_customers_seo_pages:init(),
  db_customers_websites_paths_mappings:init().
