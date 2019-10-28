-module(idea_execute_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  AnyPath = {'_', handler_any_host_any_path, []},
  AnyHost = {'_', [AnyPath]},
  Routes = [AnyHost],

  Dispatch = cowboy_router:compile(Routes),
  {ok, _} = cowboy:start_clear(http, [{port, 80}], #{env => #{dispatch => Dispatch}}),

  idea_execute_sup:start_link().

stop(_State) ->
  ok.
