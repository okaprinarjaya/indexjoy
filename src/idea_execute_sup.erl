%%%-------------------------------------------------------------------
%% @doc idea_execute top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(idea_execute_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{
    strategy => one_for_all,
    intensity => 10,
    period => 10
  },
  ChildSpecs = [
    {
      wd_download_manager_srv_id,
      {wd_download_manager_srv, start_link, []},
      permanent,
      5000,
      worker,
      [wd_download_manager_srv]
    }
  ],

  {ok, {SupFlags, ChildSpecs}}.
