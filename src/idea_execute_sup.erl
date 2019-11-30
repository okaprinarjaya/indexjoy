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
    strategy => one_for_one,
    intensity => 3,
    period => 10
  },
  ChildSpecs = [
    #{
      id => wd_download_manager_srv_id,
      start => {wd_download_manager_srv, start_link, []},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => [wd_download_manager_srv]
    }
  ],

  {ok, {SupFlags, ChildSpecs}}.
