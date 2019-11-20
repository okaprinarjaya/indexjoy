-module(wd_downloader_wrk_sup).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Id, MFA) ->
  supervisor:start_link({local, Id}, ?MODULE, MFA).

init({M, F, A}) ->
  SupFlags = #{
    strategy => simple_one_for_one,
    intensity => 10,
    period => 10
  },
  ChildSpecs = [
    {wd_downloader_wrk_id, {M, F, A}, temporary, 5000, worker, [M]}
  ],

  {ok, {SupFlags, ChildSpecs}}.
