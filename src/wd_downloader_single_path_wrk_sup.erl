-module(wd_downloader_single_path_wrk_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(MFA) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, MFA).

init({M, F, A}) ->
  SupFlags = #{
    strategy => simple_one_for_one,
    intensity => 3,
    period => 10
  },
  ChildSpecs = [
    #{
      id => wd_downloader_single_path_wrk_id,
      start => {M, F, A},
      restart => temporary,
      shutdown => 5000,
      type => worker,
      modules => [M]
    }
  ],

  {ok, {SupFlags, ChildSpecs}}.

