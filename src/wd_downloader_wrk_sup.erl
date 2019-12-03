-module(wd_downloader_wrk_sup).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(DownloaderWrkSupServerName, MFA) ->
  supervisor:start_link({local, DownloaderWrkSupServerName}, ?MODULE, MFA).

init({M, F, A}) ->
  SupFlags = #{
    strategy => simple_one_for_one,
    intensity => 3,
    period => 10
  },
  ChildSpecs = [
    #{
      id => wd_downloader_wrk_id,
      start => {M, F, A},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => [M]
    }
  ],

  {ok, {SupFlags, ChildSpecs}}.
