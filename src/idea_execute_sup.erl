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
  MFAForDownloaderSinglePath = {wd_downloader_single_path_wrk, start_link, []},
  ChildSpecs = [
    #{
      id => wd_download_manager_srv_id,
      start => {wd_download_manager_srv, start_link, []},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => [wd_download_manager_srv]
    },
    #{
      id => wd_downloader_single_path_wrk_sup_id,
      start => {wd_downloader_single_path_wrk_sup, start_link, [MFAForDownloaderSinglePath]},
      restart => transient,
      shutdown => 5000,
      type => supervisor,
      modules => [wd_downloader_single_path_wrk_sup]
    }
  ],

  {ok, {SupFlags, ChildSpecs}}.
