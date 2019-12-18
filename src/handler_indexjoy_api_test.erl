-module(handler_indexjoy_api_test).

-export([init/2]).

init(Req0, State) ->
  {ok, PostData, _} = cowboy_req:read_body(Req0),
  {JsonPostData} = jiffy:decode(PostData),

  Website = binary_to_list(proplists:get_value(<<"website">>, JsonPostData)),
  WebsiteStrSplit = string:split(Website, ":"),
  Protocol = lists:nth(1, WebsiteStrSplit),
  HostnameDirt = lists:nth(2, WebsiteStrSplit),
  Hostname = string:slice(HostnameDirt, 2, string:len(HostnameDirt)),
  Rows = proplists:get_value(<<"rows">>, JsonPostData),

  lists:foreach(
    fun({Item}) ->
      Id = proplists:get_value(<<"id">>, Item),
      Balance = proplists:get_value(<<"balance">>, Item),
      io:format("id: ~p, balance: ~p~n", [Id, Balance])
    end,
    Rows
  ),

  JsonResp = {[{<<"rows">>, [
    {[{<<"id">>, <<"ABC123">>}, {<<"balance">>, 50000000}]},
    {[{<<"id">>, <<"OKA321">>}, {<<"balance">>, 100000000}]},
    {[{<<"id">>, <<"WIWI456">>}, {<<"balance">>, 23000000}]},
    {[{<<"id">>, <<"WOWO654">>}, {<<"balance">>, 45000000}]}
  ]}]},

  %% START DOWNLOADER %%
  {ok, Sequence} = wd_download_manager_srv:start_new_downloader(Hostname, Protocol, 1),
  wd_download_manager_srv:download(Hostname, Sequence),

  Req = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"application/json">>},
    jiffy:encode(JsonResp),
    Req0
  ),
  {ok, Req, State}.
