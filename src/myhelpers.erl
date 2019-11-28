-module(myhelpers).

-export([extract_urls/3, save_page/2, create_filename/1]).

extract_urls(Contents, WebsiteHostnameBin, WebsiteHttpTypeBin) ->
  Opts = [global, dotall, ungreedy, {capture, all_but_first, binary}],
  {match, List} = re:run(Contents, <<"<a [a-z-=\" ]*href=\"([a-z0-9-:/.\"]+)\"">>, Opts),
  List2 = lists:filter(
    fun([Url]) ->
      ContainWebsiteHostname = string:find(Url, WebsiteHostnameBin),
      Url =/= <<"#">> andalso ContainWebsiteHostname =/= nomatch
    end,
    List
  ),
  lists:map(
    fun([Url]) ->
      WebsiteUrlPrefix = iolist_to_binary([WebsiteHttpTypeBin, <<"://">>, WebsiteHostnameBin]),
      UrlLen = string:length(Url),
      StartLen = string:length(WebsiteUrlPrefix),
      EndLen = UrlLen - StartLen,

      string:slice(Url, StartLen + 1, EndLen)
    end,
    List2
  ).

save_page(Contents, UrlPath) ->
  BasePath = <<"/Users/okaprinarjaya/Oprek/Erlang-Oprek-Tiga/INDEX_JOY_CUSTOMERS_SEO_PAGES/downloaded-website">>,
  Filename = iolist_to_binary([BasePath, <<"/">>, create_filename(UrlPath), <<".html">>]),

  case file:write_file(binary_to_list(Filename), Contents) of
    ok -> ok;
    {error, Reason} -> io:format("ERROR WRITING FILE! ~p~n", [Reason])
  end.

create_filename(UrlPath) ->
  ReplaceWithDashIoL = string:replace(UrlPath, "/", "-", all),
  DashedBin = iolist_to_binary(ReplaceWithDashIoL),
  Fck = string:slice(DashedBin, 1, string:length(DashedBin)),
  Fck.

