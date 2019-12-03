-module(myhelpers).

-export([extract_urls/3, save_page/2, create_filename/1]).

extract_urls(Contents, WebsiteHostnameBin, WebsiteHttpTypeBin) ->
  Opts = [global, dotall, ungreedy, {capture, all_but_first, binary}],
  IndexPage = iolist_to_binary([<<"http">>, <<"://">>, WebsiteHostnameBin]),
  IndexPageEndedSlash = iolist_to_binary([<<"http">>, <<"://">>, WebsiteHostnameBin, <<"/">>]),

  case re:run(Contents, <<"<a [a-z-=\" ]*href=\"([a-z0-9-:/.\"]+)\"">>, Opts) of
    {match, List} ->
      List2 = lists:filter(
        fun([Url]) ->
          ContainWebsiteHostname = string:find(Url, WebsiteHostnameBin),
          Url =/= <<"#">> andalso
          Url =/= <<>> andalso
          Url =/= IndexPage andalso
          Url =/= IndexPageEndedSlash andalso
          ContainWebsiteHostname =/= nomatch
        end,
        List
      ),

      lists:map(
        fun([Url]) ->
          WebsiteUrlPrefix = iolist_to_binary([WebsiteHttpTypeBin, <<"://">>, WebsiteHostnameBin]),
          UrlLen = string:length(Url),
          StartLen = string:length(WebsiteUrlPrefix),
          EndLen = UrlLen - StartLen,
          UrlPath = string:slice(Url, StartLen + 1, EndLen),

          case string:prefix(UrlPath, "/") of
            nomatch ->
             iolist_to_binary([<<"/">>, UrlPath]);
            _Str ->
              UrlPath
          end
        end,
        List2
      );

    nomatch ->
      nomatch
  end.

save_page(Contents, UrlPath) ->
  BasePath = <<"/Users/okaprinarjaya/Oprek/Erlang-Oprek-Tiga/INDEX_JOY_CUSTOMERS_SEO_PAGES/downloaded-website">>,
  Filename = iolist_to_binary([BasePath, <<"/">>, create_filename(UrlPath), <<".html">>]),

  case file:write_file(binary_to_list(Filename), Contents) of
    ok -> ok;
    {error, Reason} -> io:format("Error writing file: ~p~n", [Reason])
  end.

create_filename(UrlPath) ->
  ReplaceWithDashIoL = string:replace(UrlPath, "/", "-", all),
  DashedBin = iolist_to_binary(ReplaceWithDashIoL),
  string:slice(DashedBin, 1, string:length(DashedBin)).
