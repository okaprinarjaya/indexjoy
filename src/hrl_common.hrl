%% SSL CERTS
-define(SSL_CERTS_INSITE_CO_ID(PrivDir), [
  {cacertfile, PrivDir ++ "/ssl/www_insite_co_id.crt"},
  {certfile, PrivDir ++ "/ssl/www_insite_co_id.pem"},
  {keyfile, PrivDir ++ "/ssl/www_insite_co_id.key"}
]).

-define(SSL_CERTS_DANANUTAMA_COM(PrivDir), [
  {cacertfile, PrivDir ++ "/ssl/www_dananutama_com.crt"},
  {certfile, PrivDir ++ "/ssl/www_dananutama_com.pem"},
  {keyfile, PrivDir ++ "/ssl/www_dananutama_com.key"}
]).

-define(SSL_CERTS_INDEXJOY_COM(PrivDir), [
  {cacertfile, PrivDir ++ "/ssl/www_indexjoy_com.crt"},
  {certfile, PrivDir ++ "/ssl/www_indexjoy_com.pem"},
  {keyfile, PrivDir ++ "/ssl/www_indexjoy_com.key"}
]).

-define(SNI_HOSTS(PrivDir), [
  {"insite.co.id", ?SSL_CERTS_INSITE_CO_ID(PrivDir)},
  {"dananutama.com", ?SSL_CERTS_DANANUTAMA_COM(PrivDir)},
  {"indexjoy.com", ?SSL_CERTS_INDEXJOY_COM(PrivDir)}
]).

%% ROUTES SPECIAL FOR INDEXJOY
-define(INDEXJOY_JOY_AUTO_HTTPS_STATUS, disable_auto_https).

-define(ROUTE_PATH_LIST_INDEXJOY_FOR_REDIRECT, [
  {<<"/">>, handler_indexjoy_root, []},
  {<<"/api/test">>, handler_indexjoy_api_test, []}
]).
% -define(ROUTE_PATH_LIST_INDEXJOY_FOR_HTTPS, [
%   {<<"/">>, handler_indexjoy_root, []},
%   {<<"/api/test">>, handler_indexjoy_api_test, []}
% ]).

-define(ROUTE_INDEXJOY_HOST_REDIRECT, {<<"indexjoy.com">>, ?ROUTE_PATH_LIST_INDEXJOY_FOR_REDIRECT}).
% -define(ROUTE_INDEXJOY_HOST_FOR_HTTPS, {<<"indexjoy.com">>, ?ROUTE_PATH_LIST_INDEXJOY_FOR_HTTPS}).

%% SEARCH ENGINE CRAWLER BOT USER AGENT
-define(SEARCH_ENGINE_CRAWLER_BOT_LIST, [
  <<"Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)">>,
  <<"Mozilla/5.0 AppleWebKit/537.36 (KHTML, like Gecko; compatible; Googlebot/2.1; +http://www.google.com/bot.html) Safari/537.36">>,
  <<"Mozilla/5.0 (Linux; Android 6.0.1; Nexus 5X Build/MMB29P) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.96 Mobile Safari/537.36 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)">>,
  <<"Googlebot/2.1 (+http://www.google.com/bot.html)">>,
  <<"Googlebot-Image/1.0">>,
  <<"Googlebot-News">>,
  <<"Googlebot-Video/1.0">>,
  <<"Mozilla/5.0 (compatible; Google-Structured-Data-Testing-Tool +https://search.google.com/structured-data/testing-tool)">>,
  <<"DuckDuckBot/1.0; (+http://duckduckgo.com/duckduckbot.html)">>,
  <<"msnbot/2.0b (+http://search.msn.com/msnbot.htm)">>,
  <<"Mozilla/5.0 (compatible; bingbot/2.0; +http://www.bing.com/bingbot.htm)">>,
  <<"Mozilla/5.0 (iPhone; CPU iPhone OS 7_0 like Mac OS X) AppleWebKit/537.51.1 (KHTML, like Gecko) Version/7.0 Mobile/11A465 Safari/9537.53 (compatible; bingbot/2.0; +http://www.bing.com/bingbot.htm)">>,
  <<"Mozilla/5.0 (Windows Phone 8.1; ARM; Trident/7.0; Touch; rv:11.0; IEMobile/11.0; NOKIA; Lumia 530) like Gecko (compatible; bingbot/2.0; +http://www.bing.com/bingbot.htm)">>,
  <<"Mozilla/5.0 (seoanalyzer; compatible; bingbot/2.0; +http://www.bing.com/bingbot.htm)">>,
  <<"Mozilla/5.0 (Windows Phone 8.1; ARM; Trident/7.0; Touch; rv:11.0; IEMobile/11.0; NOKIA; Lumia 530) like Gecko BingPreview/1.0b">>,
  <<"Mozilla/5.0 (iPhone; CPU iPhone OS 7_0 like Mac OS X) AppleWebKit/537.51.1 (KHTML, like Gecko) Version/7.0 Mobile/11A465 Safari/9537.53 BingPreview/1.0b">>,
  <<"facebookexternalhit/1.1 (+http://www.facebook.com/externalhit_uatext.php)">>,
  <<"adreview/1.0">>,
  <<"cortex/1.0">>,
  <<"WhatsApp/2.19.274 A">>,
  <<"Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; GTB7.2; MRSPUTNIK 2, 4, 0, 490; .NET4.0C)">>
]).

%% CUSTOMER HOSTNAME TO IP ADDR MAPPING
-define(STATIC_CUSTOMER_HOSTNAME_TO_IPADDR, #{
  <<"dananutama.com">> => {<<"119.81.88.232">>, disable_auto_https},
  <<"insite.co.id">> => {<<"103.247.9.42">>, enable_auto_https}
}).
