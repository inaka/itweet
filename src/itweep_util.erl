-module(itweep_util).

-define(BASE_URL(X), "https://api.twitter.com/" ++ X).

-export([build_auth_url/2]).

%% @doc Helps to figure out the oauth token and url needed to authorize an
%% application.
%% First, create a twitter application, you should get the following information
%% OAuth settings
%%
%% Your application's OAuth settings. Keep the "Consumer secret" a secret. This
%% key should never be human-readable in your application.
%%
%% Consumer key  XX
%% Consumer secret  YY
%% Request token URL  https://api.twitter.com/oauth/request_token
%% Authorize URL  https://api.twitter.com/oauth/authorize
%% Access token URL  https://api.twitter.com/oauth/access_token
%% Callback URL  http://some.url.com
-spec build_auth_url(string(), string()) -> {string(), string()}.
build_auth_url(ConsumerKey, ConsumerSecret) ->
  {ok, OauthClient} = oauth_client:start_link(
    {ConsumerKey, ConsumerSecret, hmac_sha1}
  ),
  {ok, Token} = oauth_client:get_request_token(
    OauthClient, ?BASE_URL("oauth/request_token")
  ),
  TokenSecret = oauth:token_secret(
    oauth_client:request_token_params(OauthClient)
  ),
  ok = oauth_client:stop(OauthClient),
  {TokenSecret, ?BASE_URL("oauth/authenticate?oauth_token=" ++ Token)}.