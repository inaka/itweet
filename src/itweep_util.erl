%%% @author Marcelo Gornstein <marcelo@inakanetworks.com>
%%% @copyright (C) 2012 Inaka Labs SRL
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%
%%%
%%% Sample run:
%%%
%%% Create an app at twitter, get your consumer key and consumer secret. Then: 
%%%
%%% > itweep_util:build_auth_url("consumer_key", "consumer_secret").
%%% [{consumer, {"consumer_key", "consumer_secret", hmac_sha1}}, {tokensecret,"token_secret"}, {authurl,"authurl_to_hit"}]
%%%
%%% Now go and hit the authurl, you should get an access_token. Then:
%%%
%%% > itweep_util:get_auth({"consumer_key", "consumer_secret",hmac_sha1}, "access_token", "token_secret").
%%% [{tokenaccess,"token_access"},
%%% {tokenaccsecret,"token_access_secret"}]
-module(itweep_util).
-author("Marcelo Gornstein <marcelo@inakanetworks.com>").
-vsn("1.0").
-github("https://github.com/inaka/itweet").
-homepage("http://inaka.github.com/itweet/").
-license("Apache License 2.0").

%%% Macros
-define(BASE_URL(X), "https://api.twitter.com/" ++ X).

%%% Public API
-export([build_auth_url/2, get_auth/3]).

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
-spec build_auth_url(string(), string()) -> tuple().
build_auth_url(ConsumerKey, ConsumerSecret) ->
  Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
  {ok, OauthClient} = oauth_client:start_link(Consumer),
  {ok, Token} = oauth_client:get_request_token(OauthClient, ?BASE_URL("oauth/request_token")),
  TokenSecret = oauth:token_secret(oauth_client:request_token_params(OauthClient)),
  ok = oauth_client:stop(OauthClient),
  [
    {consumer, Consumer},
    {tokensecret, TokenSecret},
    {authurl, ?BASE_URL("oauth/authenticate?oauth_token=" ++ Token)}
  ].

%% @doc After calling build_auth_url/2 and hitting the web (and retrieving your
%% token, call this function with the consumer, the token you just got, and the
%% secret)
-spec get_auth(tuple(), string(), string()) -> tuple().  
get_auth(Consumer, Token, Secret) ->
  {ok, OauthClient} = oauth_client:start_link({Consumer, oauth:token_param(Token, oauth:token_secret_param(Secret, []))}),
  Result =
    case oauth_client:get_access_token(OauthClient, ?BASE_URL("oauth/access_token")) of
      ok ->
        AParams = oauth_client:access_token_params(OauthClient),
        [{tokenaccess, oauth:token(AParams)}, {tokenaccsecret, oauth:token_secret(AParams)}];
      {http_error, {"401", _Headers, _Body}} -> unauthorized
    end,
  ok = oauth_client:stop(OauthClient),
  Result.
