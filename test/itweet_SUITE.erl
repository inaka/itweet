-module(itweet_SUITE).

-include_lib("common_test/include/ct.hrl").

-export(
    [ all/0
    , init_per_suite/1
    , end_per_suite/1
    , init_per_testcase/2
    , end_per_testcase/2
    ]).
-export(
    [ test_itweet/1
    , test_itweep_searcher/1
    ]).
-export([test_test/1]).

-type config() :: [{atom(), term()}].

-spec all() -> [atom()].
%all() ->
%    [Fun || {Fun, 1} <- module_info(exports),
%        not lists:member(Fun,
%            [ init_per_suite
%            , end_per_suite
%            , init_per_testcase
%            , end_per_testcase
%            , init_per_group
%            , end_per_group
%            , module_info
%            ])].
all() -> [test_test].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    DD = ?config(data_dir, Config),
    PD = ?config(priv_dir, Config),
    ServerConfig1 =
        [ {port,          8080}
        , {server_name,   "itweet_test_httpd"}
        , {server_root,   PD}
        , {document_root, DD}
        , {bind_address,  "localhost"}
        ],
    ServerConfig2 =
        [ {port,          8080}
        , {server_name,   "itweet_test_httpd"}
        , {server_root,   PD}
        , {document_root, DD}
        , {bind_address,  "localhost"}
        ],
    [ {test_itweet_config,          ServerConfig1}
    , {test_itweep_searcher_config, ServerConfig2}
    | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    Config.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(test_test, Config) -> Config;
init_per_testcase(test_itweet, Config) ->
    inets:start(),
    ServerConfig = ?config(test_itweet_config, Config),
    {ok, Pid} = inets:start(httpd, ServerConfig),
    [{httpd_pid, Pid} | Config];
init_per_testcase(test_itweep_searcher, Config) ->
    inets:start(),
    ServerConfig = ?config(test_itweep_searcher_config, Config),
    {ok, Pid} = inets:start(httpd, ServerConfig),
    [{httpd_pid, Pid} | Config].

-spec end_per_testcase(atom(), config()) -> config().
end_per_testcase(test_test, Config) -> Config;
end_per_testcase(test_itweet, Config) ->
    Pid = ?config(httpd_pid, Config),
    ok = inets:stop(httpd, Pid),
    inets:stop(),
    Config;
end_per_testcase(test_itweep_searcher, Config) ->
    Pid = ?config(httpd_pid, Config),
    ok = inets:stop(httpd, Pid),
    inets:stop(),
    Config.

%% Test Cases

-spec test_test(config()) -> _.
test_test(Config) ->
    DD = ?config(data_dir, Config),
    PD = ?config(priv_dir, Config),
    io:format("data: ~p~npriv: ~p~n", [DD, PD]),
    ok.

% This case tests the itweet functionality.
-spec test_itweet(config()) -> _.
test_itweet(_Config) ->
    ok.

% This case tests the itweep_searcher functionality.
-spec test_itweep_searcher(config()) -> _.
test_itweep_searcher(_Config) ->
    ok.
