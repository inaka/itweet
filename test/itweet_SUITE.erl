-module(itweet_SUITE).

-export(
    [ all/0
    , init_per_suite/1
    , end_per_suite/1
    , init_per_testcase/2
    , end_per_testcase/2
    , init_per_group/2
    , end_per_group/2
    ]).

-spec all() -> [atom()].
all() ->
    [Fun || {Fun, 1} <- module_info(exports),
        not lists:member(Fun,
            [ init_per_suite
            , end_per_suite
            , module_info
            ])].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(test_itweet, Config) ->
    inets:start(),
    {ok, Pid} = inets:start(httpd,
        [ {port,          8080}
        , {server_name,   "itweet_test_httpd"}
        , {server_root,   "./log"}
        , {document_root, "./www"}
        , {bind_address,  "localhost"}
        ]),
    [{httpd_pid, Pid} | Config];
init_per_testcase(test_itweep_searcher, Config) ->
    inets:start(),
    {ok, Pid} = inets:start(httpd,
        [ {port,          8080}
        , {server_name,   "itweet_test_httpd"}
        , {server_root,   "./log"}
        , {document_root, "./www"}
        , {bind_address,  "localhost"}
        ]),
    [{httpd_pid, Pid} | Config].

end_per_testcase(test_itweet, Config) ->
    Pid = ?config(httpd_pid, Config),
    ok = inets:stop(httpd, Pid).
    inets:stop(),
    Config;
end_per_testcase(test_itweep_searcher, Config) ->
    Pid = ?config(httpd_pid, Config),
    ok = inets:stop(httpd, Pid).
    inets:stop(),
    Config.

%% Test Cases

% This case tests the itweet functionality.
test_itweet(Config) ->
    ok.

% This case tests the itweep_searcher functionality.
% It
test_itweep_searcher(Config) ->
    ok.
