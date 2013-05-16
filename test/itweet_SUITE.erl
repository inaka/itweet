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
    [ test_itweep/1
    , test_searcher/1
    , dummy_test/1
    , run_eunit/1
    ]).

-type config() :: [{atom(), term()}].

-spec all() -> [atom()].
all() ->
    [
      dummy_test
    , run_eunit
    , test_itweep
    , test_searcher
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    DD = ?config(data_dir, Config),
    PD = ?config(priv_dir, Config),
    io:format("data: ~p~npriv: ~p~n", [DD, PD]),
    ibrowse:start(),
    [{port, 10000} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    Config.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(dummy_test,  Config) -> Config;
init_per_testcase(run_eunit,   Config) -> Config;
init_per_testcase(test_itweep, Config) ->
    DD   = ?config(data_dir, Config),
    Port = ?config(port,     Config),
    MockServer = mock_server:start(
        [ {port,     Port}
        , {module,   itweep}
        , {function, test_itweep}
        , {data_dir, DD}
        ]),
    [ {mock_server, MockServer} | Config];
init_per_testcase(test_searcher, Config) ->
    DD   = ?config(data_dir, Config),
    Port = ?config(port,     Config),
    MockServer = mock_server:start(
        [ {port,     Port}
        , {module,   itweep_searcher}
        , {function, test_searcher}
        , {data_dir, DD}
        ]),
    [ {mock_server, MockServer} | Config].

-spec end_per_testcase(atom(), config()) -> config().
end_per_testcase(dummy_test,  Config) -> Config;
end_per_testcase(run_eunit,   Config) -> Config;
end_per_testcase(test_itweep, Config) ->
    MockServer = ?config(mock_server, Config),
    mock_server:stop(MockServer),
    Config;
end_per_testcase(test_searcher, Config) ->
    MockServer = ?config(mock_server, Config),
    mock_server:stop(MockServer),
    Config.

%%% Test Cases

-spec dummy_test(config()) -> _.
dummy_test(Config) ->
    DD = ?config(data_dir, Config),
    PD = ?config(priv_dir, Config),
    io:format("data: ~p~npriv: ~p~n", [DD, PD]),
    ok.

% This test case runs the former eunit tests.
-spec run_eunit(config()) -> _.
run_eunit(_Config) ->
    % ok = eunit:test(itweep_test),
    % ok = eunit:test(qa_itweep)
    ok.

% This case tests the itweet functionality.
-spec test_itweep(config()) -> _.
test_itweep(Config) ->
    DD = ?config(data_dir, Config),
    PD = ?config(priv_dir, Config),
    io:format("data: ~p~npriv: ~p~n", [DD, PD]),
    ok.

% This case tests the itweep_searcher functionality.
-spec test_searcher(config()) -> _.
test_searcher(Config) ->
    io:format("Starting test: test_searcher.~n"),

    DD     = ?config(data_dir, Config),
    PD     = ?config(priv_dir, Config),
    Port   = ?config(port,     Config),
    application:set_env(itweet, consumer_key,    "iO26pdVUB8AZfJSMWExaBA"),
    application:set_env(itweet, consumer_secret, "bzze1HOeMYJfVdWQIN1XagMwL4NBegnWFZNd1uGE"),
    Token  = "520898702-NTU3vKubs0R2QAnJLmXYPxL9bJ3dL8nB0z85KJU",
    Secret = "997jOo3jECfynlRwmkMMAcQsJsuDh5XL8m7yX3OZ2rM",
    io:format("data: ~p~npriv: ~p~n", [DD, PD]),

    MockServerUrl = "http://localhost:" ++ integer_to_list(Port) ++ "/",
    io:format("Mock server url: ~s~n", [MockServerUrl]),

    io:format("Starting test server.~n"),
    try
        {ok, Server} = itweep_searcher:start(test_searcher_cb, [],
            [ {token,  Token}
            , {secret, Secret}
            , {search_frequency, 800}
            , {url,    MockServerUrl}
            ]),
        io:format("Loading filters.~n"),
        itweep_searcher:call(Server, {load_filters, "#test"}),
        io:format("Sleeping.~n"),
        timer:sleep(30000),
        io:format("Getting responses.~n"),
        Statuses = itweep_searcher:get_statuses(Server),
        lists:map(fun(S) -> io:format("STATUS:~n~p~n~n", [S]) end, Statuses)
    catch
      _:E ->
        io:format("Error: ~p~nStacktrace: ~p", [E, erlang:get_stacktrace()])
    end,
    ok.
