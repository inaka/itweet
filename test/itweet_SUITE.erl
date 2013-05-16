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
    ]).

-type config() :: [{atom(), term()}].

-spec all() -> [atom()].
all() ->
    [ test_itweep
    , test_searcher
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    DD = ?config(data_dir, Config),
    PD = ?config(priv_dir, Config),
    io:format("data: ~p~npriv: ~p~n", [DD, PD]),
    ibrowse:start(),
    itweet:start(),
    [{port, 10000} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    ibrowse:stop(),
    itweet:stop(),
    Config.

-spec init_per_testcase(atom(), config()) -> config().
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
end_per_testcase(test_itweep, Config) ->
    MockServer = ?config(mock_server, Config),
    mock_server:stop(MockServer),
    Config;
end_per_testcase(test_searcher, Config) ->
    MockServer = ?config(mock_server, Config),
    mock_server:stop(MockServer),
    Config.

%%% Test Cases

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

    DD     = ?config(data_dir,      Config),
    PD     = ?config(priv_dir,      Config),
    Port   = ?config(port,          Config),
    {ok, Token}  = application:get_env(itweet, access_token),
    {ok, Secret} = application:get_env(itweet, access_secret),
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

        io:format("These are the received responses:~n"),
        lists:map(fun(S) -> io:format("STATUS:~n~p~n~n", [S]) end, Statuses)
    catch
      _:E ->
        io:format("Error: ~p~nStacktrace: ~p", [E, erlang:get_stacktrace()])
    end,
    ok.
