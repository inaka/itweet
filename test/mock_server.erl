%%% @ doc
%%%
%%% This is a small TCP server which can handle a single client, used to mock
%%% the Twitter server responses.
%%% It is started as part of itweet's common test cases.
%%%
%%% e.g.
%%% itweet_test_server:start([{data_dir,"itweet_SUITE_data"},{module,m1},{function,f1},{port, 10101}]).
%%%
%%% To start it, use itweet_test_server:start/1.
%%% It takes an Options parameter.
%%%
%%% To stop it, used itweet_test_server:stop/1-2
%%%
%%% The Options parameter passed to start/1 is a proplist.
%%% It may contain the following properties:
%%%
%%% {module, Module} and {fucntion, Function} are used to tell the server what
%%% testcase we are running, and hence which files it should read the responses
%%% from.
%%% It will then use files with names module_function_<N>.txt to get responses
%%% to the twitter API queries it receieves, where N is a numer starting at 1
%%% and incrementing with each query. They are obligatory and undefined behavior
%%% will result if they are missing from the Options list.
%%%
%%% {data_dir, DataDir} is the test's data directory, as passed by Common Test
%%% to the test process in its Config parameter. It is also obligatory.
%%%
%%% {name, Name} is optional and may contain an atom, under which the server
%%% process will register itself. If it is missing the server will register
%%% under it's module name.

-module(mock_server).

-export(
    [ start/1
    , stop/1
    , init/1
    , loop/1
    ]).

-record(state,
    { server_socket = undefined :: gen_tcp:socket() | undefined
    , client_socket = undefined :: gen_tcp:socket() | undefined
    , module        = none :: atom()
    , function      = none :: atom()
    , counter       = 0 :: non_neg_integer()
    , data_dir      = "" :: string()
    }).

%% This is the initial launching function.
%% Returns the PID of the server process.
-spec start(proplists:proplist()) -> pid().
start(Options) ->
    spawn_link(?MODULE, init, [Options]).

%% Stops an itweet_test_server registered under another name.
-spec stop(atom()) -> quit.
stop(Server) ->
    Server ! stop.

%% Initialization function. Creates the server's initial state.
-spec init(proplists:proplist()) -> _.
init(Options) ->
    Port     = proplists:get_value(port,     Options, 10000),
    Module   = proplists:get_value(module,   Options, itweep_searcher),
    Function = proplists:get_value(function, Options, itweep_searcher_test),
    DataDir  = proplists:get_value(data_dir, Options, "itweet_SUITE_data"),

    % Open the listening socket.
    {ok, ServerSocket} = gen_tcp:listen(0,
        [ {reuseaddr, true}
        , {port,      Port}
        , {packet,    0}
        , {active,    true}
        , binary
        ]),

    State = #state{
        server_socket = ServerSocket,
        module        = Module,
        function      = Function,
        counter       = 1,
        data_dir      = DataDir
    },
    % Start the recieve/reply loop.
    loop(State).

%% Main server loop.
%% Waits for incoming messaged on the client socket and replies with a
%% predetermined message read from a file.
%% The file is determined by the number of response in the query/reply sequence.
-spec loop(#state{}) -> _.
loop(State) ->
    ServerSocket = State#state.server_socket,
    % Get the client connection.

    case gen_tcp:accept(ServerSocket, 1000) of
        {ok, ClientSocket} ->
            receive
                {tcp, ClientSocket, Data} ->
                    io:format("mock server: socket recieved: ~p~n", [Data]),
                    % Here is where it gets the correct response based on it's state.
                    Response = get_response(State),
                    timer:sleep(1000),
                    ok = gen_tcp:send(ClientSocket, Response),
                    ok = gen_tcp:close(ClientSocket),
                    % Increment the counter so on the next request, it will get the
                    % next response.
                    NewState = State#state{counter = State#state.counter + 1},
                    loop(NewState);
                {tcp_closed, ClientSocket}->
                    io:format("mock server: socket ~p closed~n", [ClientSocket]),
                    loop(State);
                {tcp_error, ClientSocket, Reason} ->
                    io:format("mock server: error on socket ~p reason: ~p~n", [ClientSocket, Reason]),
                    loop(State);
                M ->
                    io:format("mock server: unrecognized message: ~p~n", [M]),
                    ClientSocket = State#state.client_socket
            end;
        {error, timeout} ->
            receive
                stop ->
                    io:format("mock server: received stop signal~n"),
                    quit(State)
            after 0 ->
                ok
            end,
            loop(State)
    end.

-spec quit(#state{}) -> _.
quit(State) ->
    gen_tcp:close(State#state.server_socket).

-spec get_response(#state{}) -> binary().
get_response(State) ->
    D = State#state.data_dir,
    M = State#state.module,
    F = State#state.function,
    C = State#state.counter,
    Filename = D ++ "/" ++ atom_to_list(M) ++ "_"  ++ atom_to_list(F) ++ "_" ++ integer_to_list(C) ++ ".txt",
    io:format("mock server: opening file: ~p ... ", [Filename]),
    case file:read_file(Filename) of
        {ok, Response} ->
            io:format("ok.~n"),
            Response;
        {error, enoent} ->
            io:format("mock server: error: file does not exist.~n"),
            <<"HTTP/1.1 404 Not Found\r\ncontent-length: 0\r\n\r\n">>;
        {error, Error} ->
            io:format("mock server: error: ~p~n", [Error]),
            <<"HTTP/1.1 500 Internal Server Error\r\ncontent-length: 0\r\n\r\n">>
    end.

