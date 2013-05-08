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

-module(itweet_test_server).

-export(
    [ start/1
    , stop/0
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
    , data_dir      = [] :: string()
    }).

%% This is the initial launching function.
%% Returns the PID of the server process.
-spec start(list()) -> pid().
start(Options) ->
    spawn(?MODULE, init, [Options]).

%% Stops an itweet_test_server registered under it's module name.
-spec stop() -> quit.
stop() ->
    ?MODULE ! quit.

%% Stops an itweet_test_server registered under another name.
-spec stop(atom()) -> quit.
stop(Name) ->
    Name ! quit.

%% Initialization function. Creates the server's initial state.
-spec init(list()) -> _.
init(Options) ->
    Module   = proplists:get_value(module,   Options),
    Function = proplists:get_value(function, Options),
    Port     = proplists:get_value(port,     Options),
    Register = proplists:get_value(name,     Options),
    DataDir  = proplists:get_value(data_dir, Options),

    case Register of
        undefined ->
            register(?MODULE, self());
        Name ->
            register(Name, self())
    end,

    % Open the listening socket.
    {ok, ServerSocket} = gen_tcp:listen(0,
        [ {reuseaddr, true}
        , {port,      Port}
        , {packet,    0}
        , {active,    true}
        , binary
        ]),

    % Get the client connection.
    {ok, ClientSocket} = gen_tcp:accept(ServerSocket),
    State = #state{
        server_socket = ServerSocket,
        client_socket = ClientSocket,
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
    ClientSocket = State#state.client_socket,
    receive
        quit ->
            gen_tcp:close(ClientSocket),
            gen_tcp:close(ServerSocket),
            ok;
        {tcp, ClientSocket, Data} ->
            % Here is where it gets the correct response based on it's state.
            Response = get_response(State),
            ok = gen_tcp:send(ClientSocket, Response),
            % Increment the counter so on the next request, it will get the
            % next response.
            NewState = State#state{counter = State#state.counter + 1},
            loop(NewState);
        {tcp_closed, ClientSocket}->
            io:format("Socket ~p closed~n", [ClientSocket]);
        {tcp_error, ClientSocket, Reason} ->
            io:format("Error on socket ~p reason: ~p~n", [ClientSocket, Reason]);
        M ->
            io:format("Unrecognized message: ~p~n", [M])
    end.

-spec get_response(#state{}) -> binary().
get_response(State) ->
    D = State#state.data_dir,
    M = State#state.module,
    F = State#state.function,
    C = State#state.counter,
    Filename = D ++ "/" ++ atom_to_list(M) ++ "_"  ++ atom_to_list(F) ++ "_" ++ integer_to_list(C) ++ ".txt",
    io:format("opening: ~p~n", [Filename]),
    case file:read_file(Filename) of
        {ok, Response} ->
            Response;
        {error, enoent} ->
            Response = <<"HTTP/1.1 NOT FOUND\r\n">>
    end,
    Response.

