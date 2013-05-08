-module(itweet_test_server).

-export(
    [ start/1
    , stop/1
    , init/1
    , loop/1
    ]).

-define(BACKLOG,   10000).

-define(FLAG(X), io:format("~p~n", [X])).

-record(state,
    { server_socket
    , client_socket
    , module
    , function
    , counter
    }).

% This is the initial launching function.
% The Options parameter is a list with two atoms, Module and Function.
% Module and Function are used to tell this simple server what testcase we are
% running.
% It will then use files with names module_function_<N>.txt to get responses
% to the twitter API queries it receieves, where N is a numer starting at 1 and
% incrementing with each query.
% Returns the PID of the server process.
start(Options) ->
    spawn(?MODULE, init, [Options]).

stop(Pid) ->
    Pid ! quit.

init(Options) ->
    ?FLAG(<<"start: init">>),
    Module   = proplists:get_value(module, Options),
    Function = proplists:get_value(function, Options),
    Port     = proplists:get_value(port, Options),
    % Open the listening socket.
    {ok, ServerSocket} = gen_tcp:listen(0,
        [ {backlog,   ?BACKLOG}
        , {reuseaddr, true}
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
        counter       = 0
    },
    % Start the recieve/reply loop.
    loop(State).

loop(State) ->
    ?FLAG(<<"start: loop">>),
    ClientSocket = State#state.client_socket,
    receive
        quit ->
            ?FLAG(<<"Quit message recieved:">>),
            ?FLAG(<<"Total messages received:">>),
            ?FLAG(State#state.counter);
        {tcp, ClientSocket, Data} ->
            io:format("Got packet: ~p~n", [Data]),

            % Here is where it gets the correct response based on it's state.
            Response = get_response(State),

            gen_tcp:send(ClientSocket, Response),

            NewCounter  = State#state.counter + 1,
            NewState = State#state{counter = NewCounter},
            loop(NewState);
        {tcp_closed, ClientSocket}->
            io:format("Socket ~p closed~n", [ClientSocket]);
        {tcp_error, ClientSocket, Reason} ->
            io:format("Error on socket ~p reason: ~p~n", [ClientSocket, Reason])
    end.

get_response(State) ->
    M = State#state.module,
    F = State#state.function,
    C = State#state.counter,
    Filename = atom_to_list(M) ++ "_"  ++ atom_to_list(F) ++ "_" ++ integer_to_list(C) ++ ".txt",
    ?FLAG("Opening file:" ++ Filename),

    % The contents of the file should be sent, but for now I'll return a placeholder binary.
    <<"placeholder">>.
