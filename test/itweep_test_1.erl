-module(itweep_test_1).

-behaviour(itweep).

-export(
    [ init/1
    , handle_tweets/2
    , handle_status/2
    , handle_event/3
    , handle_info/2
    , handle_call/3
    , terminate/2
    ]).

init(Arg) ->
    io:format("init ~p~n", [Arg]),
      %% assert here
    {ok, []}.

handle_tweets(Content, State) ->
    io:format("Got tweets, yeah"),

    % con el estado me fijo cual es el siguiente o el actual y me aseguro que haya recibido lo que yo queria
    {ok, State}.

handle_status(Arg1, Arg2) ->
    io:format("handle_status: ~p | ~p~n", [Arg1, Arg2]),
    {ok, []}.

handle_event(Arg1, Arg2, Arg3) ->
    io:format("handle_status: ~p | ~p | ~p~n", [Arg1, Arg2, Arg3]),
    {ok, []}.

handle_info(Arg1, Arg2) ->
    io:format("handle_info: ~p | ~p~n", [Arg1, Arg2]),
    {noreply,[]}.

handle_call(Arg1, Arg2, Arg3) ->
    io:format("handle_call: ~p | ~p | ~p~n", [Arg1, Arg2, Arg3]),
    {reply,ok,[]}.

terminate(Arg1, Arg2) ->
    io:format("terminate: ~p | ~p~n", [Arg1, Arg2]).
