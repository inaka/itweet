-module(test_itweep_cb).

-author('Iñaki Garay <inaki@inakanetworks.com>').

-behaviour(itweep).

%%% Public API
-export(
    [ handle_tweets/2
    ]).
%%% itweep behaviour exports
-export(
    [ init/1
    , handle_status/2
    , handle_event/3
    , handle_call/3
    , handle_info/2
    , terminate/2
    ]).

-record(state,
    { responses = [] :: list()
    }).

init(State) ->
    io:format("init ~p~n", [State]),
    {ok, State}.

handle_tweets(Content, State) ->
    io:format("handle_tweets: ~p | ~p~n", [Content, State]),
    % con el estado me fijo cual es el siguiente o el actual y me aseguro que haya recibido lo que yo queria
    NewState = State#state{ responses = [Content | State#state.responses] },
    {ok, NewState}.

handle_status(Arg1, State) ->
    io:format("handle_status: ~p | ~p~n", [Arg1, State]),
    {ok, State}.

handle_event(Arg1, From, State) ->
    io:format("handle_event: ~p | ~p | ~p~n", [Arg1, From, State]),
    {ok, State}.

handle_info(Arg1, State) ->
    io:format("handle_info: ~p | ~p~n", [Arg1, State]),
    {noreply, State}.

handle_call({get_content}, From, State) ->
    {reply, ok, State};
handle_call(Arg1, From, State) ->
    io:format("handle_call: ~p | ~p | ~p~n", [Arg1, From, State]),
    {reply, ok, State}.

terminate(Arg1, State) ->
    io:format("terminate: ~p | ~p~n", [Arg1, State]).
