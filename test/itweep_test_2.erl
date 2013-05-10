-module(itweep_test_2).

-behaviour(itweep_searcher).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% itweep_searcher behavior.
-export(
  [ init/1
  , handle_status/2
  , handle_info/2
  , handle_call/3
  , terminate/2
  ]).

init(Arg) ->
    io:format("init ~p~n", [Arg]),
      %% assert here
  {ok, Arg}.

handle_status(Arg1, Arg2) ->
    io:format("handle_status: ~p | ~p~n", [Arg1, Arg2]),
    {ok, []}.

handle_info(Arg1, Arg2) ->
    io:format("handle_info: ~p | ~p~n", [Arg1, Arg2]),
    {noreply,[]}.

handle_call({load_filters}, _From, State) ->
  {ok, {search, "#erlang", [include_entities]}, ok, State}.

terminate(Arg1, Arg2) ->
    io:format("terminate: ~p | ~p~n", [Arg1, Arg2]).
