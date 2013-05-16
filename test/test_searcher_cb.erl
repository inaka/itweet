-module(test_searcher_cb).

-author('Iñaki Garay <inaki@inakanetworks.com>').

-behaviour(itweep_searcher).

%%% Public API
-export(
  [ load_filters/2
  , stop/1
  , get_statuses/1
  , make_receiver/0
  , receiver_fun/0
  ]).
%%% itweep_searcher behavior exports
-export(
  [ init/1
  , handle_status/2
  , handle_info/2
  , handle_call/3
  , terminate/2
  ]).

-record(state,
    { responses = [] :: list()
    , receiver  = undefined :: atom()
    }).

make_receiver() ->
    spawn(fun() -> receiver_fun() end).

receiver_fun() ->
    receive
      M -> io:format("RECEIVED: ~n    ~p~n", [M])
    end,
    receiver_fun().






load_filters(Server, Filters) ->
    itweep_searcher:call(Server, {load_filters, Filters}).

stop(Server) ->
    itweep_searcher:call(Server, stop).

get_statuses(Server) ->
    itweep_searcher:call(Server, get_statuses).

%%% itweep_searcher behavior callbacks
init(Options) ->
    io:format("cb init options: ~p~n", [Options]),
    Receiver = proplists:get_value(receiver, Options),
    State = #state{ receiver = Receiver },
    {ok, State}.

handle_status(Status, State) ->
    % io:format("handle_status called"),
    io:format("cb handle_status: ~p | ~p~n", [Status, State]),
    % try State#state.receiver ! Status of
    %   _ -> ok
    % catch
    %   _:_ -> io:format("error: unable to send status to receiver.~n")
    % end,
    NewState = State#state{ responses = [Status | State#state.responses] },
    {ok, NewState}.

handle_info(Info, State) ->
    % io:format("handle_info: ~p | ~p~n", [Info, State]),
    {ok, State}.

handle_call(stop, From, State) ->
    io:format("cb handle_call: ~p | ~p | ~p~n", [stop, From, State]),
    {stop, normal, ok, State};
handle_call(get_statuses, From, State) ->
    io:format("cb handle_call: ~p | ~p | ~p~n", [get_statuses, From, State]),
    {ok, State#state.responses, State};
handle_call(Call = {load_filters, Filters}, From, State) ->
    io:format("cb handle_call: ~p | ~p | ~p~n", [Call, From, State]),
    {ok, {search, Filters, [include_entities]}, ok, State}.

terminate(Reason, State) ->
    % io:format("terminate: ~p | ~p~n", [Reason, State]),
    ok.
