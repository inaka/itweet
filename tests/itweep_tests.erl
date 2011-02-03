%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc Tests for itweep module
%%%-------------------------------------------------------------------

%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

-module(itweep_tests).

-behaviour(itweep).

%% @headerfile "eunit.hrl"
-include("eunit.hrl").

-define(TIMEOUT, 300000). %% 5 min.
-define(RUNNING, 30000). %% 30 secs.

-record(state, {events = []   :: {atom(), itweep_mochijson2:json_object()},
                statuses = [] :: [itweep_mochijson2:json_object()]}).
-opaque state() :: #state{}.

-export([handle_call/3, handle_event/3, handle_info/2, handle_status/2, init/1, terminate/2]).
-export([main/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec main() -> no_return().
main() ->
  application:load(itweet),
  case eunit:test(itweep, [verbose]) of
    ok -> halt(0);
    _ -> halt(1)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EUNIT FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec test() -> any().

-spec itweep_test_() -> {setup, fun(() -> ok), fun((_) -> ok), {inorder, [{timeout, ?TIMEOUT, {pos_integer(), fun(() -> any())}},...]}}.
itweep_test_() ->
  {setup,
   _Start = fun() ->
                    {ok, User} = application:get_env(itweet, user),
                    {ok, Pwd}  = application:get_env(itweet, password),
                    itweep:start_link({local, ?MODULE}, ?MODULE, [],
                                      [{user,     User},
                                       {password, Pwd}]),
                    ok
            end,
   _Stop = fun(_) ->
                   itweep:call(?MODULE, stop)
           end,
   _Tests =
     {inorder,
      [{timeout, ?TIMEOUT, Test} || Test <- [?_test(filter([follow])),
                                             ?_test(filter([track])),
                                             ?_test(filter([locations])),
                                             ?_test(filter([follow, track])),
                                             ?_test(filter([follow, locations])),
                                             ?_test(filter([locations, track])),
                                             ?_test(filter([follow, track, locations])),
                                             ?_test(sample()),
                                             ?_test(firehose()),
                                             ?_test(retweet()),
                                             ?_test(links())]]
      }
  }.

filter(OptionsToSet) ->
  Options = lists:map(fun option/1, OptionsToSet),
  ok = itweep:filter(?MODULE, Options),
  Statuses = get_results(),
  %%TODO: Validate correctness (like track terms or user ids)
  ?assertLess(0, length(Statuses)).

sample() ->
  ok = itweep:sample(?MODULE, []),
  Statuses = get_results(),
  ?assertLess(0, length(Statuses)).

firehose() ->
  ok = itweep:firehose(?MODULE, []),
  Statuses = get_results(),
  ?assertLess(0, length(Statuses)).

retweet() ->
  ok = itweep:retweet(?MODULE, []),
  Statuses = get_results(),
  ?assertLess(0, length(Statuses)).

links() ->
  ok = itweep:links(?MODULE, []),
  Statuses = get_results(),
  ?assertLess(0, length(Statuses)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ITWEEP FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec init(Args::term()) -> {ok, state()}.
init([]) ->
  ?debugFmt("~p - ~p: init", [calendar:local_time(), ?MODULE]),
  {ok, #state{}}.

%% @hidden
-spec handle_status(Status::itweep_mochijson2:json_object(), State::term()) -> {ok, state()}.
handle_status(Status, State = #state{statuses = Statuses}) ->
  case length(Statuses) of
    L when L rem 10 =:= 0 ->
      erlang:display({L+1, messages});
    _ ->
      ok
  end,
  {ok, State#state{statuses = [Status|Statuses]}}.

%% @hidden
-spec handle_event(Event::atom(), Data::itweep_mochijson2:json_object(), State::term()) -> {ok, state()}.
handle_event(Event, Data, State = #state{events = Events}) ->
  {ok, State#state{events = [{Event, Data}|Events]}}.

%% @hidden
-spec handle_call(Msg::term(), From::reference(), State::term()) -> itweep:call_result().
handle_call(get, _From, State = #state{statuses = Statuses,
                                       events   = Events}) ->
  {ok, {lists:reverse(Events), lists:reverse(Statuses)}, State};
handle_call(clear, _From, State) ->
  ?debugMsg("clearing"),
  {ok, ok, State#state{statuses = [], events = []}};
handle_call(stop, _From, State) ->
  ?debugMsg("stopping"),
  {stop, normal, ok, State}.

%% @hidden
-spec handle_info(Msg::term(), State::term()) -> {ok, state()}.
handle_info(Msg, State) ->
  ?debugFmt("info:\n\t~p", [Msg]),
  {ok, State}.

%% @hidden
-spec terminate(Reason :: normal | shutdown | term(), State::term()) -> _.
terminate(Reason, _State) ->
  ?debugFmt("terminating: ~p", [Reason]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_results() ->
  ok = itweep:call(?MODULE, clear, infinity),
  timer:sleep(?RUNNING),
  {Events, Statuses} = itweep:call(?MODULE, get),
  case lists:keyfind(stream_error, 1, Events) of
    false ->
      Statuses;
    {stream_error, ErrorJson} ->
      case itweep_mochijson2:get_value("code", ErrorJson, null) of
        <<"403">> ->
          ?fail(unauthorized);
        <<"420">> ->
          ?fail(rate_limited);
        Other ->
          ?fail({Other, ErrorJson})
      end
  end.

option(follow) -> {follow, lists:seq(1, 100000, 1000 + random:uniform(1000))};
option(track) -> {track, ["omg", "lol", "yfrog", "twitpic"]};
option(locations) -> {locations, [{33.0, -65.0, 90.0, -57.0}, {-122.75, 36.8, -121.75, 37.8}]}.