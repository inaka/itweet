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
-define(RUNNING, 10000). %% 10 secs.

-record(state, {events = []   :: [{atom(), itweep:event_data()}],
                statuses = [] :: [itweep:tweet()]}).
-type state() :: #state{}.

-export([handle_call/3, handle_event/3, handle_info/2, handle_status/2, init/1, terminate/2]).
-export([main/0, to_lower/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec main() -> no_return().
main() ->
  _ = application:load(itweet),
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
                    {ok, _CKey} = application:get_env(itweet, consumer_key),
                    {ok, _CSecret}  = application:get_env(itweet, consumer_secret),
                    {ok, Token} = application:get_env(itweet, access_token),
                    {ok, Secret} = application:get_env(itweet, access_secret),
                    {ok, _Pid} = itweep:start_link({local, ?MODULE}, ?MODULE, [],
                                                   [{token,  Token},
                                                    {secret, Secret}]),
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
                                             ?_test(filter([follow, track, locations]))
                                             %?_test(sample()),
                                             %?_test(firehose()),
                                             %?_test(retweet()),
                                             %?_test(links())
                                             ]]
      }
  }.

filter(OptionsToSet) ->
  Options = lists:map(fun option/1, OptionsToSet),
  ?debugFmt("\nfiltering ~p", [Options]),
  ok = itweep:filter(?MODULE, Options),
  Statuses = get_results(),
  % ?assertLess(0, length(Statuses)),
  lists:foreach(
    fun(Status) ->
      case lists:any(
              fun(Option) ->
                      validate(Option, Status)
              end, Options) of
            true -> ok;
            false ->
              ?fail({unexpected_status, Status, Options})
      end
    end, Statuses).

sample() ->
  ?debugMsg("\nsample"),
  ok = itweep:sample(?MODULE, []),
  Statuses = get_results(),
  ?assertLess(0, length(Statuses)).

firehose() ->
  ?debugMsg("\nfirehose"),
  ok = itweep:firehose(?MODULE, []),
  Statuses = get_results(),
  ?assertLess(0, length(Statuses)).

retweet() ->
  ?debugMsg("\nretweet"),
  ok = itweep:retweet(?MODULE, []),
  Statuses = get_results(),
  ?assertLess(0, length(Statuses)).

links() ->
  ?debugMsg("\nlinks"),
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
-spec handle_status(Status::itweep:tweet(), State::term()) -> {ok, state()}.
handle_status(Status, State = #state{statuses = Statuses}) ->
%  case length(Statuses) of
%    L when L rem 100 =:= 0 ->
%      erlang:display({L+1, messages});
%    _ ->
%      ok
%  end,
  {ok, State#state{statuses = [Status|Statuses]}}.

%% @hidden
-spec handle_event(Event::atom(), Data::itweep:event_data(), State::term()) -> {ok, state()}.
handle_event(Event, Data, State = #state{events = Events}) ->
  {ok, State#state{events = [{Event, Data}|Events]}}.

%% @hidden
-spec handle_call(Msg::term(), From::reference(), State::term()) -> {ok, ok | {[itweep:event_data()], [itweep:tweet()]}, state()} | {stop, normal, ok, state()}.
handle_call(get, _From, State = #state{statuses = Statuses,
                                       events   = Events}) ->
  {ok, {lists:reverse(Events), lists:reverse(Statuses)}, State};
handle_call(clear, _From, State) ->
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
      ?debugFmt("~p messages\n", [erlang:length(Statuses)]),
      Statuses;
    {stream_error, ErrorJson} ->
      case itweet_mochijson2:get_value("code", ErrorJson, null) of
        <<"403">> ->
          ?fail(unauthorized);
        <<"413">> ->
          ?fail(too_many);
        <<"420">> ->
          ?fail(rate_limited);
        Other ->
          ?fail({Other, ErrorJson})
      end
  end.

option(follow) -> {follow, [3806441, 39715505, 113425681, 2960221, 16151019 |
                              [random:uniform(1000000000) || _ <- lists:seq(1, 250)]]};
option(track) -> {track, ["the", "lol", "yfrog", "twitpic"]};
option(locations) -> {locations, [{-38.0, -65.0, -33.0, -56.0}, {-122.75, 36.8, -121.75, 37.8}]}.

validate({follow, Users}, Status) ->
  case itweet_mochijson2:get_value(<<"user">>, Status) of
    undefined ->
      ?debugFmt("Status without user: ~p", [Status]),
      true;
    UserJson ->
      lists:member(itweet_mochijson2:get_value(<<"id">>, UserJson, -1), Users)
  end;
validate({track, Words}, Status) ->
  Text = to_lower(list_to_binary(itweet_mochijson2:encode(Status))),
  case binary:match(Text, lists:map(fun erlang:list_to_binary/1, Words)) of
    nomatch ->
      nomatch =/= binary:match(Text, lists:map(fun erlang:list_to_binary/1, Words));
    _ ->
      true
  end;
validate({locations, Locations}, Status) ->
  case itweet_mochijson2:get_value(<<"geo">>, Status, null) of
    null ->
      true;
    GeoJson ->
      case itweet_mochijson2:get_value(<<"coordinates">>, GeoJson) of
        [Long, Lat] ->
          lists:any(fun({MinLat, MinLong, MaxLat, MaxLong}) ->
                           trunc(MinLat) =< trunc(Lat) + 1 andalso trunc(Lat) =< trunc(MaxLat) + 1 andalso
                             trunc(MinLong) =< trunc(Long) + 1 andalso trunc(Long) =< trunc(MaxLong) + 1
                    end, Locations);
        Coordinates ->
          true
      end
  end.

-spec to_lower(binary()) -> binary().
to_lower(Bin) ->
  to_lower(Bin, <<>>).

to_lower(<<>>, Acc) ->
  Acc;
to_lower(<<C, Rest/binary>>, Acc) when $A =< C, C =< $Z ->
  to_lower(Rest, <<Acc/binary, (C+32)>>);
to_lower(<<195, C, Rest/binary>>, Acc) when 128 =< C, C =< 150 -> %% A-0 with tildes plus enye
  to_lower(Rest, <<Acc/binary, 195, (C+32)>>);
to_lower(<<195, C, Rest/binary>>, Acc) when 152 =< C, C =< 158 -> %% U and Y with tilde plus greeks
  to_lower(Rest, <<Acc/binary, 195, (C+32)>>);
to_lower(<<C, Rest/binary>>, Acc) ->
  to_lower(Rest, <<Acc/binary, C>>).
