%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc Basic callback module for {@link itweep}
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

-module(qa_itweep).

-behaviour(itweep).

-record(state, {seed        :: binary(),
                waiting_for :: [binary()],
                timer       :: undefined | timer:tref()}).
-opaque state() :: #state{}.

-export([start/3, stop/1]).
-export([handle_call/3, handle_event/3, handle_info/2, handle_status/2, init/1, terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start(User::string(), Password::string(), Seed::string()) -> itweep:start_result().
start(User, Password, Seed) ->
  {ok, Pid} = itweep:start(?MODULE, Seed, [{user, User}, {password, Password}]),
  ok = itweep:filter(Pid, [{track, [Seed]}]),
  {ok, Pid}.

-spec stop(Pid::pid()) -> ok.
stop(Pid) ->
  itweep:call(Pid, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ITWEEP FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec init(Seed::string()) -> {ok, state()}.
init(Seed) ->
  io:format("~p - ~p: init~n", [calendar:local_time(), ?MODULE]),
  {ok, #state{waiting_for = [list_to_binary(Seed)], seed = list_to_binary(Seed)}}.

%% @hidden
-spec handle_status(Status::itweep:tweet(), State::term()) -> {ok, state()}.
handle_status(Status, State) ->
  User = case itweet_mochijson2:get_value("user", Status) of
           undefined -> "An annonymous user";
           JsonObj -> itweet_mochijson2:get_value("screen_name", JsonObj, "An unnamed user")
         end,
  Lang = case itweet_mochijson2:get_value("user", Status) of
           null -> null;
           JsonObj2 -> itweet_mochijson2:get_value(<<"lang">>, JsonObj2, null)
         end,
  case Lang of
    <<"en", _/binary>> ->
      Text = itweet_mochijson2:get_value("text", Status, "nothing (o_O)"),
      case Text of
        <<$@, _/binary>> ->
          {ok, State};
        <<$R, $T, _/binary>> ->
          {ok, State};
        Text ->
          case binary:match(Text, State#state.waiting_for) of
            nomatch ->
              {ok, State};
            _ ->
              io:format("~p > ~s: ~s", [calendar:local_time(), User, Text]),
              Words =
                case
                  lists:sort(
                    fun word_compare/2,
                    lists:filter(
                      fun(<<$@, _/binary>>) -> false;
                         (Word) -> erlang:size(Word) > 3 andalso
                                     nomatch =:= binary:match(Word, <<"://">>)
                      end, re:split(Text, <<"[^a-z#_0-9]">>, [caseless]))) of
                  [W1,W2,W3|_] -> [W1,W2,W3];
                  Ws -> Ws
                end,
              io:format("\t...~p~n", [Words]),
              ok = stop_timer(State#state.timer),
              {ok, Ref} = timer:apply_after(10000, itweep, call, [self(), timeout]),
              {ok, State#state{waiting_for = Words, timer = Ref}}
          end
      end;
    _ ->
      {ok, State}
  end.

%% @hidden
-spec handle_event(Event::atom(), Data::itweep:event_data(), State::term()) -> {ok, state()}.
handle_event(Event, Data, State) ->
  io:format("~p - ~p:~p -> ~p: ~p~n", [calendar:local_time(), ?MODULE, ?LINE, Event, Data]),
  {ok, State}.

%% @hidden
-spec handle_call(timeout | stop, From::reference(), State::term()) -> {ok, {filter, [{track, [string()]}]}, ok, state()} | {ok, ok, state()} | {stop, normal, ok, state()}.
handle_call(timeout, _From, State) ->
  {ok, Ref} = timer:apply_after(10000, itweep, call, [self(), timeout]),
  Track = lists:map(fun binary_to_list/1, State#state.waiting_for),
  io:format("~p - ~p:Tracking ~p~n", [calendar:local_time(), ?MODULE, Track]),
  {ok, {"filter", [{track, Track}]}, ok,
   State#state{timer = Ref,
               waiting_for = [State#state.seed | State#state.waiting_for]}};
handle_call(stop, _From, State) ->
  io:format("~p - ~p:~p stopping~n", [calendar:local_time(), ?MODULE, ?LINE]),
  {stop, normal, ok, State}.

%% @hidden
-spec handle_info(Msg::term(), State::term()) -> {ok, state()}.
handle_info(Msg, State) ->
  io:format("~p - ~p:~p -> info:~n\t~p~n", [calendar:local_time(), ?MODULE, ?LINE, Msg]),
  {ok, State}.

%% @hidden
-spec terminate(Reason :: normal | shutdown | term(), State::term()) -> _.
terminate(Reason, _State) ->
  io:format("~p - ~p:~p terminating: ~p~n", [calendar:local_time(), ?MODULE, ?LINE, Reason]),
  ok.

word_compare(<<$#, _/binary>>, <<$#, _/binary>>) -> false;
word_compare(<<$#, _/binary>>, _) -> false;
word_compare(W1, W2) ->
  case {binary:match(W1, [<<"://">>, <<"@">>]), binary:match(W2, [<<"://">>, <<"@">>])} of
    {nomatch, nomatch} ->
      erlang:size(W1) > erlang:size(W2);
    {nomatch, _} ->
      W1;
    {_, nomatch} ->
      W2;
    {_, _} ->
      erlang:size(W1) > erlang:size(W2)
  end.

-spec stop_timer(undefined | timer:tref()) -> ok.
stop_timer(undefined) -> ok;
stop_timer(Timer) ->
  case timer:cancel(Timer) of
    {ok, cancel} -> ok;
    {error, _Reason} -> ok
  end.