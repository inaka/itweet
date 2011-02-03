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

-module(basic_itweep).

-behaviour(itweep).

-record(state, {}).
-opaque state() :: #state{}.

-export([start/2, stop/1]).
-export([handle_call/3, handle_event/3, handle_info/2, handle_status/2, init/1, terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start(User::string(), Password::string()) -> itweep:start_result().
start(User, Password) ->
  itweep:start(?MODULE, [], [{user, User}, {password, Password}]).

-spec stop(Pid::pid()) -> ok.
stop(Pid) ->
  itweep:call(Pid, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ITWEEP FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec init(Args::term()) -> {ok, state()}.
init([]) ->
  io:format("~p - ~p: init~n", [calendar:local_time(), ?MODULE]),
  {ok, #state{}}.

%% @hidden
-spec handle_status(Status::itweep_mochijson2:json_object(), State::term()) -> {ok, state()}.
handle_status(_Status, State) ->
%% handle_status(Status, State) ->
%%   User = case itweep_mochijson2:get_value("user", Status) of
%%            undefined -> "An annonymous user";
%%            JsonObj -> itweep_mochijson2:get_value("screen_name", JsonObj, "An unnamed user")
%%          end,
%%   Text = itweep_mochijson2:get_value("text", Status, "nothing (o_O)"),  
%%   io:format("~p - ~p:~s says: ~s~n", [calendar:local_time(), ?MODULE, User, Text]),
  io:format("."),
  {ok, State}.

%% @hidden
-spec handle_event(Event::atom(), Data::itweep_mochijson2:json_object(), State::term()) -> {ok, state()}.
handle_event(Event, Data, State) ->
  io:format("~p - ~p:~p -> ~p: ~p~n", [calendar:local_time(), ?MODULE, ?LINE, Event, Data]),
  {ok, State}.

%% @hidden
-spec handle_call(Msg::term(), From::reference(), State::term()) -> {stop, normal, ok, state()}.
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