%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc Generic twitter search client
%%%
%%% The user module should export:
%%%<ul>
%%%   <li>
%%%   <pre>init(Args::term()) -> {@link init_result()}</pre>
%%%     Opens and/or initializes the client.<br/>
%%%   </li><li>
%%%   <pre>handle_status(Status::{@link tweet()}, State::term()) -> {@link handler_result()}</pre>
%%%     Called each time an status is received from twitter<br/>
%%%   </li><li>
%%%   <pre>handle_call(Msg::term(), From::reference(), State::term()) -> {@link call_result()}</pre>
%%%     Called from <code>itweep_search:call/2</code><br/>
%%%   </li><li>
%%%   <pre>handle_info(Msg::term(), State::term()) -> {@link handler_result()}</pre>
%%%     Called each time an erlang message is received<br/>
%%%   </li><li>
%%%   <pre>terminate(Reason :: normal | shutdown | term(), State) -> _</pre>
%%%     Let the user module clean up. Always called when server terminates.<br/>
%%%   </li></ul>
%%% @end
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

-module(itweep_searcher).
-author('Fernando Benavides <elbrujohalcon@inaka.net>').
-vsn("1.0").

-behaviour(gen_server).

-type tweet() :: itweet_mochijson2:json_object().
-export_type([tweet/0]).

-type gen_start_option() :: {timeout, non_neg_integer() | infinity | hibernate} |
                            {debug, [trace | log | {logfile, string()} | statistics | debug]}.
-type start_option() :: {search_frequency, pos_integer()} | gen_start_option().
-type start_result() :: {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
-type method() :: rest | {search, string(), [filter_option()]}.
-export_type([gen_start_option/0, start_option/0, start_result/0, method/0]).

-type init_result()     :: {ok, State::term()} | ignore | {stop, Reason::term()}.
-type handler_result()  :: {ok, State::term()} | {stop, Reason::term(), State::term()}.
-type call_result()     :: {ok, Reply::term(), State::term()} | {ok, NewMethod::method(), Reply::term(), State::term()} | {stop, Reason::term(), Reply::term(), State::term()}.
-export_type([init_result/0, handler_result/0, call_result/0]).

-type server() :: atom() | pid() | {global, atom()}.
-type geocode() :: {float(), float(), float()}.
-type filter_option() :: {geocode, geocode()} | {lang, string()} | include_entities.
-export_type([server/0, geocode/0, filter_option/0]).

-define(DEFAULT_SEARCH_FREQUENCY, 5000).
-define(DEFAULT_QS_OPTIONS, [{rpp, 100}, {result_type, recent}, {with_twitter_user_id, true}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% BEHAVIOUR
-export([behaviour_info/1]).
%% API
-export([start/3, start/4, start_link/3, start_link/4, call/2, call/3]).
-export([search/3, rest/1]).
-export([current_method/1]).
%% GEN SERVER
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNALS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {module                      :: atom(), % Callback module
                token                       :: string(),
                secret                      :: string(),
                consumer                    :: {string(), string(), hmac_sha1},
                mod_state                   :: term(), % Callback module state
                method= rest                :: method(),
                url                         :: string(),
                qs                          :: [{atom(), term()}],
                timer                       :: reference(),
                reconnect_timer             :: undefined | reference(),
                search_frequency = ?DEFAULT_SEARCH_FREQUENCY :: pos_integer()
               }).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% @hidden
-spec behaviour_info(callbacks | term()) -> undefined | [{atom(), non_neg_integer()}].
behaviour_info(callbacks) ->
  [{init, 1}, {handle_status, 2}, {handle_info, 2}, {handle_call, 3}, {terminate, 2}];
behaviour_info(_Other) ->
  undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% @doc  Starts a generic server.
-spec start(Mod::atom(), Args::term(), Options::[start_option()]) -> start_result().
start(Mod, Args, Options) ->
  {Token, Secret, SearchFrequency, OtherOptions} = parse_start_options(Options),
  gen_server:start(?MODULE, {Mod, Args, Token, Secret, SearchFrequency}, OtherOptions).

%%% @doc  Starts a named generic server.
-spec start(Name::{local|global, atom()}, Mod::atom(), Args::term(), Options::[start_option()]) -> start_result().
start(Name, Mod, Args, Options) ->
  {Token, Secret, SearchFrequency, OtherOptions} = parse_start_options(Options),
  gen_server:start(Name, ?MODULE, {Mod, Args, Token, Secret, SearchFrequency}, OtherOptions).

%%% @doc  Starts and links a generic server.
-spec start_link(Mod::atom(), Args::term(), Options::[start_option()]) -> start_result().
start_link(Mod, Args, Options) ->
  {Token, Secret, SearchFrequency, OtherOptions} = parse_start_options(Options),
  gen_server:start_link(?MODULE, {Mod, Args, Token, Secret, SearchFrequency}, OtherOptions).

%%% @doc  Starts and links a named generic server.
-spec start_link(Name::{local|global, atom()}, Mod::atom(), Args::term(), Options::[start_option()]) -> start_result().
start_link(Name, Mod, Args, Options) ->
  {Token, Secret, SearchFrequency, OtherOptions} = parse_start_options(Options),
  gen_server:start_link(Name, ?MODULE, {Mod, Args, Token, Secret, SearchFrequency}, OtherOptions).

%%% @doc  Starts using the <a href="https://dev.twitter.com/docs/api/1/get/search">search</a> API to get results
-spec search(server(), string(), [filter_option()]) -> ok.
search(Server, Query, Options) ->
  gen_server:cast(Server, {search, Query, Options}).

%%% @doc  Put the server to rest (i.e. not querying twitter)
-spec rest(server()) -> ok.
rest(Server) ->
  gen_server:cast(Server, rest).

%%% @doc Make a call to a generic server.
%%% If the server is located at another node, that node will be monitored.
%%% If the client is trapping exits and is linked server termination is handled here
-spec call(Server::server(), Request::term()) -> Response::term().
call(Server, Request) ->
  gen_server:call(Server, {call, Request}).

%%% @doc Make a call to a generic server.
%%% If the server is located at another node, that node will be monitored.
%%% If the client is trapping exits and is linked server termination is handled here
-spec call(Server::server(), Request::term(), Timeout::non_neg_integer()|infinity) -> Response::term().
call(Server, Request, Timeout) ->
  gen_server:call(Server, {call, Request}, Timeout).

%%% @doc Current method.
%%% Returns the current method and its parameters.
-spec current_method(Server::atom() | pid() | {global, atom()}) -> method().
current_method(Server) ->
  gen_server:call(Server, current_method).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN SERVER FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec init({atom(), term(), pos_integer()}) -> {ok, state()} | ignore | {stop, term()}.
init({Mod, InitArgs, Token, Secret, SearchFrequency}) ->
  _Seed = random:seed(erlang:now()),
  {ok, CKey} = application:get_env(itweet, consumer_key),
  {ok, CSecret} = application:get_env(itweet, consumer_secret),
  Consumer = {CKey, CSecret, hmac_sha1},
  case Mod:init(InitArgs) of
    {ok, ModState} ->
      {ok, #state{module            = Mod,
                  mod_state         = ModState,
                  consumer          = Consumer,
                  token             = Token,
                  url               = "http://api.twitter.com/1.1/search/tweets.json",
                  secret            = Secret,
                  qs                = ?DEFAULT_QS_OPTIONS,
                  search_frequency  = SearchFrequency}};
    Other ->
      Other
  end.

%% @hidden
-spec handle_call(term(), reference(), state()) -> {reply, term(), state()} | {stop, normal | shutdown | term(), term(), state()}.
handle_call(current_method, _From, State = #state{method = Method}) ->
  {reply, Method, State};
handle_call({call, Request}, From, State = #state{module = Mod, mod_state = ModState}) ->
  try Mod:handle_call(Request, From, ModState) of
    {ok, Reply, NewModSt} ->
      {reply, Reply, State#state{mod_state = NewModSt}};
    {ok, NewMethod, Reply, NewModSt} ->
      case handle_cast(NewMethod, State#state{mod_state = NewModSt}) of
        {noreply, NewState} ->
          {reply, Reply, NewState};
        {stop, Reason, NewState} ->
          {stop, Reason, Reply, NewState}
      end;
    {stop, Reason, Reply, NewModSt} -> {stop, Reason, Reply, State#state{mod_state = NewModSt}}
  catch
    _:{ok, Reply, NewModSt} ->
      {reply, Reply, State#state{mod_state = NewModSt}};
    _:{ok, NewMethod, Reply, NewModSt} ->
      case handle_cast(NewMethod, State#state{mod_state = NewModSt}) of
        {noreply, NewState} ->
          {reply, Reply, NewState};
        {stop, Reason, NewState} ->
          {stop, Reason, Reply, NewState}
      end;
    _:{stop, Reason, Reply, NewModSt} ->
      {stop, Reason, Reply, State#state{mod_state = NewModSt}}
  end.

%% @hidden
-spec handle_cast(rest | wait | {search, string(), [filter_option()]}, state()) -> {noreply, state()} | {stop, term(), state()}.
handle_cast(rest, State) ->
  NewState = cancel_timer(State),
  {noreply, NewState#state{method = rest, url = undefined}};
handle_cast(Method, State) ->
  NewState = cancel_timer(State),
  Qs = build_qs(Method),
  NewTimer = erlang:send_after(0, self(), run_query),
  {noreply, NewState#state{method = Method, qs = Qs, timer = NewTimer}}.

%% @hidden
-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
%% REGULAR INTERNAL STUFF --------------------------------------------------------------------------
handle_info(run_query, State = #state{method = rest}) ->
  error_logger:info_msg("Outdated timer, we're resting"),
  {noreply, State};
handle_info(run_query, State) ->
  try oauth:get(
    State#state.url, State#state.qs,
    State#state.consumer, State#state.token, State#state.secret,
    [{response_format, binary}]
  ) of
    {ok, "200", _Headers, Body} ->
      NewTimer = erlang:send_after(State#state.search_frequency, self(), run_query),
      NewState =
        try itweet_mochijson2:decode(Body) of
          {Json} ->
            NewModSt =
             lists:foldl(
               fun(Status, AccModSt) ->
                      Fun = fun() -> (State#state.module):handle_status(Status, AccModSt) end,
                      case run_handler(Fun) of
                        {ok, NextModSt} -> NextModSt;
                        {stop, Reason, NextModSt} ->
                          throw({stop, Reason, State#state{mod_state = NextModSt}})
                      end
               end, State#state.mod_state, proplists:get_value(<<"statuses">>, Json)),
            NewQs =
              case proplists:get_value(<<"search_metadata">>, Json) of
                undefined -> State#state.qs;
                {MetaData} -> case proplists:get_value(<<"refresh_url">>, MetaData) of
                  undefined -> State#state.qs;
                  RefreshUrl ->
                    [SinceId, _] = binary:split(RefreshUrl, <<"&">>),
                    [_, Id] = binary:split(SinceId, <<"=">>),
                    lists:keystore(since_id, 1, State#state.qs, {since_id, binary_to_list(Id)})
                end
              end,
            State#state{qs = NewQs, timer = NewTimer, mod_state = NewModSt}
        catch
          _:Error ->
            error_logger:error_msg("Error parsing twitter results for ~s:~n\t~p~n", [State#state.url, Error]),
            State#state{timer = NewTimer}
        end,
      {noreply, NewState};
    {ok, Code, Headers, Body} ->
      NextTimeout =
        case proplists:get_value("Retry-After", Headers, notset) of
          notset -> State#state.search_frequency;
          Value -> (list_to_integer(Value) + 1) * 1000
        end,
      error_logger:warning_msg("Error ~p querying twitter: ~p~n\tRetrying in ~p ms~n", [Code, Body, NextTimeout]),
      NewTimer = erlang:send_after(NextTimeout, self(), run_query),
      {noreply, State#state{qs = build_qs(State#state.method), timer = NewTimer}};
    Error ->
      error_logger:warning_msg("Error querying twitter: ~p~n\tRetrying in ~p ms~n", [Error, State#state.search_frequency]),
      NewTimer = erlang:send_after(State#state.search_frequency, self(), run_query),
      {noreply, State#state{qs = build_qs(State#state.method), timer = NewTimer}}
  catch
    _:Error ->
      error_logger:error_msg("ibrowse error with ~s: ~p~n", [State#state.url, Error]),
      {stop, {ibrowse_error, Error}, State}
  end;
%% OTHERs ------------------------------------------------------------------------------------------
handle_info(Info, State = #state{module = Mod, mod_state = ModState}) ->
  case run_handler(fun() -> Mod:handle_info(Info, ModState) end) of
    {ok, NewModSt} -> {noreply, State#state{mod_state = NewModSt}};
    {stop, Reason, NewModSt} -> {stop, Reason, State#state{mod_state = NewModSt}}
  end.

%% @hidden
-spec terminate(any(), state()) -> any().
terminate(Reason, #state{module = Mod, mod_state = ModState}) ->
  Mod:terminate(Reason, ModState).

%% @hidden
-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_start_options(Options) ->
  Token = case proplists:get_value(token, Options) of
           undefined -> throw({missing_option, token});
           U -> U
         end,
  Secret = case proplists:get_value(secret, Options) of
               undefined -> throw({missing_option, secret});
               P -> P
             end,
  SearchFrequency =
    case proplists:get_value(search_frequency, Options) of
      undefined -> ?DEFAULT_SEARCH_FREQUENCY;
      SF -> SF
    end,
  {
    Token, Secret, SearchFrequency,
    proplists:delete(token, proplists:delete(token, proplists:delete(secret, proplists:delete(search_frequency, Options))))
  }.

run_handler(Fun) ->
  try Fun() of
    {ok, NewModSt} -> {ok, NewModSt};
    {stop, Reason, NewModSt} -> {stop, Reason, NewModSt};
    Other -> throw({bad_return, Other})
  catch
    _:{ok, NewModSt} -> {ok, NewModSt};
    _:{stop, Reason, NewModSt} -> {stop, Reason, NewModSt}
  end.

build_qs({search, Query, Options}) ->
  NewQs = [{q, Query}|?DEFAULT_QS_OPTIONS],
  lists:foldl(
    fun ({geocode, {Lat, Lng, Radius}}, QS) ->
          lists:keystore(geocode, 1, QS, {geocode, io_lib:format("~.5g,~.5g,~.5gmi", [Lat, Lng, Radius])});
        ({lang, Lang}, QS) ->
          lists:keystore(lang, 1, QS, {lang, Lang});
        (include_entities, QS) ->
          lists:keystore(include_entities, 1, QS, {include_entities, "true"});
        (_, QS) ->
          QS
    end, NewQs, Options).

cancel_timer(State = #state{timer = undefined}) -> State;
cancel_timer(State = #state{timer = Timer}) -> erlang:cancel_timer(Timer), State#state{timer = undefined}.
