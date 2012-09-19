-module(itweet).

-behavior(application).

-export([start/0, start/2, stop/0, stop/1]).

%%-------------------------------------------------------------------
%% ADMIN API
%%-------------------------------------------------------------------
%% @doc Starts the application
-spec start() -> ok | {error, {already_started, ?MODULE}}.
start() -> application:start(?MODULE).

%% @doc Stops the application
-spec stop() -> ok.
stop() -> application:stop(?MODULE).

%%-------------------------------------------------------------------
%% BEHAVIOUR CALLBACKS
%%-------------------------------------------------------------------
%% @private
-spec start(any(), any()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) -> {ok, self()}.

%% @private
-spec stop(any()) -> ok.
stop(_State) -> ok.