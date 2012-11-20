-module(game_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-type start_type() :: normal | atom().
-type start_args() :: [{atom(), any()}] | [].

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(start_type(), start_args()) -> {ok, pid()} | {ok, pid(), _}.
start(_StartType, _StartArgs) ->
    io:format("Started~n"),
    game_sup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
    ok.


