-module(game_server).

-behaviour(gen_server).

%% API
-export([start_link/0, say_hello/0]).

-type state() :: [any()] | [] | undefined.
-type start_link_error() :: {already_started, pid()} | term().

%% Callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type reply() :: term().
-type reason() :: term().

-spec start_link() -> {ok,pid()} | {error,start_link_error()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, state()} | {ok, state(), timeout()} | {ok, state(), hibernate} |
    {stop, reason()}.
init([]) ->
    {ok, []}.

%
% Public API delegated to sync | async calls
%
-spec say_hello() -> ok.
say_hello() ->
    gen_server:call(?MODULE, hello).

%%
%% Callback functions for gen_server
%%
-spec handle_call(_, _, state()) -> 
    {noreply,state()}       | {noreply,state(),hibernate | timeout()} |
    {reply,reply(),state()} | {reply,reply(),state(),hibernate | timeout()} |
    {stop,reason(),state()} | {stop,reason(),reply(),state()}.
handle_call(hello, _From, State) ->
    {reply, hello, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(_, state()) -> {noreply, _}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(_, state()) -> {noreply, _}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(reason(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(_, state(), _) -> {ok, _}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
