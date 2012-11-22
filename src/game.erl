-module(game).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-type state() :: [any()] | [] | undefined.
-type start_link_error() :: {already_started, pid()} | term().

-export([submit/2, get_player/0, get_player/1]).

-type player_name() :: atom().
-type player() :: {player_name(), tuple()} | {error, not_found}.

%% later this will be private or moved whatever
-export([get_longest_word/1]).

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

-spec submit(player_name(), string()) -> ok.
submit(Player, Sentence) -> 
    Score=submit_score(Player, length(get_longest_word(Sentence))),
    % 
    % "Great job <USER>, you have a new <HIGHSCORE> with word <WORD>!".
    % or print score: x
    ok.

-spec get_player() -> [player()].
get_player() -> gen_server:call(?MODULE, all_players).

-spec get_player(player_name()) -> player().
get_player(Player) ->
    gen_server:call(?MODULE, {player, Player}).

%%
%% Private functions
%%

-spec submit_score(player_name(), integer()) -> integer().
submit_score(Player, Score) ->
   gen_server:call(?MODULE, {submit_score, Player, Score}).

get_longest_word(Sentence) ->
    List=[{X, length(X)} || X <- string:tokens(Sentence, " ")],
    [{Word,_}|_]=lists:reverse(lists:keysort(2,List)),
    Word.

%%
%% Callback functions for gen_server
%%
-spec handle_call(_, {pid(),_}, state()) -> 
    {noreply,state()}       | {noreply,state(),hibernate | timeout()} |
    {reply,reply(),state()} | {reply,reply(),state(),hibernate | timeout()} |
    {stop,reason(),state()} | {stop,reason(),reply(),state()}.
handle_call(all_players, _From, State) ->
    Reply=[],
    {reply, Reply, State};

handle_call({player, Player}, _From, State) ->
    %Reply={Player, {}},
    Reply={error, not_found},
    {reply, Reply, State};

handle_call({submit_score, Player, Score}, _From, State) ->
    %handle state shit here
    {reply, Score, State};

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
