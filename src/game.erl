-module(game).

-behaviour(gen_server).

-export([start_link/0, submit/2, get_player/0, get_player/1, get_scores/0, get_scores/1]).

%% later this will be private or moved whatever
-export([get_longest_word/1]).

%% Callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type player_name() :: atom().
-type player() :: {player_name(), [{atom(), term()}]}.
-type reply() :: term().
-type reason() :: term().

-record(state, {
    players :: [player()],
    scores :: [{player_name(), integer(), string()}],
    highscore :: {player_name(), integer()}
}).

%%--------------------------------------------------------------------
%% API (delegated to sync | async calls)
%%--------------------------------------------------------------------

-spec start_link() -> {ok,pid()} | {error,{already_started, pid()}}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, record()} | {ok, record(), timeout()} | {ok, record(), hibernate} |
    {stop, reason()}.
init([]) ->
    InitState=#state {players=[], scores=[], highscore={}},
    {ok, InitState}.

-spec submit(player_name(), string()) -> ok.
submit(Player, Sentence) -> 
    Score=submit_score(Player, Sentence),
    % "Great job <USER>, you have a new <HIGHSCORE> with word <WORD>!".
    Msg=lists:concat(["score: ", Score, "~n"]),
    io:format(Msg),
    ok.

-spec get_player() -> [player()] | [].
get_player() ->
    gen_server:call(?MODULE, all_players).

-spec get_player(player_name()) -> player() | {error, not_found}.
get_player(Name) ->
    case lists:keyfind(Name, 1, gen_server:call(?MODULE, all_players)) of
        false -> {error, not_found};
        T when is_tuple(T) -> T
    end.

-spec get_scores() -> [{player_name(), integer(), string()}] | [].
get_scores() ->
    []. %lists:reverse(gen_server:call(?MODULE, all_scores)).

-spec get_scores(player_name()) -> [{integer(), string()}] | [].
get_scores(Name) ->
    %list comprehension
    []. %lists:reverse(gen_server:call(?MODULE, all_scores)).

%%--------------------------------------------------------------------
%% Private functions
%%--------------------------------------------------------------------

-spec submit_score(player_name(), integer()) -> integer().
submit_score(Player, Score) ->
   gen_server:call(?MODULE, {submit_score, Player, Score}).

-spec get_longest_word(string()) -> string().
get_longest_word(Sentence) ->
    List=[{X, length(X)} || X <- string:tokens(Sentence, " ")],
    [{Word,_}|_]=lists:reverse(lists:keysort(2,List)),
    Word.

%%--------------------------------------------------------------------
%% Callback functions for gen_server
%%--------------------------------------------------------------------
-spec handle_call(_, {pid(),_}, record()) -> 
    {noreply,record()}       | {noreply,record(),hibernate | timeout()} |
    {reply,reply(),record()} | {reply,reply(),record(),hibernate | timeout()} |
    {stop,reason(),record()} | {stop,reason(),reply(),record()}.
handle_call(all_players, _From, #state{players=Players}=State) ->
    {reply, Players, State};

handle_call(all_scores, _From, #state{scores=Scores}=State) ->
    {reply, Scores, State};

handle_call({submit_score, PlayerName, Sentence}, _From, #state{players=Players,scores=Scores}=State) ->
    %make sure user is updated
    Facts=[],
    PlayersList= lists:keystore(PlayerName, 1, Players, {PlayerName, Facts}),
    % add highscore shit here
    Word=get_longest_word(Sentence),
    Score=length(Word),
    ScoresList=[{PlayerName, Score, Word}|Scores],
    {reply, Score, State#state{players=PlayersList,scores=ScoresList}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(_, record()) -> {noreply, _}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(_, record()) -> {noreply, _}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(reason(), record()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(_, record(), _) -> {ok, _}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
