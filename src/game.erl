-module(game).

-behaviour(gen_server).

-export([start_link/0, submit/2, get_player/0, get_player/1, get_highscore/0]).

%% later this will be private or moved whatever
-export([get_longest_word/1]).

%% Callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type player_name() :: atom().
-type player() :: {player_name(), [{atom(), term()}]}.
-type reply() :: term().
-type reason() :: term().
-type highscore() :: {player_name(), integer(), string()}.

-record(state, {
    players :: [player()],
    % FIXME top10 :: [{player_name(), integer(), string()}],
    highscore :: highscore()
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
    InitState=#state {players=[], highscore={factory, 0, ""}},
    {ok, InitState}.

-spec submit(player_name(), string()) -> ok.
submit(Player, Sentence) -> 
    Word=get_longest_word(Sentence),
    Score=length(Word),

    _ = update_players(Player, Score),
    lager:log(info, [], "~s submit score ~p", [Player, Score]),
    case update_highscore(Player, Score, Word) of
        new ->
            Msg = "Great job ~p you have a new highscore: ~p with word ~p!!~n",
            io:format(Msg, [Player, Score, Word]);
        untouched ->
            io:format("score: ~p~n", [Score])
    end,
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

-spec get_highscore() -> highscore().
get_highscore() ->
    gen_server:call(?MODULE, get_highscore).

%%--------------------------------------------------------------------
%% Private functions
%%--------------------------------------------------------------------
-spec update_highscore(player_name(), integer(), string()) -> highscore().
update_highscore(Player, Score, Word) ->
    gen_server:call(?MODULE, {update_highscore, Player, Score, Word}).

-spec update_players(player_name(), integer()) -> {integer(), string()}.
update_players(Player, Score) ->
    gen_server:call(?MODULE, {update_players, Player, Score}).

-spec get_longest_word(string()) -> string().
get_longest_word(Sentence) ->
    List=[{X, length(X)} || X <- string:tokens(Sentence, " ")],
    [{Word,_}|_]=lists:reverse(lists:keysort(2,List)),
    Word.

-spec update_player_facts(player(), list()) -> player().
update_player_facts({PlayerName, []}, _ScoresList) ->
    lager:log(info, [], "~s added to players list", [PlayerName]),
    Facts=[{played, 1}, {rank, 0}],
    {PlayerName, Facts};

update_player_facts({PlayerName, Facts}, _ScoresList) ->
    NewFacts=lists:map(fun(Fact) ->
            case Fact of
                {rank, R} when is_integer(R) -> {rank, give_rank(PlayerName)};
                {played, X} -> {played, X+1}
            end
    end, Facts),
    {PlayerName, NewFacts}.

-spec give_rank(player_name()) -> integer().
give_rank(_PlayerName) ->
    0.


%%--------------------------------------------------------------------
%% Callback functions for gen_server
%%--------------------------------------------------------------------
-spec handle_call(_, {pid(),_}, record()) -> 
    {noreply,record()}       | {noreply,record(),hibernate | timeout()} |
    {reply,reply(),record()} | {reply,reply(),record(),hibernate | timeout()} |
    {stop,reason(),record()} | {stop,reason(),reply(),record()}.
handle_call(all_players, _From, #state{players=Players}=State) ->
    {reply, Players, State};

handle_call(get_highscore, _From, #state{highscore=Highscore}=State) ->
    {reply, Highscore, State};

handle_call({update_players, PlayerName, Sentence}, _From, #state{players=Players}=State) ->
    Player=case lists:keyfind(PlayerName, 1, Players) of
        false -> {PlayerName, []};
        Other when is_tuple(Other) -> Other
    end,
    % add players to list
    PlayersList= lists:keystore(PlayerName, 1, Players, update_player_facts(Player, [])),
    {reply, ok, State#state{players=PlayersList}};

handle_call({update_highscore, Player, Score, Word}, _From, #state{highscore=Highscore}=State) ->
    {Status, NewOne}=case Highscore of
        {factory, 0, _} ->
            lager:log(info, [], "update_highscore, first time"),
            {new, {Player, Score, Word}};
        {_,Oldscore,_} when Score > Oldscore ->
            lager:log(info, [], "highscore replaced by ~s, old ~p, new ~p", [Player, Oldscore, Score]),
            {new, {Player, Score, Word}};
        _Else ->
            lager:log(info, [], "highscore untouched"),
            {untouched, Highscore}
    end,

    {reply, Status, State#state{highscore=NewOne}};

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
