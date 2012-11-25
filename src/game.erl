-module(game).

-behaviour(gen_server).

-export([start_link/0, submit/2, get_player/0, get_player/1, get_scores/0, get_scores/1, get_highscore/0]).

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
    InitState=#state {players=[], scores=[], highscore={factory, 0}},
    {ok, InitState}.

-spec submit(player_name(), string()) -> ok.
submit(Player, Sentence) -> 
    {Score, Word}=submit_score(Player, Sentence),
    lager:log(info, [], "~s submit score ~p", [Player, Score]),
    case gen_server:call(?MODULE, update_highscore) of
        %improve this by adding the word to the highscore results
        {Player, HighScore} ->
            io:format("Great job ~p you have a new highscore: ~p with word ~p!!~n", [Player, HighScore, Word]);
        _ ->
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

-spec get_scores() -> [{player_name(), integer(), string()}] | [].
get_scores() ->
    gen_server:call(?MODULE, all_scores).

-spec get_scores(player_name()) -> [{integer(), string()}] | [].
get_scores(Name) ->
    %filter all scores on this player
    [{Score, Word} || {Player, Score, Word} <- get_scores(), Player == Name].

-spec get_highscore() -> {player_name(), integer()}.
get_highscore() ->
    gen_server:call(?MODULE, get_highscore).

%%--------------------------------------------------------------------
%% Private functions
%%--------------------------------------------------------------------

-spec submit_score(player_name(), integer()) -> {integer(), string()}.
submit_score(Player, Score) ->
    gen_server:call(?MODULE, {submit_score, Player, Score}).

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

handle_call(all_scores, _From, #state{scores=Scores}=State) ->
    {reply, Scores, State};

handle_call(get_highscore, _From, #state{highscore=Highscore}=State) ->
    {reply, Highscore, State};

handle_call({submit_score, PlayerName, Sentence}, _From, #state{players=Players,scores=Scores}=State) ->
    %make sure user is updated
    Player=case lists:keyfind(PlayerName, 1, Players) of
        false -> {PlayerName, []};
        Other when is_tuple(Other) -> Other
    end,
    PlayersList= lists:keystore(PlayerName, 1, Players, update_player_facts(Player, [])),
    % add highscore shit here
    Word=get_longest_word(Sentence),
    Score=length(Word),
    ScoresList=[{PlayerName, Score, Word}|Scores],
    {reply, {Score, Word}, State#state{players=PlayersList,scores=ScoresList}};

handle_call(update_highscore, _From, #state{highscore=Highscore, scores=Scores}=State) ->
    [{Player,Topscore,_}|_]=lists:reverse(lists:keysort(2, Scores)),
    lager:log(info, [], "highscore-state ~p, top ~p", [Highscore, Topscore]),
    NewOne=case Highscore of
        {factory, 0} ->
            lager:log(info, [], "update_highscore, first time"),
            {Player,Topscore};
        {_,Oldscore} when Topscore > Oldscore ->
            lager:log(info, [], "highscore replaced by ~s, old ~p, new ~p", [Player, Oldscore, Topscore]),
            {Player,Topscore};
        {_,_} ->
            lager:log(info, [], "highscore untouched"),
            Highscore
    end,
    {reply, NewOne, State#state{highscore=NewOne}};

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
