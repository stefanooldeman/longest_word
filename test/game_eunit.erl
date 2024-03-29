-module(game_eunit).
-include_lib("eunit/include/eunit.hrl").

%% Setup function
start() ->
    Name=game,
    %application:stop(Name),
    ok = application:start(Name),
    Name.

%% Cleanup function
stop(Name) ->
    application:stop(Name),
    timer:sleep(500).

%% Setup function
game_well_played() ->
    ok = application:start(game),
    Max=[{max, S1} || S1 <- [
        "To insert into a text too hurriedly or inappropriately", %score 15 (highest highscore)
        "pertaining to membership by citizenship rather than kinship", %score 11
        "Boustrophedon, Of writing, alternating left to right then right to left." %score 14
    ]],
    Seb=[{seb, S2} || S2 <- [
        "Condition of having abnormally large digits.", %score 10
        "Barathrum, a bottomless pit, which characterises the stomach of some people", %score 13
        "government by the people" %score 10
    ]],
    Jay=[{jay, S3} || S3 <- [
       "bronze-coloured-metal alloy used for decorative vessels", %score 19 (highscore overwritten)
       "dolee, one who receives a government benefit, especially unemployment benefits", %score 12
       "dulia, inferior veneration of saints and angels in comparison with God" %score 10
    ]],
    Players=[Max, Seb, Jay],
    %submit all sentences
    lists:map(fun(Batch) ->
        lists:map(fun(Row) ->
            {PlayerName, Val}=Row,
            game:submit(PlayerName, Val)
        end, Batch)
    end, Players),
    Players.

%% Cleanup function
game_finished(_) ->
    application:stop(game),
    timer:sleep(500).

ranking_scores_test_() ->
    {setup,
     fun game_well_played/0, % Setup
     fun game_finished/1,    % Cleanup
     fun (SetupData) ->
        [ % --- list of tests ----
         %basic_numbers(),
         %scores_ratings()
        ]
     end}.

basic_numbers() ->
    [?_assertMatch(9, length(game:get_scores())),
     ?_assertMatch(3, length(game:get_player())),
     ?_assertMatch(3, length(game:get_scores(jay))),
     ?_assertMatch(3, length(game:get_scores(max))),
     ?_assertMatch(3, length(game:get_scores(seb)))
    ].

scores_ratings() ->
    [?_assertMatch({jay, 21}, game:get_highscore()),
     ?_assertMatch([
                {10, "comparison"},
                {12, "unemployment"},
                {21, "bronze-coloured-metal"}
            ], game:get_scores(jay))
    ].

basic_actions_test_() ->
    {foreach,
     fun start/0, % Setup
     fun stop/1,  % Cleanup
     [ % --- list of tests ----
        %players
        fun create_first_player/1,
        fun players_should_be_added_once/1,
        fun players_ordered/1,
        %scores
        % fun scores_get_saved/1,
        % fun scores_dataformat/1,
        % fun get_scores_player/1,
        fun show_highscore_record/1,
        %internals
        fun get_longest_word/1
      ]
    }.

create_first_player(_) ->
    PlayerName=stefano,
    [?_assertMatch([], game:get_player()),
     ?_assertMatch({error, not_found}, game:get_player(PlayerName)),
     ?_assertMatch(ok, game:submit(PlayerName, "this is my foobar")),
     ?_assertMatch({PlayerName, Facts} when is_list(Facts), game:get_player(PlayerName))
     ].

players_should_be_added_once(_) ->
    [?_assertMatch(ok, game:submit(foo, "bla bla")),
     ?_assertMatch(ok, game:submit(foo, "this is my foobar")),
     ?_assertMatch([{foo, [{played, 2}, {rank, Rank}]}] when is_integer(Rank), game:get_player())
     ].

% players are retrieved in the order that they applied
players_ordered(_) ->
    [?_assertMatch([], game:get_player()),
     ?_assertMatch(ok, game:submit(foo, "this is my foobar")),
     ?_assertMatch(ok, game:submit(bar, "this is my bar")),
     ?_assertMatch([
            {foo, [{played, 1}, {rank, 0}]},
            {bar, [{played, 1}, {rank, 0}]}
        ], game:get_player())
     ].

scores_get_saved(_) ->
    game:submit(john, "arithmic"),
    game:submit(tommie_lie, "cadence"),
    game:submit(simson, "Whoopsiedoh!"),
    [?_assertMatch([Ta, _, _] when is_tuple(Ta), game:get_scores())].

scores_dataformat(_) ->
    game:submit(john, "foobar"),
    [?_assertMatch([{john, 6, "foobar"}], game:get_scores())].

% Results are in reversed chronoligical order
% _that means, head recursive, no ordering and no reversing
get_scores_player(_) ->
    [?_assertMatch(ok, game:submit(john, "reversed chronoligical order")),
     ?_assertMatch(ok, game:submit(john, "cadence")),
     ?_assertMatch([{7, "cadence"}, {13, "chronoligical"}], game:get_scores(john))
     ].

show_highscore_record(_) ->
    [?_assertMatch(ok, game:submit(john, "reversed chronoligical order")),
     ?_assertMatch(ok, game:submit(random_dude, "fooobar is used here often")),
     ?_assertMatch(ok, game:submit(john, "running and cadence are related")),
     ?_assertMatch(ok, game:submit(john, "this is a nice training for your vocabulaire")),
     ?_assertMatch({john, 13, "chronoligical"}, game:get_highscore())
     ].

get_longest_word(_) ->
    [?_assertEqual("basics", game:get_longest_word("this are the basics")),
     ?_assertMatch("thingy", game:get_longest_word("Foo, is a weird! thingy")),
     ?_assertMatch("weird!", game:get_longest_word("Foo, is a thingy weird!")), % it will pick the last one
     %todo filter special chars
     ?_assertEqual("bla.", game:get_longest_word("bla bla bla.")) % it should ignore . sign
    ].

