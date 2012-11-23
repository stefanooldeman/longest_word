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

stateless_actions_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [ % --- list of tests ----
        %players
        fun create_first_player/1,
        fun players_should_be_added_once/1,
        fun players_ordered/1,
        %scores
        fun scores_get_saved/1,
        fun scores_dataformat/1,
        fun get_scores_player/1,
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
     ?_assertMatch([{foo, []}], game:get_player())
     ].

% players are retrieved in the order that they applied
players_ordered(_) ->
    [?_assertMatch([], game:get_player()),
     ?_assertMatch(ok, game:submit(foo, "this is my foobar")),
     ?_assertMatch(ok, game:submit(bar, "this is my bar")),
     ?_assertMatch([{foo, []}, {bar, []}], game:get_player())
     ].

scores_get_saved(_) ->
    game:submit(john, "arithmic"),
    game:submit(tommie_lie, "cadence"),
    game:submit(simson, "Whoopsiedoh!"),
    [?_assertMatch([Ta, Tb, Tc] when is_tuple(Ta), game:get_scores())].

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

get_longest_word(_) ->
    [?_assertEqual("basics", game:get_longest_word("this are the basics")),
     ?_assertMatch("thingy", game:get_longest_word("Foo, is a weird! thingy")),
     ?_assertMatch("weird!", game:get_longest_word("Foo, is a thingy weird!")), % it will pick the last one
     %todo filter special chars
     ?_assertEqual("bla.", game:get_longest_word("bla bla bla.")) % it should ignore . sign
    ].

