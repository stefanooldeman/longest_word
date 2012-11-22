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
        fun create_first_player/1,
        fun get_longest_word/1
      ]
    }.

create_first_player(_) ->
    PlayerName=stefano,
    [?_assertMatch([], game:get_player()),
     ?_assertMatch({error, not_found}, game:get_player(PlayerName)),
     ?_assertMatch(ok, game:submit(PlayerName, "this is my foobar")),
     ?_assertMatch({PlayerName, T} when is_tuple(T), game:get_player(PlayerName))
     ].


get_longest_word(_) ->
    [?_assertEqual("basics", game:get_longest_word("this are the basics")),
     ?_assertMatch("thingy", game:get_longest_word("Foo, is a weird! thingy")), % it should leave ! sign out
     ?_assertEqual("bla", game:get_longest_word("bla bla bla.")) % it should ignore . sign
    ].

