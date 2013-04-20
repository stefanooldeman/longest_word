## Longest word game
this is a good example on how to use erlang OTP and build a application with `gen_server`.
this is made very easy because of [rebar (see at github)](https://github.com/rebar/rebar), which can setup the basic project for
you, nothing too fancy. the basics contain:
- a supervisor (see `src/game_sup.erl`)
- basic OTP folder structure
- application resource file etc

Also eunit is shipped with standard erl, and `make test` generates
the coverage (because thats defined in `rebar.config`).

## How to play
Run `make docs` for to take a look at the API (always up to date).

For quick start this should get you playing:
```
make run
(game_server@localhost)1> game:submit(stefano, "The supersophisticated cats").
Great job stefano you have a new highscore: 18 with word "supersophisticated"!!
ok
(game_server@localhost)1> game:get_highscore().
{stefano,18,"supersophisticated"}
```

This repos was a learning project for me. I might be a nice resource for others.
