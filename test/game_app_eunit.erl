-module(game_app_eunit).
-include_lib("eunit/include/eunit.hrl").

%% Setup function
start() ->
    Name=game,
    ok = application:start(Name),
    Sup=whereis(game_sup),
    Server=whereis(game_server),
    {Name, Sup, Server}.


%% Cleanup function
stop({Name, _Supervisor, _Server}) ->
    ok = application:stop(Name),
    timer:sleep(500).

start_test_() ->
    {setup, 
     fun start/0,
     fun stop/1,
     fun({_Name, Sup, Server}) ->
        [?_assertEqual(true, is_pid(Sup)),
         ?_assertEqual(true, is_pid(Server)),
         ?_assertEqual(undefined, whereis(game_app)),
         ?_assertEqual({error, {already_started, game}}, application:start(game))]
     end
    }.

stop_test_() ->
    {setup, 
     fun start/0,
     fun stop/1,
     fun({Name, _, _}) ->
        [?_assertEqual(ok, application:stop(Name)),
         ?_assertEqual({error,{not_started,Name}}, application:stop(Name))]
     end
    }.
