#!/bin/sh
make compile
erl -sname game_server -config app -pa ./deps/lager/ebin -pa ./ebin -eval "application:start(game), lager:start()."
