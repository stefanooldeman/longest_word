.PHONY: deps test

all: deps compile

# Compiles the whole application
compile:
	rebar compile
	-rebar skip_deps=true xref

# Gets the dependencies to the deps folder. This is necessary for compile to succeed.
deps:
	rebar get-deps

# Cleans any generated files from the repo (except dependencies)
clean:
	rm -rf .eunit/*.xml .eunit/*.html .eunit/*.beam .eunit/*.erl .eunit/*.log
	rm -f ebin/*
	rm -f test/ebin/*
	rm -f doc/*.html doc/*.css doc/*.png doc/edoc-info

# Cleans any downloaded dependencies
distclean: clean
	rebar delete-deps

# Runs every test suite under test/ abd generates an html page with detailed info about test coverage
test: compile
	rebar skip_deps=true eunit

run: compile
	erl -sname game_server -config app -pa ./deps/lager/ebin -pa ./ebin -eval "application:start(game), lager:start()."

# Generates the edoc documentation and places it under doc/ .
docs:
	rebar skip_deps=true doc

# While developing with vi, :!make dialyzer | grep '%:t' can be used to run dialyzer in the current file
dialyzer: clean compile
	dialyzer -Wno_return -c ebin

