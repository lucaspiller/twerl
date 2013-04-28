all: compile

deps:
	./rebar get-deps

compile: deps
	./rebar compile

dev: compile
	erl -pa deps/*/ebin -pa ebin -s inets start -s ssl start -s sync start

test: spec

spec: compile
	./espec spec

clean:
	./rebar clean
	rm -Rf .eunit
