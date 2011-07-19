deps:
	./rebar get-deps

compile: deps
	./rebar compile

dev: compile
	erl -pa deps/*/ebin -pa ebin -s inets start -s reloader start

eunit: compile
	./rebar eunit app=twerl

clean:
	./rebar clean
	rm -Rf .eunit
