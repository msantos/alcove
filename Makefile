.PHONY: test dialyzer typer clean compile examples eg all generate gen doc

REBAR ?= rebar3

all: compile

compile:
	@$(REBAR) compile

generate: gen
gen:
	bin/alcove_version.escript > c_src/alcove_version.h
	bin/alcove_calls.sh c_src/alcove_call.proto > c_src/alcove_calls.h
	bin/alcove_call.sh c_src/alcove_call.proto > c_src/alcove_call.h
	bin/alcove_nr.sh c_src/alcove_call.proto > c_src/alcove_nr.h
	bin/alcove_proto.escript alcove_proto c_src/alcove_call.proto > src/alcove_proto.erl
	bin/alcove.escript alcove c_src/alcove_call.proto > src/alcove.erl

clean:
	@$(REBAR) clean

test:
	@$(REBAR) ct

examples: eg
eg:
	@mkdir -p ebin
	@erlc -I $(PWD)/.. -o ebin examples/*.erl

dialyzer:
	@$(REBAR) dialyzer

typer:
	@typer -pa _build/default/lib/alcove/ebin -I include --plt _build/default/*_plt -r ./src
