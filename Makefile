REBAR ?= rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps

test: compile
	@$(REBAR) xref eunit recursive=false

examples: eg
eg:
	@erlc -I deps -o ebin examples/*.erl

.PHONY: test dialyzer typer clean

dialyzer:
	@$(REBAR) dialyzer

typer:
	@typer -I include --plt _build/default/*_plt -r ./src
