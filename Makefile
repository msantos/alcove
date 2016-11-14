.PHONY: test dialyzer typer clean compile examples eg all

REBAR ?= rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

test:
	@$(REBAR) ct

examples: eg
eg:
	@erlc -I deps -o ebin examples/*.erl

dialyzer:
	@$(REBAR) dialyzer

typer:
	@typer -pa _build/default/lib/alcove/ebin -I include --plt _build/default/*_plt -r ./src
