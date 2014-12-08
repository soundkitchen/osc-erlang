#
#
#

PROJECT=osc
REBAR=./rebar
CONFIG=./rebar.config

.PHONY: all compile test shell clean

all: compile

compile:
	@${REBAR} -C ${CONFIG} compile skip_deps=true
	@${REBAR} -C ${CONFIG} xref skip_deps=true

test:
	@rm -rf .eunit
	@${REBAR} -C ${CONFIG} eunit skip_deps=true

shell:
	@erl -pa ../${PROJECT}/ebin

clean:
	@${REBAR} -C ${CONFIG} clean skip_deps=true
