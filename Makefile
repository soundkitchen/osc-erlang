#
#
#

PROJECT=osc
REBAR=./rebar
CONFIG=./rebar.config

.PHONY: all compile shell clean

all: compile

compile:
	@${REBAR} -C ${CONFIG} compile skip_deps=true

shell:
	@erl -pa ../${PROJECT}/ebin

clean:
	@${REBAR} -C ${CONFIG} clean
