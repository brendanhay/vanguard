#
# Targets
#

REBAR=./bin/rebar

.PHONY: build deps backends

all: deps compile

clean:
	rm -rf ebin
	rm -rf log
	rm -rf logs
	rm -rf rel/package
	$(REBAR) skip_deps=true clean

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile
	$(MAKE) xref

# Target named as such because of build tool compatibility
# (usually I would use 'package' here)
build: all
	rm -rf rel/package
	$(REBAR) generate -f

doc:
	$(REBAR) skip_deps=true doc

#
# Tests
#

unit:
	$(REBAR) eunit skip_deps=true suite=$(T)

integration:
	$(REBAR) skip_deps=true compile
	$(REBAR) ct skip_deps=true suites=$(T)

test: compile unit integration

#
# Run
#

DEPS=deps/*/ebin
ERL=exec erl -pa ebin $(DEPS) -sname vanguard -hidden -connect_all false

.PHONY: dev

console: compile
	rel/package/bin/vanguard console

noconsole: compile
	rel/package/bin/vanguard noconsole

boot: compile
	$(ERL) -s vanguard

noboot: compile
	$(ERL)

dev:
	./dev/run

#
# Analysis
#

PLT=./plt/R15B.plt

WARNINGS=-Werror_handling \
  -Wrace_conditions \
  -Wunderspecs \
  -Wunmatched_returns

APPS=kernel stdlib sasl erts ssl observer \
  tools os_mon runtime_tools crypto otp_mibs \
  inets xmerl webtool snmp public_key \
  mnesia eunit syntax_tools compiler hipe

compile-plt: all
	dialyzer --compile_plt --output_plt $(PLT) \
	  --apps $(APPS) $(DEPS)

dialyzer: compile
	dialyzer ebin --plt $(PLT) $(WARNINGS) \
	  | grep -v 'lager_not_running'

xref:
	$(REBAR) skip_deps=true xref

typer:
	typer --annotate --plt $(PLT) -I deps/
