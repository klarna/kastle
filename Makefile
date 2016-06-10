PROJECT = kastle
PROJECT_DESCRIPTION = Apache Kafka REST Proxy
PROJECT_VERSION = 1.2.1

# Whitespace to be used when creating files from templates.
SP = 2
DEPS = brod cowboy lager jiffy jesse
TEST_DEPS = meck

dep_brod_commit = 2.1.2
dep_cowboy_commit = 1.0.4

rel:: rel/sys.config

rel/sys.config: | rel/sys.config.example
	cp $| $@

include erlang.mk

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}' -DAPPLICATION=kastle

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

.PHONY: test-env stop-daemon start-daemon t

test-env:
	./scripts/setup-test-env.sh

stop-daemon:
	./scripts/kastle-daemon.sh stop

start-daemon:
	./scripts/kastle-daemon.sh start

clean-t: stop-daemon test-env t
	$(verbose) :

t: stop-daemon rel start-daemon ct
	$(verbose) :

TOPDIR = /tmp/kastle-rpm
PWD = $(shell pwd)

rpm: all
	@rpmbuild -v -bb \
			--define "_sourcedir $(PWD)" \
			--define "_builddir $(PWD)" \
			--define "_rpmdir $(PWD)" \
			--define "_topdir $(TOPDIR)" \
			--define "_name $(PROJECT)" \
			--define "_description $(PROJECT_DESCRIPTION)" \
			--define "_version $(PROJECT_VERSION)" \
			rpm/kastle.spec

