PROJECT = kastle
PROJECT_DESCRIPTION = Apache Kafka REST interface
PROJECT_VERSION = 1.0.0

# Whitespace to be used when creating files from templates.
SP = 2
DEPS = brod cowboy lager jiffy jesse
dep_brod = git https://github.com/klarna/brod.git add-option-to-autostart-producers

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

