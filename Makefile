PROJECT = kastle
PROJECT_DESCRIPTION = Apache Kafka REST interface
PROJECT_VERSION = 1.0.0

# Whitespace to be used when creating files from templates.
SP = 2
DEPS = brod cowboy lager jiffy jesse
dep_brod = git https://github.com/klarna/brod.git 2.0-dev

include erlang.mk

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}' -DAPPLICATION=kastle

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

.PHONY: test-env t

test-env: rel
	./scripts/setup-test-env.sh

t: test-env ct
	$(verbose) :

