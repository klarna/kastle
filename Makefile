PROJECT = kastle
DEPS = brod cowboy lager
dep_brod = git https://github.com/klarna/brod.git 2.0-dev

include erlang.mk

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}'

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)