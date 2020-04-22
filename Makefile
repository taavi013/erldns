PROJECT = erldns
PROJECT_DESCRIPTION = Serve DNS authoritative responses... with Erlang.
PROJECT_VERSION = 1.0.0

DEPS = lager recon folsom jsx iso8601
LOCAL_DEPS = dns_erlang

TEST_DEPS = proper

include erlang.mk

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}'

#
# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
