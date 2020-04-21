PROJECT = erldns
PROJECT_DESCRIPTION = Serve DNS authoritative responses... with Erlang.
PROJECT_VERSION = 1.0.0

DEPS = lager recon folsom jsx iso8601
LOCAL_DEPS = dns_erlang

TEST_DEPS = proper

include erlang.mk
