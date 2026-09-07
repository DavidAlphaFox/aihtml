# NOTE: rebar3 is the primary build path for aihtml (see designs/05-rebar3-plugin.md).
#
#   rebar3 compile
#   rebar3 ct
#   rebar3 dialyzer
#   rebar3 xref
#
# This erlang.mk Makefile is kept for compatibility with downstream projects that
# still build with erlang.mk. It is not exercised by CI and may lag behind.

PROJECT = aihtml
PROJECT_DESCRIPTION = Mustache and Jinja2 template engines that compile templates to Erlang modules
PROJECT_VERSION = 0.5.0

ERLC_OPTS = -Werror +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard

# aihtml has no dependencies by design (decision D5, see designs/02-architecture.md).
DEPS =

include erlang.mk

COMPILE_FIRST +=
