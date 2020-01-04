PROJECT = aihtml
PROJECT_DESCRIPTION = html tool for productions from ailink.io
PROJECT_VERSION = 0.3.3

ERLC_OPTS = -Werror +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
DEPS = ailib

dep_ailib = git https://github.com/DavidAlphaFox/ailib.git tag-0.4.1

include erlang.mk 
