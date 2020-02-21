PROJECT = aihtml
PROJECT_DESCRIPTION = html tool for productions from ailink.io
PROJECT_VERSION = 0.3.6

ERLC_OPTS = -Werror +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
DEPS = ailib

dep_ailib = git https://github.com/DavidAlphaFox/ailib.git v0.4.3

include erlang.mk 
