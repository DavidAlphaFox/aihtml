PROJECT = aihtml
PROJECT_DESCRIPTION = html tool for productions from ailink.io
<<<<<<< HEAD
PROJECT_VERSION = 0.1.2
=======
PROJECT_VERSION = 0.1.4
>>>>>>> b9bb94c5b9cb0f444e45ace8b6fd79dc888277f0

ERLC_OPTS = -Werror +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
DEPS = ailib

dep_ailib = git https://github.com/DavidAlphaFox/ailib.git tag-0.1.2

include erlang.mk 
