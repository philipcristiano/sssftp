PROJECT = sssftp
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

LOCAL_DEPS = ssh

SHELL_OPTS = -eval 'application:ensure_all_started(sssftp).'

include erlang.mk
