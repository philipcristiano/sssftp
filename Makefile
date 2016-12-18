PROJECT = sssftp
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

LOCAL_DEPS = sasl ssh
DEPS = erlcloud

dep_erlcloud = git https://github.com/erlcloud/erlcloud.git 2.0.4

SHELL_OPTS = -eval 'application:ensure_all_started(sssftp).'

include erlang.mk
