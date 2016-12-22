PROJECT = sssftp
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

SHELL_DEPS = sync
LOCAL_DEPS = sasl ssh
DEPS = lager erlcloud
EUNIT_OPTS = verbose
EUNIT_ERL_OPTS = -config sssftpd.config -s lager

dep_lager = git https://github.com/basho/lager.git 3.2.4
dep_erlcloud = git https://github.com/erlcloud/erlcloud.git 2.0.4

SHELL_OPTS = -eval 'application:ensure_all_started(sssftp).' -config sssftpd

include erlang.mk
