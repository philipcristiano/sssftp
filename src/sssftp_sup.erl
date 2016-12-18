-module(sssftp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    AWS_BUCKET = os:getenv("AWS_BUCKET"),
	Procs = [{server,
               {ssh, daemon, [8989, [{system_dir, "/tmp/"},
                              {system_dir, "/etc/ssh"},
                              {key_cb, sssftp_server_key},
                              {connectfun, fun sssftp_s3_api:connectfun/3},
                              {subsystems, [ssh_sftpd:subsystem_spec([{file_handler, {sssftp_s3_api, [{aws_bucket, AWS_BUCKET}]}}])
                                        ]}]]},
                permanent,
                5000,
                worker,
                [sssftp_s3_api]},
             {user_session, {sssftp_user_session, start_link, []},
                permanent,
                5000,
                worker,
                [user_session]}],
	{ok, {{one_for_one, 1, 5}, Procs}}.
