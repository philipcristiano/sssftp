-module(sssftp_sup).
-behaviour(supervisor).
-compile([{parse_transform, lager_transform}]).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {Port, _} = string:to_integer(os:getenv("PORT", "8989")),
    AWS_BUCKET = os:getenv("AWS_BUCKET"),
    lager:info("Starting on port ~p", [Port]),
	Procs = [{server,
               {ssh, daemon, [Port, [{system_dir, "/tmp/"},
                              {system_dir, "/etc/ssh"},
                              {key_cb, sssftp_server_key},
                              {connectfun, fun sssftp_s3_api:connectfun/3},
                              {subsystems, [ssh_sftpd:subsystem_spec([
                                    {file_handler, {sssftp_s3_api, [{aws_bucket, AWS_BUCKET}]}},
                                    {cwd, "/"}])
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
