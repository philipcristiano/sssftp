-module(sssftp_app).
-behaviour(application).

-compile([{parse_transform, lager_transform}]).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	{ok, Pid} = sssftp_sup:start_link(),
    {Port, _} = string:to_integer(os:getenv("PORT", "8989")),
    AWS_BUCKET = os:getenv("AWS_BUCKET"),
    ok = lager:info("Starting on port ~p", [Port]),
    UserAuthServerOpt = {user_auth_server, sssftp_user_session},
    SFTPProc = {sftp_proc,
                 {ssh, daemon, [Port, [{system_dir, "/tmp/"},
                              {system_dir, "/etc/ssh"},
                              {key_cb, sssftp_server_key},
                              {connectfun, fun sssftp_s3_api:connectfun/3},
                              {subsystems, [ssh_sftpd:subsystem_spec([
                                    {file_handler, {sssftp_s3_api, [{aws_bucket, AWS_BUCKET}, UserAuthServerOpt]}},
                                    UserAuthServerOpt,
                                    {cwd, "/"}])
                                        ]}]]},
                permanent,
                5000,
                worker,
                [sssftp_s3_api]},
    UserSessionProc = {user_session, {sssftp_user_session, start_link, []},
                       permanent,
                       5000,
                       worker,
                       [user_session]},
    {ok, _} = supervisor:start_child(Pid, UserSessionProc),
    {ok, _} = supervisor:start_child(Pid, SFTPProc),
    {ok, Pid}.

stop(_State) ->
	ok.
