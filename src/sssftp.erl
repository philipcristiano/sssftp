-module(sssftp).

-export([add_sftp_server/1,
         create_child_spec/1]).

-compile([{parse_transform, lager_transform}]).

add_sftp_server(Opts) ->
    {ok, [SFTPProc| UserSessionProc]} = create_child_spec(Opts),
    {ok, _} = supervisor:start_child(sssftp_sup, UserSessionProc),
    {ok, _} = supervisor:start_child(sssftp_sup, SFTPProc),
    ok.


create_child_spec(Opts) ->
    Port = proplists:get_value(port, Opts),
    AWS_BUCKET = proplists:get_value(aws_bucket, Opts),
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
                       [sssftp_user_session]},
    {ok, [UserSessionProc, SFTPProc]}.
