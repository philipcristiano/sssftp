-module(sssftp_server_key).

-behaviour(ssh_server_key_api).
-compile([{parse_transform, lager_transform}]).


-export([host_key/2,
         is_auth_key/3]).


host_key(Algorithm, DaemonOptions) ->
    ssh_file:host_key(Algorithm, DaemonOptions).

is_auth_key(Key, User, DaemonOptions) ->
    Subsystems = proplists:get_value(subsystems, DaemonOptions),
    SFTPOptions = proplists:get_value("sftp", Subsystems),
    SSHSFTPOptions = proplists:get_value(ssh_sftpd, [SFTPOptions]),
    UserAuthServer = proplists:get_value(user_auth_server, SSHSFTPOptions),
    AWSBucket = proplists:get_value(aws_bucket, SSHSFTPOptions),
    {ok, S3Config0} = erlcloud_aws:auto_config(),
    S3Config1 = sssftp_s3_api:assume_role(S3Config0, SSHSFTPOptions, 900),
    ok = sssftp_user_session:add(UserAuthServer, User),
    OurKey = get_key_for_user(User, AWSBucket, S3Config1),
    IsValid = is_valid_key(Key, OurKey),
    ok = lager:info("Key for user '~p' valid? ~p", [User, IsValid]),
    IsValid.


get_key_for_user(User, Bucket, Config) ->
    KeyPath = "credentials/" ++ User ++ "/id_rsa.pub",
    ok = lager:debug("Getting key for user '~p' from'~p'", [User, {Bucket, KeyPath}]),
    Object = erlcloud_s3:get_object(Bucket, KeyPath, Config),
    Content = proplists:get_value(content, Object),
    [{Key, _Comment}|_Rest] = public_key:ssh_decode(Content, public_key),
    ok = lager:debug("Returning user key"),
    Key.

is_valid_key(Key, Key) ->
    true;
is_valid_key(_A, _B) ->
    false.
