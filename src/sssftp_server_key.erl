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
    ok = sssftp_user_session:add(UserAuthServer, User),
    OurKey = get_key_for_user(User),
    IsValid = is_valid_key(Key, OurKey),
    ok = lager:info("Key for user '~p' valid? ~p", [User, IsValid]),
    IsValid.


get_key_for_user(User) ->
    KeyPath = "credentials/" ++ User ++ "/id_rsa.pub",
    ok = lager:debug("Getting key for user '~p' from'~p'", [User, KeyPath]),
    Bucket = os:getenv("AWS_BUCKET"),
    Object = erlcloud_s3:get_object(Bucket, KeyPath),
    Content = proplists:get_value(content, Object),
    [{Key, _Comment}|_Rest] = public_key:ssh_decode(Content, public_key),
    ok = lager:debug("Returning user key"),
    Key.

is_valid_key(Key, Key) ->
    true;
is_valid_key(_A, _B) ->
    false.
