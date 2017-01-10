-module(sssftp_server_key).

-behaviour(ssh_server_key_api).
-compile([{parse_transform, lager_transform}]).


-export([host_key/2,
         is_auth_key/3]).


host_key(Algorithm, DaemonOptions) ->
    ssh_file:host_key(Algorithm, DaemonOptions).

is_auth_key(Key, User, _DaemonOptions) ->
    ok = sssftp_user_session:add(sssftp_user_session, User),
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
