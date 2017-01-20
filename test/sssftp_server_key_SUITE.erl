-module(sssftp_server_key_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_testcase/2, end_per_testcase/2]).

-export([is_auth_key_test/1]).

-define(MUT, sssftp_server_key).

all() -> [{group, test_init}].

groups() -> [{test_init,
             [],
             [is_auth_key_test]}].


init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

is_auth_key_test(_Config) ->
    DaemonOptions = [{subsystems,
                        [{"sftp",
                            {ssh_sftpd,
                                [{user_auth_server, user_auth_server}]}}]}],

    ok = meck:new(erlcloud_s3, []),
    ok = meck:new(sssftp_user_session, []),
    ok = meck:new(public_key, []),

    ok = meck:expect(erlcloud_s3, get_object, fun(_, _) -> [{contents, []}] end),
    ok = meck:expect(sssftp_user_session, add, fun(user_auth_server, _) -> ok end),
    ok = meck:expect(public_key, ssh_decode, fun(_, public_key) -> [{key, undefined} | undefined] end),

    true = ?MUT:is_auth_key(key, "USER", DaemonOptions).
