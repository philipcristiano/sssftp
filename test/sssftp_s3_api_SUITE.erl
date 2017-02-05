-module(sssftp_s3_api_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_testcase/2, end_per_testcase/2]).

-export([get_cwd_test/1,
         list_root_dir_test/1,
         open_test_no_file/1,
         connectfun_test/1]).

-define(MUT, sssftp_s3_api).

all() -> [{group, test_init}].

groups() -> [{test_init,
             [],
             [get_cwd_test,
              open_test_no_file,
              list_root_dir_test,
              connectfun_test]}].



init_per_testcase(connectfun_test, Config) ->
    Config;
init_per_testcase(get_cwd_test, Config) ->
    application:ensure_all_started(lager),

    ok = meck:new(erlcloud_s3, []),
    ok = meck:new(sssftp_user_session, []),

    InitState = {initstate, [{aws_bucket, "TESTBUCKET"},
                             {user_auth_server, user_auth_server}]},
    [InitState | Config];
init_per_testcase(_, Config) ->
    ok = lager_common_test_backend:bounce(debug),

    ok = meck:new(erlcloud_s3, []),
    ok = meck:new(sssftp_user_session, []),
    Contents = [
        [{key, "uploads/USER/file.txt"},
         {content_length, 1024}]],

    ok = meck:expect(erlcloud_s3, list_objects, fun(_, _) -> [{contents, Contents}] end),
    ok = meck:expect(sssftp_user_session, get, fun(user_auth_server, _) -> {ok, "USER"} end),

    State0 = [{aws_bucket, "TESTBUCKET"},
              {user_auth_server, user_auth_server}],
    {_, State1} = ?MUT:get_cwd(State0),

    InitState = {initstate, State1},
    [InitState | Config].

end_per_testcase(connectfun_test, Config) ->
    Config;
end_per_testcase(_, Config) ->
    ok = meck:unload(erlcloud_s3),
    ok = meck:unload(sssftp_user_session),
    Config.

get_cwd_test(Config) ->
    InitState = ?config(initstate, Config),
    ok = meck:expect(erlcloud_s3, list_objects, fun(_, _) -> [] end),
    ok = meck:expect(sssftp_user_session, get, fun(user_auth_server, _) -> {ok, "USER"} end),

    {{ok, "/"}, _State} = ?MUT:get_cwd(InitState),

    true = meck:validate(erlcloud_s3),
    true = meck:validate(sssftp_user_session),

    ok.

list_root_dir_test(Config) ->
    InitState = ?config(initstate, Config),

    {{ok, LS}, _State1} = ?MUT:list_dir("/", InitState),

    ?assertEqual(["file.txt"], LS),

    true = meck:validate(erlcloud_s3),
    true = meck:validate(sssftp_user_session),

    ok.

open_test_no_file(Config) ->
    InitState = ?config(initstate, Config),
    io:format("State ~p", [InitState]),
    ok = meck:expect(erlcloud_s3, list_objects, fun(_, _) -> [] end),

    {{error, enoent}, _State} = ?MUT:open("PATH", [binary, read], InitState),
    ok.

connectfun_test(_Config) ->
    ?MUT:connectfun(user, ip, method).
