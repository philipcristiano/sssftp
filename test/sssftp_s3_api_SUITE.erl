-module(sssftp_s3_api_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_testcase/2, end_per_testcase/2]).

-export([get_cwd_test/1]).

-define(MUT, sssftp_s3_api).

all() -> [{group, test_init}].

groups() -> [{test_init,
             [],
             [get_cwd_test]}].


init_per_testcase(_, Config) ->
    application:ensure_all_started(lager),

    ok = meck:new(erlcloud_s3, []),
    ok = meck:new(sssftp_user_session, []),

    InitState = {initstate, [{aws_bucket, "TESTBUCKET"}]},
    [InitState | Config].

end_per_testcase(_, Config) ->
    ok = meck:unload(erlcloud_s3),
    ok = meck:unload(sssftp_user_session),
    Config.

get_cwd_test(Config) ->
    InitState = ?config(initstate, Config),
    ok = meck:expect(erlcloud_s3, list_objects, fun(_, _) -> [] end),
    ok = meck:expect(sssftp_user_session, get, fun(_) -> {ok, "USER"} end),

    {{ok, "/"}, _State} = ?MUT:get_cwd(InitState),

    true = meck:validate(erlcloud_s3),
    true = meck:validate(sssftp_user_session),

    ok.
