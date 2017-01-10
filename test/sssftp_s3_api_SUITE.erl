-module(sssftp_s3_api_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_testcase/2, end_per_testcase/2]).

-export([get_cwd_test/1,
         open_test_no_file/1]).

-include_lib("sssftp/src/sssftp_s3_api_lib.hrl").

-define(MUT, sssftp_s3_api).

all() -> [{group, test_init}].

groups() -> [{test_init,
             [],
             [get_cwd_test,
              open_test_no_file]}].



init_per_testcase(get_cwd_test, Config) ->
    application:ensure_all_started(lager),

    ok = meck:new(erlcloud_s3, []),
    ok = meck:new(sssftp_user_session, []),

    InitState = {initstate, [{aws_bucket, "TESTBUCKET"}]},
    [InitState | Config];
init_per_testcase(_, Config) ->
    application:ensure_all_started(lager),

    ok = meck:new(erlcloud_s3, []),
    ok = meck:new(sssftp_user_session, []),

    ok = meck:expect(erlcloud_s3, list_objects, fun(_, _) -> [{contents, []}] end),
    ok = meck:expect(sssftp_user_session, get, fun(_) -> {ok, "USER"} end),

    State0 = [{aws_bucket, "TESTBUCKET"}],
    {_, State1} = ?MUT:get_cwd(State0),

    InitState = {initstate, State1},
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

open_test_no_file(Config) ->
    InitState = ?config(initstate, Config),
    io:format("State ~p", [InitState]),
    ok = meck:expect(erlcloud_s3, list_objects, fun(_, _) -> [] end),

    {{error, enoent}, _State} = ?MUT:open("PATH", [binary, read], InitState),
    ok.
