-module(sssftp_s3_api_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_testcase/2, end_per_testcase/2]).

-export([get_cwd_test/1,
         get_cwd_test_with_role/1,
         list_root_dir_test/1,
         open_test_no_file/1,
         get_file_test/1,
         put_file_test/1,
         delete_file_test/1,
         delete_file_doesnt_exist_test/1,
         make_symlink_is_error_test/1,
         make_dir_test/1,
         connectfun_test/1]).

-define(MUT, sssftp_s3_api).

all() -> [{group, test_init}].

groups() -> [{test_init,
             [],
             [get_cwd_test,
              get_cwd_test_with_role,
              open_test_no_file,
              list_root_dir_test,
              get_file_test,
              put_file_test,
              delete_file_test,
              delete_file_doesnt_exist_test,
              make_symlink_is_error_test,
              make_dir_test,
              connectfun_test]}].



init_per_testcase(connectfun_test, Config) ->
    Config;
init_per_testcase(get_cwd_test, Config) ->
    application:ensure_all_started(lager),

    ok = meck:new(erlcloud_s3, []),
    ok = meck:new(erlcloud_aws, []),
    ok = meck:new(sssftp_user_session, []),

    InitState = {initstate, [{aws_bucket, "TESTBUCKET"},
                             {user_auth_server, user_auth_server}]},
    [InitState | Config];
init_per_testcase(get_cwd_test_with_role, Config) ->
    application:ensure_all_started(lager),

    ok = meck:new(erlcloud_s3, []),
    ok = meck:new(erlcloud_aws, []),
    ok = meck:new(erlcloud_sts, []),
    ok = meck:new(sssftp_user_session, []),

    InitState = {initstate, [{aws_bucket, "TESTBUCKET"},
                             {role, "AWS_ROLE"},
                             {external_id, "EXTERNAL_ID"},
                             {user_auth_server, user_auth_server}]},
    [InitState | Config];
init_per_testcase(_, Config) ->
    ok = lager_common_test_backend:bounce(debug),

    ok = meck:new(erlcloud_s3, []),
    ok = meck:new(erlcloud_aws, []),
    ok = meck:new(sssftp_user_session, []),
    Contents = [
        [{key, "uploads/USER/file.txt"},
         {content_length, 1024}]],

    ok = meck:expect(erlcloud_s3, list_objects, fun(_, _, _) -> [{contents, Contents}] end),
    ok = meck:expect(erlcloud_aws, auto_config, fun() -> {ok, autoconfig} end),
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
    ok = meck:expect(erlcloud_s3, list_objects, fun(_, _, _) -> [] end),
    ok = meck:expect(erlcloud_aws, auto_config, fun() -> {ok, autoconfig} end),
    ok = meck:expect(sssftp_user_session, get, fun(user_auth_server, _) -> {ok, "USER"} end),
    {{ok, "/"}, _State} = ?MUT:get_cwd(InitState),

    true = meck:validate(erlcloud_s3),
    true = meck:validate(sssftp_user_session),

    ok.

get_cwd_test_with_role(Config) ->
    InitState = ?config(initstate, Config),
    ok = meck:expect(erlcloud_s3, list_objects, fun(_, _, _) -> [] end),
    ok = meck:expect(erlcloud_aws, auto_config, fun() -> {ok, autoconfig} end),
    ok = meck:expect(sssftp_user_session, get, fun(user_auth_server, _) -> {ok, "USER"} end),
    ok = meck:expect(erlcloud_sts, assume_role, fun(autoconfig,
                                                    "AWS_ROLE",
                                                    "sssftp",
                                                    _Seconds,
                                                    "EXTERNAL_ID") -> {assumedConfig, foo} end),
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

get_file_test(Config) ->
    InitState = ?config(initstate, Config),
    Path = "/file.txt",
    Size = 1024,
    Data = crypto:strong_rand_bytes(Size),
    Object = [{content_length, erlang:integer_to_list(Size)},
              {content, Data}],
    ok = meck:expect(erlcloud_s3, get_object, fun("TESTBUCKET", "uploads/USER/file.txt", _) -> Object end),

    {false, State1} = ?MUT:is_dir(Path, InitState),
    {{ok, FileState0}, State2} = ?MUT:open(Path, [binary,read], State1),
    {{ok, 0}, State3} = ?MUT:position(FileState0, {bof, 0}, State2),
    {{ok, Data}, State4} = ?MUT:read(FileState0, 1024, State3),
    {{ok, 1024}, State5} = ?MUT:position(FileState0, {bof, 1024}, State4),
    {eof, State6} = ?MUT:read(FileState0, {bof, 1024}, State5),
    {ok, _} = ?MUT:close(FileState0, State6),

    true = meck:validate(erlcloud_s3),
    true = meck:validate(sssftp_user_session),

    ok.

put_file_test(Config) ->
    InitState = ?config(initstate, Config),
    Path = "/uploaded_file.txt",
    Size = 1024,
    Data = crypto:strong_rand_bytes(Size),
    ok = meck:expect(erlcloud_s3, put_object, fun("TESTBUCKET", "uploads/USER/uploaded_file.txt", UploadedData, _) -> UploadedData = Data end),

    {false, State1} = ?MUT:is_dir(Path, InitState),
    {{ok, FileState0}, State2} = ?MUT:open(Path, [binary, write], State1),
    {{ok, FileState0}, State3} = ?MUT:position(FileState0, {bof, 0}, State2),
    {ok, State4} = ?MUT:write(FileState0, Data, State3),
    {ok, _State5} = ?MUT:close(FileState0, State4),

    true = meck:validate(erlcloud_s3),
    true = meck:validate(sssftp_user_session),

    ok.

delete_file_test(Config) ->
    InitState = ?config(initstate, Config),
    Path = "/file.txt",
    ok = meck:expect(erlcloud_s3, delete_object, fun("TESTBUCKET", "uploads/USER/file.txt", _) -> ok end),

    {false, State1} = ?MUT:is_dir(Path, InitState),
    {ok, _State2} = ?MUT:delete(Path, State1),

    true = meck:validate(erlcloud_s3),
    true = meck:validate(sssftp_user_session),
    ok.

delete_file_doesnt_exist_test(Config) ->
    InitState = ?config(initstate, Config),
    Path = "/not-the-file.txt",

    {false, State1} = ?MUT:is_dir(Path, InitState),
    {{error, enoent}, _State2} = ?MUT:delete(Path, State1),

    true = meck:validate(erlcloud_s3),
    true = meck:validate(sssftp_user_session),
    ok.

make_symlink_is_error_test(Config) ->
    InitState = ?config(initstate, Config),
    Path = "/file.txt",
    SymLinkPath = "/file.txt",

    {{error, enotsup}, _State1} = ?MUT:make_symlink(Path, SymLinkPath, InitState),

    true = meck:validate(erlcloud_s3),
    true = meck:validate(sssftp_user_session),
    ok.

open_test_no_file(Config) ->
    InitState = ?config(initstate, Config),
    ok = meck:expect(erlcloud_s3, list_objects, fun(_, _, _) -> [] end),

    {{error, enoent}, _State} = ?MUT:open("PATH", [binary, read], InitState),
    ok.

make_dir_test(Config) ->
    InitState = ?config(initstate, Config),
    Path = "/new_dir",
    FullPath = "uploads/USER" ++ Path ++ "/",
    ok = meck:expect(erlcloud_s3, put_object, fun(_, F, <<"">>, _) -> F = FullPath end),

    {ok, InitState} = ?MUT:make_dir(Path, InitState),
    ok.

connectfun_test(_Config) ->
    ?MUT:connectfun(user, ip, method).
