-module(sssftp_user_session_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_testcase/2, end_per_testcase/2]).
-export([dead_link_removes_user/1,
         cast_test/1,
         code_change_test/1,
         register_and_get_user/1,
         register_twice_and_get_user/1,
         register_twice_with_different_users/1,
         get_without_user_is_error/1]).

-export([register_user_and_exit/2]).

-define(MUT, sssftp_user_session).

all() -> [{group, test_module}, {group, test_with_process}].

groups() -> [{test_module,
              [],
              [code_change_test,
               cast_test]},
             {test_with_process,
              [],
              [get_without_user_is_error,
               register_and_get_user,
               register_twice_and_get_user,
               register_twice_with_different_users,
               dead_link_removes_user]}].


init_per_testcase(Case, Config) ->
    GroupProperties = ?config(tc_group_properties, Config),
    GroupName = ?config(name, GroupProperties),
    init_per_testcase(GroupName, Case, Config).

init_per_testcase(test_with_process, _Case, Config) ->
    {ok, USPid} = sssftp_user_session:start_link(),
    [{uspid, USPid} | Config];
init_per_testcase(_Group, _Case, Config) ->
    Config.

end_per_testcase(Case, Config) ->
    GroupProperties = ?config(tc_group_properties, Config),
    GroupName = ?config(name, GroupProperties),
    end_per_testcase(GroupName, Case, Config).

end_per_testcase(test_with_process, _, Config) ->
    Pid = ?config(uspid, Config),
    ok = ?MUT:stop(Pid),
    Config;
end_per_testcase(_, _, Config) ->
    Config.

cast_test(_Config) ->
    Ref = make_ref(),
    {noreply, Ref} = ?MUT:handle_cast(a, Ref).

code_change_test(_Config) ->
    Ref = make_ref(),
    {ok, Ref} = ?MUT:code_change(a, Ref, b).

register_and_get_user(Config) ->
    Username = "username",
    USPid = ?config(uspid, Config),
    ok = ?MUT:add(USPid, Username),
    Resp = ?MUT:get(USPid, self()),
    ?assertEqual({ok, Username}, Resp).

register_twice_and_get_user(Config) ->
    Username = "username",
    USPid = ?config(uspid, Config),
    ok = ?MUT:add(USPid, Username),
    ok = ?MUT:add(USPid, Username),
    Resp = ?MUT:get(USPid, self()),
    ?assertEqual({ok, Username}, Resp).

register_twice_with_different_users(Config) ->
    Username1 = "username1",
    Username2 = "username2",
    USPid = ?config(uspid, Config),
    ok = ?MUT:add(USPid, Username1),
    Resp = ?MUT:add(USPid, Username2),
    ?assertEqual({error, already_defined}, Resp).

dead_link_removes_user(Config) ->
    USPid = ?config(uspid, Config),
    Ref = make_ref(),
    {Pid, MonRef} = spawn_monitor(?MODULE, register_user_and_exit, [self(), Ref]),
    ok = wait_for_ref(Ref),
    ok = wait_for_monitor(MonRef),
    false = is_process_alive(Pid),
    Resp = ?MUT:get(USPid, self()),
    ?assertEqual({error, undefined}, Resp),
    ok.

get_without_user_is_error(_Config) ->
    Resp = ?MUT:get(self()),
    ?assertEqual({error, undefined}, Resp),
    ok.

% Helpers
register_user_and_exit(Parent, Ref) ->
    ok = ?MUT:add(?MUT, <<"Username">>),
    Parent ! Ref,
    ok.

wait_for_ref(Ref) ->
    Resp = receive Ref ->
        ok
    after 1000 ->
        error
    end,
    Resp.

wait_for_monitor(MonitorRef)->
    receive {'DOWN', MonitorRef, _Type, _Object, _Info} ->
        ok
    after 1000 ->
        error
    end.
