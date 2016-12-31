-module(sssftp_user_session_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_testcase/2, end_per_testcase/2]).
-export([dead_link_removes_user/1,
         register_test/1, get_without_user_is_error/1]).

-export([register_user_and_exit/2]).

-define(MUT, sssftp_user_session).

all() -> [{group, test_registration}].

groups() -> [{test_registration,
              [],
              [dead_link_removes_user,
               register_test,
               get_without_user_is_error]}].

init_per_testcase(get_without_user_is_error, Config) ->
    {ok, USPid} = sssftp_user_session:start_link(),
    [{uspid, USPid} | Config];
init_per_testcase(dead_link_removes_user, Config) ->
    {ok, USPid} = sssftp_user_session:start_link(),
    [{uspid, USPid} | Config];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(register_test_2, Config) ->
    Config;
end_per_testcase(_, Config) ->
    Config.

register_test(_Config) ->
    ok.

dead_link_removes_user(_Config) ->
    Ref = make_ref(),
    {Pid, MonRef} = spawn_monitor(?MODULE, register_user_and_exit, [self(), Ref]),
    ok = wait_for_ref(Ref),
    ok = wait_for_monitor(MonRef),
    false = is_process_alive(Pid),
    Resp = ?MUT:get(self()),
    ?assertEqual({error, undefined}, Resp),
    ok.

get_without_user_is_error(_Config) ->
    Resp = ?MUT:get(self()),
    ?assertEqual({error, undefined}, Resp),
    ok.

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
