-module(sssftp_user_session_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_testcase/2, end_per_testcase/2]).
-export([register_test/1, get_without_user_is_error/1]).

-define(MUT, sssftp_user_session).

all() -> [{group, test_registration}].

groups() -> [{test_registration,
              [],
              [register_test, get_without_user_is_error]}].

init_per_testcase(get_without_user_is_error, Config) ->
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

get_without_user_is_error(_Config) ->
    Resp = ?MUT:get(self()),
    ?assertEqual(error, Resp),
    ok.
