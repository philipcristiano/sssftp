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
    Config.

end_per_testcase(_, Config) ->
    Config.

get_cwd_test(_Config) ->
    ok.
