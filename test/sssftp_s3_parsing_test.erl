-module(sssftp_s3_parsing_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


-define(MUT, sssftp_s3_parsing).


make_s3(Key) ->
    [{key, Key}].

make_sample_s3_group() ->
    [make_s3("uploads/user/"),
     make_s3("uploads/user/example.jpg")].

filter_empty_test() ->
    ?assertEqual({[], []},
                 ?MUT:filter_s3_abs_path("uploads/user/", "/", [])).

filter_root_test_doesnt_show_root() ->
    ?assertEqual({[], []},
                 ?MUT:filter_s3_abs_path("uploads/user/", "/", [make_s3("/")])).
