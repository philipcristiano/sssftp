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

filter_root_doesnt_show_root_test() ->
    ?assertEqual({[], []},
                 ?MUT:filter_s3_abs_path("uploads/user/", "/", [make_s3("/")])).

filter_root_single_file_test() ->
    ?assertEqual({[], ["file.jpg"]},
                 ?MUT:filter_s3_abs_path("uploads/user/", "/", [make_s3("uploads/user/file.jpg")])).

filter_root_single_dir_test() ->
    ?assertEqual({["dir"], []},
                 ?MUT:filter_s3_abs_path("uploads/user/", "/", [make_s3("uploads/user/dir/")])).

filter_root_for_non_root_file_test() ->
    ?assertEqual({["dir"], []},
                 ?MUT:filter_s3_abs_path("uploads/user/", "/", [make_s3("uploads/user/dir/file.jpg")])).
filter_root_dir_and_file_test() ->
    Files = [make_s3("uploads/user/dir/"), make_s3("uploads/user/file.jpg")],
    ?assertEqual({["dir"], ["file.jpg"]},
                 ?MUT:filter_s3_abs_path("uploads/user/", "/", Files)).

filter_cd_for_non_root_dir_test() ->
    ?assertEqual({["nested_dir"], []},
                 ?MUT:filter_s3_abs_path("uploads/user/", "/dir", [make_s3("uploads/user/dir/nested_dir/")])).

filter_cd_for_non_root_file_test() ->
    ?assertEqual({["nested_dir"], ["second_file"]},
                 ?MUT:filter_s3_abs_path("uploads/user/", "/dir",
                [make_s3("uploads/user/dir/nested_dir/"),
                 make_s3("uploads/user/dir/second_file")])).

filter_when_other_dirs_test() ->
    ?assertEqual({[], ["foo_a"]},
                 ?MUT:filter_s3_abs_path("uploads/user/", "dir_a",
                [make_s3("uploads/user/dir/nested_dir/"),
                 make_s3("uploads/user/dir_a/foo_a"),
                 make_s3("uploads/user/dir_b/foo_b"),
                 make_s3("uploads/user/dir_is_longer/")])).

is_dir_root_test() ->
    ?assertEqual(true,
                 ?MUT:is_dir("uploads/user/", "/", [make_s3("uploads/user/")])).

is_dir_child_dir_test() ->
    ?assertEqual(false,
                 ?MUT:is_dir("uploads/user/", "/foo", [make_s3("uploads/user/bar")])).
