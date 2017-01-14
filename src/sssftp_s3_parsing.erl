-module(sssftp_s3_parsing).

-compile([{parse_transform, lager_transform}]).

-export([filter_s3_abs_path/3, is_dir/3]).

-spec filter_s3_abs_path(list(), list(), list()) -> {list(), list()}.
filter_s3_abs_path(S3Root, "/", Contents) ->
    ok = lager:debug("Filter /"),
    filter_s3_abs_path(S3Root, "", Contents);

filter_s3_abs_path(S3Root, [$/|T], Contents) ->
    ok = lager:debug("Filter T"),
    filter_s3_abs_path(S3Root, T, Contents);

filter_s3_abs_path(S3Root, Path, Contents) ->
    AbsPath = filename:join([S3Root, Path]) ++ "/",
    ok = lager:debug("Filter for path ~p ~p", [Path, AbsPath]),
    Files = [proplists:get_value(key, X) || X <- Contents],
    APL = length(AbsPath),
    LFiles = lists:filter(fun(El) ->
                            SubStr = string:sub_string(El, 1, APL),
                            SubStr =:= AbsPath
                          end, Files),

    StrippedObjs = strip_path(AbsPath, LFiles),
    ok = lager:debug("Stripped objs ~p", [StrippedObjs]),
    FilteredFiles = filter_s3_files(StrippedObjs),
    FilteredDirs = filter_s3_dirs(StrippedObjs),
    {FilteredDirs, FilteredFiles}.

-spec is_dir(list(), list(), list()) -> boolean().
is_dir(S3Root, Path, Contents) ->
    ok = lager:debug("Parser is_dir ~p", [Path]),
    AbsPath = normalize_dir_path(S3Root, Path),
    ok = lager:debug("Parser is_dir Abspath ~p", [AbsPath]),
    lists:any(fun(El) -> proplists:get_value(key, El) =:= AbsPath end, Contents).

-spec normalize_dir_path(nonempty_string(), list() | nil()) -> list().
normalize_dir_path(S3Root, "/") ->
    normalize_dir_path(S3Root, "");
normalize_dir_path(S3Root, [$/|T]) ->
    normalize_dir_path(S3Root, T);
normalize_dir_path(S3Root, []) ->
    normalize_dir_path(S3Root, [], []);
normalize_dir_path(S3Root, Path) ->
    normalize_dir_path(S3Root, Path, lists:last(Path)).

-spec normalize_dir_path(nonempty_string(), list() | nil(), char() | nil()) -> list().
normalize_dir_path(S3Root, Path, $/) ->
    filename:join([S3Root, Path]);
normalize_dir_path(S3Root, Path, _Last) ->
    filename:join([S3Root, Path]) ++ "/".


-spec strip_path(nonempty_string(), list()) -> list().
strip_path(Prefix, Strings) when is_list(Prefix) ->
    strip_path(Prefix, string:len(Prefix) + 1, Strings).

-spec strip_path(nonempty_string(), integer(), string()) -> list().
strip_path(Prefix, Len, [Prefix|T]) when is_integer(Len) ->
    strip_path(Prefix, Len, T);
strip_path(Prefix, Len, [H|T]) when is_integer(Len) ->
    ok = lager:debug("strip ~p ~p", [Prefix, H]),
    [string:sub_string(H, Len) | strip_path(Prefix, Len, T)];
strip_path(_Prefix, _Len, []) ->
    [].

filter_s3_files(Paths) ->
    lists:filtermap(fun(El) ->
                ok = lager:debug("Filtering ~p", [El]),
                case lists:last(El) of
                    $/ -> false;
                    _  -> filtered_for_multipart(El)
                end
              end, Paths).

filtered_for_multipart(El) ->
    Splits = filename:split(El),
    case length(Splits) of
        1 -> {true, El};
        _ -> false
    end.

filter_s3_dirs(Paths) ->
    A =lists:foldl(fun(El, Acc) ->
                       Splits = filename:split(El),
                       ok = lager:debug("dir splits ~p", [Splits]),
                       case {length(Splits), lists:last(El)}  of
                           {0, _} -> Acc;
                           {1, $/} -> ok = lager:debug("1 match ~p", [Splits]),
                                      sets:add_element(hd(Splits), Acc);
                           {1, _ } -> Acc;
                           _ -> sets:add_element(hd(Splits), Acc)
                       end
                   end, sets:new(), Paths),
    sets:to_list(A).
