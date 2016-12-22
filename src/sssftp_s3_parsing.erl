-module(sssftp_s3_parsing).

-compile([{parse_transform, lager_transform}]).

-export([filter_s3_abs_path/3]).

filter_s3_abs_path(S3Root, "/", Contents) ->
    filter_s3_abs_path(S3Root, "", Contents);

filter_s3_abs_path(S3Root, Path, Contents) ->
    lager:info("Filter for path2 ~p", [Path]),
    io:format("Filter for path_ ~p~n", [Path]),
    AbsPath = filename:join([S3Root, Path]) ++ "/",
    io:format("Filter for path ~p~n", [AbsPath]),
    Files = [proplists:get_value(key, X) || X <- Contents],
    APL = length(AbsPath),
    LFiles = lists:filter(fun(El) -> length(El) >= APL end, Files),

    StrippedObjs = strip_path(AbsPath, LFiles),
    io:format("Stripped objs ~p~n", [StrippedObjs]),
    FilteredFiles = filter_s3_files(StrippedObjs),
    FilteredDirs = filter_s3_dirs(StrippedObjs),
    {FilteredDirs, FilteredFiles}.

strip_path(Prefix, Strings) when is_list(Prefix) ->
    strip_path(Prefix, string:len(Prefix) + 1, Strings).

strip_path(Prefix, Len, [Prefix|T]) when is_integer(Len) ->
    strip_path(Prefix, Len, T);
strip_path(Prefix, Len, [H|T]) when is_integer(Len) ->
    io:format("strip ~p ~p~n", [Prefix, H]),
    [string:sub_string(H, Len) | strip_path(Prefix, Len, T)];
strip_path(_Prefix, _Len, []) ->
    [].

filter_s3_files(Paths) ->
    lists:filtermap(fun(El) ->
                io:format("Filtering ~p~n", [El]),
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
                       case length(Splits) of
                           0 -> Acc;
                           1 -> Acc;
                           _ -> sets:add_element(hd(Splits), Acc)
                       end
                   end, sets:new(), Paths),
    sets:to_list(A).
