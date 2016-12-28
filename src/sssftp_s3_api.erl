-module(sssftp_s3_api).

-behaviour(ssh_sftpd_file_api).
-compile([{parse_transform, lager_transform}]).

-include_lib("kernel/include/file.hrl").
%% API
-export([close/2, delete/2, del_dir/2, get_cwd/1, is_dir/2, list_dir/2,
	 make_dir/2, make_symlink/3, open/3, position/3, read/3,
	 read_file_info/2, read_link/2, read_link_info/2, rename/3,
	 write/3, write_file_info/3]).

-export([connectfun/3]).

-record(state, {aws_bucket=undefined,
                dirs=undefined,
                ls_info=undefined,
                filenames=undefined,
                file_position=0,
                uploading_bin=undefined,
                path=undefined,
                s3_root=undefined,
                user=undefined}).

-record(reading_file, {bin=undefined,
                       length=undefined}).

connectfun(User, _IP, _Method) ->
    io:format("User connected ~p~n", [{User, self()}]).

close({writing_file, Path}, State=#state{s3_root=S3Root,
                                         aws_bucket=Bucket,
                                         uploading_bin=Bin}) ->

    FilePath = S3Root ++ Path,

    io:format("close ~p~n", [FilePath]),
    erlcloud_s3:put_object(Bucket, FilePath, Bin),
    {ok, State};

close(_IoDevice, State) ->
    io:format("close~n"),
    {ok, State}.

delete(Path, State) ->
    io:format("delete~n"),
    {file:delete(Path), State}.

del_dir(Path, State) ->
    io:format("del_dir~n"),
    {file:del_dir(Path), State}.

get_cwd(State) ->
    AWS_BUCKET = proplists:get_value(aws_bucket, State),
    io:format("CWD ~p~n", [{State, self()}]),
    {ok, User} = sssftp_user_session:get(self()),
    Root = "uploads/" ++ User,
    io:format("User is: ~p~n", [User]),
    {file:get_cwd(), #state{aws_bucket=AWS_BUCKET,
                            s3_root=Root,
                            user=User}}.

is_dir(AbsPath, State0) ->
    io:format("is_dir ~p~n", [AbsPath]),
    Dir = filename:dirname(AbsPath),
    {true, State1} = get_s3_path(Dir, State0),
    S3Root = State1#state.s3_root,
    Contents = State1#state.ls_info,
    IsDir = sssftp_s3_parsing:is_dir(S3Root, AbsPath, Contents),
    {IsDir, State1}.

get_s3_path(Path, State=#state{aws_bucket=Bucket, s3_root=S3Root}) ->
    Prefix = S3Root ++ Path,
    Options = [{prefix, Prefix}],
    io:format("S3 Options ~p~n", [Options]),
    Result = erlcloud_s3:list_objects(Bucket, Options),
    Contents = proplists:get_value(contents, Result),
    {true, State#state{ls_info=Contents,
                       path=Path}}.

list_dir(AbsPath, State) ->
    io:format("List dir ~p~n", [{AbsPath, State}]),
    S3Root = State#state.s3_root,
    Contents = State#state.ls_info,
    {Dirs, Files} = sssftp_s3_parsing:filter_s3_abs_path(S3Root, AbsPath, Contents),
    LS = lists:append([Files, Dirs]),
    {{ok, LS}, State}.

make_dir(Dir, State=#state{s3_root=S3Root, aws_bucket=Bucket}) ->
    FilePath = S3Root ++ Dir ++ "/",
    io:format("mkdir ~p~n", [{Bucket, FilePath}]),
    erlcloud_s3:put_object(Bucket, FilePath, <<"">>),
    {ok, State}.

make_symlink(Path2, Path, State) ->
    io:format("make_symlink~n"),
    {file:make_symlink(Path2, Path), State}.

open(Path, [binary, write], State) ->
    {{ok, {writing_file, Path}}, State#state{uploading_bin= <<"">>}};

open(Path, [binary, read], State=#state{aws_bucket=Bucket, s3_root=S3Root}) ->
    AbsPath = S3Root ++ Path,
    Obj = erlcloud_s3:get_object(Bucket, AbsPath),
    Length = proplists:get_value(content_length, Obj),
    Content = proplists:get_value(content, Obj),
    {ILength, _} = string:to_integer(Length),
    RF = #reading_file{bin=Content, length=ILength},
    io:format("open ~n"),
    {{ok, RF}, State#state{file_position=0}}.

position(IoDevice, {bof, Pos}, State) ->
    io:format("position ~p~n", [Pos]),
    {{ok, IoDevice}, State#state{file_position=Pos}}.

read(#reading_file{length=TotalLength}, _Len, State=#state{file_position=TotalLength}) ->
    {eof, State};
read(#reading_file{bin=Bin, length=TotalLength}, Len, State=#state{file_position=Pos}) ->
    io:format("read ~p~n",[{Pos, Len, TotalLength, Pos+Len}]),
    Data = case Pos+Len > TotalLength of
        false -> <<_:Pos/binary, D:Len/binary, _/binary>> = Bin,
                 D;
        true -> <<_:Pos/binary, D/binary>> = Bin,
                D
    end,
    {{ok, Data}, State}.

read_link(Path, State) ->
    io:format("read link ~p~n", [Path]),
    {{error, einval}, State}.

read_link_info([], State) ->
    {{error, enoent}, State};
read_link_info(Path, State) ->
    read_info(Path, State).

get_file_info_from_content([]) ->
    #file_info{type=directory,
               access=read_write};
get_file_info_from_content([H|_]) ->
    IsDir = lists:suffix("/", proplists:get_value(key, H)),
    Info0 = file_type(IsDir, #file_info{}),
    Info0#file_info{size=proplists:get_value(size, H),
                    access=read_write}.

file_type(true, Info) ->
    Info#file_info{type=directory,
                   mode=16877};
file_type(false, Info) ->
    Info#file_info{type=regular}.

find_content_from_key(Value, Contents) ->
    lists:filter(fun(E) -> CValue = proplists:get_value(key, E),
                           CValue =:= Value orelse (CValue =:= Value ++ "/")
                           end,
                 Contents).

read_file_info(Path, State) ->
    io:format("read file info ~p~n", [Path]),
    read_info(Path, State).

read_info(Path, State=#state{ls_info=Info, s3_root=Prefix}) ->
    FullPath = Prefix ++ string:sub_string(Path, 1),
    io:format("Path: ~p~n", [FullPath]),
    Data = find_content_from_key(FullPath, Info),
    FileInfo = get_file_info_from_content(Data),
    io:format("Info ~p~n", [{Data, FileInfo}]),
    {{ok, FileInfo}, State}.

rename(Path, Path2, State) ->
    io:format("rename~n"),
    {file:rename(Path, Path2), State}.

write({writing_file, _Path}, Data, State=#state{uploading_bin=Bin}) ->
    io:format("write~n"),
    PartialBin = <<Bin/binary, Data/binary>>,
    {ok, State#state{uploading_bin=PartialBin}}.

write_file_info(Path,Info, State) ->
    io:format("write_file_info~n"),
    {file:write_file_info(Path, Info), State}.
