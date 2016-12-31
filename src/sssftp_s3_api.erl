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
    lager:info("User connected ~p", [{User, self()}]).

close({writing_file, Path}, State=#state{s3_root=S3Root,
                                         aws_bucket=Bucket,
                                         uploading_bin=Bin}) ->

    FilePath = S3Root ++ Path,

    lager:debug("Closing file for writing ~p", [FilePath]),
    erlcloud_s3:put_object(Bucket, FilePath, Bin),
    {ok, State};

close(_IoDevice, State) ->
    lager:debug("Closing file for reading"),
    {ok, State}.

delete(Path, State=#state{aws_bucket=Bucket,
                          s3_root=S3Root,
                          ls_info=Contents}) ->
    AbsPath = S3Root ++ Path,
    DirName = filename:dirname(Path),
    FileName = filename:basename(Path),
    lager:debug("Deleting file ~p ~p", [Path, AbsPath]),
    {_Dirs, Files} = sssftp_s3_parsing:filter_s3_abs_path(S3Root, DirName, Contents),
    lager:debug("Found files ~p", [Files]),
    FileExists = lists:member(FileName, Files),
    Result = case FileExists of
        true -> lager:debug("Actually deleting file"),
                erlcloud_s3:delete_object(Bucket, AbsPath),
                ok;
        false -> {error, enoent}
    end,
    {Result, State}.


del_dir(Path, State=#state{aws_bucket=Bucket,
                           s3_root=S3Root,
                           ls_info=Contents}) ->
    AbsPath = S3Root ++ Path ++ "/",
    lager:debug("Deleting directory ~p ~p", [Path, AbsPath]),
    {Dirs, Files} = sssftp_s3_parsing:filter_s3_abs_path(S3Root, Path, Contents),
    lager:debug("Dir Contains ~p, ~p", [Dirs, Files]),
    Result = case could_delete(Dirs, Files) of
        true -> lager:debug("Can delete dir."),
                erlcloud_s3:delete_object(Bucket, AbsPath),
                ok;
        false -> lager:debug("Cant delete dir, it still has contents."),
                 {error, eexist}
    end,

    {Result, State}.

could_delete([], []) -> true;
could_delete(_, _) -> false.

get_cwd(State0) ->
    AWS_BUCKET = proplists:get_value(aws_bucket, State0),
    lager:debug("CWD ~p", [{State0, self()}]),
    {ok, User} = sssftp_user_session:get(self()),
    Root = "uploads/" ++ User,
    lager:debug("User is: ~p", [User]),
    State1 = #state{aws_bucket=AWS_BUCKET,
                    s3_root=Root,
                    user=User},
    Dir = AWS_BUCKET ++ Root ++ "/",
    {true, State2} = get_s3_path(Dir, State1),
    {file:get_cwd(), State2 }.

is_dir(AbsPath, State0) ->
    lager:debug("is_dir ~p", [AbsPath]),
    Dir = filename:dirname(AbsPath),
    {true, State1} = get_s3_path(Dir, State0),
    S3Root = State1#state.s3_root,
    Contents = State1#state.ls_info,
    IsDir = sssftp_s3_parsing:is_dir(S3Root, AbsPath, Contents),
    {IsDir, State1}.

get_s3_path(Path, State=#state{aws_bucket=Bucket, s3_root=S3Root}) ->
    Prefix = S3Root ++ Path,
    Options = [{prefix, Prefix}],
    lager:debug("S3 Options ~p", [Options]),
    Result = erlcloud_s3:list_objects(Bucket, Options),
    Contents = proplists:get_value(contents, Result),
    {true, State#state{ls_info=Contents,
                       path=Path}}.

list_dir(AbsPath, State) ->
    lager:debug("List dir ~p", [AbsPath}]),
    S3Root = State#state.s3_root,
    Contents = State#state.ls_info,
    {Dirs, Files} = sssftp_s3_parsing:filter_s3_abs_path(S3Root, AbsPath, Contents),
    LS = lists:append([Files, Dirs]),
    {{ok, LS}, State}.

make_dir(Dir, State=#state{s3_root=S3Root, aws_bucket=Bucket}) ->
    FilePath = S3Root ++ Dir ++ "/",
    lager:debug("mkdir ~p", [{Bucket, FilePath}]),
    erlcloud_s3:put_object(Bucket, FilePath, <<"">>),
    {ok, State}.

make_symlink(Path2, Path, State) ->
    lager:debug("make_symlink"),
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
    lager:debug("open "),
    {{ok, RF}, State#state{file_position=0}}.

position(IoDevice, {bof, Pos}, State) ->
    lager:debug("position ~p", [Pos]),
    {{ok, IoDevice}, State#state{file_position=Pos}}.

read(#reading_file{length=TotalLength}, _Len, State=#state{file_position=TotalLength}) ->
    {eof, State};
read(#reading_file{bin=Bin, length=TotalLength}, Len, State=#state{file_position=Pos}) ->
    lager:debug("read ~p",[{Pos, Len, TotalLength, Pos+Len}]),
    Data = case Pos+Len > TotalLength of
        false -> <<_:Pos/binary, D:Len/binary, _/binary>> = Bin,
                 D;
        true -> <<_:Pos/binary, D/binary>> = Bin,
                D
    end,
    {{ok, Data}, State}.

read_link(Path, State) ->
    lager:debug("read link ~p", [Path]),
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
    lager:debug("read file info ~p", [Path]),
    read_info(Path, State).

read_info(Path, State=#state{ls_info=Info, s3_root=Prefix}) ->
    FullPath = Prefix ++ string:sub_string(Path, 1),
    lager:debug("Path: ~p", [FullPath]),
    Data = find_content_from_key(FullPath, Info),
    FileInfo = get_file_info_from_content(Data),
    lager:debug("Info ~p", [{Data, FileInfo}]),
    {{ok, FileInfo}, State}.

rename(Path, Path2, State) ->
    lager:debug("rename"),
    {file:rename(Path, Path2), State}.

write({writing_file, _Path}, Data, State=#state{uploading_bin=Bin}) ->
    lager:debug("write"),
    PartialBin = <<Bin/binary, Data/binary>>,
    {ok, State#state{uploading_bin=PartialBin}}.

write_file_info(Path,Info, State) ->
    lager:debug("write_file_info"),
    {file:write_file_info(Path, Info), State}.
