-module(sssftp_s3_api).

-behaviour(ssh_sssftpd_file_api).
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
                storage_api=undefined,
                storage_config=unefined,
                user=undefined}).

-type state() :: #state{}.

-record(reading_file, {bin=undefined,
                       length=undefined}).

-type reading_file() :: #reading_file{}.

connectfun(User, _IP, _Method) ->
    ok = lager:info("User connected ~p", [{User, self()}]).

close({writing_file, Path}, State=#state{s3_root=S3Root,
                                         aws_bucket=Bucket,
                                         storage_api=StorageApi,
                                         storage_config=StorageConfig,
                                         uploading_bin=Bin}) ->

    FilePath = S3Root ++ Path,

    ok = lager:debug("Closing file for writing ~p", [FilePath]),
    StorageApi:put_object(Bucket, FilePath, Bin, StorageConfig),
    {ok, State};

close(_IoDevice, State) ->
    ok = lager:debug("Closing file for reading"),
    {ok, State}.

delete(Path, State=#state{aws_bucket=Bucket,
                          s3_root=S3Root,
                          storage_api=StorageApi,
                          storage_config=StorageConfig,
                          ls_info=Contents}) ->
    AbsPath = S3Root ++ Path,
    DirName = filename:dirname(Path),
    FileName = filename:basename(Path),
    ok = lager:debug("Deleting file ~p ~p", [Path, AbsPath]),
    {_Dirs, Files} = sssftp_s3_parsing:filter_s3_abs_path(S3Root, DirName, Contents),
    ok = lager:debug("Found files ~p", [Files]),
    FileExists = lists:member(FileName, Files),
    Result = case FileExists of
        true -> ok = lager:debug("Actually deleting file"),
                StorageApi:delete_object(Bucket, AbsPath, StorageConfig),
                ok;
        false -> {error, enoent}
    end,
    {Result, State}.


del_dir(Path, State=#state{aws_bucket=Bucket,
                           s3_root=S3Root,
                           storage_api=StorageApi,
                           storage_config=StorageConfig,
                           ls_info=Contents}) ->
    AbsPath = S3Root ++ Path ++ "/",
    ok = lager:debug("Deleting directory ~p ~p", [Path, AbsPath]),
    {Dirs, Files} = sssftp_s3_parsing:filter_s3_abs_path(S3Root, Path, Contents),
    ok = lager:debug("Dir Contains ~p, ~p", [Dirs, Files]),
    Result = case could_delete(Dirs, Files) of
        true -> ok = lager:debug("Can delete dir."),
                StorageApi:delete_object(Bucket, AbsPath, StorageConfig),
                ok;
        false -> ok = lager:debug("Cant delete dir, it still has contents."),
                 {error, eexist}
    end,

    {Result, State}.

could_delete([], []) -> true;
could_delete(_, _) -> false.

assume_role(Config, Options) ->
    Role = proplists:get_value(role, Options),
    Id = proplists:get_value(external_id, Options),
    assume_role(Config, Role, Id).

assume_role(Config, undefined, undefined) ->
    Config;
assume_role(Config, Role, Id) ->
    ok = lager:debug("Assuming role ~p", [Role]),
    {AssumedConfig, _} = erlcloud_sts:assume_role(Config, Role, "sssftp", 900, Id),
    AssumedConfig.

get_cwd(State0) ->
    AWS_BUCKET = proplists:get_value(aws_bucket, State0),
    UserAuthServer = proplists:get_value(user_auth_server, State0),
    StorageApi = proplists:get_value(storage_api, State0, erlcloud_s3),
    {ok, StorageConfig1} = erlcloud_aws:auto_config(),
    ok = lager:debug("Got config ~p", [StorageConfig1]),
    StorageConfig2 = assume_role(StorageConfig1, State0),
    ok = lager:debug("Got role"),
    ok = lager:debug("CWD ~p", [{State0, self()}]),

    {ok, User} = sssftp_user_session:get(UserAuthServer, self()),
    Root = "uploads/" ++ User,
    ok = lager:debug("User is: ~p", [User]),
    State1 = #state{aws_bucket=AWS_BUCKET,
                    s3_root=Root,
                    storage_api=StorageApi,
                    storage_config=StorageConfig2,
                    user=User},
    Dir = AWS_BUCKET ++ Root ++ "/",
    {true, State2} = get_s3_path(Dir, State1),
    {{ok, "/"}, State2}.

is_dir(AbsPath, State0) ->
    ok = lager:debug("is_dir ~p", [AbsPath]),
    Dir = filename:dirname(AbsPath),
    {true, State1} = get_s3_path(Dir, State0),
    S3Root = State1#state.s3_root,
    Contents = State1#state.ls_info,
    IsDir = sssftp_s3_parsing:is_dir(S3Root, AbsPath, Contents),
    ok = lager:debug("Is This a dir ~p, ~p", [AbsPath, IsDir]),
    {IsDir, State1}.

get_s3_path(Path, State=#state{aws_bucket=Bucket,
                               s3_root=S3Root,
                               storage_api=StorageApi,
                               storage_config=StorageConfig}) ->
    Prefix = S3Root ++ Path,
    Options = [{prefix, Prefix}],
    ok = lager:debug("S3 Options ~p", [{Bucket, Options}]),
    Result = StorageApi:list_objects(Bucket, Options, StorageConfig),
    Contents = proplists:get_value(contents, Result),
    State1 = State#state{ls_info=Contents, path=Path},
    {true, State1}.

list_dir(AbsPath, State) ->
    ok = lager:debug("List dir ~p", [AbsPath]),
    S3Root = State#state.s3_root,
    Contents = State#state.ls_info,
    {Dirs, Files} = sssftp_s3_parsing:filter_s3_abs_path(S3Root, AbsPath, Contents),
    LS = lists:append([Files, Dirs]),
    {{ok, LS}, State}.

make_dir(Dir, State=#state{s3_root=S3Root,
                           aws_bucket=Bucket,
                           storage_api=StorageApi,
                           storage_config=StorageConfig}) ->
    FilePath = S3Root ++ Dir ++ "/",
    ok = lager:debug("mkdir ~p", [{Bucket, FilePath}]),
    StorageApi:put_object(Bucket, FilePath, <<"">>, StorageConfig),
    {ok, State}.

make_symlink(_, _, State) ->
    ok = lager:debug("make_symlink"),
    {{error, enotsup}, State}.

open(Path, [binary, write], State) ->
    {{ok, {writing_file, Path}}, State#state{uploading_bin= <<"">>}};

open(Path, [binary, read], State=#state{aws_bucket=Bucket,
                                        s3_root=S3Root,
                                        storage_api=StorageApi,
                                        storage_config=StorageConfig,
                                        ls_info=Contents}) ->
    AbsPath = S3Root ++ Path,
    ItemInfo = find_content_from_key(AbsPath, Contents),
    case ItemInfo of
        [] -> {{error, enoent}, State};
        _  ->
              ok = lager:debug("Item info ~p", [ItemInfo]),
              Obj = StorageApi:get_object(Bucket, AbsPath, StorageConfig),
              Length = proplists:get_value(content_length, Obj),
              Content = proplists:get_value(content, Obj),
              {ILength, _} = string:to_integer(Length),
              RF = #reading_file{bin=Content, length=ILength},
              ok = lager:debug("open"),
              {{ok, RF}, State#state{file_position=0}}
    end.

-spec position(reading_file() | {writing_file, nonempty_string()}, {bof, integer()}, state()) -> {{ok, integer}, state()}.
position(#reading_file{length=TotalLength}, {bof, Pos}, State) ->
    NewPos = erlang:min(TotalLength, Pos),
    ok = lager:debug("position ~p", [{Pos, TotalLength}]),
    {{ok, NewPos}, State#state{file_position=NewPos}};
position({writing_file, Name}, {bof, Pos}, State) ->
    {{ok, {writing_file, Name}}, State#state{file_position=Pos}}.

read(#reading_file{length=TotalLength}, _Len, State=#state{file_position=TotalLength}) ->
    {eof, State};
read(#reading_file{bin=Bin, length=TotalLength}, Len, State=#state{file_position=Pos}) ->
    ok = lager:debug("read ~p",[{Pos, Len, TotalLength, Pos+Len}]),
    Data = case Pos+Len > TotalLength of
        false -> <<_:Pos/binary, D:Len/binary, _/binary>> = Bin,
                 D;
        true -> <<_:Pos/binary, D/binary>> = Bin,
                D
    end,
    {{ok, Data}, State}.

read_link(Path, State) ->
    ok = lager:debug("read link ~p", [Path]),
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
    ok = lager:debug("read file info ~p", [Path]),
    read_info(Path, State).

read_info(Path, State=#state{ls_info=Info, s3_root=Prefix}) ->
    FullPath = Prefix ++ string:sub_string(Path, 1),
    ok = lager:debug("Path: ~p", [FullPath]),
    Data = find_content_from_key(FullPath, Info),
    FileInfo = get_file_info_from_content(Data),
    ok = lager:debug("Info ~p", [{Data, FileInfo}]),
    {{ok, FileInfo}, State}.

rename(_Path, _Path2, State) ->
    ok = lager:debug("rename"),
    {{error, exdev}, State}.

write({writing_file, _Path}, Data, State=#state{uploading_bin=Bin}) ->
    ok = lager:debug("write"),
    PartialBin = <<Bin/binary, Data/binary>>,
    {ok, State#state{uploading_bin=PartialBin}}.

write_file_info(Path,Info, State) ->
    ok = lager:debug("write_file_info"),
    {file:write_file_info(Path, Info), State}.
