-module(sssftp_s3_api).

-behaviour(ssh_sftpd_file_api).

-include_lib("kernel/include/file.hrl").
%% API
-export([close/2, delete/2, del_dir/2, get_cwd/1, is_dir/2, list_dir/2,
	 make_dir/2, make_symlink/3, open/3, position/3, read/3,
	 read_file_info/2, read_link/2, read_link_info/2, rename/3,
	 write/3, write_file_info/3]).

-export([connectfun/3]).

-record(state, {aws_bucket=undefined,
                ls_info=undefined,
                filenames=undefined,
                root=undefined,
                user=undefined}).

connectfun(User, _IP, _Method) ->
    io:format("User connected ~p~n", [{User, self()}]).

close(IoDevice, State) ->
    {file:close(IoDevice), State}.

delete(Path, State) ->
    {file:delete(Path), State}.

del_dir(Path, State) ->
    {file:del_dir(Path), State}.

get_cwd(State) ->
    AWS_BUCKET = proplists:get_value(aws_bucket, State),
    io:format("CWD ~p~n", [{State, self()}]),
    {ok, User} = sssftp_user_session:get(self()),
    Root = "uploads/" ++ User,
    io:format("User is: ~p~n", [User]),
    {file:get_cwd(), #state{aws_bucket=AWS_BUCKET,
                            root=Root,
                            user=User}}.

is_dir(AbsPath, State=#state{root=Root}) ->
    io:format("is_dir ~pn", [AbsPath]),
    AWS_BUCKET = State#state.aws_bucket,
    Prefix = Root ++ AbsPath,
    Options = [{prefix, Prefix}],
    io:format("S3 Options ~p~n", [Options]),
    Result = erlcloud_s3:list_objects(AWS_BUCKET, Options),
    Contents = proplists:get_value(contents, Result),
    Files = [proplists:get_value(key, X) || X <- Contents],
    StrippedFiles = strip_path(Prefix, Files),
    IsDir = lists:suffix("/", AbsPath),
    io:format("IsDir ~p~n", [{AbsPath, IsDir}]),
    {true, State#state{ls_info=Contents, filenames=StrippedFiles}}.

list_dir(AbsPath, State=#state{filenames=StrippedFiles}) ->
    io:format("List dir ~p~n", [AbsPath]),
    io:format("Files ~p~n", [StrippedFiles]),
    {{ok, StrippedFiles}, State}.

strip_path(Prefix, Strings) when is_list(Prefix) ->
    strip_path(Prefix, string:len(Prefix) + 2, Strings).

strip_path(Prefix, Len, [Prefix|T]) when is_integer(Len) ->
    strip_path(Prefix, Len, T);
strip_path(Prefix, Len, [H|T]) when is_integer(Len) ->
    [strip_trailing_slash(string:sub_string(H, Len)) | strip_path(Prefix, Len, T)];
strip_path(_Prefix, _Len, []) ->
    [].

strip_trailing_slash(String) ->
    string:strip(String, right, $/).

make_dir(Dir, State) ->
    {file:make_dir(Dir), State}.

make_symlink(Path2, Path, State) ->
    {file:make_symlink(Path2, Path), State}.

open(Path, Flags, State) ->
    {file:open(Path, Flags), State}.

position(IoDevice, Offs, State) ->
    {file:position(IoDevice, Offs), State}.

read(IoDevice, Len, State) ->
    {file:read(IoDevice, Len), State}.

read_link(Path, State) ->
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
    read_info(Path, State).

read_info(Path, State=#state{ls_info=Info, root=Prefix}) ->
    FullPath = Prefix ++ string:sub_string(Path, 1),
    io:format("Path: ~p~n", [FullPath]),
    Data = find_content_from_key(FullPath, Info),
    FileInfo = get_file_info_from_content(Data),
    io:format("Info ~p~n", [{Data, FileInfo}]),
    {{ok, FileInfo}, State}.

rename(Path, Path2, State) ->
    {file:rename(Path, Path2), State}.

write(IoDevice, Data, State) ->
    {file:write(IoDevice, Data), State}.

write_file_info(Path,Info, State) ->
    {file:write_file_info(Path, Info), State}.
