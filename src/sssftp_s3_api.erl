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
    io:format("User is: ~p~n", [User]),
    {file:get_cwd(), #state{aws_bucket=AWS_BUCKET,
                            user=User}}.

is_dir(AbsPath, State) ->
    {filelib:is_dir(AbsPath), State}.

list_dir(AbsPath, State) ->
    io:format("List dir ~p~n", [AbsPath]),
    AWS_BUCKET = State#state.aws_bucket,
    User = State#state.user,
    Prefix = "uploads/" ++ User,
    Options = [{prefix, Prefix}],
    Result = erlcloud_s3:list_objects(AWS_BUCKET, Options),
    Contents = proplists:get_value(contents, Result),
    Files = [proplists:get_value(key, X) || X <- Contents],
    StrippedFiles = strip_prefix(Prefix, Files),
    io:format("Files ~p~n", [StrippedFiles]),
    {{ok, StrippedFiles}, State}.

strip_prefix(Prefix, Strings) when is_list(Prefix) ->
    strip_prefix(string:len(Prefix) + 2, Strings);
strip_prefix(Len, [H|T]) when is_integer(Len) ->
    [string:sub_string(H, Len) | strip_prefix(Len, T)];
strip_prefix(_Len, []) ->
    [].

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
    {file:read_link(Path), State}.

read_link_info([], State) ->
    {{error, enoent}, State};
read_link_info(_Path, State) ->
    {{ok, #file_info{}}, State}.

read_file_info(Path, State) ->
    {file:read_file_info(Path), State}.

rename(Path, Path2, State) ->
    {file:rename(Path, Path2), State}.

write(IoDevice, Data, State) ->
    {file:write(IoDevice, Data), State}.

write_file_info(Path,Info, State) ->
{file:write_file_info(Path, Info), State}.
