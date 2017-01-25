%%%-------------------------------------------------------------------
%%% @author philipcristiano
%%% @copyright 2016 philipcristiano
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(sssftp_user_session).

-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

%% API functions
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% API
-export([add/2,
         get/1,
         get/2,
         stop/1]).

-record(state, {client_pid, user=undefined}).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec add(pid()| atom(), nonempty_string()) -> ok.
add(USPid, Username) ->
    ok = lager:debug("Called add"),
    gen_server:call(USPid, {add, self(), Username}).

-spec get(pid()) -> {ok, nonempty_string()} | {error, undefined}.
get(SessPid) ->
    get(?MODULE, SessPid).

-spec get(pid() | atom(), pid()) -> {ok, nonempty_string()} | {error, undefined}.
get(USPid, SessPid) ->
    gen_server:call(USPid, {get, SessPid}).

-spec stop(pid() | atom()) -> ok.
stop(USPid) ->
    gen_server:call(USPid, stop).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    ok = lager:debug("Init user session"),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add, SessPid, Username}, _From, #state{user=undefined}) ->
    ok = lager:debug("Adding undefined ~p", [Username]),
    true = link(SessPid),
    {reply, ok, #state{client_pid=SessPid, user=Username}};
handle_call({add, _SessPid, Username}, _From, #state{user=Username}) ->
    ok = lager:debug("Adding SameUser ~p", [Username]),
    {reply, ok, #state{user=Username}};
handle_call({add, _SessPid, Username}, _From, #state{user=_IncorrectUser}) ->
    ok = lager:debug("Adding Different User ~p", [Username]),
    {reply, error, #state{user=Username}};

handle_call({get, SessPid}, _From, #state{client_pid=undefined, user=User}) ->
    ok = lager:debug("Getting ~p", [SessPid]),
    Reply = get_resp(User),
    ok = lager:debug("Reply ~p,~p", [User,Reply]),
    {reply, Reply, #state{user=undefined}};
handle_call({get, SessPid}, _From, #state{client_pid=OrigSess, user=User}) ->
    unlink(OrigSess),
    ok = lager:debug("Getting ~p", [SessPid]),
    Reply = get_resp(User),
    ok = lager:debug("Reply ~p,~p", [User,Reply]),
    {reply, Reply, #state{client_pid=undefined, user=undefined}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


get_resp(undefined) ->
    {error, undefined};
get_resp(Resp) ->
    {ok, Resp}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, _Reason}, State=#state{client_pid=Pid}) ->
    {noreply, State#state{client_pid=undefined, user=undefined}};
handle_info(Info, State) ->
    ok = lager:debug("Unhandled info ~p", [{Info, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
