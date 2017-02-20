-module(sssftp_app).
-behaviour(application).

-compile([{parse_transform, lager_transform}]).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok, Servers} = application:get_env(sssftp, servers),
    ok = lager:debug("Servers ~p", [Servers]),
    OkProcs = lists:map(fun sssftp:create_child_spec/1, Servers),
    ok = lager:debug("OkProcs ~p", [OkProcs]),
    [SFTPProcs] = [X || {ok, X} <- OkProcs],
    ok= lager:debug("SFTPProcs ~p", [SFTPProcs]),
    % {ok, SFTPProcs} = sssftp:create_child_spec(Opts),
    {ok, Pid} = sssftp_sup:start_link([{children, SFTPProcs}]),
    {ok, Pid}.

stop(_State) ->
	ok.
