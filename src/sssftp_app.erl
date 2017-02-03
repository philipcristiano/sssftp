-module(sssftp_app).
-behaviour(application).

-compile([{parse_transform, lager_transform}]).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {Port, _} = string:to_integer(os:getenv("PORT", "8989")),
    Opts = [{aws_bucket, os:getenv("AWS_BUCKET")},
            {port, Port}],
    {ok, SFTPProcs} = sssftp:create_child_spec(Opts),
    {ok, Pid} = sssftp_sup:start_link([{children, SFTPProcs}]),
    {ok, Pid}.

stop(_State) ->
	ok.
