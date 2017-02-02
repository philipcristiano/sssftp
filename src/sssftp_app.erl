-module(sssftp_app).
-behaviour(application).

-compile([{parse_transform, lager_transform}]).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok, Pid} = sssftp_sup:start_link(),
    {Port, _} = string:to_integer(os:getenv("PORT", "8989")),
    Opts = [{aws_bucket, os:getenv("AWS_BUCKET")},
            {port, Port}],
    sssftp:add_sftp_server(Opts),
    {ok, Pid}.

stop(_State) ->
	ok.
