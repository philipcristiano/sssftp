-module(sssftp_sup).
-behaviour(supervisor).
-compile([{parse_transform, lager_transform}]).

-export([start_link/1]).
-export([init/1]).

start_link(Opts) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

init(Opts) ->
    Children = proplists:get_value(children, Opts, []),
	{ok, {{one_for_one, 1, 5}, Children}}.
