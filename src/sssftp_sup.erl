-module(sssftp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [{server,
               {ssh, daemon, [8989, [{system_dir, "/tmp/"},
                              {system_dir, "/etc/ssh"},
                              {subsystems, [ssh_sftpd:subsystem_spec([{file_handler, sssftp_s3_api}])
                                        ]}]]},
                permanent,
                5000,
                worker,
                [sssftp_s3_api]}],
	{ok, {{one_for_one, 1, 5}, Procs}}.
