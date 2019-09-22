-module(sip_client_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(Configuration) -> {ok, pid()} when
    Configuration :: map().
start_link(Configuration) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Configuration).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec init(Configuration) -> {ok, SupSpec} when
    Configuration :: map(),
    SupSpec       :: {SupFlags, [ChildSpec]},
    SupFlags      :: supervisor:sup_flags(),
    ChildSpec     :: supervisor:child_spec().
init(Configuration) ->
  SupFlags = #{strategy => one_for_one, intensity => 2, period => 5},
  ChildSpecs =
    [#{id       => sc_line_sup,
       start    => {sc_line_sup, start_link, [maps:with([ip_addr, username, password, realm, period_ms], Configuration)]},
       restart  => permanent,
       shutdown => brutal_kill,
       type     => supervisor
      },

     #{id       => sc_call_scheduler,
       start    => {sc_call_scheduler, start_link, []},
       restart  => temporary,
       shutdown => 5000
      }
    ],
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
