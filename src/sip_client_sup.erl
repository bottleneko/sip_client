-module(sip_client_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, Pid :: pid()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec(init([]) ->
  {ok, {SupFlags :: supervisor:sup_flags(),
    [ChildSpec :: supervisor:child_spec()]
  }}).
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 2, period => 5},
  ChildSpecs = [#{id => sc_line,
                  start => {sc_line, start_link, []},
                  restart => permanent,
                  shutdown => brutal_kill,
                  type => supervisor,
                  modules => [sc_line]},
                #{id => sc_call_scheduler,
                  start => {sc_call_scheduler, start_link, []},
                  restart => temporary,
                  shutdown => 5000,
                  type => worker,
                  modules => [sc_call_scheduler]}],
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
