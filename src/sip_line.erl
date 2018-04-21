-module(sip_line).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec(init([]) ->
  {ok, {SupFlags :: supervisor:sup_flags(),
    [ChildSpec :: supervisor:child_spec()]
  }}).
init([]) ->
  SupFlags = #{strategy => simple_one_for_one, intensity => 1, period => 5},
  ChildSpecs = [#{id => sip_phone,
                  start => {sip_phone, start_link, []},
                  restart => transient,
                  shutdown => brutal_kill,
                  type => worker,
                  modules => [sip_phone]}],
  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
