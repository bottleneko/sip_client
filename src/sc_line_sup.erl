-module(sc_line_sup).
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

-spec init(Configuration) -> {ok, SupSpec} when
    Configuration :: map(),
    SupSpec       :: {SupFlags, [ChildSpec]},
    SupFlags      :: supervisor:sup_flags(),
    ChildSpec     :: supervisor:child_spec().
init(Configuration) ->
  SupFlags = #{strategy => simple_one_for_one},
  ChildSpecs =
    [#{id       => sc_line,
       start    => {sc_line, start_link, [Configuration]},
       restart  => transient,
       shutdown => brutal_kill
      }],
  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
