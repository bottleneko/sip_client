-module(sip_client_app).
-behaviour(application).

%%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

start(_StartType, _StartArgs) ->
  Configuration =
    #{ip_addr   => application:get_env(ip_addr, {0, 0, 0, 0}),
      username  => application:get_env(username),
      password  => application:get_env(password),
      realm     => application:get_env(realm),
      period_ms => application:get_env(period_ms)
     },
  sip_client_sup:start_link(Configuration).

stop(_State) ->
  ok.
