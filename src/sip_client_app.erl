-module(sip_client_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, choose_interface/0]).

%%====================================================================
%% API
%%====================================================================

-spec start(_StartType ::  atom(), _StartArgs :: list()) -> Ret :: {ok, pid()}.
start(_StartType, _StartArgs) ->
  Addr = case application:get_env(sip_client, ip_addr) of
          {ok, Ip} ->
            Ip;
          _ ->
            choose_interface()
         end,
  application:set_env(sip_client, ip, Addr),
  sip_client_sup:start_link(),
  call_scheduler:start_link(),
  {ok, self()}.

%%--------------------------------------------------------------------
-spec stop(_State :: any) -> any().
stop(_State) ->
  supervisor:terminate_child(sip_client_sup, call_scheduler),
  exit(erlang:whereis(sip_client_sup), terminate).

%%====================================================================
%% Internal functions
%%====================================================================

-spec choose_interface() -> Ret :: tuple().
choose_interface() ->
  {ok, InetIf} = inet:getifaddrs(),
  IfOpts = lists:foldl(fun(X, Acc) -> Acc ++ erlang:element(2, X) end, [], InetIf),
  IfAddrs = lists:foldl(fun(X, Acc) -> Acc ++ addr_filter(X) end, [], IfOpts),
  choose_interfaceT(IfAddrs).

-spec choose_interfaceT(Addrs :: list(tuple())) -> Ret :: tuple().
choose_interfaceT([]) ->
  exit("Inet interface not found");
choose_interfaceT([{127, _, _, _}|Xs]) ->
  choose_interfaceT(Xs);
choose_interfaceT([X|_]) ->
  X.


-spec addr_filter(Addr :: tuple() | any()) -> Ret :: [tuple()].
addr_filter({addr, Addr = {_, _, _, _}}) ->
  [Addr];
addr_filter(_) ->
  [].
