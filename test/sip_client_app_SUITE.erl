-module(sip_client_app_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-spec test() -> any().

-spec all() -> list().
all() ->
  [
    start_test,
    start_with_ip_test,
    start_no_ip_test,
    stop_test,
    choose_interface
  ].

choose_interface(_Config) ->
  ?assert(is_tuple(sip_client_app:choose_interface())).

start_test(_Config) ->
  meck:new(sip_client_sup),
  meck:expect(sip_client_sup, start_link, fun() -> list_to_pid("<0.123.0>") end),
  meck:new(call_scheduler),
  meck:expect(call_scheduler, start_link, fun() -> list_to_pid("<0.124.0>") end),
  {ok, Pid} = sip_client_app:start(normal, []),
  ?assert(is_pid(Pid)),
  Ref = erlang:monitor(process, Pid),
  ProcStat =  receive
                {'DOWN', Ref, _, Pid, _} ->
                  'DOWN'
              after
                10 -> ok
              end,
  ?assertEqual(ok, ProcStat),
  meck:unload(sip_client_sup),
  meck:unload(call_scheduler).

start_with_ip_test(_Config) ->
  meck:new(sip_client_sup),
  meck:expect(sip_client_sup, start_link, fun() -> list_to_pid("<0.123.0>") end),
  meck:new(call_scheduler),
  meck:expect(call_scheduler, start_link, fun() -> list_to_pid("<0.124.0>") end),
  IpAddr = {16,16,16,16},
  application:set_env(sip_client, ip_addr, IpAddr),
  sip_client_app:start(normal, []),
  ?assertEqual(application:get_env(sip_client, ip_addr), {ok, IpAddr}),
  meck:unload(sip_client_sup),
  meck:unload(call_scheduler).

start_no_ip_test(_Config) ->
  process_flag(trap_exit, true),
  meck:new(sip_client_sup),
  meck:expect(sip_client_sup, start_link, fun() -> list_to_pid("<0.123.0>") end),
  meck:new(call_scheduler),
  meck:expect(call_scheduler, start_link, fun() -> list_to_pid("<0.124.0>") end),
  meck:new(inet, [unstick]),
  meck:expect(inet, getifaddrs, fun() -> {ok, []} end),
  catch application_controller:unset_env(sip_client, ip_addr),
  {Pid, Ref} = spawn_monitor(sip_client_app, start, [normal, []]),
  Result =  receive
              {'DOWN', Ref, process, Pid, "Inet interface not found"} -> {ok, 'DOWN'}
            after 1000 -> error
            end,
  ?assertEqual({ok, 'DOWN'}, Result),
  meck:unload(inet),
  meck:unload(sip_client_sup),
  meck:unload(call_scheduler).



stop_test(_Config) ->
  meck:new(supervisor, [unstick]),
  meck:expect(supervisor, terminate_child, fun(sip_client_sup, call_scheduler) -> ok end),
  Pid = erlang:spawn(?MODULE, mock_sip_client, [self()]),
  erlang:register(sip_client_sup, Pid),
  sip_client_app:stop([]),
  ?assertEqual(ok,receive
                    {terminated, Pid} -> ok
                  end),
  meck:unload(supervisor).


mock_sip_client(Parent) ->
  process_flag(trap_exit, true),
  receive
    {'EXIT', _, terminate} ->
        Parent ! {terminated, self()}
  end.