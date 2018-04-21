-module(sip_phone_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include("sip_session_data.hrl").

-include_lib("kernel/include/inet.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-spec test() -> any().

-spec all() -> list().
all() ->
  [
    start_link_test,
    register_test,
    init_test,
    handle_register_test,
    handle_call_stub_test,
    handle_info_matched_test,
    handle_info_unmatched_test,
    handle_ping_test,
    handle_code_100_request_test,
    handle_code_180_request_test,
    handle_code_183_request_test,
    handle_code_200_request_ok_test,
    handle_code_200_request_not_ok_test,
    handle_code_503_request_test,
    handle_code_401_request_test,
    handle_code_407_request_test,
    handle_code_487_request_test
  ].

start_link_test(_Config) ->
  process_flag(trap_exit, true),
  ExpectedNumber = <<"88000000000">>,
  meck:new(sip_phone, [passthrough]),
  meck:expect(sip_phone, init, fun([Number]) when ExpectedNumber =:= Number ->
                                          exit(Number)
                               end),
  sip_phone:start_link(ExpectedNumber),
  ?assertEqual({'EXIT', ExpectedNumber}, receive {'EXIT', _, Number} -> {'EXIT', Number} end),
  meck:unload(sip_phone).

register_test(_Config) ->
  process_flag(trap_exit, true),
  meck:new(sip_phone, [passthrough]),
  meck:expect(sip_phone, init, fun([Number]) ->
                                {ok, [Number]}
                               end),
  meck:expect(sip_phone, handle_cast, fun(register, State) ->
                                        exit(State)
                                      end),
  ExpectedNumber = <<"88000000000">>,
  {ok, Pid} = sip_phone:start_link(ExpectedNumber),
  Ref = erlang:monitor(process, Pid),
  sip_phone:register(Pid),
  Stat =  receive
            {'DOWN', Ref, process, Pid, _} -> {ok, 'DOWN'}
          end,
  ?assertEqual({ok, 'DOWN'}, Stat),
  meck:unload(sip_phone).

init_test(_Config) ->
  process_flag(trap_exit, true),
  Self = self(),
  meck:new(gen_udp, [unstick]),
  meck:expect(gen_udp, open, fun(Port, Opts) ->
                              [{ifaddr, Interface}] = lists:filter( fun
                                                                     ({ifaddr, _}) -> true;
                                                                     (_) -> false
                                                                    end, Opts),
                              Self ! {Interface, Port},
                              {ok, socket}
                             end),
  ExpectedNumber = <<"88000000000">>,
  IpAddr = {8,8,8,8},
  application:set_env(sip_client, ip, IpAddr),
  Username = "username",
  application:set_env(sip_client, username, Username),
  Password = "password",
  application:set_env(sip_client, password, Password),
  Realm = "example.com",
  application:set_env(sip_client, realm, Realm),
  {ok, State} = sip_phone:init([ExpectedNumber]),
  ?assertEqual(IpAddr, State#data.endpoint_ip),
  ?assertEqual(Username, State#data.username),
  ?assertEqual(Password, State#data.password),
  ?assertEqual(Realm, State#data.realm),
  ?assertNotEqual(undefined, State#data.from_tag),
  ?assertNotEqual(undefined, State#data.branch),
  ?assertNotEqual(undefined, State#data.call_id),
  ?assertNotEqual(undefined, State#data.c_sec),
  ?assertEqual(socket, State#data.socket),
  {IpAddr, Port} = receive A -> A end,
  ?assert(Port > 59000 andalso Port =< 65535),
  meck:unload(gen_udp).

handle_register_test(_Config) ->
  ExpectedState = #data{socket = socket, realm = "example.com"},
  meck:new(sip_packets_generator),
  meck:expect(sip_packets_generator, packet, fun(register, State) when State =:= ExpectedState ->
                                              {msg, {ExpectedState, new}}
                                             end),
  Self = self(),
  meck:new(gen_udp, [unstick]),
  meck:expect(gen_udp, send,  fun(socket, "example.com", 5060, Msg) ->
                                Self ! {ok, Msg}
                              end),
  application:set_env(sip_client, period, 0),
  {noreply, {ExpectedState, new}} = sip_phone:handle_cast(register, ExpectedState),
  ?assertEqual({ok, msg}, receive A -> A end),
  meck:unload(sip_packets_generator),
  meck:unload(gen_udp).

handle_call_stub_test(_Config) ->
  ?assertEqual({reply, ok, #data{}}, sip_phone:handle_call(msg, from, #data{})).

handle_info_matched_test(_Config) ->
  Self = self(),
  meck:new(sip_phone, [passthrough]),
  meck:expect(sip_phone, handle_code_request, fun(Code, Msg, State) ->
                                                Self ! {Code, Msg, State},
                                                {noreply, State}
                                              end),
  MatchedMessage = <<"SIP/2.0 402 SomeStatus">>,
  sip_phone:handle_info({udp, test, test, test, MatchedMessage}, #data{}),
  ?assertEqual({"402", MatchedMessage, #data{}}, receive A -> A end),
  meck:unload(sip_phone).

handle_info_unmatched_test(_Config) ->
  Self = self(),
  meck:new(sip_phone, [passthrough]),
  UnmatchedMessage = <<"SIP/2.0 42 SomeStatus">>,
  meck:expect(sip_phone, handle_ping, fun(Msg, State) ->
                                        Self ! {Msg, State},
                                        {noreply, State}
                                      end),
  sip_phone:handle_info({udp, test, test, test, UnmatchedMessage}, #data{}),
  ?assertEqual({UnmatchedMessage, #data{}}, receive A -> A end),
  meck:unload(sip_phone).

handle_ping_test(_Config) ->
  ?assertEqual({noreply, #data{}}, sip_phone:handle_ping(message, #data{})).

handle_code_100_request_test(_Config) ->
  ?assertEqual({noreply, #data{}}, sip_phone:handle_code_request("100", message, #data{})).

handle_code_180_request_test(_Config) ->
  meck:new(sip_packets_generator),
  meck:expect(sip_packets_generator, packet, fun(cancel, Number, State) ->
                                                {{cancel, Number, State}, State}
                                             end),
  Self = self(),
  meck:new(gen_udp, [unstick]),
  meck:expect(gen_udp, send,  fun(socket, "example.com", 5060, Msg) ->
                                Self ! {ok, Msg}
                              end),
  Number = <<"88000000000">>,
  State = #data{socket = socket, realm = "example.com", number = Number},
  {noreply, State} = sip_phone:handle_code_request("180", Number, State),
  ?assertEqual({ok, {cancel, Number, State}}, receive A -> A end),
  meck:unload(sip_packets_generator),
  meck:unload(gen_udp).

handle_code_183_request_test(_Config) ->
  ?assertEqual({noreply, #data{}}, sip_phone:handle_code_request("183", message, #data{})).

handle_code_200_request_ok_test(_Config) ->
  meck:new(sip_packets_generator),
  meck:expect(sip_packets_generator, packet, fun(invite, Number, State) ->
                                               {{cancel, Number, State}, State}
                                             end),
  meck:new(gen_udp, [unstick]),
  Self = self(),
  meck:expect(gen_udp, send,  fun(socket, "example.com", 5060, Msg) ->
                                Self ! {ok, Msg}
                              end),
  Number = <<"88000000000">>,
  State = #data{socket = socket, realm = "example.com", number = Number, post_invite = false},
  OKMessage = <<"SIP/2.0 200 OK">>,
  {noreply, NewState} = sip_phone:handle_code_request("200", OKMessage, State),
  ?assertEqual({ok, {cancel, Number, NewState}}, receive A -> A end),
  meck:unload(sip_packets_generator),
  meck:unload(gen_udp).

handle_code_200_request_not_ok_test(_Config) ->
  meck:new(sip_packets_generator),
  meck:expect(sip_packets_generator, packet, fun(invite, Number, State) ->
                                                {{cancel, Number, State}, State}
                                             end),
  Number = <<"88000000000">>,
  State = #data{socket = socket, realm = "example.com", number = Number, post_invite = false},
  OKMessage = <<"SIP/2.0 200 NOTOK">>,
  {noreply, #data{}} = sip_phone:handle_code_request("200", OKMessage, State),
  meck:unload(sip_packets_generator).

handle_code_401_request_test(_Config) ->
  meck:new(sip_packets_generator),
  meck:expect(sip_packets_generator, packet, fun(register_auth, Msg, State) ->
                                                {{cancel, Msg, State}, State}
                                             end),
  Self = self(),
  meck:new(gen_udp, [unstick]),
  meck:expect(gen_udp, send,  fun(socket, "example.com", 5060, Msg) ->
                                Self ! {ok, Msg}
                              end),
  State = #data{socket = socket, realm = "example.com"},
  {noreply, State} = sip_phone:handle_code_request("401", message, State),
  ?assertEqual({ok, {cancel, message, State}}, receive A -> A end),
  meck:unload(sip_packets_generator),
  meck:unload(gen_udp).

handle_code_407_request_test(_Config) ->
  meck:new(sip_packets_generator),
  meck:expect(sip_packets_generator, packet, fun
                                               (ack, Number, Msg, State) ->
                                                 {{ack, Number, Msg, State}, {State, immediate}};
                                               (invite_auth, Number, Msg, State) ->
                                                 {{invite_auth, Number, Msg, State}, {State, new}}
                                             end),
  Self = self(),
  meck:new(gen_udp, [unstick]),
  meck:expect(gen_udp, send,  fun(socket, "example.com", 5060, Msg) ->
                                  Self ! {ok, Msg}
                              end),
  Number = <<"88000000000">>,
  State = #data{socket = socket, realm = "example.com", number = Number},
  {noreply, {{State, immediate}, new}} = sip_phone:handle_code_request("407", message, State),
  ?assertEqual({ok, {ack, Number, message, State}}, receive A -> A end),
  State = #data{socket = socket, realm = "example.com", number = Number},
  ?assertEqual({ok, {invite_auth, Number, message, {State, immediate}}}, receive A -> A end),
  meck:unload(sip_packets_generator),
  meck:unload(gen_udp).

handle_code_487_request_test(_Config) ->
  meck:new(sip_packets_generator),
  meck:expect(sip_packets_generator, packet, fun(terminated_ack, Number, Msg, State) ->
                                                {{terminated_ack, Number, Msg, State}, State}
                                             end),
  Self = self(),
  meck:new(gen_udp, [unstick]),
  meck:expect(gen_udp, send,  fun(socket, "example.com", 5060, Msg) ->
                                Self ! {ok, Msg}
                              end),
  Number = <<"88000000000">>,
  State = #data{socket = socket, realm = "example.com", number = Number},
  {stop, normal, State} = sip_phone:handle_code_request("487", message, State),
  ?assertEqual({ok, {terminated_ack, Number, message, State}}, receive A -> A end),
  meck:unload(sip_packets_generator),
  meck:unload(gen_udp).

handle_code_503_request_test(_Config) ->
  ?assertEqual({stop, normal, #data{}}, sip_phone:handle_code_request("503", message, #data{})).