-module(sc_line_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include("sip_session_data.hrl").

-include_lib("kernel/include/inet.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-spec test() -> any().

-spec all() -> list().
all() ->
  [handle_call_stub_test,
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

handle_call_stub_test(_Config) ->
  ?assertEqual({reply, ok, #data{}}, sc_line:handle_call(msg, from, #data{})).

handle_info_matched_test(_Config) ->
  Self = self(),
  meck:new(sc_line, [passthrough]),
  meck:expect(sc_line, handle_code_request, fun(Code, Msg, State) ->
                                                Self ! {Code, Msg, State},
                                                {noreply, State}
                                              end),
  MatchedMessage = <<"SIP/2.0 402 SomeStatus">>,
  sc_line:handle_info({udp, test, test, test, MatchedMessage}, #data{}),
  ?assertEqual({"402", MatchedMessage, #data{}}, receive A -> A end),
  meck:unload(sc_line).

handle_info_unmatched_test(_Config) ->
  Self = self(),
  meck:new(sc_line, [passthrough]),
  UnmatchedMessage = <<"SIP/2.0 42 SomeStatus">>,
  meck:expect(sc_line, handle_ping, fun(Msg, State) ->
                                        Self ! {Msg, State},
                                        {noreply, State}
                                      end),
  sc_line:handle_info({udp, test, test, test, UnmatchedMessage}, #data{}),
  ?assertEqual({UnmatchedMessage, #data{}}, receive A -> A end),
  meck:unload(sc_line).

handle_ping_test(_Config) ->
  ?assertEqual({noreply, #data{}}, sc_line:handle_ping(message, #data{})).

handle_code_100_request_test(_Config) ->
  ?assertEqual({noreply, #data{}}, sc_line:handle_code_request("100", message, #data{})).

handle_code_180_request_test(_Config) ->
  meck:new(sc_packets_generator),
  meck:expect(sc_packets_generator, packet, fun(cancel, Number, State) ->
                                                {{cancel, Number, State}, State}
                                             end),
  Self = self(),
  meck:new(gen_udp, [unstick]),
  meck:expect(gen_udp, send,  fun(socket, "example.com", 5060, Msg) ->
                                Self ! {ok, Msg}
                              end),
  Number = <<"88000000000">>,
  State = #data{socket = socket, realm = "example.com", number = Number},
  {noreply, State} = sc_line:handle_code_request("180", Number, State),
  ?assertEqual({ok, {cancel, Number, State}}, receive A -> A end),
  meck:unload(sc_packets_generator),
  meck:unload(gen_udp).

handle_code_183_request_test(_Config) ->
  ?assertEqual({noreply, #data{}}, sc_line:handle_code_request("183", message, #data{})).

handle_code_200_request_ok_test(_Config) ->
  meck:new(sc_packets_generator),
  meck:expect(sc_packets_generator, packet, fun(invite, Number, State) ->
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
  {noreply, NewState} = sc_line:handle_code_request("200", OKMessage, State),
  ?assertEqual({ok, {cancel, Number, NewState}}, receive A -> A end),
  meck:unload(sc_packets_generator),
  meck:unload(gen_udp).

handle_code_200_request_not_ok_test(_Config) ->
  meck:new(sc_packets_generator),
  meck:expect(sc_packets_generator, packet, fun(invite, Number, State) ->
                                                {{cancel, Number, State}, State}
                                             end),
  Number = <<"88000000000">>,
  State = #data{socket = socket, realm = "example.com", number = Number, post_invite = false},
  OKMessage = <<"SIP/2.0 200 NOTOK">>,
  {noreply, #data{}} = sc_line:handle_code_request("200", OKMessage, State),
  meck:unload(sc_packets_generator).

handle_code_401_request_test(_Config) ->
  meck:new(sc_packets_generator),
  meck:expect(sc_packets_generator, packet, fun(register_auth, Msg, State) ->
                                                {{cancel, Msg, State}, State}
                                             end),
  Self = self(),
  meck:new(gen_udp, [unstick]),
  meck:expect(gen_udp, send,  fun(socket, "example.com", 5060, Msg) ->
                                Self ! {ok, Msg}
                              end),
  State = #data{socket = socket, realm = "example.com"},
  {noreply, State} = sc_line:handle_code_request("401", message, State),
  ?assertEqual({ok, {cancel, message, State}}, receive A -> A end),
  meck:unload(sc_packets_generator),
  meck:unload(gen_udp).

handle_code_407_request_test(_Config) ->
  meck:new(sc_packets_generator),
  meck:expect(sc_packets_generator, packet, fun
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
  {noreply, {{State, immediate}, new}} = sc_line:handle_code_request("407", message, State),
  ?assertEqual({ok, {ack, Number, message, State}}, receive A -> A end),
  State = #data{socket = socket, realm = "example.com", number = Number},
  ?assertEqual({ok, {invite_auth, Number, message, {State, immediate}}}, receive A -> A end),
  meck:unload(sc_packets_generator),
  meck:unload(gen_udp).

handle_code_487_request_test(_Config) ->
  meck:new(sc_packets_generator),
  meck:expect(sc_packets_generator, packet, fun(terminated_ack, Number, Msg, State) ->
                                                {{terminated_ack, Number, Msg, State}, State}
                                             end),
  Self = self(),
  meck:new(gen_udp, [unstick]),
  meck:expect(gen_udp, send,  fun(socket, "example.com", 5060, Msg) ->
                                Self ! {ok, Msg}
                              end),
  Number = <<"88000000000">>,
  State = #data{socket = socket, realm = "example.com", number = Number},
  {stop, normal, State} = sc_line:handle_code_request("487", message, State),
  ?assertEqual({ok, {terminated_ack, Number, message, State}}, receive A -> A end),
  meck:unload(sc_packets_generator),
  meck:unload(gen_udp).

handle_code_503_request_test(_Config) ->
  ?assertEqual({stop, normal, #data{}}, sc_line:handle_code_request("503", message, #data{})).
