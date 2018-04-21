-module(sip_phone_statem_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include("sip_session_data.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec test() -> any().

-spec all() -> list().
all() ->
  [
    start_link_test,
    register_line_test,
    match_code_test,
    init_test,
    terminate_test,
    format_status_test,
    handle_event_test,
    unauthorized_register_test,
    unauthorized_test,
    idle_test,
    proxy_unauthenticated_test,
    trying_test,
    session_progress_test,
    ringing_test,
    cancelling_test,
    bye_test
  ].

start_link_test(_Config) ->
  process_flag(trap_exit, true),
  Self = self(),
  meck:new(sip_phone_statem, [passthrough]),
  meck:expect(sip_phone_statem, init,
    fun(Args) ->
      Self ! {ok, Args, #data{}},
      {stop, test}
    end),
  sip_phone_statem:start_link(#{}),
  ?assertEqual({ok, [#{}], #data{}}, receive A -> A end),
  meck:unload(sip_phone_statem).

register_line_test(_Config) ->
  Self = self(),
  meck:new(sip_phone_statem, [passthrough]),
  meck:expect(sip_phone_statem, init,
    fun(_Args) ->
      {ok, unauthorized, #data{}}
    end),
  meck:expect(sip_phone_statem, unauthorized,
    fun(Reason, StateName, Data) ->
      Self ! {ok, Reason, StateName, Data},
      {stop, normal}
    end),
  meck:expect(sip_phone_statem, terminate,
    fun(_Reason, _StateName, _Data) ->
      ok
    end),
  {ok, Pid} = sip_phone_statem:start_link(#{}),
  sip_phone_statem:register_line(Pid),
  ?assertEqual({ok, cast, register, #data{}}, receive A -> A end),
  meck:unload(sip_phone_statem).

match_code_test(_Config) ->
  ?assertEqual("123", sip_phone_statem:match_code(<<"SIP/2.0 123 test">>)),
  ?assertEqual("123", sip_phone_statem:match_code(<<"SIP/2.0 123 234 test">>)),
  ?assertMatch({'EXIT', {{"Error matching code in message",<<"SIP/2.0 12">>}, _}},
    catch sip_phone_statem:match_code(<<"SIP/2.0 12">>)).

init_test(_Config) ->
  Self = self(),
  meck:new(gen_udp, [unstick, passthrough]),
  meck:expect(gen_udp, open,
    fun(Port, _OptsList) ->
      Self ! {ok, Port},
      {ok, socket}
    end),
  meck:new(sip_phone_statem, [passthrough]),
  meck:expect(sip_phone_statem, register_line,
    fun(Pid) ->
      Self ! {ok, Pid},
      ok
    end),
  {ok, unauthorized, Data} = sip_phone_statem:init([#{
    endpoint_ip   => endpoint_ip,
    endpoint_port => endpoint_port,
    host_port     => host_port,
    real_name     => real_name,
    username      => username,
    password      => password,
    realm         => realm,
    number        => number,
    delay         => delay
  }]),
  ?assertEqual(socket, Data#data.socket),
  ?assertEqual(endpoint_ip, Data#data.endpoint_ip),
  ?assertEqual(endpoint_port, Data#data.endpoint_port),
  ?assertEqual(host_port, Data#data.host_port),
  ?assertEqual(real_name, Data#data.real_name),
  ?assertEqual(username, Data#data.username),
  ?assertEqual(password, Data#data.password),
  ?assertEqual(realm, Data#data.realm),
  ?assertEqual(number, Data#data.number),
  ?assertEqual(delay, Data#data.delay),
  ?assertNotEqual(undefined, Data#data.c_sec),
  ?assertNotEqual(undefined, Data#data.call_id),
  ?assertNotEqual(undefined, Data#data.branch),
  ?assertNotEqual(undefined, Data#data.from_tag),
  ?assertEqual({ok, endpoint_port}, receive A -> A end),
  {ok, Pid} = receive A -> A end,
  ?assert(is_pid(Pid)),
  meck:unload(gen_udp).

terminate_test(_Config) ->
  Self = self(),
  meck:new(gen_udp, [unstick, passthrough]),
  meck:expect(gen_udp, close,
    fun(Socket) ->
      Self ! {ok, Socket},
      ok
    end),
  sip_phone_statem:terminate(normal, test, #data{socket = socket}),
  ?assertEqual({ok, socket}, receive A -> A end),
  meck:unload(gen_udp).

format_status_test(_Config) ->
  ?assertEqual(some_term, sip_phone_statem:format_status(opt, [pdict, state, data])).

handle_event_test(_Config) ->
  ?assertEqual({next_state, the_next_state_name, #data{}},
    sip_phone_statem:handle_event(event, content, state_name, #data{})).

unauthorized_register_test(_Config) ->
  Self = self(),
  meck:new(sip_packets_generator),
  meck:expect(sip_packets_generator, packet,
    fun(register, State) ->
      Self ! {register, State}
    end),
  Self = self(),
  meck:new(gen_udp, [unstick, passthrough]),
  meck:expect(gen_udp, send,
    fun(Socket, Host, Port, Message) ->
      Self ! {ok, Socket, Host, Port, Message},
      ok
    end),
  Data = #data{realm = "example.com",host_port = 5060, delay = 0},
  {next_state, unauthorized, Data} = sip_phone_statem:unauthorized(cast, register, Data),
  ?assertEqual({register, Data}, receive A -> A end),
  ?assertMatch({ok, _, "example.com", 5060, register}, receive A -> A end),
  meck:unload(gen_udp),
  meck:unload(sip_packets_generator).

unauthorized_test(_Config) ->
  meck:new(sip_packets_generator),
  meck:expect(sip_packets_generator, packet,
    fun(register_auth, Msg, State) ->
      {{cancel, Msg, State}, State}
    end),
  Self = self(),
  meck:new(gen_udp, [unstick]),
  meck:expect(gen_udp, send,
    fun(socket, "example.com", 5060, Msg) ->
      Self ! {ok, Msg}
    end),
  Data = #data{socket = socket, host_port = 5060, realm = "example.com"},
  {next_state, idle, Data} = sip_phone_statem:unauthorized(info, {udp, undef, undef, undef, message}, Data),
  ?assertEqual({ok, {cancel, message, Data}}, receive A -> A end),
  meck:unload(sip_packets_generator),
  meck:unload(gen_udp).

idle_test(_Config) ->
  meck:new(sip_packets_generator),
  meck:expect(sip_packets_generator, packet,
    fun(invite, Number, State) ->
      {{cancel, Number, State}, State}
    end),
  meck:new(gen_udp, [unstick]),
  Self = self(),
  meck:expect(gen_udp, send,
    fun(socket, "example.com", 5060, Msg) ->
      Self ! {ok, Msg}
    end),
  Number = <<"88000000000">>,
  Data = #data{socket = socket, realm = "example.com", host_port = 5060, number = Number},
  OKMessage = <<"SIP/2.0 200 OK">>,
  {next_state, proxy_unauthenticated, NewData} =
    sip_phone_statem:idle(info, {udp, undef, undef, undef, OKMessage}, Data),
  ?assertEqual({ok, {cancel, Number, NewData}}, receive A -> A end),
  meck:unload(sip_packets_generator),
  meck:unload(gen_udp).

proxy_unauthenticated_test(_Config) ->
  meck:new(sip_packets_generator),
  meck:expect(sip_packets_generator, packet,
    fun
       (ack, Number, Msg, State) ->
         {{ack, Number, Msg, State}, {State, immediate}};
       (invite_auth, Number, Msg, State) ->
         {{invite_auth, Number, Msg, State}, {State, new}}
     end),
  Self = self(),
  meck:new(gen_udp, [unstick]),
  meck:expect(gen_udp, send,
    fun(socket, "example.com", 5060, Msg) ->
      Self ! {ok, Msg}
    end),
  Number = <<"88000000000">>,
  Data = #data{socket = socket, realm = "example.com", host_port = 5060, number = Number},
  {next_state, trying, {{Data, immediate}, new}} =
    sip_phone_statem:proxy_unauthenticated(info, {udp, undef, undef, undef, message}, Data),
  ?assertEqual({ok, {ack, Number, message, Data}}, receive A -> A end),
  ?assertEqual({ok, {invite_auth, Number, message, {Data, immediate}}}, receive A -> A end),
  meck:unload(sip_packets_generator),
  meck:unload(gen_udp).

trying_test(_Config) ->
  meck:new(sip_packets_generator),
  meck:expect(sip_packets_generator, packet,
    fun(cancel, Number, State) ->
      {{cancel, Number, State}, State}
    end),
  Self = self(),
  meck:new(gen_udp, [unstick]),
  meck:expect(gen_udp, send,
    fun(socket, "example.com", 5060, Msg) ->
      Self ! {ok, Msg}
    end),
  Number = <<"88000000000">>,
  Data = #data{socket = socket, realm = "example.com", host_port = 5060, number = Number},
  {next_state, session_progress, Data} =
    sip_phone_statem:trying(info, {udp, undef, undef, undef, message}, Data),
  ?assertEqual({ok, {cancel, Number, Data}}, receive A -> A end),
  meck:unload(sip_packets_generator),
  meck:unload(gen_udp).

session_progress_test(_Config) ->
  Data = #data{},
  ?assertEqual({next_state, cancelling, Data},
    sip_phone_statem:session_progress(info, {udp, undef, undef, undef, <<"SIP/2.0 200 OK">>}, Data)),
  ?assertEqual({next_state, ringing, Data},
    sip_phone_statem:session_progress(info, {udp, undef, undef, undef, <<"SIP/2.0 123 TEST">>}, Data)).

ringing_test(_Config) ->
  ?assertEqual({next_state, cancelling, #data{}},
    sip_phone_statem:ringing(info, {udp, undef, undef, undef, message}, #data{})).

cancelling_test(_Config) ->
  meck:new(sip_packets_generator),
  meck:expect(sip_packets_generator, packet,
    fun
      (terminated_ack, Number, Msg, State) ->
        {{terminated_ack, Number, Msg, State}, {State, immediate}};
      (bye, Number, Msg, State) ->
        {{bye, Number, Msg, State}, {State, new}}
    end),
  Self = self(),
  meck:new(gen_udp, [unstick]),
  meck:expect(gen_udp, send,
    fun(socket, "example.com", 5060, Msg) ->
      Self ! {ok, Msg}
    end),
  Number = <<"88000000000">>,
  Data = #data{socket = socket, realm = "example.com", host_port = 5060, number = Number},
  {next_state, bye, {{Data, immediate}, new}} = sip_phone_statem:cancelling(info, {udp, undef, undef, undef, message}, Data),
  ?assertEqual({ok, {terminated_ack, Number, message, Data}}, receive A -> A end),
  ?assertEqual({ok, {bye, Number, message, {Data, immediate}}}, receive A -> A end),
  meck:unload(sip_packets_generator),
  meck:unload(gen_udp).

bye_test(_Config) ->
  process_flag(trap_exit, true),
  meck:new(sip_phone_statem, [passthrough]),
  meck:expect(sip_phone_statem, init,
    fun(_Args) ->
      {ok, bye, #data{}}
    end),
  meck:expect(sip_phone_statem, terminate,
    fun(_Reason, _StateName, _Data) ->
      ok
    end),
  {ok, Pid} = sip_phone_statem:start_link(#{}),
  Pid ! {udp, undef, undef, undef, message},
  ?assertEqual({'EXIT', Pid, normal}, receive A -> A end),
  meck:unload(sip_phone_statem).