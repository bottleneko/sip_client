-module(sc_line_statem_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include("sip_session_data.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec test() -> any().

-spec all() -> list().
all() ->
  [start_link_test,
   register_line_test,
   match_code_test,
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
  meck:new(sc_line_statem, [passthrough]),
  meck:expect(sc_line_statem, init,
    fun(Args) ->
      Self ! {ok, Args, #data{}},
      {stop, test}
    end),
  sc_line_statem:start_link(#{}),
  ?assertEqual({ok, [#{}], #data{}}, receive A -> A end),
  meck:unload(sc_line_statem).

register_line_test(_Config) ->
  Self = self(),
  meck:new(sc_line_statem, [passthrough]),
  meck:expect(sc_line_statem, init,
    fun(_Args) ->
      {ok, unauthorized, #data{}}
    end),
  meck:expect(sc_line_statem, unauthorized,
    fun(Reason, StateName, Data) ->
      Self ! {ok, Reason, StateName, Data},
      {stop, normal}
    end),
  meck:expect(sc_line_statem, terminate,
    fun(_Reason, _StateName, _Data) ->
      ok
    end),
  {ok, Pid} = sc_line_statem:start_link(#{}),
  sc_line_statem:register_line(Pid),
  ?assertEqual({ok, cast, register, #data{}}, receive A -> A end),
  meck:unload(sc_line_statem).

match_code_test(_Config) ->
  ?assertEqual("123", sc_line_statem:match_code(<<"SIP/2.0 123 test">>)),
  ?assertEqual("123", sc_line_statem:match_code(<<"SIP/2.0 123 234 test">>)),
  ?assertMatch({'EXIT', {{"Error matching code in message",<<"SIP/2.0 12">>}, _}},
    catch sc_line_statem:match_code(<<"SIP/2.0 12">>)).

terminate_test(_Config) ->
  Self = self(),
  meck:new(gen_udp, [unstick, passthrough]),
  meck:expect(gen_udp, close,
    fun(Socket) ->
      Self ! {ok, Socket},
      ok
    end),
  sc_line_statem:terminate(normal, test, #data{socket = socket}),
  ?assertEqual({ok, socket}, receive A -> A end),
  meck:unload(gen_udp).

format_status_test(_Config) ->
  ?assertEqual(some_term, sc_line_statem:format_status(opt, [pdict, state, data])).

handle_event_test(_Config) ->
  ?assertEqual({next_state, the_next_state_name, #data{}},
    sc_line_statem:handle_event(event, content, state_name, #data{})).

unauthorized_register_test(_Config) ->
  Self = self(),
  meck:new(sc_packets_generator),
  meck:expect(sc_packets_generator, packet,
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
  {next_state, unauthorized, Data} = sc_line_statem:unauthorized(cast, register, Data),
  ?assertEqual({register, Data}, receive A -> A end),
  ?assertMatch({ok, _, "example.com", 5060, register}, receive A -> A end),
  meck:unload(gen_udp),
  meck:unload(sc_packets_generator).

unauthorized_test(_Config) ->
  meck:new(sc_packets_generator),
  meck:expect(sc_packets_generator, packet,
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
  {next_state, idle, Data} = sc_line_statem:unauthorized(info, {udp, undef, undef, undef, message}, Data),
  ?assertEqual({ok, {cancel, message, Data}}, receive A -> A end),
  meck:unload(sc_packets_generator),
  meck:unload(gen_udp).

idle_test(_Config) ->
  meck:new(sc_packets_generator),
  meck:expect(sc_packets_generator, packet,
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
    sc_line_statem:idle(info, {udp, undef, undef, undef, OKMessage}, Data),
  ?assertEqual({ok, {cancel, Number, NewData}}, receive A -> A end),
  meck:unload(sc_packets_generator),
  meck:unload(gen_udp).

proxy_unauthenticated_test(_Config) ->
  meck:new(sc_packets_generator),
  meck:expect(sc_packets_generator, packet,
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
    sc_line_statem:proxy_unauthenticated(info, {udp, undef, undef, undef, message}, Data),
  ?assertEqual({ok, {ack, Number, message, Data}}, receive A -> A end),
  ?assertEqual({ok, {invite_auth, Number, message, {Data, immediate}}}, receive A -> A end),
  meck:unload(sc_packets_generator),
  meck:unload(gen_udp).

trying_test(_Config) ->
  meck:new(sc_packets_generator),
  meck:expect(sc_packets_generator, packet,
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
    sc_line_statem:trying(info, {udp, undef, undef, undef, message}, Data),
  ?assertEqual({ok, {cancel, Number, Data}}, receive A -> A end),
  meck:unload(sc_packets_generator),
  meck:unload(gen_udp).

session_progress_test(_Config) ->
  Data = #data{},
  ?assertEqual({next_state, cancelling, Data},
    sc_line_statem:session_progress(info, {udp, undef, undef, undef, <<"SIP/2.0 200 OK">>}, Data)),
  ?assertEqual({next_state, ringing, Data},
    sc_line_statem:session_progress(info, {udp, undef, undef, undef, <<"SIP/2.0 123 TEST">>}, Data)).

ringing_test(_Config) ->
  ?assertEqual({next_state, cancelling, #data{}},
    sc_line_statem:ringing(info, {udp, undef, undef, undef, message}, #data{})).

cancelling_test(_Config) ->
  meck:new(sc_packets_generator),
  meck:expect(sc_packets_generator, packet,
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
  {next_state, bye, {{Data, immediate}, new}} = sc_line_statem:cancelling(info, {udp, undef, undef, undef, message}, Data),
  ?assertEqual({ok, {terminated_ack, Number, message, Data}}, receive A -> A end),
  ?assertEqual({ok, {bye, Number, message, {Data, immediate}}}, receive A -> A end),
  meck:unload(sc_packets_generator),
  meck:unload(gen_udp).

bye_test(_Config) ->
  process_flag(trap_exit, true),
  meck:new(sc_line_statem, [passthrough]),
  meck:expect(sc_line_statem, init,
    fun(_Args) ->
      {ok, bye, #data{}}
    end),
  meck:expect(sc_line_statem, terminate,
    fun(_Reason, _StateName, _Data) ->
      ok
    end),
  {ok, Pid} = sc_line_statem:start_link(#{}),
  Pid ! {udp, undef, undef, undef, message},
  ?assertEqual({'EXIT', Pid, normal}, receive A -> A end),
  meck:unload(sc_line_statem).
