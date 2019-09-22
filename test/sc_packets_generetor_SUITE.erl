-module(sc_packets_generetor_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include("sip_session_data.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(DATA, #data{
  endpoint_ip     = {127, 0, 0, 1},
  endpoint_port   = 5060,
  from_tag   = <<"testfromtag">>,
  to_tag     = <<"testtotag">>,
  branch     = <<"testbranch">>,
  call_id    = <<"testcallid">>,
  real_name  = <<"Test Test">>,
  username   = "test",
  password   = "password",
  realm      = "example.com",
  c_sec      = 1,
  number     = "88000000000",
  post_invite = false
}).

-spec test() -> any().

-spec all() -> list().
all() ->
  [
    packet_register_test,
    packet_register_auth_test,
    packet_invite_test,
    packet_cancel_test,
    packet_ack_test,
    packet_terminated_ack_test,
    packet_invite_auth_test,
    packet_bye_test,
    get_csec_test,
    get_callid_test,
    get_from_tag_test,
    get_username_test,
    get_branch_test,
    get_port_test,
    get_host_test,
    get_realm_test
  ].

packet_register_test(_Config) ->
  {Packet, _} = sc_packets_generator:packet(register, ?DATA),
  ?assertEqual( <<"REGISTER sip:example.com SIP/2.0\r\n"
                  "Via: SIP/2.0/UDP 127.0.0.1:5060;rport;branch=testbranch\r\n"
                  "Max-Forwards: 70\r\n"
                  "From: \"Test Test\" <sip:test@example.com>;tag=testfromtag\r\n"
                  "To: \"Test Test\" <sip:test@example.com>\r\n"
                  "Call-ID: testcallid\r\n"
                  "CSeq: 1 REGISTER\r\n"
                  "Contact: \"Test Test\" <sip:test@127.0.0.1:5060;ob>\r\n"
                  "Expires: 300\r\n"
                  "Allow: PRACK, INVITE, ACK, BYE, CANCEL, UPDATE, INFO, SUBSCRIBE, NOTIFY, REFER, MESSAGE, OPTIONS\r\n"
                  "Content-Length:  0\r\n\r\n">>, Packet).

packet_register_auth_test(_Config) ->
  Msg = <<"SIP/2.0 401 Unauthorized\r\n"
          "Via: SIP/2.0/UDP 127.0.0.1:5060;rport=5060;branch=testbranch;received=127.0.0.1\r\n"
          "From: \"Test Test\" <sip:test@example.com>;tag=testfromtag\r\n"
          "To: \"Test Test\" <sip:test@example.com>;tag=testtotag\r\n"
          "Call-ID: testcallid\r\n"
          "CSeq: 1 REGISTER\r\n"
          "WWW-Authenticate: Digest realm=\"example.com\", nonce=\"testnonce\", qop=\"auth\"\r\n"
          "Server: Zadarma server\r\n"
          "Content-Length: 0\r\n\r\n">>,
  {Packet, NewState} = sc_packets_generator:packet(register_auth, binary_to_list(Msg), ?DATA),
  Nonce = <<"testnonce">>,
  {match, [CNonce]} = re:run(Packet, "(?<=cnonce=\")[^ ]+(?=\")", [{capture, first, list}]),
  Response = sc_utils:auth_response(?DATA#data.username, ?DATA#data.realm, ?DATA#data.password, "REGISTER", "sip:" ++ ?DATA#data.realm, Nonce, "00000001", CNonce, "auth"),
  ?assertEqual( <<"REGISTER sip:example.com SIP/2.0\r\n"
                  "Via: SIP/2.0/UDP 127.0.0.1:5060;rport;branch=testbranch\r\n"
                  "Max-Forwards: 70\r\n"
                  "From: \"Test Test\" <sip:test@example.com>;tag=testfromtag\r\n"
                  "To: \"Test Test\" <sip:test@example.com>\r\n"
                  "Call-ID: testcallid\r\n"
                  "CSeq: 1 REGISTER\r\n"
                  "Contact: \"Test Test\" <sip:test@127.0.0.1:5060;ob>\r\n"
                  "Expires: 300\r\n"
                  "Allow: PRACK, INVITE, ACK, BYE, CANCEL, UPDATE, INFO, SUBSCRIBE, NOTIFY, REFER, MESSAGE, OPTIONS\r\n"
                  "Authorization: Digest username=\"test\", realm=\"example.com\", nonce=\"testnonce\", uri=\"sip:example.com\", response=\"", (list_to_binary(Response))/binary, "\", cnonce=\"", (list_to_binary(CNonce))/binary, "\", qop=auth, nc=00000001\r\n"
                  "Content-Length:  0\r\n\r\n">>, Packet),
  ?assertEqual(?DATA#data.c_sec + 1, NewState#data.c_sec).

packet_invite_test(_Config) ->
  {Packet, _} = sc_packets_generator:packet(invite, list_to_binary(?DATA#data.number), ?DATA),
  ?assertEqual( <<"INVITE sip:88000000000@example.com SIP/2.0\r\n"
                  "Via: SIP/2.0/UDP 127.0.0.1:5060;rport;branch=testbranch\r\n"
                  "Max-Forwards: 70\r\n"
                  "From: \"Test Test\" <sip:test@example.com>;tag=testfromtag\r\n"
                  "To: <sip:88000000000@example.com>\r\n"
                  "Contact: \"Test Test\" <sip:test@127.0.0.1:5060;ob>\r\n"
                  "Call-ID: testcallid\r\n"
                  "CSeq: 1 INVITE\r\n"
                  "Allow: PRACK, INVITE, ACK, BYE, CANCEL, UPDATE, INFO, SUBSCRIBE, NOTIFY, REFER, MESSAGE, OPTIONS\r\n"
                  "Supported: replaces, 100rel, norefersub\r\n"
                  "Content-Type: application/sdp\r\n"
                  "Content-Length:  0\r\n\r\n">>, Packet).

packet_cancel_test(_Config) ->
  {Packet, _} = sc_packets_generator:packet(cancel, list_to_binary(?DATA#data.number), ?DATA),
  ?assertEqual( <<"CANCEL sip:88000000000@example.com SIP/2.0\r\n"
                  "Via: SIP/2.0/UDP 127.0.0.1:5060;rport;branch=testbranch\r\n"
                  "Max-Forwards: 70\r\n"
                  "From: \"Test Test\" <sip:test@example.com>;tag=testfromtag\r\n"
                  "To: <sip:88000000000@example.com>\r\n"
                  "Call-ID: testcallid\r\n"
                  "CSeq: 1 CANCEL\r\n"
                  "Content-Length:  0\r\n\r\n">>, Packet).

packet_ack_test(_Config) ->
  Msg = <<"SIP/2.0 407 Proxy Authentication Required\r\n"
          "Via: SIP/2.0/UDP 127.0.0.1:5060;rport;branch=testbranch\r\n"
          "From: \"Test Test\" <sip:test@example.com>;tag=testfromtag\r\n"
          "To: <sip:88000000000@example.com>;tag=testtotag\r\n"
          "Call-ID: testcallid\r\n"
          "CSeq: 1 INVITE\r\n"
          "Proxy-Authenticate: Digest realm=\"example.com\", nonce=\"testnonce\", qop=\"auth\"\r\n"
          "Server: Zadarma server\r\n"
          "Content-Length: 0\r\n\r\n">>,
  {Packet, _} = sc_packets_generator:packet(ack, list_to_binary(?DATA#data.number), Msg, ?DATA),
  ?assertEqual( <<"ACK sip:88000000000@example.com SIP/2.0\r\n"
                  "Via: SIP/2.0/UDP 127.0.0.1:5060;rport;branch=testbranch\r\n"
                  "Max-Forwards: 70\r\n"
                  "From: \"Test Test\" <sip:test@example.com>;tag=testfromtag\r\n"
                  "To: <sip:88000000000@example.com>;tag=testtotag\r\n"
                  "Call-ID: testcallid\r\n"
                  "CSeq: 0 ACK\r\n"
                  "Content-Length:  0\r\n\r\n">>, Packet).

packet_terminated_ack_test(_Config) ->
  Msg = <<"SIP/2.0 407 Proxy Authentication Required\r\n"
          "Via: SIP/2.0/UDP 127.0.0.1:5060;rport;branch=testbranch\r\n"
          "From: \"Test Test\" <sip:test@example.com>;tag=testfromtag\r\n"
          "To: <sip:88000000000@example.com>;tag=testtotag\r\n"
          "Call-ID: testcallid\r\n"
          "CSeq: 1 INVITE\r\n"
          "Proxy-Authenticate: Digest realm=\"example.com\", nonce=\"testnonce\", qop=\"auth\"\r\n"
          "Server: Zadarma server"
          "Content-Length: 0\r\n\r\n">>,
  {Packet, State} = sc_packets_generator:packet(terminated_ack, list_to_binary(?DATA#data.number), Msg, ?DATA),
  ?assertEqual( <<"ACK sip:88000000000@example.com SIP/2.0\r\n"
                  "Via: SIP/2.0/UDP 127.0.0.1:5060;rport;branch=testbranch\r\n"
                  "Max-Forwards: 70\r\n"
                  "From: \"Test Test\" <sip:test@example.com>;tag=testfromtag\r\n"
                  "To: <sip:88000000000@example.com>;tag=testtotag\r\n"
                  "Call-ID: testcallid\r\n"
                  "CSeq: 1 ACK\r\n"
                  "Content-Length:  0\r\n\r\n">>, Packet),
  ?assertEqual(?DATA, State).

packet_invite_auth_test(_Config) ->
  Msg = <<"SIP/2.0 407 Proxy Authentication Required\r\n"
          "Via: SIP/2.0/UDP 127.0.0.1:5060;rport;branch=testbranch\r\n"
          "From: \"Test Test\" <sip:test@example.com>;tag=testfromtag\r\n"
          "To: <sip:88000000000@example.com>;tag=testtotag\r\n"
          "Call-ID: testcallid\r\n"
          "CSeq: 1 INVITE\r\n"
          "Proxy-Authenticate: Digest realm=\"example.com\", nonce=\"testnonce\", qop=\"auth\"\r\n"
          "Server: Zadarma server"
          "Content-Length: 0\r\n\r\n">>,
  {Packet, NewState} = sc_packets_generator:packet(invite_auth, list_to_binary(?DATA#data.number), binary_to_list(Msg), ?DATA),
  Nonce = <<"testnonce">>,
  {match, [CNonce]} = re:run(Packet, "(?<=cnonce=\")[^ ]+(?=\")", [{capture, first, list}]),
  Response = sc_utils:auth_response(?DATA#data.username, ?DATA#data.realm, ?DATA#data.password, "INVITE", "sip:88000000000@" ++ ?DATA#data.realm, Nonce, "00000001", CNonce, "auth"),
  ?assertEqual( <<"INVITE sip:88000000000@example.com SIP/2.0\r\n"
                  "Via: SIP/2.0/UDP 127.0.0.1:5060;rport;branch=testbranch\r\n"
                  "Max-Forwards: 70\r\n"
                  "From: \"Test Test\" <sip:test@example.com>;tag=testfromtag\r\n"
                  "To: <sip:88000000000@example.com>\r\n"
                  "Contact: \"Test Test\" <sip:test@127.0.0.1:5060;ob>\r\n"
                  "Call-ID: testcallid\r\n"
                  "CSeq: 1 INVITE\r\n"
                  "Allow: PRACK, INVITE, ACK, BYE, CANCEL, UPDATE, INFO, SUBSCRIBE, NOTIFY, REFER, MESSAGE, OPTIONS\r\n"
                  "Supported: replaces, 100rel, norefersub\r\n"
                  "Proxy-Authorization: Digest username=\"test\", realm=\"example.com\", nonce=\"testnonce\", uri=\"sip:88000000000@example.com\", response=\"", (list_to_binary(Response))/binary, "\", cnonce=\"", (list_to_binary(CNonce))/binary, "\", qop=auth, nc=00000001\r\n"
                  "Content-Type: application/sdp\r\n"
                  "Content-Length:  0\r\n\r\n">>, Packet),
  ?assertEqual(?DATA, NewState).

packet_bye_test(_Config) ->
  Msg = <<"SIP/2.0 407 Proxy Authentication Required\r\n"
          "Via: SIP/2.0/UDP 127.0.0.1:5060;rport;branch=testbranch\r\n"
          "From: \"Test Test\" <sip:test@example.com>;tag=testfromtag\r\n"
          "To: <sip:88000000000@example.com>;tag=testtotag\r\n"
          "Call-ID: testcallid\r\n"
          "CSeq: 1 INVITE\r\n"
          "Proxy-Authenticate: Digest realm=\"example.com\", nonce=\"testnonce\", qop=\"auth\"\r\n"
          "Server: Zadarma server"
          "Content-Length: 0\r\n\r\n">>,
  {Packet, State} = sc_packets_generator:packet(bye, list_to_binary(?DATA#data.number), Msg, ?DATA),
  ?assertEqual( <<"BYE sip:88000000000@example.com SIP/2.0\r\n"
                  "Via: SIP/2.0/UDP 127.0.0.1:5060;rport;branch=testbranch\r\n"
                  "Max-Forwards: 70\r\n"
                  "From: \"Test Test\" <sip:test@example.com>;tag=testfromtag\r\n"
                  "To: <sip:88000000000@example.com>;tag=testtotag\r\n"
                  "Call-ID: testcallid\r\n"
                  "CSeq: 1 BYE\r\n"
                  "Route: <sip:example.com;lr;ftag=testfromtag;nat=yes>\r\n"
                  "Content-Length:  0\r\n\r\n">>, Packet),
                  ?assertEqual(?DATA, State).

get_csec_test(_Config) ->
  ?assertEqual(erlang:integer_to_binary(?DATA#data.c_sec),sc_packets_generator:get_cseq(?DATA)).

get_callid_test(_Config) ->
  ?assertEqual(?DATA#data.call_id, sc_packets_generator:get_callid(?DATA)).

get_from_tag_test(_Config) ->
  ?assertEqual(?DATA#data.from_tag, sc_packets_generator:get_from_tag(?DATA)).

get_username_test(_Config) ->
  ?assertEqual(list_to_binary(?DATA#data.username), sc_packets_generator:get_username(?DATA)).

get_branch_test(_Config) ->
  ?assertEqual(?DATA#data.branch, sc_packets_generator:get_branch(?DATA)).

get_port_test(_Config) ->
  ?assertEqual(integer_to_binary(?DATA#data.endpoint_port), sc_packets_generator:get_port(?DATA)).

get_host_test(_Config) ->
  ?assertEqual(<<"127.0.0.1">>, sc_packets_generator:get_host(?DATA)).

get_realm_test(_Config) ->
  ?assertEqual(list_to_binary(?DATA#data.realm), sc_packets_generator:get_realm(?DATA)).
