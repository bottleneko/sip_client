-module(sc_packets_generator).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

-include("sip_session_data.hrl").

%% API
-export([
  packet/2,
  packet/3,
  packet/4
]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec get_cseq(Data) -> Result when
  Data  :: tuple(),
  Result :: binary().
get_cseq(Data) ->
  integer_to_binary(Data#data.c_sec).

-spec get_callid(Data) -> Result when
  Data  :: tuple(),
  Result :: binary().
get_callid(Data) ->
  Data#data.call_id.

-spec get_from_tag(Data) -> Result when
  Data  :: tuple(),
  Result :: binary().
get_from_tag(Data) ->
  Data#data.from_tag.

-spec get_username(Data) -> Result when
  Data  :: tuple(),
  Result :: binary().
get_username(Data) ->
  list_to_binary(Data#data.username).

-spec get_branch(Data) -> Result when
  Data  :: tuple(),
  Result :: binary().
get_branch(Data) ->
  Data#data.branch.

-spec get_port(Data) -> Result when
  Data  :: tuple(),
  Result :: binary().
get_port(Data) ->
  integer_to_binary(Data#data.endpoint_port).

-spec get_host(Data) -> Result when
  Data  :: tuple(),
  Result :: binary().
get_host(Data) ->
  list_to_binary(lists:join(".", lists:map(fun erlang:integer_to_list/1, tuple_to_list(Data#data.endpoint_ip)))).

-spec get_realm(Data) -> Result when
  Data  :: tuple(),
  Result :: binary().
get_realm(Data) ->
  list_to_binary(Data#data.realm).

-spec get_real_name(Data) -> Result when
  Data  :: tuple(),
  Result :: binary().
get_real_name(Data) ->
  Data#data.real_name.


%%====================================================================
%% API
%%====================================================================

-spec packet(Type, Data :: #data{}) ->
  {Packet :: binary(), NewData :: #data{}} when
  Type :: register.
packet(register, Data) ->
  Packet =  <<"REGISTER sip:", (get_realm(Data))/binary," SIP/2.0\r\n"
              "Via: SIP/2.0/UDP ", (get_host(Data))/binary, ":", (get_port(Data))/binary,";rport;branch=", (get_branch(Data))/binary,"\r\n"
              "Max-Forwards: 70\r\n"
              "From: \"", (get_real_name(Data))/binary, "\" <sip:", (get_username(Data))/binary, "@", (get_realm(Data))/binary,">;tag=", (get_from_tag(Data))/binary,"\r\n"
              "To: \"", (get_real_name(Data))/binary, "\" <sip:", (get_username(Data))/binary, "@", (get_realm(Data))/binary,">\r\n"
              "Call-ID: ", (get_callid(Data))/binary,"\r\n"
              "CSeq: ", (get_cseq(Data))/binary," REGISTER\r\n"
              "Contact: \"", (get_real_name(Data))/binary, "\" <sip:", (get_username(Data))/binary, "@", (get_host(Data))/binary,":", (get_port(Data))/binary,";ob>\r\n"
              "Expires: 300\r\n"
              "Allow: PRACK, INVITE, ACK, BYE, CANCEL, UPDATE, INFO, SUBSCRIBE, NOTIFY, REFER, MESSAGE, OPTIONS\r\n"
              "Content-Length:  0\r\n\r\n">>,
  {Packet, Data}.


-spec packet(Type, Msg :: binary(), Data :: #data{}) ->
  {Packet :: binary(), NewData :: #data{}} when
  Type :: register_auth | invite | cancel.
packet(register_auth, Msg, Data) ->
  Tokens = sc_utils:parse_unauthorized(list_to_binary(Msg)),
  #{nonce := Nonce} = Tokens,
  CNonce = sc_utils:new_cnonce(),
  Response = sc_utils:auth_response(Data#data.username, Data#data.realm, Data#data.password, "REGISTER", "sip:" ++ Data#data.realm, Nonce, "00000001", binary_to_list(CNonce), "auth"),
  Packet =  <<"REGISTER sip:", (get_realm(Data))/binary, " SIP/2.0\r\n"
              "Via: SIP/2.0/UDP ", (get_host(Data))/binary, ":", (get_port(Data))/binary,";rport;branch=", (get_branch(Data))/binary,"\r\n"
              "Max-Forwards: 70\r\n"
              "From: \"", (get_real_name(Data))/binary, "\" <sip:", (get_username(Data))/binary, "@", (get_realm(Data))/binary,">;tag=", (get_from_tag(Data))/binary,"\r\n"
              "To: \"", (get_real_name(Data))/binary, "\" <sip:", (get_username(Data))/binary, "@", (get_realm(Data))/binary,">\r\n"
              "Call-ID: ", (get_callid(Data))/binary,"\r\n"
              "CSeq: ", (get_cseq(Data))/binary," REGISTER\r\n"
              "Contact: \"", (get_real_name(Data))/binary, "\" <sip:", (get_username(Data))/binary, "@", (get_host(Data))/binary,":", (get_port(Data))/binary,";ob>\r\n"
              "Expires: 300\r\n"
              "Allow: PRACK, INVITE, ACK, BYE, CANCEL, UPDATE, INFO, SUBSCRIBE, NOTIFY, REFER, MESSAGE, OPTIONS\r\n"
              "Authorization: Digest username=\"", (get_username(Data))/binary, "\", realm=\"", (get_realm(Data))/binary,"\", nonce=\"", (list_to_binary(Nonce))/binary ,"\", uri=\"sip:", (get_realm(Data))/binary,"\", response=\"", (list_to_binary(Response))/binary, "\", cnonce=\"", (CNonce)/binary, "\", qop=auth, nc=00000001\r\n"
              "Content-Length:  0\r\n\r\n">>,
  {Packet, Data#data{c_sec = Data#data.c_sec + 1}};
packet(invite, Number, Data) ->
  Packet =  <<"INVITE sip:", Number/binary,"@", (get_realm(Data))/binary, " SIP/2.0\r\n"
              "Via: SIP/2.0/UDP ",(get_host(Data))/binary,":", (get_port(Data))/binary,";rport;branch=", (get_branch(Data))/binary,"\r\n"
              "Max-Forwards: 70\r\n"
              "From: \"", (get_real_name(Data))/binary, "\" <sip:", (get_username(Data))/binary, "@", (get_realm(Data))/binary,">;tag=", (get_from_tag(Data))/binary,"\r\n"
              "To: <sip:", Number/binary,"@", (get_realm(Data))/binary, ">\r\n"
              "Contact: \"", (get_real_name(Data))/binary, "\" <sip:", (get_username(Data))/binary, "@", (get_host(Data))/binary,":", (get_port(Data))/binary,";ob>\r\n"
              "Call-ID: ", (get_callid(Data))/binary,"\r\n"
              "CSeq: ", (get_cseq(Data))/binary," INVITE\r\n"
              "Allow: PRACK, INVITE, ACK, BYE, CANCEL, UPDATE, INFO, SUBSCRIBE, NOTIFY, REFER, MESSAGE, OPTIONS\r\n"
              "Supported: replaces, 100rel, norefersub\r\n"
              "Content-Type: application/sdp\r\n"
              "Content-Length:  0\r\n\r\n">>,
  {Packet, Data#data{c_sec = Data#data.c_sec + 1}};
packet(cancel, Number, Data) ->
  Packet =  <<"CANCEL sip:", Number/binary,"@", (get_realm(Data))/binary, " SIP/2.0\r\n"
              "Via: SIP/2.0/UDP ",(get_host(Data))/binary,":", (get_port(Data))/binary,";rport;branch=", (get_branch(Data))/binary,"\r\n"
              "Max-Forwards: 70\r\n"
              "From: \"", (get_real_name(Data))/binary, "\" <sip:", (get_username(Data))/binary, "@", (get_realm(Data))/binary,">;tag=", (get_from_tag(Data))/binary,"\r\n"
              "To: <sip:", Number/binary,"@", (get_realm(Data))/binary, ">\r\n"
              "Call-ID: ", (get_callid(Data))/binary,"\r\n"
              "CSeq: ", (get_cseq(Data))/binary," CANCEL\r\n"
              "Content-Length:  0\r\n\r\n">>,
  {Packet, Data}.



-spec packet(Type, Number :: binary(), Msg :: binary(), Data :: #data{}) ->
  {Packet :: binary(), NewData :: #data{}} when
  Type :: ack | terminated_ack | invite_ack | invite_auth.
packet(ack, Number, Msg, Data) ->
  ToTag = list_to_binary(sc_utils:parse_to_tag(Msg)),
  Packet =  <<"ACK sip:", Number/binary,"@", (get_realm(Data))/binary, " SIP/2.0\r\n"
              "Via: SIP/2.0/UDP ", (get_host(Data))/binary,":", (get_port(Data))/binary,";rport;branch=", (get_branch(Data))/binary,"\r\n"
              "Max-Forwards: 70\r\n"
              "From: \"", (get_real_name(Data))/binary, "\" <sip:", (get_username(Data))/binary, "@", (get_realm(Data))/binary,">;tag=", (get_from_tag(Data))/binary,"\r\n"
              "To: <sip:", Number/binary,"@", (get_realm(Data))/binary, ">;tag=", ToTag/binary,"\r\n"
              "Call-ID: ", (get_callid(Data))/binary,"\r\n"
              "CSeq: ", (integer_to_binary(binary_to_integer(get_cseq(Data)) - 1))/binary," ACK\r\n"
              "Content-Length:  0\r\n\r\n">>,
  {Packet, Data};
packet(terminated_ack, Number, Msg, Data) ->
  ToTag = list_to_binary(sc_utils:parse_to_tag(Msg)),
  Packet =  <<"ACK sip:", Number/binary,"@", (get_realm(Data))/binary, " SIP/2.0\r\n"
              "Via: SIP/2.0/UDP ", (get_host(Data))/binary,":", (get_port(Data))/binary,";rport;branch=", (get_branch(Data))/binary,"\r\n"
              "Max-Forwards: 70\r\n"
              "From: \"", (get_real_name(Data))/binary, "\" <sip:", (get_username(Data))/binary, "@", (get_realm(Data))/binary,">;tag=", (get_from_tag(Data))/binary,"\r\n"
              "To: <sip:", Number/binary,"@", (get_realm(Data))/binary, ">;tag=", ToTag/binary,"\r\n"
              "Call-ID: ", (get_callid(Data))/binary,"\r\n"
              "CSeq: ", (integer_to_binary(binary_to_integer(get_cseq(Data))))/binary," ACK\r\n"
              "Content-Length:  0\r\n\r\n">>,
  {Packet, Data};
packet(invite_auth, Number, Msg, Data) ->
  Tokens = sc_utils:parse_unauthorized(list_to_binary(Msg)),
  #{nonce := Nonce} = Tokens,
  CNonce = sc_utils:new_cnonce(),
  Response = sc_utils:auth_response(Data#data.username, Data#data.realm, Data#data.password, "INVITE", "sip:" ++ binary_to_list(Number) ++ "@" ++ Data#data.realm, Nonce, "00000001", binary_to_list(CNonce), "auth"),
  Packet =  <<"INVITE sip:", Number/binary,"@", (get_realm(Data))/binary, " SIP/2.0\r\n"
              "Via: SIP/2.0/UDP ", (get_host(Data))/binary,":", (get_port(Data))/binary,";rport;branch=", (get_branch(Data))/binary,"\r\n"
              "Max-Forwards: 70\r\n"
              "From: \"", (get_real_name(Data))/binary, "\" <sip:", (get_username(Data))/binary, "@", (get_realm(Data))/binary,">;tag=", (get_from_tag(Data))/binary,"\r\n"
              "To: <sip:", Number/binary,"@", (get_realm(Data))/binary, ">\r\n"
              "Contact: \"", (get_real_name(Data))/binary, "\" <sip:", (get_username(Data))/binary, "@", (get_host(Data))/binary,":", (get_port(Data))/binary,";ob>\r\n"
              "Call-ID: ", (get_callid(Data))/binary,"\r\n"
              "CSeq: ", (get_cseq(Data))/binary," INVITE\r\n"
              "Allow: PRACK, INVITE, ACK, BYE, CANCEL, UPDATE, INFO, SUBSCRIBE, NOTIFY, REFER, MESSAGE, OPTIONS\r\n"
              "Supported: replaces, 100rel, norefersub\r\n"
              "Proxy-Authorization: Digest username=\"", (get_username(Data))/binary, "\", realm=\"", (get_realm(Data))/binary,"\", nonce=\"", (list_to_binary(Nonce))/binary ,"\", uri=\"sip:", Number/binary,"@", (get_realm(Data))/binary,"\", response=\"", (list_to_binary(Response))/binary, "\", cnonce=\"", (CNonce)/binary, "\", qop=auth, nc=00000001\r\n"
              "Content-Type: application/sdp\r\n"
              "Content-Length:  0\r\n\r\n">>,
  {Packet, Data};
packet(bye, Number, Msg, Data) ->
  ToTag = list_to_binary(sc_utils:parse_to_tag(Msg)),
  Packet =
    <<"BYE sip:", Number/binary,"@", (get_realm(Data))/binary, " SIP/2.0\r\n"
      "Via: SIP/2.0/UDP ", (get_host(Data))/binary,":", (get_port(Data))/binary,";rport;branch=", (get_branch(Data))/binary,"\r\n"
      "Max-Forwards: 70\r\n"
      "From: \"", (get_real_name(Data))/binary, "\" <sip:", (get_username(Data))/binary, "@", (get_realm(Data))/binary,">;tag=", (get_from_tag(Data))/binary,"\r\n"
      "To: <sip:", Number/binary,"@", (get_realm(Data))/binary, ">;tag=", ToTag/binary,"\r\n"
      "Call-ID: ", (get_callid(Data))/binary,"\r\n"
      "CSeq: ", (get_cseq(Data))/binary," BYE\r\n"
      "Route: <sip:", (get_realm(Data))/binary,";lr;ftag=", (get_from_tag(Data))/binary,";nat=yes>\r\n"
      "Content-Length:  0\r\n\r\n">>,
  {Packet, Data}.
