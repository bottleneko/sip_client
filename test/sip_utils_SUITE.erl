-module(sip_utils_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

-define(MESSAGE, <<"SIP/2.0 401 Unauthorized\r\n
                    Via: SIP/2.0/UDP 192.168.0.100:58641;rport=58641;branch=z9hG4bKPj7KhMDgiVwNPDp5Ukwff2liHXP5UK.0YS;received=192.168.0.100\r\n
                    From: \"Test\" <sip:100000@example.com>;tag=v2hQOMuS7QdKmpmrDnZGMnkXmE0BCsmB\r\n
                    To: \"Test\" <sip:100000@example.com>;tag=5149c1126e702672db415b970eae4141.39b5\r\n
                    Call-ID: sw333efFH2Vz5LGlb0Xc0seqffx0kAU.\r\n
                    CSeq: 8499 REGISTER\r\n
                    WWW-Authenticate: Digest realm=\"example.com\", nonce=\"Wn6kqVp+o33imPKOmTRBtzQuwrcMxi1N\", qop=\"auth\"\r\n
                    Server: Zadarma server\r\n
                    Content-Length: 0\r\n\r\n">>).


-spec test() -> any().

-spec all() -> list().
all() ->
  [
    hex_test,
    parse_to_tag_test,
    parse_unauthorized_test,
    new_cnonce_test,
    unique_new_cnonce_test,
    new_callid_test,
    unique_new_callid_test,
    new_branch_test,
    unique_new_branch_test,
    new_tag_test,
    unique_new_tag_test,
    new_csec_test,
    unique_new_csec_test,
    ha1_test,
    ha2_test,
    auth_response_test
  ].

hex_test(_Config) ->
  MD5 = sip_utils:hex(erlang:binary_to_list(erlang:md5("The quick brown fox jumped over the lazy dog's back"))),
  ?assertEqual("e38ca1d920c4b8b8d3946b2c72f01680", MD5).

parse_to_tag_test(_Config) ->
  Tag = sip_utils:parse_to_tag(?MESSAGE),
  ?assertEqual("5149c1126e702672db415b970eae4141.39b5", Tag).

parse_unauthorized_test(_Config) ->
  Unauthorized = sip_utils:parse_unauthorized(?MESSAGE),
  ?assertEqual(#{nonce => "Wn6kqVp+o33imPKOmTRBtzQuwrcMxi1N",
                 qop => "auth",
                 realm => "example.com"},
               Unauthorized).

new_cnonce_test(_Config) ->
  CNonce = sip_utils:new_cnonce(),
  ?assertEqual(34, erlang:byte_size(CNonce)),
  ?assert(binary:matches(CNonce,[<<"/">>, <<"+">>],[]) =:= []).

unique_new_cnonce_test(_Config) ->
  ?assertNotEqual(sip_utils:new_cnonce(), sip_utils:new_cnonce()).

new_callid_test(_Config) ->
  CallID = sip_utils:new_callid(),
  ?assertEqual(32, erlang:byte_size(CallID)),
  ?assert(binary:matches(CallID,[<<"/">>, <<"+">>],[]) =:= []).

unique_new_callid_test(_Config) ->
  ?assertNotEqual(sip_utils:new_callid(), sip_utils:new_callid()).

new_branch_test(_Config) ->
  <<BranchHead:7/binary, BranchTail/binary>> = sip_utils:new_branch(),
  ?assertEqual(<<"z9hG4bK">>, BranchHead),
  ?assertEqual(34, erlang:byte_size(BranchTail)),
  ?assert(binary:matches(BranchTail,[<<"/">>, <<"+">>],[]) =:= []).

unique_new_branch_test(_Config) ->
  ?assertNotEqual(sip_utils:new_branch(), sip_utils:new_branch()).

new_tag_test(_Config) ->
  Tag = sip_utils:new_tag(),
  ?assertEqual(32, erlang:byte_size(Tag)),
  ?assert(binary:matches(Tag,[<<"/">>, <<"+">>],[]) =:= []).

unique_new_tag_test(_Config) ->
  ?assertNotEqual(sip_utils:new_tag(), sip_utils:new_tag()).

%% RFC 3261
new_csec_test(_Config) ->
  CSec = sip_utils:new_csec(),
  ?assert(CSec > 0),
  ?assert(CSec < math:pow(2,31)).

unique_new_csec_test(_Config) ->
  ?assertNotEqual(sip_utils:new_csec(), sip_utils:new_csec()).


auth_response_test(_Config) ->
  Username = "Test",
  Realm = "example.com",
  Password = "test",
  Method = "TEST",
  Route = "Example",
  Nonce = "1",
  NonceCounter = "1",
  CNonce = "1",
  Qop = "1",
  HA1 = sip_utils:ha1(Username, Realm, Password),
  HA2 = sip_utils:ha2(Method, Route),
  ?assertEqual(md5(lists:flatten(lists:join(":", [HA1, Nonce, NonceCounter, CNonce, Qop, HA2]))),
    sip_utils:auth_response(Username, Realm, Password, Method, Route, Nonce, NonceCounter, CNonce, Qop)).

ha1_test(_Config) ->
  Username = "Test",
  Realm = "example.com",
  Password = "test",
  ?assertEqual(md5(Username ++ [$:] ++ Realm ++ [$:] ++ Password), sip_utils:ha1(Username, Realm, Password)).

ha2_test(_Config) ->
  Method = "TEST",
  Route = "Example",
  ?assertEqual(md5(Method ++ [$:] ++ Route), sip_utils:ha2(Method, Route)).

md5(S) ->
  string:to_lower(
    lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= erlang:md5(S)])
  ).