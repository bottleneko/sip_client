-module(sip_utils).

%% API
-export([
  auth_response/9,
  parse_unauthorized/1,
  new_branch/0,
  new_tag/0,
  new_csec/0,
  new_callid/0,
  new_cnonce/0,
  parse_to_tag/1]).

%% Internal
-export([
  ha1/3,
  ha2/2,
  hex0/1,
  hex/1]).

%%====================================================================
%% API
%%====================================================================

-spec new_cnonce() -> CNonce when
  CNonce :: binary().
new_cnonce() ->
  <<Tail:34/binary, _/binary>> = base64:encode(crypto:strong_rand_bytes(34)),
  binary:replace(Tail, [<<"/">>, <<"+">>], <<"A">>, [global]).

-spec new_callid() -> CallId when
  CallId :: binary().
new_callid() ->
  <<Tail:32/binary, _/binary>> = base64:encode(crypto:strong_rand_bytes(32)),
  binary:replace(Tail, [<<"/">>, <<"+">>], <<"A">>, [global]).

-spec new_branch() -> Branch when
  Branch :: binary().
new_branch() ->
  <<Tail:34/binary, _/binary>> = base64:encode(crypto:strong_rand_bytes(34)),
  NewTail = binary:replace(Tail, [<<"/">>, <<"+">>], <<"A">>, [global]),
  <<"z9hG4bK", NewTail/binary>>.

-spec new_tag() -> Tag when
  Tag :: binary().
new_tag() ->
  <<Tail:32/binary, _/binary>> = base64:encode(crypto:strong_rand_bytes(32)),
  binary:replace(Tail, [<<"/">>, <<"+">>], <<"A">>, [global]).

-spec new_csec() -> CSec when
  CSec :: integer().
new_csec() ->
  <<A:8, B:8>> = crypto:strong_rand_bytes(2),
  A*1000 + B.

-spec auth_response(Username, Realm, Password, Method, Route, Nonce, NonceCounter, CNonce, Qop) -> Response when
  Username      :: string(),
  Realm         :: string(),
  Password      :: string(),
  Method        :: string(),
  Route         :: string(),
  Nonce         :: string(),
  NonceCounter  :: string(),
  CNonce        :: string(),
  Qop           :: string(),
  Response      :: string().
auth_response(Username, Realm, Password, Method, Route, Nonce, NonceCounter, CNonce, Qop) ->
  HA1 = ha1(Username, Realm, Password),
  HA2 = ha2(Method, Route),
  hex(binary_to_list(erlang:md5(lists:flatten(lists:join(":", [HA1, Nonce, NonceCounter, CNonce, Qop, HA2]))))).

-spec ha1(Username, Realm, Password) -> Hash when
  Username :: string(),
  Realm    :: string(),
  Password :: string(),
  Hash     :: string().
ha1(Username, Realm, Password) ->
  hex(binary_to_list(erlang:md5(Username ++ [$:] ++ Realm ++ [$:] ++ Password))).

-spec ha2(Method, Route) -> Hash when
  Method   :: string(),
  Route    :: string(),
  Hash     :: string().
ha2(Method, Route) ->
  hex(binary_to_list(erlang:md5(Method ++ [$:] ++ Route))).

-spec parse_unauthorized(Message) -> Parsed when
  Message :: binary(),
  Parsed  :: map().
parse_unauthorized(Message) ->
  Trimmed = string:trim(Message),
  Tokens = string:split(Trimmed, "\r\n", all),
  Authorization = lists:nth(7, Tokens),
  {match, [[Realm], [Nonce], [Qop]]} = re:run(Authorization, "(?<=\")[^ ]+(?=\")", [{capture, all, list}, global]),
  #{realm => Realm, nonce => Nonce, qop => Qop}.

-spec parse_to_tag(Message) -> Parsed when
  Message :: binary(),
  Parsed  :: string().
parse_to_tag(Message) ->
  Trimmed = string:trim(Message),
  Tokens = string:split(Trimmed, "\r\n", all),
  To = lists:nth(4, Tokens),
  {match, [ToTag]} = re:run(To, "(?<=(tag=)).*", [{capture, first, list}]),
  ToTag.

%%====================================================================
%% Internal functions
%%====================================================================

-spec hex(L) -> Ret when
  L :: string() | byte(),
  Ret :: string().
hex(L) when is_list(L) ->
  lists:flatten([hex(I) || I <- L]);
hex(I) when I > 16#f ->
  [hex0((I band 16#f0) bsr 4), hex0(I band 16#0f)];
hex(I) -> [$0, hex0(I)].

-spec hex0(Num) -> Symbol when
  Num :: integer(),
  Symbol :: char().
hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I) ->  $0 + I.
