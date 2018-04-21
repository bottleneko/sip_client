-module(sip_phone_statem).
-behaviour(gen_statem).

-compile(nowarn_missing_spec).

%% API
-export([start_link/1, register_line/1, match_code/1]).

%% gen_statem callbacks
-export([
  init/1,
  format_status/2,
  handle_event/4,
  terminate/3,
  callback_mode/0
]).

%% gen_statem states
-export([
  idle/3,
  unauthorized/3,
  proxy_unauthenticated/3,
  trying/3,
  session_progress/3,
  ringing/3,
  cancelling/3,
  bye/3
]).

-define(SERVER, ?MODULE).

-include("sip_session_data.hrl").

%%====================================================================
%% API
%%====================================================================

-spec start_link(Args :: maps:new()) -> {ok, pid()} | {error, term()} | ignore.
start_link(Args) ->
  gen_statem:start_link(?MODULE, [Args], []).

-spec register_line(Pid :: pid()) -> ok.
register_line(Pid) ->
  gen_statem:cast(Pid, register),
  ok.

%%====================================================================
%% gen_statem callbacks
%%====================================================================

-spec init([map()]) -> {ok, State :: atom(), Data :: #data{}}.
init([#{endpoint_ip   := InterfaceAddr,
        endpoint_port := EndpointPort,
        host_port     := HostPort,
        real_name     := RealName,
        username      := Username,
        password      := Password,
        realm         := Realm,
        number        := Number,
        delay         := Delay
  }]) ->
  {ok, Socket} = gen_udp:open(EndpointPort, [list, inet, {active, true}, {reuseaddr, true}, {ifaddr, InterfaceAddr}]),
  sip_phone_statem:register_line(self()),
  {ok, unauthorized, #data{
    socket        = Socket,
    endpoint_ip   = InterfaceAddr,
    endpoint_port = EndpointPort,
    host_port     = HostPort,
    from_tag      = sip_utils:new_tag(),
    branch        = sip_utils:new_branch(),
    call_id       = sip_utils:new_callid(),
    real_name     = RealName,
    username      = Username,
    password      = Password,
    realm         = Realm,
    c_sec         = sip_utils:new_csec(),
    number        = Number,
    delay         = Delay
  }}.

-spec format_status(Opt :: atom(), list()) -> Status :: atom().
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

-spec handle_event(_EventType :: atom(), _EventContent :: term(), _StateName :: atom(), Data :: #data{}) ->
  {next_state, NexStateName :: atom(), Data :: #data{}}.
handle_event(_EventType, _EventContent, _StateName, Data) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, Data}.

-spec terminate(_Reason :: term(), _StateName :: atom(), Data :: #data{}) -> any().
terminate(_Reason, _StateName, #data{socket = Socket}) ->
  gen_udp:close(Socket).

-spec callback_mode() -> state_functions.
callback_mode() ->
  state_functions.

%%====================================================================
%% States
%%====================================================================

-spec unauthorized(info | cast, {udp, _, _, _, _Recv :: binary()} | register, Data :: #data{}) ->
  {next_state, idle | unauthorized, Data :: #data{}}.
unauthorized(cast, register, Data) ->
  {Msg, NewData} = sip_packets_generator:packet(register, Data),
  timer:apply_after(Data#data.delay*1000, gen_udp, send, [Data#data.socket, Data#data.realm, Data#data.host_port, Msg]),
  {next_state, unauthorized, NewData};
unauthorized(info, {udp, _, _, _, Recv}, Data) ->
  {Msg, NewData} = sip_packets_generator:packet(register_auth, Recv, Data),
  gen_udp:send(Data#data.socket, Data#data.realm, Data#data.host_port, Msg),
  {next_state, idle, NewData}.

-spec idle(info, {udp, _, _, _, _Recv :: binary()}, Data :: #data{}) ->
  {next_state, proxy_unauthenticated, Data :: #data{}}.
idle(info, {udp, _, _, _, _}, Data) ->
  {Msg, NewData} = sip_packets_generator:packet(invite, Data#data.number, Data),
  gen_udp:send(Data#data.socket, Data#data.realm, Data#data.host_port, Msg),
  {next_state, proxy_unauthenticated, NewData}.

-spec proxy_unauthenticated(info, {udp, _, _, _, _Recv :: binary()}, Data :: #data{}) ->
  {next_state, trying, Data :: #data{}}.
proxy_unauthenticated(info, {udp, _, _, _, Recv}, Data) ->
  {AckReply, IntermediateData} = sip_packets_generator:packet(ack, Data#data.number, Recv, Data),
  gen_udp:send(Data#data.socket, Data#data.realm, Data#data.host_port, AckReply),
  {InviteReply, NewData} = sip_packets_generator:packet(invite_auth, Data#data.number, Recv, IntermediateData),
  gen_udp:send(Data#data.socket, Data#data.realm, Data#data.host_port, InviteReply),
  {next_state, trying, NewData}.

-spec trying(info, {udp, _, _, _, _Recv :: binary()}, Data :: #data{}) ->
  {next_state, session_progress, Data :: #data{}}.
trying(info, {udp, _, _, _, _Recv}, Data) ->
  {Reply, NewData} = sip_packets_generator:packet(cancel, Data#data.number, Data),
  gen_udp:send(Data#data.socket, Data#data.realm, Data#data.host_port, Reply),
  {next_state, session_progress, NewData}.

-spec session_progress(info, {udp, _, _, _, _Recv :: binary()}, Data :: #data{}) ->
  {next_state, cancelling | ringing, NewData :: #data{}}.
session_progress(info, {udp, _, _, _, Recv}, Data) ->
  case match_code(Recv) of
    "200" ->
      {next_state, cancelling, Data};
    _     ->
      {next_state, ringing, Data}
  end.

-spec ringing(info, {udp, _, _, _, _Recv :: binary()}, Data :: #data{}) ->
  {next_state, idle, Data :: #data{}}.
ringing(info, {udp, _, _, _, _Recv}, Data) ->
  {next_state, cancelling, Data}.

-spec cancelling(info, {udp, _, _, _, _Recv :: binary()}, _Data :: #data{}) ->
  {next_state, bye, NewData :: #data{}}.
cancelling(info, {udp, _, _, _, Recv}, Data) ->
  {AckReply, IntermediateData} = sip_packets_generator:packet(terminated_ack, Data#data.number, Recv, Data),
  gen_udp:send(Data#data.socket, Data#data.realm, Data#data.host_port, AckReply),
  {InviteReply, NewData} = sip_packets_generator:packet(bye, Data#data.number, Recv, IntermediateData),
  gen_udp:send(Data#data.socket, Data#data.realm, Data#data.host_port, InviteReply),
  {next_state, bye, NewData}.

-spec bye(info, {udp, _, _, _, _Recv :: binary()}, _Data :: #data{}) -> ok.
bye(info, {udp, _, _, _, _Recv}, _Data) ->
  {stop, normal}.

-spec match_code(Msg :: binary()) -> Code :: string() | ok.
match_code(Msg) ->
  RegExp = re:run(Msg, "(?<= )\\d{3}(?= )", [{capture, first, list}]),
  case RegExp of
    {match, [Code]} ->
      Code;
    nomatch ->
      error({"Error matching code in message", Msg})
  end.
