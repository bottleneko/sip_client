-module(sc_line).
-behaviour(gen_server).

%% API
-export([start_link/2,
         register/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_code_request/3,
         handle_ping/2
        ]).

-include("sip_session_data.hrl").
-include_lib("kernel/include/inet.hrl").

%%====================================================================
%% API
%%====================================================================

-spec start_link(Configuration, Number) -> {ok, pid()} when
    Configuration :: #{ip_addr   => inet:ip4_address(),
                       username  => binary(),
                       password  => binary(),
                       realm     => binary(),
                       period_ms => non_neg_integer()
                      },
    Number        :: binary().
start_link(Configuration, Number) ->
  ChildConfiguration =
    Configuration#{'@child_configuration' => #{number => Number}
                  },
  gen_server:start_link(?MODULE, [ChildConfiguration], []).

-spec register(pid()) -> ok.
register(Pid) ->
  gen_server:cast(Pid, register).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(Configuration) -> {ok, State} when
    Configuration :: #{ip_addr   => inet:ip4_address(),
                       username  => binary(),
                       password  => binary(),
                       realm     => binary(),
                       period_ms => non_neg_integer(),

                       '@child_configuration' =>
                         #{number   => binary()
                          }
                      },
    State         :: #data{}.
init(#{ip_addr   := IpAddr,
       username  := Username,
       password  := Password,
       realm     := Realm,
       period_ms := PeriodMs,

       '@child_configuration' :=
         #{number := Number
          }
      } = _Configuration) ->

  io:format("Call to ~p~n", [Number]),
  <<Rand:8/integer>> = crypto:strong_rand_bytes(1),
  Port = 59000 + Rand,
  {ok, Socket} = gen_udp:open(Port, [list, inet, {active, true}, {reuseaddr, true}, {ifaddr, IpAddr}]),
  gen_server:cast(self(), register),
  {ok, #data{socket        = Socket,
             endpoint_ip   = IpAddr,
             endpoint_port = Port,
             from_tag      = sc_utils:new_tag(),
             branch        = sc_utils:new_branch(),
             call_id       = sc_utils:new_callid(),
             username      = Username,
             password      = Password,
             realm         = Realm,
             period_ms     = PeriodMs,
             c_sec         = sc_utils:new_csec(),
             number        = Number,
             post_invite   = false
  }}.

-spec(handle_cast(Request :: term(), State :: #data{}) ->
  {noreply, NewState :: #data{}}).
handle_cast(register, #data{period_ms = PeriodMs} = State) ->
  {Msg, NewState} = sc_packets_generator:packet(register, State),
  timer:apply_after(PeriodMs, gen_udp, send, [State#data.socket, State#data.realm, 5060, Msg]),
  {noreply, NewState}.


-spec handle_call(Msg, From, State) -> {reply, ok, State} when
  Msg :: term(),
  From :: term(),
  State :: #data{}.
handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #data{}) ->
  {noreply, NewState :: #data{}}).
handle_info({udp, _, _, _, Msg}, State) ->
  RegExp = re:run(Msg, "(?<= )\\d{3}(?= )", [{capture, first, list}]),
  case RegExp of
    {match, [Code]} ->
      sc_line:handle_code_request(Code, Msg, State);
    nomatch ->
      io:format("~p~n~p~n", [?LINE, Msg]),
      sc_line:handle_ping(Msg, State)
  end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec handle_code_request(Code :: string(), Msg :: term(), State :: #data{}) -> {noreply, NewState :: #data{}}.
handle_code_request("100", _Msg, State) ->
  {noreply, State};
handle_code_request("180", _Msg, State = #data{number = Number}) ->
  {Reply, NewState} = sc_packets_generator:packet(cancel, Number, State),
  gen_udp:send(State#data.socket, State#data.realm, 5060, Reply),
  {noreply, NewState};
handle_code_request("183", _Msg, State) ->
  {noreply, State};
handle_code_request("200", Msg, State = #data{number = Number, post_invite = false}) ->
  {match, [Type]} = re:run(Msg, "(?<=\\d{3} )\\w+",[{capture, first, list}]),
  NewState = case Type of
               "OK" ->
                 {Reply, RetState} = sc_packets_generator:packet(invite, Number, State#data{call_id=sc_utils:new_callid(), post_invite = true}),
                 gen_udp:send(State#data.socket, State#data.realm, 5060, Reply),
                 RetState;
               _    ->
                 State
             end,
  {noreply, NewState};
handle_code_request("401", Msg, State) ->
  {Reply, NewState} = sc_packets_generator:packet(register_auth, Msg, State),
  gen_udp:send(State#data.socket, State#data.realm, 5060, Reply),
  {noreply, NewState};
handle_code_request("407", Msg, State = #data{number = Number}) ->
  {AckReply, IntermediateState} = sc_packets_generator:packet(ack, Number, Msg, State),
  gen_udp:send(State#data.socket, State#data.realm, 5060, AckReply),
  {InviteReply, NewState} = sc_packets_generator:packet(invite_auth, Number, Msg, IntermediateState),
  gen_udp:send(State#data.socket, State#data.realm, 5060, InviteReply),
  {noreply, NewState};
handle_code_request("487", Msg, State = #data{number = Number}) ->
  {Reply, NewState} = sc_packets_generator:packet(terminated_ack, Number, Msg, State),
  gen_udp:send(State#data.socket, State#data.realm, 5060, Reply),
  {stop, normal, NewState};
handle_code_request("503", _Msg, State) ->
  {stop, normal, State}.

-spec handle_ping(_Message :: term(), State :: #data{}) -> {noreply, NewState :: #data{}}.
handle_ping(_Message, State) ->
  {noreply, State}.
