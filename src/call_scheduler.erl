-module(call_scheduler).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  start_pool/0
]).

%% Internal functions
-export([
  fill_ets_from_file/2,
  start_call/2
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2]).

-define(SERVER, ?MODULE).

-include("call_scheduler.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, Pid :: pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_pool() -> ok.
start_pool() ->
  {ok, PoolSize} = application:get_env(sip_client, pool_size),
  gen_server:cast(?MODULE, {start_pool, PoolSize}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: list()) ->
  {ok, State :: #state{}}).
init([]) ->
  erlang:process_flag(trap_exit, true),
  %% Prepare source
  {ok, PathToSource} = application:get_env(sip_client, source_path),
  Tid = ets:new(phone_numbers, [private]),
  call_scheduler:fill_ets_from_file(PathToSource, Tid),
  ets:safe_fixtable(Tid, true),
  %% Completed file
  {ok, PathToCompleted} = application:get_env(sip_client, completed_path),
  {ok, Period} = application:get_env(sip_client, period),
  {ok, CompletedHandler} = file:open(PathToCompleted, [write]),
  {ok, #state{ets_tid = Tid,
              completed_handler = CompletedHandler,
              period = Period}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}}).
handle_cast({start_pool, N}, State) ->
  NewState = call_scheduler:start_call(N, State),
  {noreply, NewState};
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec start_call(N, State) -> NewState when
  N :: non_neg_integer(),
  State :: #state{},
  NewState :: #state{}.
start_call(0, State) ->
  State;
start_call(N, State = #state{pool = Pool, ets_tid = Tid, last_sended = undef}) ->
  case ets:first(Tid) of
    '$end_of_table' ->
      start_call(0, State#state{last_sended = '$end_of_table'});
    Number ->
      {ok, Pid} = supervisor:start_child(sip_line, [list_to_binary(Number)]),
      Ref = erlang:monitor(process, Pid),
      start_call(N -1, State#state{pool = [{Ref, Number}|Pool], last_sended = Number})
  end;
start_call(N, State = #state{pool = Pool, ets_tid = Tid, last_sended = Last}) ->
  case ets:next(Tid, Last) of
    '$end_of_table' ->
      start_call(0, State#state{last_sended = '$end_of_table'});
    Number ->
      {ok, Pid} = supervisor:start_child(sip_line, [list_to_binary(Number)]),
      Ref = erlang:monitor(process, Pid),
      start_call(N-1, State#state{pool = [{Ref, Number}|Pool], last_sended = Number})
  end.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}}).
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State = #state{last_sended = '$end_of_table'}) ->
  case lists:keytake(Ref, 1, State#state.pool) of
    {value, {Ref, Number}, NewPool} ->
      io:fwrite(State#state.completed_handler, "~s\r\n", [Number]);
    false ->
      NewPool = State#state.pool
  end,
  {noreply, State#state{pool = NewPool}};
handle_info({'DOWN', Ref, process, _Pid, _Reason},  State) ->
  case lists:keytake(Ref, 1, State#state.pool) of
    {value, {Ref, Number}, NewPool} ->
      io:fwrite(State#state.completed_handler, "~s\r\n", [Number]);
    false ->
      NewPool = State#state.pool
  end,
  %%NewState = timer:apply_after(Period*1000, ?MODULE,  start_call, [1, State#state{pool = NewPool}]),
  NewState = call_scheduler:start_call(1, State#state{pool = NewPool}),
  {noreply, NewState};
handle_info(_Req, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, #state{completed_handler = CompletedHandler}) ->
    file:close(CompletedHandler).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec fill_ets_from_file(PathToFile, Tid) -> Ret when
  PathToFile :: string() | file:io_device(),
  Tid :: ets:tab(),
  Ret :: atom().
fill_ets_from_file(PathToFile, Tid) when is_list(PathToFile) ->
  case file:open(PathToFile, [read]) of
    {ok, IODevice} ->
      fill_ets_from_file(IODevice, Tid);
    {error, Reason} ->
      exit(Reason)
  end;
fill_ets_from_file(PathToFile, Tid) ->
  case file:read_line(PathToFile) of
    eof ->
      file:close(PathToFile),
      ok;
    {ok, Line} ->
      Stripped1 = string:strip(Line, both, $\n),
      Stripped2 = string:strip(Stripped1, both, $\n),
      ets:insert(Tid, {Stripped2, number}),
      fill_ets_from_file(PathToFile, Tid)
  end.