%%%-------------------------------------------------------------------
%%% @author neko
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Март 2018 20:48
%%%-------------------------------------------------------------------
-module(sc_call_scheduler_SUITE).
-author("neko").

-compile(export_all).
-compile(nowarn_export_all).

-include("sc_call_scheduler.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-spec test() -> any().

-spec all() -> list().
all() ->
  [start_link_test,
   init_test,
   start_pool_test,
   handle_call_stub_test,
   handle_cast_test,
   handle_info_test,
   terminated_test,
   fill_ets_from_file_test
  ].

start_link_test(_Config) ->
  process_flag(trap_exit, true),
  meck:new(sc_call_scheduler, [passthrough]),
  meck:expect(sc_call_scheduler, init, fun([]) ->
                                      exit(in_init)
                                    end),
  sc_call_scheduler:start_link(),
  ?assertEqual({'EXIT', in_init}, receive {'EXIT', _, Stat} -> {'EXIT', Stat} end),
  meck:unload(sc_call_scheduler).

init_test(_Config) ->
  Self = self(),
  meck:new(sc_call_scheduler, [passthrough]),
  meck:expect(sc_call_scheduler, fill_ets_from_file, fun(PathToFile, Tid) ->
                                                    Self ! {ok, PathToFile, Tid}
                                                  end),
  SourcePath = "./test.txt",
  application:set_env(sip_client, source_path, SourcePath),
  {ok, CurrentPath} = file:get_cwd(),
  CompletedPath = CurrentPath ++ "/test_file.txt",
  file:write_file(CompletedPath, io_lib:nl()),
  application:set_env(sip_client, completed_path, CompletedPath),
  Period = 1,
  application:set_env(sip_client, period, Period),
  {ok, State} = sc_call_scheduler:init([]),
  ?assertEqual({ok, SourcePath, State#state.ets_tid}, receive A -> A end),
  ?assertNotEqual(State#state.ets_tid, undefined),
  ?assertNotEqual(State#state.completed_handler, undefined),
  ?assertEqual(Period, State#state.period),
  file:close(State#state.completed_handler),
  file:delete(CompletedPath),
  meck:unload(sc_call_scheduler).

start_pool_test(_Config) ->
  process_flag(trap_exit, true),
  Self = self(),
  meck:new(sc_call_scheduler, [passthrough]),
  meck:expect(sc_call_scheduler, handle_cast, fun({start_pool, N}, State) ->
                                              Self ! {ok, N, State}
                                           end),
  meck:expect(sc_call_scheduler, init, fun([]) ->
                                      {ok, []}
                                    end),
  PoolSize = 0,
  application:set_env(sip_client, pool_size, PoolSize),
  {ok, Pid} = sc_call_scheduler:start_link(),
  sc_call_scheduler:start_pool(),
  ?assertEqual({ok, PoolSize, []}, receive A -> A end),
  exit(Pid, "Test ended"),
  ?assertEqual({'EXIT', Pid, "Test ended"}, receive A -> A end),
  meck:unload(sc_call_scheduler).

handle_call_stub_test(_Console) ->
  ?assertEqual({reply, ok, #state{}}, sc_call_scheduler:handle_call(request, from, #state{})).

handle_cast_test(_Console) ->
  Self = self(),
  meck:new(sc_call_scheduler, [passthrough]),
  meck:expect(sc_call_scheduler, start_call, fun(N, State) ->
                                             Self ! {ok, N, State},
                                             {State, new}
                                          end),
  N = 0,
  State = #state{},
  {noreply, {State, new}} = sc_call_scheduler:handle_cast({start_pool, N}, State),
  ?assertEqual({ok, N, State}, receive A -> A end),
  ?assertEqual({noreply, #state{}}, sc_call_scheduler:handle_cast(msg, #state{})),
  meck:unload(sc_call_scheduler).

%%start_call_test(_Console) ->
%%  process_flag(trap_exit, true),
%%  ?assertEqual(#state{}, sc_call_scheduler:start_call(0, #state{})),
%%  Self = self(),
%%  {ok, SupPid} = sc_line_sup:start_link(),
%%  meck:new(sc_line, [passthrough]),
%% meck:expect(sc_line, init, fun([Number]) ->
%%                                       Self ! {ok, Number},
%%                                      {ok, []}
%%                                    end),
%%  Tid = ets:new(numbers, [public]),
%%  Number = "8800000000",
%%  AnotherNumber = "88000000001",
%%  ets:insert(Tid, [{Number, number}, {AnotherNumber, number}]),
%%  #state{ets_tid = Tid, last_sended = '$end_of_table'} = sc_call_scheduler:start_call(3, #state{ets_tid = Tid, last_sended = undef}),
%%  ?assertEqual({ok, list_to_binary(Number)}, receive A -> A end),
%%  ?assertEqual({ok, list_to_binary(AnotherNumber)}, receive A -> A end),
%%  ets:delete_all_objects(Tid),
%%  #state{ets_tid = Tid, last_sended = '$end_of_table'} = sc_call_scheduler:start_call(1, #state{ets_tid = Tid, last_sended = undef}),
%%  exit(SupPid, kill),
%%  meck:unload(sc_line).

handle_info_test(_Config) ->
  Self = self(),
  meck:new(sc_call_scheduler, [passthrough]),
  meck:expect(sc_call_scheduler, start_call, fun(N, State) ->
                                            Self ! {ok, N, State},
                                            {State, new}
                                          end),
  {ok, CurrentPath} = file:get_cwd(),
  CompletedPath = CurrentPath ++ "/test_file.txt",
  file:write_file(CompletedPath, ""),
  {ok, CompletedHandler} = file:open(CompletedPath, [write]),
  Ref = erlang:make_ref(),
  State = #state{completed_handler = CompletedHandler, pool = [{Ref, number}], last_sended = '$end_of_table'},
  {noreply, NewState} = sc_call_scheduler:handle_info({'DOWN', Ref, process, pid, reason}, State),
  file:sync(CompletedHandler),
  ?assertEqual([], NewState#state.pool),
  ?assertEqual({ok, <<"number\r\n">>}, file:read_file(CompletedPath)),
  EmptyPoolState = State#state{pool = []},
  {noreply, EmptyPoolState} = sc_call_scheduler:handle_info({'DOWN', Ref, process, pid, reason}, EmptyPoolState),
  NotEndState = State#state{last_sended = undefuned},
  {noreply, {NewNotEndState, new}} = sc_call_scheduler:handle_info({'DOWN', Ref, process, pid, reason}, NotEndState),
  file:sync(CompletedHandler),
  ?assertEqual([], NewNotEndState#state.pool),
  ?assertEqual({ok, 1, NewNotEndState}, receive A -> A end),
  ?assertEqual({ok, <<"number\r\nnumber\r\n">>}, file:read_file(CompletedPath)),
  EmptyPoolNotEndState = NotEndState#state{pool = []},
  {noreply, {EmptyPoolNotEndState, new}} = sc_call_scheduler:handle_info({'DOWN', Ref, process, pid, reason}, EmptyPoolNotEndState),
  ?assertEqual({ok, 1, EmptyPoolNotEndState}, receive A -> A end),
  {noreply, state} = sc_call_scheduler:handle_info(info, state),
  file:close(CompletedHandler),
  meck:unload(sc_call_scheduler).

terminated_test(_Config) ->
  {ok, CurrentPath} = file:get_cwd(),
  CompletedPath = CurrentPath ++ "/test_file.txt",
  file:write_file(CompletedPath, ""),
  process_flag(trap_exit, true),
  meck:new(sc_call_scheduler, [passthrough]),
  meck:expect(sc_call_scheduler, init, fun([]) ->
                                      {ok, CompletedHandler} = file:open(CompletedPath, [write]),
                                      {ok, #state{completed_handler = CompletedHandler}}
                                    end),
  {ok, Pid} = sc_call_scheduler:start_link(),
  gen_server:stop(Pid),
  meck:unload(sc_call_scheduler).

fill_ets_from_file_test(_Config) ->
  {ok, CurrentPath} = file:get_cwd(),
  CompletedPath = CurrentPath ++ "/test_file.txt",
  file:write_file(CompletedPath, "88000000000" ++ io_lib:nl() ++ "88000000001"),
  Tid = ets:new(numbers, [public]),
  sc_call_scheduler:fill_ets_from_file(CompletedPath, Tid),
  ?assertEqual("88000000000", ets:first(Tid)),
  ?assertEqual("88000000001", ets:next(Tid, ets:first(Tid))),
  NotExistFilePath = CompletedPath ++ "t",
  ?assertEqual({'EXIT',enoent}, catch sc_call_scheduler:fill_ets_from_file(NotExistFilePath, Tid)).
