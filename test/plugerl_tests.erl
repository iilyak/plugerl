-module(plugerl_tests).

-include_lib("eunit/include/eunit.hrl").

-define(t2l(V), lists:flatten(io_lib:format("~p", [V]))).
-define(R, plugerl_registry).
-define(K, plugerl_table_keeper).
-define(SUP, test_sup).

-export([save/3]).

start() ->
    ets:new(?MODULE, [named_table, public]),
    {ok, Pid} = plugerl:start_link(test_plug),
    plugerl:register_plugin(test_plug, ?MODULE),
    register(?SUP, Pid),
    Pid.

start(P) ->
    start(),
    P.

stop(_) ->
    plugerl:stop(test_plug),
    ets:delete(?MODULE).

setup() ->
    setup(plugin).

setup(plugin) ->
    plugerl:start_plugin(test_plug, ?MODULE),
    plugerl:register_handler(test_plug, ?MODULE, handler_foo, [1], []),
    plugerl:register_handler(test_plug, ?MODULE, handler_bar, [2], []),
    ok.

teardown(ok) ->
    plugerl:unregister_handler(test_plug, ?MODULE, handler_foo),
    plugerl:unregister_handler(test_plug, ?MODULE, handler_bar),
    plugerl:stop_plugin(test_plug, ?MODULE),
    ets:delete_all_objects(?MODULE),
    cleanup(handler_foo),
    cleanup(handler_bar);
teardown(Id) when is_atom(Id) ->
    start_child(Id),
    teardown(ok).

%% ------------------------------------------------------------------
%% Test cases
%% ------------------------------------------------------------------

keep_when_restarts_test_() ->
    [make_case(P) || P <- kill_order_permutations()].

dispatch_test_() ->
    Funs = [
        fun ensure_can_call/1,
        fun ensure_can_notify/1,
        fun ensure_can_handle/1,
        fun ensure_can_iterate/1,
        fun ensure_can_funout/1,
        fun ensure_can_handle_info/1
    ],
    {
        "Make sure we can always dispatch",
        {
            setup,
            fun start/0, fun stop/1,
            [
                dispatch_cases("Basic dispatch", Funs),
                should_dispatch_when_no_registry(Funs),
                should_dispatch_when_no_keeper(Funs),
                should_stop_handle_on_reply()
            ]

        }
    }.

should_not_dispatch_test_() ->
    Funs = [
        fun ensure_cannot_call/1,
        fun ensure_cannot_notify/1,
        fun ensure_cannot_handle/1,
        fun ensure_cannot_iterate/1
    ],
    {
        "Make sure we cannot dispatch when stopped",
        {
            setup,
            fun start/0, fun stop/1,
            [
                should_not_dispatch_when_stopped(Funs),
                should_not_dispatch_when_no_handlers(Funs)
            ]

        }
    }.

error_handling_test_() ->
    {
        "Make sure we handle errors and cannot dispatch when stopped",
        {
            setup,
            fun start/0, fun stop/1,
            [
                should_restart_handlers_on_plugin_restart(),
                should_stop_when_any_failure_on_handle(),
                should_call_handle_error(),
                should_call_terminate(),
                should_notify_rest_when_single_handler_failed(),
                should_stop_if_parent_die(),
                should_stop_handler_when_parent_die()
            ]
        }
    }.

should_restart_handlers_on_plugin_restart() ->
    Setup = fun() ->
        setup(),
        plugerl:stop_plugin(test_plug, ?MODULE),
        plugerl:start_plugin(test_plug, ?MODULE),
        ok
    end,
    {
        "Ensure we restart handlers on plugin restart",
        {
            setup, Setup, fun teardown/1,
            ?_assertMatch([1, 2], get_state())
        }
    }.

should_stop_handle_on_reply() ->
    {
        "Ensure we stop handling of request when handler return reply handlers",
        {
            setup, fun setup/0, fun teardown/1,
            ?_test(begin
                ?assertMatch(2, plugerl:handle(?MODULE, reply_one, 0, [])),
                ?assertMatch([3, 1], get_state())
            end)
        }
    }.

should_stop_when_any_failure_on_handle() ->
    {
        "Ensure we return default value in case of `handle` call failure",
        {
            setup, fun setup/0, fun teardown/1,
            ?_test(begin
                Result = plugerl:handle(?MODULE, fail_one, 0, [5]),
                ?assertMatch({error, {handler_error, handler_foo, fail}}, Result),
                ?assertMatch([1, 7], get_state())
            end)
        }
    }.

should_call_handle_error() ->
    make_failed_case("Ensure we call handle_error in case of failures",
        ?_test(begin
            ?assertMatch({fail_one, fail, [5], 1}, get_errors(handler_foo)),
            ?assertMatch(undefined, get_errors(handler_bar))
        end)).

should_call_terminate() ->
    {
        "Ensure we call terminate",
        {
            setup, fun setup/0, fun teardown/1,
            ?_test(begin
                plugerl:stop_plugin(test_plug, ?MODULE),
                ?assertMatch({shutdown, 1}, get_terminate(handler_foo)),
                ?assertMatch({shutdown, 2}, get_terminate(handler_bar))
            end)
        }
    }.

should_stop_if_parent_die() ->
    {
        "Ensure we stop plugin when parent die",
        {
            setup,
            fun() ->
                Pid = start_plugin_from_another_process(),
                plugerl:register_handler(test_plug, ?MODULE, handler_foo, [1], []),
                plugerl:register_handler(test_plug, ?MODULE, handler_bar, [2], []),
                Pid
            end,
            fun(_Pid) ->
                plugerl:start_plugin(test_plug, ?MODULE),
                teardown(ok)
            end,
            fun(Pid) ->
                ?_test(begin
                    exit(Pid, stop),
                    wait_terminate([handler_foo, handler_bar]),
                    ?assertMatch({stop, 1}, get_terminate(handler_foo)),
                    ?assertMatch({stop, 2}, get_terminate(handler_bar))
                end)
            end
        }
    }.

should_notify_rest_when_single_handler_failed() ->
    {
        "Ensure we notify rest handlers when single handler fail",
        {
            setup, fun setup/0, fun teardown/1,
            ?_test(begin
                plugerl:notify(?MODULE, fail_one, [5]),
                ?assertMatch([1, 7], get_state()),
                ?assertMatch({fail_one, fail, [5], 1}, get_errors(handler_foo)),
                ?assertMatch(undefined, get_errors(handler_bar))
            end)
        }
    }.

should_stop_handler_when_parent_die() ->
    {
        "Ensure we stop handler when parent which registered it die",
        {
            setup,
            fun() ->
                plugerl:start_plugin(test_plug, ?MODULE),
                plugerl:register_handler(test_plug, ?MODULE, handler_foo, [1], []),
                register_handler_from_another_process(handler_bar, [2])
            end,
            fun(_) -> teardown(ok) end,
            fun(Pid) ->
                ?_test(begin
                    exit(Pid, myreason),
                    wait_terminate([handler_bar]),
                    ?assertMatch(undefined, get_terminate(handler_foo)),
                    ?assertMatch({myreason, 2}, get_terminate(handler_bar))
                end)
            end
        }
    }.

should_dispatch_when_no_registry(Funs) ->
    dispatch_cases("No registry", Funs, fun() -> setup(), stop_child(?K) end).

should_dispatch_when_no_keeper(Funs) ->
    dispatch_cases("No keeper", Funs, fun() -> setup(), stop_child(?K) end).

should_not_dispatch_when_stopped(Funs) ->
    dispatch_cases("Stopped plugin", Funs, fun() ->
        setup(),
        plugerl:stop_plugin(test_plug, ?MODULE)
    end).

should_not_dispatch_when_no_handlers(Funs) ->
    dispatch_cases("No handlers", Funs, fun() ->
        setup(),
        plugerl:unregister_handler(test_plug, ?MODULE, handler_foo),
        plugerl:unregister_handler(test_plug, ?MODULE, handler_bar)
    end).

ensure_keep_when_restarts(P) ->
    ?_test(begin
        lists:foreach(fun(Id) ->
            ?assertMatch([?MODULE], plugerl:plugins(test_plug)),
            ?assertMatch(ok, kill_and_wait_restart(Id)),
            ?assertMatch([?MODULE], plugerl:plugins(test_plug))
        end, P)
    end).

ensure_can_call(_) ->
    ?_test(begin
        Result = plugerl:call(?MODULE, add, 0, [5]),
        ?assertMatch([5, 5], Result),
        ?assertMatch([6, 7], get_state()),
        ?assertMatch(undefined, get_errors(handler_foo)),
        ?assertMatch(undefined, get_errors(handler_bar))
    end).

ensure_can_notify(_) ->
    ?_test(begin
        plugerl:notify(?MODULE, add, [5]),
        ?assertMatch([6, 7], get_state()),
        ?assertMatch(undefined, get_errors(handler_foo)),
        ?assertMatch(undefined, get_errors(handler_bar))
    end).

ensure_can_handle(_) ->
    ?_test(begin
        plugerl:handle(?MODULE, add, 0, [5]),
        ?assertMatch([6, 7], get_state()),
        ?assertMatch(undefined, get_errors(handler_foo)),
        ?assertMatch(undefined, get_errors(handler_bar))
    end).

ensure_can_handle_info(_) ->
    ?_test(begin
        Pid = whereis(?MODULE),
        Pid ! {add, 5},
        ?assertMatch([6, 7], get_state()),
        ?assertMatch(undefined, get_errors(handler_foo)),
        ?assertMatch(undefined, get_errors(handler_bar))
    end).

ensure_can_iterate(_) ->
    ?_test(begin
        Result = plugerl:iterate(?MODULE, add, [5], [foo], fun(M, R, Acc) ->
            {ok, [{M, R}|Acc]}
        end),
        ?assertMatch([{handler_foo, 5}, {handler_bar, 5}, foo], Result),
        ?assertMatch([6, 7], get_state()),
        ?assertMatch(undefined, get_errors(handler_foo)),
        ?assertMatch(undefined, get_errors(handler_bar))
    end).

ensure_can_funout(_) ->
    ?_test(begin
        Result = plugerl:funout(test_plug, ?MODULE, add, [5]),
        ?assertMatch([7, 6], Result),
        ?assertMatch(undefined, get_errors(handler_foo)),
        ?assertMatch(undefined, get_errors(handler_bar))
    end).

ensure_cannot_call(_) ->
    ?_test(begin
        Result = plugerl:call(?MODULE, add, 0, [5]),
        ?assertMatch(0, Result)
    end).

ensure_cannot_notify(_) ->
    ?_test(begin
        plugerl:notify(?MODULE, add, [5])
    end).

ensure_cannot_handle(_) ->
    ?_test(begin
        Result = plugerl:handle(?MODULE, add, 0, [5]),
        ?assertMatch(0, Result)
    end).

ensure_cannot_iterate(_) ->
    ?_test(begin
        Result = plugerl:iterate(?MODULE, add, [5], [foo], fun(M, R, Acc) ->
            {ok, [{M, R}|Acc]}
        end),
        ?assertMatch([foo], Result)
    end).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

dispatch_cases(Name, Funs) ->
    {
        Name,
        {
            foreach, fun setup/0, fun teardown/1,
            Funs
        }
    }.

dispatch_cases(Name, Funs, Setup) when is_function(Setup) ->
    {
        Name,
        {
            foreach, Setup, fun teardown/1,
            Funs
        }
    }.

make_case(P) ->
    {
        lists:flatten("Kill in order: " ++ format_permutation(P)),
        {
            foreach, fun() -> start(P) end, fun stop/1,
            [fun ensure_keep_when_restarts/1]
        }
    }.

make_failed_case(Name, Fun) ->
    SetupFuns = [
        {"call", failure_setup(fun() ->
            plugerl:call(?MODULE, fail_one, 0, [5])
        end)},
        {"notify", failure_setup(fun() ->
             plugerl:notify(?MODULE, fail_one, [5]),
             timer:sleep(50)
        end)},
        {"handle", failure_setup(fun() ->
             plugerl:handle(?MODULE, fail_one, 0, [5])
        end)},
        {"iterate", failure_setup(fun() ->
            plugerl:iterate(?MODULE, fail_one, [5], [foo], fun(M, R, Acc) ->
               {ok, [{M, R}|Acc]}
            end)
        end)}
    ],
    {
        Name,
        [{setup, Setup, fun teardown/1, [{Id, Fun}]}
            || {Id, Setup} <- SetupFuns]
    }.

failure_setup(Fun) ->
    fun() ->
        setup(),
        Fun(),
        ok
    end.


format_permutation(P) ->
    string:join(lists:map(fun
        (plugerl_table_keeper) -> "keeper";
        (plugerl_registry) -> "registry"
    end, P), " - ").

kill_and_wait_restart(Id) ->
    Pid = plugerl_sup:lookup(?SUP, Id),
    ?assert(is_pid(Pid)),
    exit(Pid, kill),

    wait(fun() ->
        NewPid = plugerl_sup:lookup(?SUP, Id),
        is_pid(NewPid) andalso NewPid /= Pid andalso is_ready(NewPid, test_plug)
    end).

stop_child(Id) ->
    supervisor:terminate_child(?SUP, Id),
    Id.

start_child(Id) ->
    {ok, Pid} = supervisor:restart_child(?SUP, Id),
    wait(fun() ->
        is_ready(Pid, test_plug)
    end).

is_ready(Pid, Name) ->
    case {ets:info(Name, owner), ets:info(Name, heir)} of
        {Pid, _} -> true;
        {_, Pid} -> true;
        _Else -> false
    end.

kill_order_permutations() ->
    [
        [?R, ?R, ?R, ?R],
        [?R, ?R, ?R, ?K],
        [?R, ?R, ?K, ?R],
        [?R, ?R, ?K, ?K],
        [?R, ?K, ?R, ?R],
        [?R, ?K, ?R, ?K],
        [?R, ?K, ?K, ?R],
        [?R, ?K, ?K, ?K],
        [?K, ?R, ?R, ?R],
        [?K, ?R, ?R, ?K],
        [?K, ?R, ?K, ?R],
        [?K, ?R, ?K, ?K],
        [?K, ?K, ?R, ?R],
        [?K, ?K, ?R, ?K],
        [?K, ?K, ?K, ?R],
        [?K, ?K, ?K, ?K]
     ].

wait(Fun) ->
    Now = now_us(),
    Timeout = 5000,
    wait(Fun, Timeout * 1000, Now, Now).

wait(_Fun, Timeout, Started, Prev) when Prev - Started > Timeout ->
    timeout;
wait(Fun, Timeout, Started, _Prev) ->
    case Fun() of
        true -> ok;
        false ->
            ok = timer:sleep(50),
            wait(Fun, Timeout, Started, now_us())
    end.

now_us() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.

get_state() ->
    lists:reverse(plugerl:call(?MODULE, get, undefined, [])).

cleanup(Handler) ->
    erase({Handler, handle_error}),
    erase({Handler, terminate}),
    ok.


get_errors(Handler) ->
    lookup(Handler, handle_error).

get_terminate(Handler) ->
    lookup(Handler, terminate).

lookup(Handler, What) ->
    case ets:lookup(?MODULE, {Handler, What}) of
        [] -> undefined;
        [{_, Value}] -> Value
    end.

save(Handler, What, Value) ->
    ets:insert(?MODULE, {{Handler, What}, Value}).

wait_terminate(Handlers) ->
    wait(fun() ->
        not lists:any(fun(Handler) ->
                get_terminate(Handler) == undefined
            end, Handlers)
    end).

start_plugin_from_another_process() ->
    start_from_another_process(fun() ->
        plugerl:start_plugin(test_plug, ?MODULE)
    end).

register_handler_from_another_process(Handler, Args) ->
    start_from_another_process(fun() ->
        plugerl:register_handler(test_plug, ?MODULE, Handler, Args, [{monitor, self()}])
    end).

start_from_another_process(Fun) ->
    Self = self(),
    Pid = spawn(fun() ->
        Fun(),
        Self ! ready,
        receive _ -> ok end
    end),
    receive
        ready -> Pid
    end.
