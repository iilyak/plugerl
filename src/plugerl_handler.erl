% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(plugerl_handler).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/4]).

-export([start_handler/3, stop_handler/2]).

-export([call/4]).
-export([notify/3]).
-export([handle/4]).
-export([iterate/5]).
-export([funout/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
    {name, monitor, args, handlers = [], started = [], table, reason}).

%% ------------------------------------------------------------------
%% Private API Function Definitions
%% ------------------------------------------------------------------

start_link(TableName, Parent, Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE,
        {TableName, Parent, Name, Args}, []).

start_handler(Server, Module, Parent) ->
    maybe_call(whereis(Server),
        {error, {plugin_not_started, Server}},
        {start_handler, Server, Module, Parent}).

stop_handler(Server, Module) ->
    maybe_call(whereis(Server), ok, {stop_handler, Server, Module}).

%% ------------------------------------------------------------------
%% Public API Function Definitions (exposed via plugerl.erl)
%% ------------------------------------------------------------------

call(Server, Function, Default, Args) ->
    maybe_call(whereis(Server), Default, {call, Function, Default, Args}).

notify(Server, Function, Args) ->
    gen_server:cast(Server, {notify, Function, Args}).

handle(Server, Function, Default, Args) ->
    maybe_call(whereis(Server), Default, {handle, Function, Default, Args}).

iterate(Server, Function, Args, Acc, Fun) ->
    maybe_call(whereis(Server), Acc, {iterate, Function, Args, Acc, Fun}).

funout(Handlers, Function, Args) ->
    [call_handler(Module, Function, Args) || Module <- Handlers].

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({TableName, Parent, Name, Args}) ->
    process_flag(trap_exit, true),
    MonitorRef = maybe_monitor(Parent, Args),
    gen_server:cast(self(), {start_registered_handlers, Name}),
    {ok, #state{name = Name,
                args = Args,
                table = TableName,
                monitor = MonitorRef}}.

handle_call({call, Function, Default, Args}, _From,
        #state{handlers = Handlers} = State) ->
    {ok, Result, NewHandlers} = call_int(Handlers, Function, Args),
    {reply, maybe_default(Result, Default), State#state{handlers = NewHandlers}};
handle_call({handle, Function, Default, Args}, _From,
        #state{handlers = Handlers} = State) ->
    {ok, Result, NewHandlers} = handle_int(Handlers, Function, Default, Args),
    {reply, maybe_default(Result, Default), State#state{handlers = NewHandlers}};
handle_call({iterate, Function, Args, Acc, Fun}, _From,
        #state{handlers = Handlers} = State) ->
    {ok, Result, NewHandlers} = iterate_int(Handlers, Function, Args, Acc, Fun),
    {reply, Result, State#state{handlers = NewHandlers}};
handle_call({start_handler, Type, Module, _Parent}, _From, #state{} = State) ->
    case is_started(Module, State) of
        false ->
            {Reply, NewState} = start_handler_int(Type, Module, State),
            {reply, Reply, NewState};
        true ->
            {reply, {error, {already_started, Module}}, State}
    end;
handle_call({stop_handler, Type, Module}, _From, #state{} = State) ->
    case is_started(Module, State) of
        true ->
            {Reply, NewState} = stop_handler(Type, Module, stop, State),
            {reply, Reply, NewState};
        false ->
            {reply, {error, {not_started, Module}}, State}
    end;
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

handle_cast({notify, Function, Args}, #state{handlers = Handlers} = State) ->
    {ok, _, NewHandlers} = notify_int(Handlers, Function, Args),
    {noreply, State#state{handlers = NewHandlers}};
handle_cast({start_registered_handlers, Type},
        #state{handlers = [], table = Table} = State) ->
    NewState = start_all_handlers(plugerl_registry:get_handlers(Table, Type), State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _, Reason}, #state{monitor = Ref} = State) ->
    {stop, normal, State#state{reason = Reason}};
handle_info({'DOWN', Ref, process, _, Reason} = Info,
        #state{name = Type, started = Started, handlers = Handlers} = State) ->
    case lists:keytake(Ref, 2, Started) of
        {value, {Module, Ref}, NewStarted} ->
            {_, NewState} = stop_handler(Type, Module, Reason, State),
            {noreply, NewState#state{started = NewStarted}};
        false ->
            {ok, _, NewHandlers} = notify_int(Handlers, handle_info, [Info]),
            {noreply, State#state{handlers = NewHandlers}}
    end;
handle_info(Info, #state{handlers = Handlers} = State) ->
    {ok, _, NewHandlers} = notify_int(Handlers, handle_info, [Info]),
    {noreply, State#state{handlers = NewHandlers}}.

terminate(Reason, #state{reason = undefined} = State) ->
    stop(Reason, State),
    ok;
terminate(_Reason, #state{reason = Reason} = State) ->
    stop(Reason, State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

is_started(Module, #state{started = Started}) ->
    lists:keymember(Module, 1, Started).

start_all_handlers(Handlers, State) ->
    lists:foldl(fun({{Type, Module}, Args, Options}, StateAcc) ->
          case start_handler_int(Type, Module, {Args, Options}, StateAcc) of
              {{error, _Reason}, NewStateAcc} ->
                  NewStateAcc;
              {ok, NewStateAcc} ->
                  NewStateAcc
          end
    end, State, Handlers).

start_handler_int(Type, Module, #state{table = Table} = State) ->
    Spec = plugerl_registry:lookup(Table, Type, Module),
    start_handler_int(Type, Module, Spec, State).

start_handler_int(_Type, Module, undefined, State) ->
    {{error, {no_args, Module}}, State};
start_handler_int(_Type, Module, {Args, Options},
        #state{started = Started, handlers = Handlers} = State) ->
    case call_handler(Module, init, [Args]) of
        {error, Reason} ->
            {{error, {init, Module, Reason}}, State};
        {ok, HandlerState} ->
            NewState = State#state{
                started = [{Module, maybe_monitor(Options)}|Started],
                handlers = [{Module, HandlerState}|Handlers]
            },
            {ok, NewState};
        Else ->
            {{error, {init, Module, unexpected_return, Else}}, State}
    end.

stop_handler(_Type, Module, Reason,
        #state{handlers = Handlers, started = Started} = State) ->
    case lists:keytake(Module, 1, Handlers) of
        {value, {Module, HandlerState}, NewHandlers} ->
            {{_, MonitorRef}, NewStarted} =
                keytake(Module, 1, Started, {undefined, undefined}),
            maybe_demonitor(MonitorRef),
            NewState = State#state{
                 started = NewStarted,
                 handlers = NewHandlers
            },
            case call_handler(Module, terminate, [Reason], HandlerState) of
                {error, FailureReason} ->
                    {{error, {terminate, Module, FailureReason}}, NewState};
                _ ->
                    {ok, NewState}
            end;
        false ->
            {{error, {no_state, Module}}, State}
    end.

keytake(Key, Pos, List, Default) ->
    case lists:keytake(Key, Pos, List) of
        {value, Tuple, NewList} ->
            {Tuple, NewList};
        false ->
            {Default, List}
    end.

maybe_monitor(Options) ->
    case proplists:get_value(monitor, Options, undefined) of
        Pid when is_pid(Pid) -> erlang:monitor(process, Pid);
        _ -> undefined
    end.

maybe_monitor(Parent, Options) ->
    case proplists:get_value(monitor, Options, true) of
        true when is_pid(Parent) -> erlang:monitor(process, Parent);
        false -> undefined;
        Pid when is_pid(Pid) -> erlang:monitor(Pid)
    end.

maybe_demonitor(undefined) -> ok;
maybe_demonitor(MonitorRef) -> erlang:demonitor(MonitorRef, [flush]).

stop(Reason, State) ->
    stop_all_started_handlers(Reason, State),
    stop_plugin(Reason, State),
    ok.

stop_plugin(_Reason, #state{monitor = undefined}) ->
    ok;
stop_plugin(Reason, #state{monitor = MonitorRef} = State) ->
    erlang:demonitor(MonitorRef, [flush]),
    stop_plugin(Reason, State#state{monitor = undefined}).

call_int(Handlers, Function, Args) ->
    iterate_int(Handlers, Function, Args, [], fun(Handler, Result, Acc) ->
        case Result of
            {error, Reason} ->
                {ok, [{error, {handler_error, Handler, Reason}}|Acc]};
            Result ->
                {ok, [Result|Acc]}
        end
    end).

notify_int(Handlers, Function, Args) ->
    iterate_int(Handlers, Function, Args, [], fun(_Handler, _Result, Acc) ->
        {ok, Acc}
     end).

handle_int(Handlers, Function, Default, Args) ->
    iterate_int(Handlers, Function, Args, Default, fun(Handler, Result, Acc) ->
        case Result of
            {reply, Res} ->
                {stop, Res};
            {error, Reason} ->
                {stop, {error, {handler_error, Handler, Reason}}};
            Result ->
                {ok, Acc}
        end
    end).

iterate_int(Handler, Function, Args, Acc, Fun) ->
    iterate_int(Handler, Function, Args, Acc, [], Fun).

iterate_int(undefined, _Function, _Args, Acc, _Result, _Fun) ->
    {ok, Acc, undefined};
iterate_int([], _Function, _Args, Acc, Result, _Fun) ->
    {ok, Acc, Result};
iterate_int([{Module, State}|Rest], Function, Args, Acc, Result, Fun) ->
    {Res, NewState} = call_handler(Module, Function, Args, State),
    NewResult = [{Module, NewState}|Result],
    case Fun(Module, Res, Acc) of
        {stop, NewAcc} ->
            {ok, NewAcc, NewResult ++ Rest};
        {ok, NewAcc} ->
            iterate_int(Rest, Function, Args, NewAcc, NewResult, Fun)
    end.

call_handler(Module, Function, Args, State) ->
    case call_handler(Module, Function, Args ++ [State]) of
        {error, Reason} = Error ->
            handle_error(Module, Function, Reason, Args, State),
            {Error, State};
        Else ->
            Else
    end.

call_handler(Module, Function, Args) ->
    try
        apply(Module, Function, Args)
    catch _:Reason ->
        {error, Reason}
    end.

handle_error(Module, Function, Reason, Args, State) ->
    (catch Module:handle_error(Function, Reason, Args, State)).

maybe_default([], Default) -> Default;
maybe_default(Result, _Default) -> Result.

stop_all_started_handlers(Reason,
        #state{started = Started, name = Type} = State) ->
    [stop_handler(Type, Module, Reason, State) || {Module, _Ref} <- Started].

maybe_call(undefined, Default, _Msg) ->
    Default;
maybe_call(Pid, _Default, Message) ->
    Res = gen_server:call(Pid, Message),
    Res.
