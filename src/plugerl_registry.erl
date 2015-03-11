-module(plugerl_registry).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

-export([register_plugin/3, unregister_plugin/2]).
-export([get_plugins/1, get_plugins/2, plugins/1]).

-export([register_handler/5, unregister_handler/3]).
-export([get_handlers/1, get_handlers/2, handlers/2]).

-export([lookup/2, lookup/3]).

-export([replace_keeper/3]).
-export([get_supervisor/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {table, name, pending = [], supervisor, monitor}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name, Parent) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Parent], []).

register_plugin(Server, Module, Options) ->
    gen_server:call(Server, {register_plugin, Module, Options}).

unregister_plugin(Server, Module) ->
    gen_server:call(Server, {unregister_plugin, Module}).

register_handler(Server, Type, Module, Args, Options) ->
    gen_server:call(Server, {register_handler, Type, Module, Args, Options}).

unregister_handler(Server, Type, Module) ->
    gen_server:call(Server, {unregister_handler, Type, Module}).

get_plugins(Table) ->
    get_plugins(Table, '_').

get_plugins(Table, Type) ->
    ets:match_object(Table, {Type, '_'}).

get_handlers(Table) ->
    get_handlers(Table, '_').

get_handlers(Table, Type) ->
    ets:match_object(Table, {{Type, '_'}, '_', '_'}).

replace_keeper(Server, Name, Keeper) ->
    gen_server:call(Server, {replace_keeper, Name, Keeper}).

lookup(Table, Type) ->
    case ets:lookup(Table, Type) of
        [{_, Value}] -> Value;
        [] -> undefined
    end.

lookup(Table, Type, Module) ->
    case ets:lookup(Table, {Type, Module}) of
        [{_, Args, Options}] -> {Args, Options};
        [] -> undefined
    end.

plugins(Table) ->
    [Type || {Type, _Args} <- get_plugins(Table)].

handlers(Table, _Type) ->
    [Module || {{_, Module}, _, _} <- get_handlers(Table)].

get_supervisor(Server) ->
    gen_server:call(Server, get_supervisor).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Name, Parent]) ->
    Keeper = ets:info(Name, owner),
    process_flag(trap_exit, true),
    ok = plugerl_table_keeper:transfer_table(Keeper),
    {ok, #state{name = Name, supervisor = Parent}}.

handle_call(get_supervisor, _From, #state{supervisor = Sup} = State) ->
    {reply, Sup, State};
handle_call({replace_keeper, Name, Keeper} = Action, From,
        #state{name = Name, table = Table, pending = Pending} = State) ->
    case Table of
        undefined ->
            {noreply, State#state{pending = [{From, Action}|Pending]}};
        Table ->
            MonitorRef = erlang:monitor(process, Keeper),
            {reply, handle(Action, Table), State#state{monitor = MonitorRef}}
    end;
handle_call(Action, From,
        #state{table = undefined, pending = Pending} = State) ->
    {noreply, State#state{pending = [{From, Action}|Pending]}};
handle_call(Action, _From,
        #state{table = Table} = State) ->
    {reply, handle(Action, Table), State};
handle_call(Request, {Pid, _Ref}, State) ->
    {reply, {error, {unhandled_call, Request, Pid}}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'ETS-TRANSFER', Table, Keeper, [Name]},
        #state{table = undefined, name = Name} = State) ->
    MonitorRef = erlang:monitor(process, Keeper),
    set_keeper(Table, Keeper),
    handle_pending(State#state.pending, Table),
    {noreply, State#state{table = Table, monitor = MonitorRef}};
handle_info({'DOWN', MonitorRef, process, _Keeper, _Reason},
        #state{monitor = MonitorRef} = State) ->
    {noreply, State#state{monitor = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handle_pending([], _Table) -> ok;
handle_pending([{From, Action}|Rest], Table) ->
    gen_server:reply(From, handle(Action, Table)),
    handle_pending(Rest, Table).

handle({register_plugin, Module, Options}, Table) ->
    ets:insert(Table, {Module, Options});
handle({register_handler, Type, Module, Args, Options}, Table) ->
    ets:insert(Table, {{Type, Module}, Args, Options});
handle({unregister_plugin, Module}, Table) ->
    ets:delete(Table, Module);
handle({unregister_handler, Type, Module}, Table) ->
    ets:delete(Table, {Type, Module});
handle({replace_keeper, _Name, Keeper}, Table) ->
    set_keeper(Table, Keeper);
handle(Request, _Table) ->
    {error, {unknown_request, Request}}.

set_keeper(Table, Keeper) ->
    ets:setopts(Table, [{heir, Keeper, []}]).
