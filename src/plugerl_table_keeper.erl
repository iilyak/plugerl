-module(plugerl_table_keeper).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([transfer_table/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {name}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

transfer_table(Server) ->
    gen_server:cast(Server, {transfer_table, self()}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Name]) ->
    process_flag(trap_exit, true),
    Owner = maybe_create_table(Name),
    maybe_notify_owner(Owner, Name),
    {ok, #state{name = Name}}.

handle_call(Request, {Pid, _Ref}, State) ->
    {reply, {error, {unhandled_call, Request, Pid}}, State}.

handle_cast({transfer_table, NewOwner}, #state{name = Name} = State)  ->
    ets:give_away(Name, NewOwner, [Name]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'ETS-TRANSFER', _Table, _Owner, _}, #state{} = State) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

maybe_create_table(Name) ->
    case ets:info(Name, owner) of
        undefined ->
            ets:new(Name, [named_table, protected, {heir, self(), []}]),
            undefined;
        Owner ->
            Owner
    end.

maybe_notify_owner(undefined, _Name) -> ok;
maybe_notify_owner(Owner, Name) ->
    plugerl_registry:replace_keeper(Owner, Name, self()).
