-module(plugerl_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, stop/1, lookup/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(WORKER(I, A),
    {I, {I, start_link, A}, permanent, 5000, worker, [I]}).
-define(SUP(I, A),
    {I, {I, start_link, A}, permanent, infinity, supervisor, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================

start_link(Name) ->
    supervisor:start_link(?MODULE, [Name]).

stop(Sup) ->
    supervisor:terminate_child(Sup, plugerl_handlers_sup),
    unlink(Sup),
    sync_stop(Sup).

lookup(Sup, Id) ->
    Children = supervisor:which_children(Sup),
    case lists:keyfind(Id, 1, Children) of
        {Id, Child, _Type, _Modules} when is_pid(Child) -> Child;
        _ -> undefined
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Name]) ->
    Children = [
        ?SUP(plugerl_handlers_sup, [Name]),
        ?WORKER(plugerl_table_keeper, [Name]),
        ?WORKER(plugerl_registry, [Name, self()])
    ],
    {ok, { {one_for_one, 5, 10}, Children} }.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

sync_stop(Pid) ->
    Ref = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    after 5000 ->
            error(exit_timeout)
    end.
