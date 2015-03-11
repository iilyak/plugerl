-module(plugerl_handlers_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

-export([start_child/3, terminate_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, A), {I, {I, start_link, A}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Name) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Name]).

start_child(Parent, Type, Args) ->
    supervisor:start_child(?MODULE, [Parent, Type, Args]).

terminate_child(undefined) -> ok;
terminate_child(Child) when is_atom(Child) ->
    terminate_child(whereis(Child));
terminate_child(ChildPid) ->
    supervisor:terminate_child(?MODULE, ChildPid).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Name]) ->
    Children = [
        ?CHILD(plugerl_handler, worker, [Name])
    ],
    {ok, { {simple_one_for_one, 5, 10}, Children} }.
