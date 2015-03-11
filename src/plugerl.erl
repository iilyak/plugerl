-module(plugerl).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, stop/1]).

-export([register_plugin/2, register_plugin/3, unregister_plugin/2]).
-export([start_plugin/2, stop_plugin/2]).
-export([register_handler/5, unregister_handler/3]).

-export([plugins/1]).
-export([handlers/2]).

-export([call/4]).
-export([notify/3]).
-export([handle/4]).
-export([iterate/4, iterate/5]).
-export([funout/4]).

%% ------------------------------------------------------------------
%% Public API Function Definitions
%% ------------------------------------------------------------------

start_link(Engine) ->
    plugerl_sup:start_link(Engine).

stop(Engine) when is_atom(Engine) ->
    Sup = plugerl_registry:get_supervisor(Engine),
    stop(Sup);
stop(Sup) when is_pid(Sup) ->
    plugerl_sup:stop(Sup).

register_plugin(Engine, Type) ->
    register_plugin(Engine, Type, []).

register_plugin(Engine, Type, Options) ->
    plugerl_registry:register_plugin(Engine, Type, Options).

unregister_plugin(Engine, Type) ->
    plugerl_registry:unregister_plugin(Engine, Type).

start_plugin(Engine, Type) ->
    case plugerl_registry:lookup(Engine, Type) of
        undefined ->
            {error, {unregistered_plugin, Type}};
        Args ->
            plugerl_handlers_sup:start_child(self(), Type, Args)
    end.

stop_plugin(Engine, Type) ->
    case plugerl_registry:lookup(Engine, Type) of
        undefined ->
            ok;
        _Args ->
            plugerl_handlers_sup:terminate_child(Type)
    end.

register_handler(Engine, Type, Module, Args, Options) ->
    plugerl_registry:register_handler(Engine, Type, Module, Args, Options),
    plugerl_handler:start_handler(Type, Module, self()).

unregister_handler(Engine, Type, Module) ->
    plugerl_registry:unregister_handler(Engine, Type, Module),
    plugerl_handler:stop_handler(Type, Module),
    ok.

call(Type, Function, Default, Args) ->
    plugerl_handler:call(Type, Function, Default, Args).

notify(Type, Function, Args) ->
    plugerl_handler:notify(Type, Function, Args).

handle(Type, Function, Default, Args) ->
    plugerl_handler:handle(Type, Function, Default, Args).

iterate(Type, Function, Args, Fun) ->
    iterate(Type, Function, Args, [], Fun).

iterate(Type, Function, Args, Acc, Fun) ->
    plugerl_handler:iterate(Type, Function, Args, Acc, Fun).

funout(Engine, Type, Function, Args) ->
    Handlers = plugerl_registry:handlers(Engine, Type),
    plugerl_handler:funout(Handlers, Function, Args).


plugins(Engine) ->
    plugerl_registry:plugins(Engine).

handlers(Engine, Type) ->
    plugerl_registry:handlers(Engine, Type).
