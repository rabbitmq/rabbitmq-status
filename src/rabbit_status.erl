-module(rabbit_status).

-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
start() ->
    ensure_started(rabbit_mochiweb),
    application:start(rabbit_status).
 
stop() ->
    application:stop(rabbit_status).