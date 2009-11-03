-module(rabbit_status_app).

-behaviour(application).
-export([start/2, stop/1]).


start(_Type, _StartArgs) ->
    Res = rabbit_status_sup:start_link(),
    rabbit_mochiweb:register_global_handler(
                fun(Req) ->
                    rabbit_status_web:handle_request_unauth(Req)
                end),
    Res.
    
stop(_State) ->
    ok.
