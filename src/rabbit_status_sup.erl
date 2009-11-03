-module(rabbit_status_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

init([]) ->
    Status = {rabbit_status_web,
             {rabbit_status_web, start_link, []},
              permanent, 5000, worker, dynamic},
 
    {ok, {{one_for_one, 10, 10}, [Status]}}.
    
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
