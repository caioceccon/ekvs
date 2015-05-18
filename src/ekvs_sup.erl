-module(ekvs_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Port = case application:get_env(port) of
        {ok, P} -> list_to_integer(P);
        undefined -> 9023
    end,

    {ok, {{one_for_one, 5, 10},
          [?CHILD(ekvs_db, worker, []),
           ?CHILD(ekvs_telnetserv_sup, supervisor, [Port]),
           ?CHILD(ekvs_telnetserv, worker, [{udp, Port+1}])]
         }
    }.
