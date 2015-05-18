-module(ekvs_db).

-behaviour(gen_server).

-compile([debug_info]).

-export([start_link/0, store_data/2, get_by_id/1, list_all/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Client API.
start_link() -> gen_server:start_link({local, dbserv}, ?MODULE, [], []).

store_data(Key, Value) ->
    gen_server:call(dbserv, {store, Key, Value}).

get_by_id(Id) ->
    gen_server:call(dbserv, {id, Id}).

list_all() ->
    gen_server:call(dbserv, list).

%%% Server functions
init([]) -> {ok, orddict:new()}.

handle_call({store, Key, Value}, _From, Dict) when is_bitstring(Key),
                                                   is_bitstring(Value) ->
    {reply, ok, orddict:store(Key, Value, Dict)};

handle_call({id, Id}, _From, Dict) when is_bitstring(Id) ->
    {reply, orddict:find(Id, Dict), Dict};

handle_call(list, _From, Dict) ->
    {reply, orddict:to_list(Dict), Dict}.

handle_cast(_, Dict) ->
    {noreply, Dict}.

handle_info(Msg, Dict) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, Dict}.

terminate(normal, _Dict) ->
    ok.

code_change(_OldVsn, Dict, _Extra) ->
    {ok, Dict}.