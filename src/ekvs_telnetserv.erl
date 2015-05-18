-module(ekvs_telnetserv).

-behaviour(gen_server).

-compile([debug_info, export_all]).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER_HELLO, "Welcome to Erlang Key Value Store!").

start_link(Socket) -> gen_server:start_link(?MODULE, Socket, []).

%%% Server functions
init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, Socket}.

handle_cast(accept, ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    ekvs_telnetserv_sup:start_socket(),
    send(AcceptSocket, ?SERVER_HELLO, []),
    {noreply, AcceptSocket}.

handle_info({tcp, _Socket, <<"Add:", KeyValue/binary>>}, Socket) ->
    send(Socket, store_data(KeyValue), []),
    {noreply, Socket};

handle_info({tcp, _Socket, <<"Get:", Id/binary>>}, Socket) ->
    send(Socket, get_data_by_id(remove_blank_spaces(Id)), []),
    {noreply, Socket};

handle_info({tcp, _Socket, <<"GetAll:", _Rest/binary>>}, Socket) ->
    send(Socket, get_all_data(), []),
    {noreply, Socket};

handle_info({tcp_closed, _Socket}, Socket) ->
    {stop, normal, Socket};

handle_info({tcp, _Socket, Str}, Socket) ->
    send(Socket, <<"Unknown command~p">>, [Str]),
    {noreply, Socket}.

terminate(normal, _Socket) ->
    io:format("Client ~p has disconnected.~n", [self()]),
    ok.

code_change(_OldVsn, Socket, _Extra) ->
    {ok, Socket}.

%% Private functions.
store_data(KeyValue) ->
    KeyValueStr = binary_to_list(KeyValue),
    ColonPosition = string:str(KeyValueStr, ":"),
    Key = list_to_binary(string:substr(KeyValueStr, 1, ColonPosition - 1)),
    Value = list_to_binary(string:substr(KeyValueStr, ColonPosition + 1)),
    ekvs_db:store_data(Key, Value),
    ok.

get_data_by_id(Id) ->
    case ekvs_db:get_by_id(Id) of
        {ok, Value} ->
            format_key_value(Id, Value);
        _ -> "Not found"
    end.

get_all_data() ->
    FormatedKeyValue = [format_key_value(K,V) || {K,V} <- ekvs_db:list_all()],
    string:join(FormatedKeyValue, ", ").

format_key_value(Key, Value) ->
    io_lib:format("key:~p -> value:~p", [binary_to_list(Key), binary_to_list(Value)]).

send(Socket, <<BitStr/binary>>, Args) ->
    send(Socket, binary_to_list(BitStr), Args);

send(Socket, Atom, Args) when is_atom(Atom) ->
    send(Socket, atom_to_list(Atom), Args);

send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.

remove_blank_spaces(Str) ->
    re:replace(Str, "(\\s+\\s+)", "", [{return,binary}]).
