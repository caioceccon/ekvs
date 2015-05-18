-module(ekvs_telnetserv).

-behaviour(gen_server).

-compile([debug_info, export_all]).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER_HELLO, "Welcome to Erlang Key Value Store!\r\n"
                      "To add a item use `Add:Key:Value`\r\n"
                      "To get a item use `Get:Key`\r\n"
                      "To get all items use `GetAll:`\r\n").

start_link({tcp, Socket}) -> gen_server:start_link(?MODULE, {tcp, Socket}, []);
start_link({udp, Port}) -> gen_server:start_link(?MODULE, {udp, Port}, []).

%%% Server functions
init({tcp, Socket}) ->
    gen_server:cast(self(), accept),
    {ok, Socket};

init({udp, Port}) ->
    {ok, UdpSocket} = gen_udp:open(Port, [binary, {active,true}]),
    {ok, UdpSocket}.

handle_cast(accept, ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    ekvs_telnetserv_sup:start_socket(),
    send(tcp, AcceptSocket, ?SERVER_HELLO),
    {noreply, AcceptSocket}.

%%% TCP Handlers.
handle_info({tcp, _Socket, <<"Add:", KeyValue/binary>>}, Socket) ->
    send(tcp, Socket, store_data(KeyValue)),
    {noreply, Socket};

handle_info({tcp, _Socket, <<"Get:", Id/binary>>}, Socket) ->
    send(tcp, Socket, get_data_by_id(remove_blank_spaces(Id))),
    {noreply, Socket};

handle_info({tcp, _Socket, <<"GetAll:", _Rest/binary>>}, Socket) ->
    send(tcp, Socket, get_all_data()),
    {noreply, Socket};

handle_info({tcp_closed, _Socket}, Socket) ->
    {stop, normal, Socket};

handle_info({tcp, _Socket, Str}, Socket) ->
    Msg = io_lib:format(<<"Unknown command~p">>, [Str]),
    send(tcp, Socket, Msg),
    {noreply, Socket};

%%% UDP Handlers.
handle_info({udp, _UdpSocket, Address, Port, <<"Add:", KeyValue/binary>>}, Socket) ->
    Data = store_data(KeyValue),
    send(udp, {Socket, Address, Port}, Data),
    {noreply, Socket};

handle_info({udp, _UdpSocket, Address, Port, <<"Get:", Id/binary>>}, Socket) ->
    Data = get_data_by_id(remove_blank_spaces(Id)),
    send(udp, {Socket, Address, Port}, Data),
    {noreply, Socket};

handle_info({udp, _UdpSocket, Address, Port, <<"GetAll:", _Rest/binary>>}, Socket) ->
    Data = get_all_data(),
    send(udp, {Socket, Address, Port}, Data),
    {noreply, Socket};

handle_info({udp, _UdpSocket, Address, Port, <<Str/binary>>}, Socket) ->
    Msg = io_lib:format(<<"Unknown command~p">>, [Str]),
    send(udp, {Socket, Address, Port}, Msg),
    {noreply, Socket}.

terminate(normal, _Socket) ->
    io:format("Client ~p has disconnected.~n", [self()]),
    ok.

code_change(_OldVsn, Socket, _Extra) ->
    {ok, Socket}.

%% Private functions.
store_data(KeyValue) ->
    KeyValueStr = binary_to_list(KeyValue),
    case string:str(KeyValueStr, ":") of
        ColonPosition when ColonPosition > 0 ->
            Key = list_to_binary(string:substr(KeyValueStr, 1, ColonPosition - 1)),
            Value = list_to_binary(string:substr(KeyValueStr, ColonPosition + 1)),
            ekvs_db:store_data(Key, Value),
            ok;
        ColonPosition when  ColonPosition =:= 0 -> unknown_pattern
    end.

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

send(Protocol, Socket, <<BitStr/binary>>) ->
    send(Protocol, Socket, binary_to_list(BitStr));

send(Protocol, Socket, Atom) when is_atom(Atom) ->
    send(Protocol, Socket, atom_to_list(Atom));

send(tcp, Socket, Str) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", [])),
    ok = inet:setopts(Socket, [{active, once}]),
    ok;

send(udp, {Socket, Address, Port}, Str) ->
  gen_udp:send(Socket, Address, Port, Str).

remove_blank_spaces(Str) ->
    re:replace(Str, "(\\s+\\s+)", "", [{return,binary}]).
