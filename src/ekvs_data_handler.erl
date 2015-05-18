-module(ekvs_data_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
    ok.

allowed_methods(Req, State) ->  
    {[<<"POST">>, <<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle}], Req, State}.

handle(Req, State) ->
    {Path, Req1} = cowboy_req:path(Req),
    {Method, Req2} = cowboy_req:method(Req1),
    data_handle(Path, Method, Req2, State).

data_handle(<<"/data/">>, <<"GET">>, Req, State) ->
    Msg = jsx:encode(get_all_data()),
    {ok, Res1} = cowboy_req:reply(200, [], Msg, Req),
    {ok, Res1, State};

data_handle(<<"/data/", Id/binary>>, <<"GET">>, Req, State) ->
    {Msg, Code} = case get_data_by_id(Id) of
        {ok, Data} ->
            {jsx:encode(Data), 200};
        {error} ->
            {jsx:encode(<<"Id: not found">>), 404}
    end,

    {ok, Res1} = cowboy_req:reply(Code, [], Msg, Req),
    {ok, Res1, State};

data_handle(<<"/data/", _Id/binary>>, <<"POST">>, Req, State) ->
    {ok, Bin, Req1} = cowboy_req:body(Req),
    
    {Msg, Code} = case get_key_value(Bin) of
        {error, not_a_json} ->
            {io_lib:format("Is not a real Json:~p", [Bin]), 422};
        {ok, undefined, _} ->
            {io_lib:format("Missing key on Json:~p", [Bin]), 422};
        {ok, _, undefined} ->
            {io_lib:format("Missing value on Json:~p", [Bin]), 422};
        {ok, Key, Value} ->
            store_data(Key, Value),
            {jsx:encode(<<"ok">>), 200}
    end,

    {ok, Req2} = cowboy_req:reply(Code, [], Msg, Req1),
    {ok, Req2, State};

data_handle(Path, Method, Req, State) ->
    Msg = io_lib:format("Method:~p, Path:~p", [Method, Path]),
    {ok, Req1} = cowboy_req:reply(200, [], Msg, Req),
    {ok, Req1, State}.

%% Private Funcitons
get_key_value(BinaryJson) when is_binary(BinaryJson) ->
    case jsx:is_json(BinaryJson) of
        true ->
            DecodeJson = jsx:decode(BinaryJson),
            {ok, proplists:get_value(<<"key">>, DecodeJson),
                proplists:get_value(<<"value">>, DecodeJson)};
        false ->
            {error, not_a_json}
    end.

store_data(Key, Value) ->
    ekvs_db:store_data(Key, Value),
    ok.

get_data_by_id(Id) ->
    case ekvs_db:get_by_id(Id) of
        {ok, Value} ->
            {ok, [{<<"key">>,Id},{<<"value">>,Value}]};
        {_} -> error
    end.

get_all_data() ->
    ekvs_db:list_all().