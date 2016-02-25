-module(conn_man).
-export([start/0, do_client_request/0]).
-import(tcp, [start/3]).

start() ->
    %%createServer(8088, 10, get_do_client_request()).
    tcp:start(8088, 10, fun() -> do_client_request() end).

do_client_request() ->
    receive 
    {ok, BinRecv} ->
        io:format("conn_man:do_client_request Recv='~p'~n", [BinRecv]),
        do_client_request();
    _ ->
        ok
    end.

%% 1. 이렇게 함수의 함수를 spawn에 넘기거나,  
%% 2. 아니면 spawn(fun() -> procClientRequest() end) 이렇게.
%% 2번이 더 편할것 같음.
%%get_do_client_request() ->
%%    fun do_client_request/0.
    