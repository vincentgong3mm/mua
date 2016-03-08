-module(conn_man).
-export([
    start/0, 
    do_client_request/0,
    test_module_callback/1
    ]).
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
    
%% 아래와 같이 해서 module을 arg로 넘긴 후 callback호출 하듯이
%% 기본기능과 로직을 분리하면될듯.
%% 즉, tcp 또는 공통 module에 로직 구현을 필요로하는 module을 넘기고 
%% 로직 구현하는 module을 함수를 tcp또는 공통모듈에서 호출.    
test_module_callback(Handler) ->
    Handler:init(req, state),
    Handler:handle(recv_data, "---packet", state),
    Handler:terminate(a, b, c).

%% 1. 이렇게 함수의 함수를 spawn에 넘기거나,  
%% 2. 아니면 spawn(fun() -> procClientRequest() end) 이렇게.
%% 2번이 더 편할것 같음.
%%get_do_client_request() ->
%%    fun do_client_request/0.
    