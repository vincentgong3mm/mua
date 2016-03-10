-module(tcp_handler).
-behavior(tcp_dispatch).

-include("mua_const.hrl").  % for ?LOG

-export([
    dispatch/1
    ]).

dispatch({recv, ClientSock, Packet}) ->
    ?LOG(Packet),
    
    tcp_receive:send(ClientSock, <<"echo:">>),
    tcp_receive:send(ClientSock, Packet),
    tcp_receive:send(ClientSock, <<"\r\n">>),
    
    {ok};

dispatch({disconnect}) ->
    ?LOG(disconnect),
    {ok}.
