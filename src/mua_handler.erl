-module(mua_handler).

-export([
    init/2,
    handle/2,
    handle/3,
    terminate/3
]).


init(Req, State) ->
    io:format("~p:~p~n", [?MODULE, ?LINE]),
    {ok, State}.
    
handle(connect, State) ->
    io:format("~p:~p~n", [?MODULE, ?LINE]),
    {ok, State};
handle(disconnect, State) ->
    io:format("~p:~p~n", [?MODULE, ?LINE]),
    {ok, State};
handle(error, State) ->
    io:format("~p:~p~n", [?MODULE, ?LINE]),
    {ok, State}.
    
handle(recv_data, BinPacket, State) ->
    io:format("~p:~p~n", [?MODULE, ?LINE]),
    {ok, State}.
    
terminate(_Reason, _Req, _State) ->
    io:format("~p:~p~n", [?MODULE, ?LINE]),
    ok.
    