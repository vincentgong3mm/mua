-module(tcp_receive).
-behaviour(gen_server).

-record(state, {clientSock, handler, log_receive_count = 0}).

-export([
    start_link/2
    ]).
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    code_change/3, 
    terminate/2
    ]).

start_link(ClientSock, Handler) ->
    gen_server:start_link(?MODULE, [ClientSock, Handler], []).
    
init([ClientSock, Handler]) ->
    {ok, #state{clientSock=ClientSock, handler=Handler}}.
    
handle_call({recv_packet}, _From, State) ->
    ClientSock = State#state.clientSock,
    Handler = State#state.handler,
    case do_recv(ClientSock) of
        {ok, BinRecv} ->
            Handler:recv_packet({ok, BinRecv}),
        
            State2 = State#state.log_receive_count + 1,
            recv_packet(),
        
            {reply, State2};
        {error, close} ->
            Handler:recv_packet({error, close}),
            % maybe termicate process
            recv_packet(),
            {reply, State}
    end.    
        
do_recv(ClientSock) ->
    case gen_tcp:recv(ClientSock, 0) of
        {ok, BinRecv} ->
            %% \r\n제거 
            case binary:match(BinRecv, [<<"\r\n">>]) of
            {Pos, Length} ->  
                B2 = binary:part(BinRecv, {0, Pos}),
                %%io:format("recv=[~p]\n", [B2]),
                {ok, B2};
            nomatch ->
                io:format("nomatch ---------"),
                {error, close}  
            end;
        {error, close} ->
            {ok, list_to_binary([])}
      end.

handle_cast(accept, State) ->
    {noreply, State}.
    
handle_info(E, S) ->
    io:format("unexpected: ~p~n", [E]),
    {noreply, S}.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(normal, _State) ->
    ok;
terminate(_Reason, _State) ->
    ok.
    
recv_packet() ->
    gen_server:call(?MODULE, {recv_packet}).
    
    
