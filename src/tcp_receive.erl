-module(tcp_receive).
-include("mua_const.hrl").

-record(state, {parent, name, client_sock, handler, log_receive_count = 0}).

-export([
    start_link/3
    ]).
-export([
    init/2, 
    loop/1
    ]).

start_link(Name, ClientSock, Handler) ->
    State = #state{name=Name, client_sock = ClientSock, handler = Handler},
    proc_lib:start_link(?MODULE, init, [self(), State]).

init(Parent, State) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(State).
    
loop(State) ->
    ClientSock = State#state.client_sock,
    Handler = State#state.handler,
    case do_recv(ClientSock) of
        {ok, BinRecv} ->
            Handler:recv_packet({ok, BinRecv}),
        
            State2 = State#state.log_receive_count + 1,
            loop(State2);

        {error, close} ->
            Handler:recv_packet({error, close}),
            
            % maybe termicate process
            {error, close}
    end.    
        
do_recv(ClientSock) ->
    ?LOG("dorecv"),
    case gen_tcp:recv(ClientSock, 0) of
        {ok, BinRecv} ->
            %% \r\n제거 
            case binary:match(BinRecv, [<<"\r\n">>]) of
            {Pos, Length} ->  
                B2 = binary:part(BinRecv, {0, Pos}),
                ?LOG(B2),
                {ok, B2};
            nomatch ->
                ?LOG("nomatch ---------"),
                {error, close}  
            end;
        {error, close} ->
            {ok, list_to_binary([])}
      end.
    
