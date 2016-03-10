-module(tcp_receive).
-include("mua_const.hrl").  % for ?LOG

%% receve 프로세스에서 참조할 값들
-record(state, {
    parent, % parent process id 
    name, % 로그용 프로세스이름
    handler, % 패킷받았을 때 호출할 Moudle
    client_sock,  % client socket
    log_receive_count = 0   % 로그용. 패킷받은 횟수
    }).

-export([
    start_link/3,
    init/2,
    send/2
    ]).

start_link(Name, Handler, ClientSock) ->
    State = #state{name=Name, client_sock = ClientSock, handler = Handler},
    
    % 프로레스 생성
    proc_lib:start_link(?MODULE, init, [self(), State]).

init(Parent, State) ->
    State2 = State#state{parent=Parent},
    
    % proc_lib:start_link를 생성하면 반드시 init_ack해줘야함.
    proc_lib:init_ack(Parent, {ok, self()}),
    
    % init_ack하고 loop
    loop(State2),
    
    % loop가 끝나면 프로세스 종료하는 것임.
    ?LOG("terminate process").
    
loop(State) ->
    ?LOG(State),
    ClientSock = State#state.client_sock,
    Handler = State#state.handler,
    case do_recv(ClientSock) of
        {ok, BinRecv} ->
            ?LOG(BinRecv),
            
            % call handler function
            Handler:dispatch({recv, ClientSock, BinRecv}),
           
            % 그냥 로그를 위해서 패킷받은 횟수 저장
            Log_receive_count = State#state.log_receive_count, 
            State2 = State#state{log_receive_count = Log_receive_count + 1},
            
            % 다시 recv하기 위해서 loop
            loop(State2);
        
        {error, close} ->
            gen_tcp:close(ClientSock),
            
            % call handler function
            Handler:dispatch({disconnect}),
            {error, close}
    end.    
        
do_recv(ClientSock) ->
    case gen_tcp:recv(ClientSock, 0) of
        {ok, BinRecv} ->
            %% \r\n제거 
            case binary:match(BinRecv, [<<"\r\n">>]) of
            {Pos, _Length} ->  
                B2 = binary:part(BinRecv, {0, Pos}),
                
                % echo
                gen_tcp:send(ClientSock, <<"echo:">>),
                gen_tcp:send(ClientSock, B2),
                gen_tcp:send(ClientSock, <<"\r\n">>),
                
                {ok, B2};
            nomatch ->
                % 테스트로 \r\n이 없는 경우(console에서 ctrl+C)는 서버가 끊어졌다고 인정.
                {error, close}  
            end;
        {error, close} ->
            ?LOG("disconnect??"),
            {ok, list_to_binary([])}
      end.

%% 구현해야함 
send(ClientSock, BinData) ->
    gen_tcp:send(ClientSock, BinData),
    ok.
