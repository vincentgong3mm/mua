-module(tcp).
%% spawn 으로 생성하는 함수도 export 해야한다. 하지 않으면 아래 에러 발생함.
%% {undef,[{tcpModule,procClientRequest,[#Port<0.2352>,0],[]}]}
-export([start/3, accept_process/1, do_recv/1]).
-record(status, {processId, processName, doRequestFun, doRequestProcessId}).


start(Port, AcceptProcessCount, DoRequestFun) ->
    %% {packet, 0} : 좀더 찾아봐야하지만, 일반적인 소켓인 경우 0, erlang term주고 받을 때는 별도 사이즈 정의
    %% {active, false} : acitve, passive 구분, 일반적으로 특정 길이 만큼 받을 때 passive
    %% {reuseaddr, true} : time wait 소켓을 재사옹 하기 위해서. 하지 않으면 process종료 후 다시 listen할 때 에러발생
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, 
                                        {active, false}, {reuseaddr, true}]),
                                        
    spawn_accept_process(AcceptProcessCount, LSock, DoRequestFun).

%% 설정한 개수만큼 accept process 생성함
spawn_accept_process(0, _, _) ->
    0;
spawn_accept_process(Count, ListenSocket, DoRequestFun) ->
    Status = #status{doRequestFun = DoRequestFun},
    spawn(?MODULE, accept_process, [{ListenSocket, Status}]),
    spawn_accept_process(Count - 1, ListenSocket, DoRequestFun).
   
%% 클라이언트가 접속하면 프로세스 하나 생성해서 담당해서 recv 처리하도록함.
%% 생성한 프로레스에서 recv후 패키 파싱 후 처리 해도 될겠지만
%% 상세로직은 콜백함수 처리 또는 로직을 처리하는 프로세스로 던지는 것으로 처리하는게 좋을 듯.
%% 콜백 함수 보다는 프로세스가 좋겠음. 콜백함수로 처리하면 recv가 지연될 수 있을 것 같음. 
accept_process({ListenSocket, Status}) ->
    io:format("accept_process ProcessId=~p~n", [self()]),
    
    case gen_tcp:accept(ListenSocket) of 
    {ok, ClientSocket} ->

        DoRequestProcessId = spawn(Status#status.doRequestFun),
        %%ProcRequestProcessId = spawn(connectionManager, procClientRequest, []),
        
        NewStatus = Status#status{doRequestProcessId = DoRequestProcessId},
        
        Pid = spawn(?MODULE, do_recv, [{ClientSocket, NewStatus}]),
        accept_process({ListenSocket, Status});
    _ ->
        exit
    end.
    
do_recv({ClientSocket, Status}) ->
    io:format("do_recv ProcessId=~p~n", [self()]),
    case recv(ClientSocket) of
    {ok, BinRecv} ->
        Status#status.doRequestProcessId ! {ok, BinRecv},
        
        do_recv({ClientSocket, Status});
    {error, close} ->
        Status#status.doRequestProcessId ! {error, close},
        ok
    end.    
          
%% 일반적인 사이즈 받고 사이즈 만큼 받는 것 처리할려면
%% recv 아래 함수에서 recv 0이 아닌 size 하고
%% procRecv에서 사이즈 받고, 바디 받고 하면됨
recv(ClientSocket) ->
    case gen_tcp:recv(ClientSocket, 0) of
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
           
