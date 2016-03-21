-module(tcp_async_listener).
-include("mua_const.hrl").
-define (ACCEPT_TIMEOUT, 250).

%% 하다보니, 가능하면 record를 처음 만들 때 초기화 가능한게 좋은것 같음.
%% 그렇지 않으면 type을 정할 때 | 로 undefined를 해야함.  
-record(state, {
    parent :: pid(), % parent pid
    name :: atom(),  % atom 
    %handler :: module(), % module
    handler :: {module(), pid()}, % module, pid 
    port = 0 :: non_neg_integer() ,  % >= 0 
    listen_sock = undefined :: undefined | inet:socket(), % socket
    log_accept_count = 0 :: non_neg_integer(),  % >= 0
    receiver_pid = undefined :: undefined | pid()   % pid
    }).

-export([
    start_link/3
    ]).
    
-export([
    init/2, 
    loop/1
    ]).
    
start_link(Name, {Module, Pid}, Port) ->
    State = #state{parent = self(), name = Name, handler = {Module, Pid}, port = Port},
    proc_lib:start_link(?MODULE, init, [self(), State]).
       
init(Parent, State) ->
    ?LOG("init"),
    Port = State#state.port,
    
    %% {packet, 0} : 좀더 찾아봐야하지만, 일반적인 소켓인 경우 0, erlang term주고 받을 때는 별도 사이즈 정의
    %% {active, true} : acitve, recv 호출하지 않고 시스템으로 부터 이벤트를 받게 하도록할 때 설정, {active, once}도 같이 참고해야함.
    %% {reuseaddr, true} : time wait 소켓을 재사옹 하기 위해서. 하지 않으면 process종료 후 다시 listen할 때 에러발생
    {ok, ListenSock} = gen_tcp:listen(Port, 
                                [binary, {packet, 0}, {active, true}, {reuseaddr, true}]),
    
    
    proc_lib:init_ack(Parent, {ok, self()}),
    
    %% 여러 client socket을 하나의 process에서 messgae를 받을 수 있나 테스트 하기 
    {ok, Pid} = tcp_async_receiver:start_link(State#state.handler),
    State2 = State#state{parent=Parent, listen_sock=ListenSock, receiver_pid = Pid},
    
    loop(State2).
    
 loop(State) ->
    ?LOG(State),
    case gen_tcp:accept(State#state.listen_sock) of
    {ok, ClientSock} ->
        ?LOG("connect Client"),
        
        % create receive process for one client.
        %%tcp_receive:start_link(State#state.name, State#state.handler, ClientSock),
        %%{ok, Pid} = tcp_async_receiver:start_link(),
        
        %% ClientSocket으로 부터 발생하는 이벤트는 이 프로세서에서 받기 위한 설정
        %% ClientSocket에서 뭔가 발생하면 이 프로세스의 handle_info호출됨
        %% 여기서 하면 안됨. 반듯이 accept 받은 process해야함
        %% 여기서 하면 {error,not_owner} 발생함
        gen_tcp:controlling_process(ClientSock, State#state.receiver_pid),
        tcp_async_receiver:set_socket(State#state.receiver_pid, ClientSock),
        
        Log_accept_count = State#state.log_accept_count, 
        State2 = State#state{log_accept_count = Log_accept_count + 1},
                
        % loop
        ?LOG(State2),
        
        loop(State2);
    _ ->
        
        % send to parent, termicate process
        ok
   end.
    
