-module(tcp_accept).
-include("mua_const.hrl").  % for ?LOG

-define (ACCEPT_TIMEOUT, 250).

-record(state, {parent, name, handler, port, listen_sock, log_accept_count = 0}).

-export([
    start_link/3
    ]).
-export([
    init/2, 
    loop/1
    ]).

start_link(Name, Handler, Port) ->
    State = #state{name = Name, handler = Handler, port = Port},
    %proc_lib:start_link(?MODULE, init, [self(), State]).
    proc_lib:start_link(?MODULE, init, [self(), State]).
    
init(Parent, State) ->
    Port = State#state.port,
    
    %% {packet, 0} : 좀더 찾아봐야하지만, 일반적인 소켓인 경우 0, erlang term주고 받을 때는 별도 사이즈 정의
    %% {active, false} : acitve, passive 구분, 일반적으로 특정 길이 만큼 받을 때 passive
    %% {reuseaddr, true} : time wait 소켓을 재사옹 하기 위해서. 하지 않으면 process종료 후 다시 listen할 때 에러발생
    {ok, ListenSock} = gen_tcp:listen(Port, 
                                [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    
    State2 = State#state{parent=Parent, listen_sock=ListenSock},
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(State2).
    
 loop(State) ->
    ?LOG(State),
    case gen_tcp:accept(State#state.listen_sock) of
    {ok, ClientSock} ->
        ?LOG("connect Client"),
        
        % create receive process for one client.
        tcp_receive:start_link(State#state.name, State#state.handler, ClientSock),
        
        Log_accept_count = State#state.log_accept_count, 
        State2 = State#state{log_accept_count = Log_accept_count + 1},
                
        % loop
        ?LOG(State2),
        
        loop(State2);
    _ ->
        
        % send to parent, termicate process
        ok
   end.
    
