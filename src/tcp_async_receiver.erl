-module(tcp_async_receiver).
-include("mua_const.hrl").  % for ?LOG
-behaviour(gen_server).
-export([
    start_link/1,
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-export([
   set_socket/2
]).


-record(client_socket, {
    socket :: inet:socket() , % 없어도되지만 테스트 용으로 추가 
    state = none :: none | wait | closed,   % c의 enum 처럼 해봄 
    etc}).

%%-type http_headers() :: [{binary(), iodata()}].

-record(state, {
    handler :: {module(), pid()},   % {module, pid}
    %sockets = [] :: [] | [#client_socket{}]    % list로 테스트 하기 
    sockets = #{} :: map()  % map으로 테스트 하기 
    }).

start_link(Handler) ->
    State = #state{handler = Handler, sockets = maps:new()},
    gen_server:start_link(?MODULE,  % Module 
                        [State], % Arg
                        []).    % Opt
                            
init([State]) ->
    ?LOG({init, State}),
    {ok, State}.    
terminate(_Reason, _State) ->
    ok.
    
set_socket(Pid, ClientSocket) ->
    ?LOG({Pid, ClientSocket}),
    gen_server:call(Pid, {set_socket, ClientSocket}).
    
handle_call({set_socket, ClientSocket}, _From, State) ->
    %% 새로운 socket record생성
    NewClientSocket = #client_socket{socket = ClientSocket, state = wait, etc = 0},
    
    %% 새로운 socket + 현재까지 저장된 socket 저장
    %State2 = #state{sockets=[NewClientSocket | State#state.sockets]},   % for list test
    
    %% State를 유지하고 변경해야함. 아래는 잘못된것 
    %%%State2 = #state{sockets = 
    %%%                    maps:put(ClientSocket, NewClientSocket, State#state.sockets)
    %%%                },
    State2 = State#state{sockets = 
                        maps:put(ClientSocket, NewClientSocket, State#state.sockets)
    },
                    
                    
    ?LOG({handle_call, State2}),
    
    %% State2 = State#state{clientSocket = ClientSocket},
    
    %% ClientSocket으로 부터 발생하는 이벤트는 이 프로세서에서 받기 위한 설정
    %% ClientSocket에서 뭔가 발생하면 이 프로세스의 handle_info호출됨
    %% 여기서 하면 안됨. 반듯이 accept 받은 process해야함
    %% 여기서 하면 {error,not_owner} 발생함 
    %% Ret = gen_tcp:controlling_process(ClientSocket, self()),
    {reply, _From, State2}.
    
handle_cast(Request, State) ->
    {noreply, State}.
    
%% 클라이언트로 부터 데이터가 왔을 때 시스템으로 부터 메시지 받음.
handle_info({tcp, Socket, Bin}, State) ->
    ?LOG({tcp, ",", Socket, ", ", Bin, State}),
    
    ?LOG(State#state.handler),
    
    {Module, Pid} = State#state.handler,
    Module:binary_from_client(Pid, {Socket, Bin}),
    
    {noreply, State};

%% 클라이언트가 끊어졌을 때 시스템으로 부터 메시지 받음.
handle_info({tcp_closed, Socket}, State) ->
    ?LOG({tcp_closed, ",", Socket}),
 
    State2 = #state{
                    sockets = maps:remove(Socket, State#state.sockets)
                    },                
    ?LOG(State2),
    
    {Module, Pid} = State#state.handler,
    Module:disconnected_from_client(Pid, {Socket}),
    
    {noreply, State2}.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
    