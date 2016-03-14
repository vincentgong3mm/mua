-module(tcp_async_receiver).
-include("mua_const.hrl").  % for ?LOG
-behavior(gen_server).
-export([
    start_link/0,
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

-record(state, {clientSocket, socketState}).

start_link() ->
    
    gen_server:start_link(?MODULE,  % Module 
                        [], % Arg
                        []).    % Opt
    
init([]) ->
    State = #state{clientSocket = 0, socketState = 0},
    {ok, State}.
terminate(_Reason, _State) ->
    ok.
    
set_socket(Pid, ClientSocket) ->
    ?LOG({Pid, ClientSocket}),
    gen_server:call(Pid, {set_socket, ClientSocket}).
    
handle_call({set_socket, ClientSocket}, _From, State) ->
    State2 = State#state{clientSocket = ClientSocket},
    
    %% ClientSocket으로 부터 발생하는 이벤트는 이 프로세서에서 받기 위한 설정
    %% ClientSocket에서 뭔가 발생하면 이 프로세스의 handle_info호출됨
    %% 여기서 하면 안됨. 반듯이 accept 받은 process해야함
    %% 여기서 하면 {error,not_owner} 발생함 
    %% Ret = gen_tcp:controlling_process(ClientSocket, self()),
    {reply, _From, State2}.
    
handle_cast(Request, State) ->
    {noreply, State}.
    
handle_info({tcp, Socket, Bin}, State) ->
    ?LOG({tcp, ",", Socket, ", ", Bin}),
    {noreply, State};

handle_info({tcp_closed, Socket}, State) ->
    ?LOG({tcp_closed, ",", Socket}),
    {noreply, State}.


%%handle_info(Info, State) ->
%%    {noreply, State}.

    
code_change(OldVsn, State, Extra) ->
    {ok, State}.
    