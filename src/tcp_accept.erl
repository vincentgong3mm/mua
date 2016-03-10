-module(tcp_accept).
-include("mua_const.hrl").

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
    proc_lib:start_link(?MODULE, init, [self(), State]).
    
init(Parent, State) ->
    Port = State#state.port,
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
    
