-module(tcp_accept).

-define (debug, 1).

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.

-define (ACCEPT_TIMEOUT, 250).

-record(state, {parent, name, port, listenSock, log_accept_count = 0}).

-export([
    start_link/2
    ]).
-export([
    init/2, 
    loop/1
    ]).

start_link(Name, Port) ->
    State = #state{name=Name, port=Port},
    proc_lib:start_link(?MODULE, init, [self(), State]).
    
init(Parent, State) ->
    Port = State#state.port,
    {ok, ListenSock} = gen_tcp:listen(Port, 
                                [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    
    State2 = State#state{parent=Parent, listenSock=ListenSock},
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(State2).
    
 loop(State) ->
    %io:format("~p handle_call, accept pid=~p~n", [?MODULE, self()]),
    ?LOG(State),
    case gen_tcp:accept(State#state.listenSock) of
    {ok, ClientSock} ->
        Log_accept_count=State#state.log_accept_count, 
        State2 = State#state{log_accept_count = Log_accept_count + 1},
        
        % spawn recv process 
        ?LOG("connect Client"),
        
        % loop
        ?LOG(State2),
        loop(State2);
    _ ->
        
        % send to parent, termicate process
        ok
   end.
    
