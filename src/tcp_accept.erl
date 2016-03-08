-module(tcp_accept).
-behaviour(gen_server).

-record(state, {port, listenSock, log_accept_count = 0}).

-export([
    start_link/1
    ]).
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    code_change/3, 
    terminate/2
    ]).

start_link(Port) ->
    gen_server:start_link(?MODULE, [Port], []).
    
init([Port]) ->
    {ok, ListenSock} = gen_tcp:listen(Port, [binary, {packet, 0}, 
                                        {active, false}, {reuseaddr, true}]),
    
    {ok, #state{port=Port, listenSock=ListenSock}}.
    

handle_call({accept}, _From, State) ->
    case gen_server:accept(State#state.listenSock) of
    {ok, ClientSock} ->
        State2 = State#state.log_accept_count + 1,
        accept(),
        {reply, State2};
    _ ->
        {reply, State}
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
    
accept() ->
    gen_server:call(?MODULE, {accept}).