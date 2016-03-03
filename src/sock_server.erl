-module(sock_server).
-behaviour(gen_server).

-record(state, {socket}).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).
    
init(Socket) ->
    io:format("sock_server init~n"),
    {ok, #state{socket=Socket}}.
    
%% We never need you, handle_call!
handle_call(_E, _From, State) ->
    {noreply, State}.

handle_cast(accept, State) ->
    {ok, State}.
    
handle_info(E, S) ->
    io:format("unexpected: ~p~n", [E]),
    {noreply, S}.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(normal, _State) ->
    ok;
terminate(_Reason, _State) ->
    io:format("terminate reason: ~p~n", [_Reason]).