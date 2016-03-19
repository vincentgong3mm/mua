-module(tcp_test_async_handler).
-include("mua_const.hrl").  % for ?LOG
-behavior(gen_server).
-behavior(tcp_async_dispatcher).

-export([
    start_link/0,
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

start_link() ->
    ok.
init([State]) ->
    {ok, State}.

dispatch(Pid, {tcp, Socket, BinRecv}) ->
    gen_server:cast(Pid, {tcp, Socket, BinRecv}).
    
terminate(_Reason, _State) ->
    ok.

handle_call({}, _From, State) ->
    {reply, _From, State}.

handle_cast({tcp, Socket, BinRecv}, State) ->
    ?LOG({handle_cast, tcp, Socket, BinRecv, State}),
    ?LOG({<<"do game logic.............">>}),
    {noreply, State}.
    
%%handle_cast({}, State) ->
%%    {noreply, State}.
    
handle_info({}, State) ->
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

