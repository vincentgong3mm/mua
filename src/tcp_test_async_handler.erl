-module(tcp_test_async_handler).
-include("mua_const.hrl").  % for ?LOG
-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-record(state, {
    etc :: any()
    }).

start_link() ->
    gen_server:start_link(?MODULE, [], []),
    ok.
init([]) ->
    State = #state{etc = test_value},
    {ok, State}.
    
terminate(_Reason, _State) ->
    ok.

handle_call({}, _From, State) ->
    {reply, _From, State}.

handle_cast({tcp, Socket, BinRecv}, State) ->
    ?LOG({handle_cast, tcp, Socket, BinRecv, State}),
    ?LOG({<<"do game logic.............">>}),
    {noreply, State};

handle_cast({tcp_closed, Socket}, State) ->
    ?LOG({handle_cast, tcp_closed, Socket, State}),
    ?LOG({<<"disconnected client. must exit process?.............">>}),
    {noreply, State}.
    
%%handle_cast({}, State) ->
%%    {noreply, State}.
    
handle_info({}, State) ->
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

