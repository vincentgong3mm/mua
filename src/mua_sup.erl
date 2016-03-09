-module(mua_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0, 
    start_child/1,
    start_child/2
    ]).

%% Supervisor callbacks
-export([
    init/1
    ]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
    
start_child(accept) ->
    io:format("mua_sup start_chind accept~n"),
   
    supervisor:start_child(?MODULE, [8088]).
    
%%start_child(receive, {ClientSock, Handler}) ->
start_child(recv_packet, {ClientSock, Handler}) ->
    io:format("mua_sup start_chind receive~n"),
    
    Receive = {tcp_receive, {tcp_receive, start_link, [ClientSock, Handler]},
                  permanent, 2000, worker, [tcp_receive]},

    supervisor:start_child(?MODULE, Receive).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    io:format("mua_sup int~n"),
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    Accept = {tcp_accept, {tcp_accept, start_link, []},
                  permanent, 2000, worker, [tcp_accept]},

    %%Receive = {tcp_receive, {tcp_receive, start_link, [1000, conn_man]},
    %%              permanent, 2000, worker, [tcp_receive]},
    
    {ok, {SupFlags, [Accept]}}.              
    %%{ok, {SupFlags, [Accept, Receive]}}.
    %%{ok, {SupFlags, [Accept, Receive]}}.
    