-module(mua_sup).
-include("mua_const.hrl").  % for ?LOG
-behaviour(supervisor).

%% API
-export([
    start_link/0, 
    start_child/1,
    start_child/2,
    start_child/3,
    start_all_child/0
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

start_all_child() ->
    start_child(accept, {server1, 8088}),
    start_child(accept, {server2, 8089}),
    tcp_accept:accept().


start_child(test) ->
    ?LOG("start child test"),
    supervisor:start_child(?MODULE, []).
    
% test start child tcp_accept module
start_child(accept, {Name, Port}) ->
    io:format("mua_sup start_chind accept~n"),
    supervisor:start_child(?MODULE, [Name, temp_handle, Port]);
start_child(recv_packet, {ClientSock, Handler}) ->
    io:format("mua_sup start_chind receive~n"),
    
    Receive = {tcp_receive, {tcp_receive, start_link, [ClientSock, Handler]},
                  permanent, 2000, worker, [tcp_receive]},

    supervisor:start_child(?MODULE, Receive).

start_child(listener, {Name, Module, Pid}, Port) ->
    ?LOG("start child listener"),
    supervisor:start_child(?MODULE, [Name, {temp_handle, temp_pid}, Port]).


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

    AsyncListner = {tcp_async_listener, {tcp_async_listener, start_link, []},
                  permanent, 2000, worker, [tcp_async_listener]},

    TestHandler = {tcp_test_async_handler, {tcp_test_async_handler, start_link, []},
                  permanent, 2000, worker, [tcp_test_async_handler]},


    %%Receive = {tcp_receive, {tcp_receive, start_link, [1000, conn_man]},
    %%              permanent, 2000, worker, [tcp_receive]},
    
    {ok, {SupFlags, [TestHandler]}}.
    %{ok, {SupFlags, [AsyncListner]}}.
    %{ok, {SupFlags, [Accept, AsyncListner]}}.              
    %%{ok, {SupFlags, [Accept, Receive]}}.
    %%{ok, {SupFlags, [Accept, Receive]}}.
    