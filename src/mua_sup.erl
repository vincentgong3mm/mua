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
    init/1,
    init/2
    ]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
    
start_child(listener) ->
    io:format("mua_sup start_chind listener~n"),
    supervisor:start_child(?MODULE, []).

start_child(receiver, {ClientSock, Handler}) ->
    io:format("mua_sup start_chind receiver~n"),
    supervisor:start_child(?MODULE, [tpc_receiver, ClientSock, Handler]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    io:format("mua_sup int~n"),
    {ok, {{simple_one_for_one, 5, 10}, 
         [{testProcess, 
          {sock_server, start_link, [10000]},
          temporary, 1000, worker, [sock_server]}
         ]}}.

init(tcp_recevier, {ClientSock, Handler}) ->
    io:format("mua_sup int tcp_receiver ~n"),
    {ok, {{simple_one_for_one, 5, 10}, 
         [{testProcess, 
          {sock_server, start_link, [ClientSock, Handler]},
          temporary, 1000, worker, [sock_server]}
         ]}}. 
         
         
         
    %{ok, { {one_for_one, 5, 10}, []} }.

