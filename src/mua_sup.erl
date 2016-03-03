-module(mua_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
    
start_child() ->
    io:format("mua_sup start_chind~n"),
    supervisor:start_child(?MODULE, []).

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
    %{ok, { {one_for_one, 5, 10}, []} }.

