-module(tcp_async_server).
-behavior(gen_server).

-export([
    start_link/0
]).

start_link() ->
    ok.
    
    