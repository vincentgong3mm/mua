-module(tcp_async_dispatcher).

-callback binary_from_client(Pid :: pid(), {ClientSocket :: inet:socket(), BinRecv :: binary()}) -> 
    ok.

%%-callback binary_from_client(
%%    Pid :: pid(), 
%%    {
%%        ClientSocket :: inet:socket(), 
%%        BinRecv :: binary()
%%     }) ->
%%    ok.
