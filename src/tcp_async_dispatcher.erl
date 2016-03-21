%% this is test behaviour

-module(tcp_async_dispatcher).

-callback recv_from_client(
    Pid :: pid(), 
    {
        Message :: tcp | tcp_close, 
        ClientSocket :: inet:socket(), 
        BinRecv :: binary()
    }) -> 
    ok.

-callback binary_from_client(
    Pid :: pid(), 
    { 
        ClientSocket :: inet:socket(), 
        BinRecv :: binary()
    }) -> 
    ok.

-callback disconnected_from_client(
    Pid :: pid(), 
    { 
        ClientSocket :: inet:socket()
    }) -> 
    ok.
