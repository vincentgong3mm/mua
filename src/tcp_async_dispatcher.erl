-module(tcp_async_dispatcher).

-callback dispatch(Pid, {tcp, ClientSocket, BinRecv} | Pid, {tcp_close, ClientSocket}) ->
    {ok}
    when ClientSocket :: inet:socket(), BinRecv :: binary().
