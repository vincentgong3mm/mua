-module(tcp_dispatch).

-callback dispatch({recv, ClientSock, BinRecv} | {disconnect}) ->
    {ok} | {error}
    when ClientSock::any(), BinRecv::binary().