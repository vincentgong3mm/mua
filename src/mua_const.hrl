
-define (debug, 1).

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p,~p}: ~p~n", [?MODULE, ?LINE, self(), X])).
-else.
-define(LOG(X), true).
-endif.
