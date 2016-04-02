-module(macros).
-export([message/0, whoami/0]).

-define(MESSAGE, "Hello from macro!~n").

message() ->
    io:format(?MESSAGE).

whoami() ->
    io:format("module <~s>, in '~s':~p~n", [?MODULE, ?FILE, ?LINE]).
