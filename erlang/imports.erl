-module(imports).

-export([rev/1]).
-import(lists, [reverse/1]).

rev(Lst) ->
    reverse(Lst).
