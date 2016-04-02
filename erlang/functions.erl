-module(functions).
-import(lists, [reverse/1]).
-export([even/1, all_even/1, all_odd/1, sublist/2]).

even(N) -> N rem 2 =:= 0.

%% using function as arg:
all_even(Lst) -> lists:all(fun even/1, Lst).

%% binding function to variable:
all_odd(Lst) ->
    Odd = fun(N) -> N rem 2 =:= 1 end,
    lists:all(Odd, Lst).

sublist(Lst, Items) -> reverse(sublist(Lst, Items, [])).

sublist([], _, Accu) -> Accu;
sublist(_, 0, Accu) -> Accu;
sublist([First|Rest], Items, Accu) ->
    sublist(Rest, Items-1, [First|Accu]).
