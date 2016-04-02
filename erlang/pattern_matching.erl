-module(pattern_matching).
-export([head/1, count/1, first/1]).

head([H|_]) -> H.

count([_|Rest]) ->
  1 + count(Rest);
count(_) ->
  0.

first({E,_}) -> E;
first({E,_,_}) -> E;
first({E,_, _,_}) -> E;
first({E,_,_,_,_}) -> E.
