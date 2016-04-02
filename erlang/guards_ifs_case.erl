-module(guards_ifs_case).
-export([positive_values/1, all_even/1, is_even/1]).

%% Guards:
positive_values([First|Rest]) when First >= 0 ->
        1 + positive_values(Rest);
positive_values([_|Rest]) ->
        positive_values(Rest);
positive_values([]) ->
        0.

%% util (non-exported) function:
even(N) -> N rem 2 =:= 0.

%% if:
all_even([First|Rest]) ->
    %% can't use user-defined functions in ifs or guards:
    if First rem 2 =:= 0 -> true and all_even(Rest);
       true -> false
    end;
all_even([]) -> true.

%% case:
is_even(N) ->
    %% in 'case' it is ok to use user-defined functions:
    case even(N) of
        true -> "Yes, it is.";
        false -> "No."
    end.
