
[2*N || N <- [1,2,3,4]]. % => [1,2,3,4]

[2*N || N <- [1,2,3,4], N rem 2 =:= 0]. % => [4,8]


% returns [[2,3],[2,4],[3,3],[3,4]]
[[X,Y] || X<-[1,2,3], Y<-[3,4,5], X > 1, Y < 5].

% failed pattern match is ignored:
% returns [100, 25]
[X || {celsius, X} <- [{celsius, 100}, {kelvin, 273}, {celsius, 25}]].
