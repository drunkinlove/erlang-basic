-module(bool).

-export([b_not/1, b_and/2, b_or/2, b_xor/2]).

b_not(false) -> true;
b_not(true) -> false.

b_and(false, false) -> false;
b_and(_,_) -> true.

b_or(true, true) -> true;
b_or(_,_) -> false.

b_xor(X, Y) when X =:= Y -> false;
b_xor(_,_) -> true.
