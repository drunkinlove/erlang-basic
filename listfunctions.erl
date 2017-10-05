-module(listfunctions).

-export([create/1, createrev/1, print/1, printodd/1, filter/2, concatenate/1]).


create(Num) when Num > 0 -> auxcreate([], 1, Num).

auxcreate(List, Num, Num) -> List ++ [Num];
auxcreate(List, Step, Num) -> auxcreate(List ++ [Step], Step + 1, Num).


createrev(Num) when Num > 0 -> auxcreaterev([], Num).

auxcreaterev(List, 0) -> List;
auxcreaterev(List, Num) -> auxcreaterev(List ++ [Num], Num - 1).


print(Num) when Num > 0 -> printone(1, Num).

printone(X, X) -> io:format("~p ~n",[X]);
printone(Step, Num) ->
	io:format("~p ~n",[Step]),
	printone(Step + 1, Num).


printodd(Num) when Num > 0 -> printoneodd(1, Num).

printoneodd(X, X) -> io:format("~p ~n",[X]);
printoneodd(Step, Num) ->
	io:format("~p ~n",[Step]),
	printoneodd(Step + 2, Num). % нужно только заменить Step + 1 на Step + 2


filter(List, Val) when Val > 0 -> [N || N <- List, N >= Val].


%reverse(List) -> 
%	Len = length(List),
%	grabreverse(List, Len, []).

%grabreverse(_List, 0, RevList) -> RevList;
%grabreverse(List, Step, RevList) ->
%	grabreverse(List, Step-1, RevList ++ grabtail(List, Step)).

%grabtail([_|X], 0) -> X;
%grabtail([_|X], N) -> grabtail(X, N-1).


concatenate(List) ->
	NewList=[],
	NewList ++ [X || [[X]] <- List].
