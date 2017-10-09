-module(listfunctions).

-export([create/1, createrev/1, print/1, printodd/1, filter/2, reverse/1, concatenate/1, flatten/1, dna/1, cut_rdna/2]).


create(Num) when Num > 0 -> auxcreate([], 1, Num).

auxcreate(List, Num, Num) -> List ++ [Num];
auxcreate(List, Step, Num) -> auxcreate(List ++ [Step], Step + 1, Num).


createrev(Num) when Num > 0 -> auxcreaterev([], Num).

auxcreaterev(List, 0) -> List;
auxcreaterev(List, Num) -> auxcreaterev(List ++ [Num], Num - 1).


print(Num) when Num > 0 -> printone(1, Num).

printone(X, X) -> io:format("~p ~n",[X]);
printone(Step, Num) ->
	io:format("~p ~n", [Step]),
	printone(Step + 1, Num).


printodd(Num) when Num > 0 -> printoneodd(1, Num).

printoneodd(X, X) -> io:format("~p ~n",[X]);
printoneodd(Step, Num) ->
	io:format("~p ~n", [Step]),
	printoneodd(Step + 2, Num). % нужно только заменить Step + 1 на Step + 2


filter(List, Val) when Val > 0 -> [N || N <- List, N >= Val].


reverse(L) -> auxreverse(L,[]).

auxreverse([], Acc) -> Acc;
auxreverse([H|T], Acc) -> auxreverse(T, [H|Acc]).

	
concatenate(X) -> auxconcat(X, []).

auxconcat([], Acc) -> Acc;
auxconcat([H|T], Acc) when is_list(H)=:=false -> auxconcat(T, Acc++[H]);
auxconcat([H|T], Acc) -> auxconcat(T, Acc++H).


flatten(X) -> auxflatten(X, []).

auxflatten([], Acc) -> Acc;
auxflatten([H|T], Acc) when is_list(H)=:=true -> auxflatten(T, Acc++concatenate(H));
auxflatten([H|T], Acc) -> auxflatten(T, Acc++[H]).


dna(X) -> auxdna(X, []).

auxdna([], Acc) -> Acc;
auxdna([H|T], Acc) ->
	case H of
		g -> auxdna(T, Acc++[c]);
		c -> auxdna(T, Acc++[g]); 
		t -> auxdna(T, Acc++[a]); 
		a -> auxdna(T, Acc++[u]); 
		71 -> auxdna(T, Acc++"C");
		67 -> auxdna(T, Acc++"G");
		84 -> auxdna(T, Acc++"A");
		65 -> auxdna(T, Acc++"U")
	end.


cut_rdna(Inp, Cut) -> checkfirst(Inp, Cut, []).
%стоит заметить, что работает на любых строках, не только последовательностях нуклеотидов

checkfirst(Inp, [], Acc) -> Inp; %если нечего вырезать, вернем список
checkfirst([], Cut, Acc) -> Acc; %если уже не из чего вырезать, вернем аккум
checkfirst([A, B, C|T], [X, Y, Z], Acc) when A=:=X, B=:=Y, C=:=Z -> checkfirst(T, [X, Y, Z], Acc);
%если первые три элемента списка совпадают с вырезаемыми, просто проверяем дальше без них
checkfirst(Inp, Cut, Acc) when length(Inp) =< 2 -> Acc ++ Inp;
%если в списке осталось менее трех элементов, возвращаем его вместе с аккумом
checkfirst([A, B, C|T], [X, Y, Z], Acc) -> checkfirst([B, C] ++ T, [X, Y, Z], Acc ++ [A]).
%наконец, если первые три не совпадают, то один скидываем в аккум и идем дальше
