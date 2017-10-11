-module(lambdas).
-export([recursivearith/3, distance/2, ardist/2, mapfoldl/2, filterfoldl/2]).

recursivearith(Op, L1, L2) -> 

	Recur = fun Recur(Operation, [], [], Acc) -> Acc;
	    	   	Recur(Operation, [H1|T1], [H2|T2], Acc) ->
					Recur(Operation, T1, T2, Acc ++ [Operation(H1, H2)]) end,

	Recur(Op, L1, L2).

	% арифметическую функцию можно вызвать, например, так: fun erlang:'+'/2

distance(Point1, Point2) ->

	Dist = fun Dist([], [], Acc) -> math:sqrt(Acc);
			   Dist([H1|T1], [H2|T2], Acc) -> 
						Dist(T1, T2, Acc + (H1 - H2)*(H1 - H2)) end,

	Dist(Point1, Point2, 0).

ardist(Points1, Points2) ->

	ArDist = fun ArDist([], [], AccList, Acc) -> AccList;
				 ArDist([[]|T1], [[]|T2], AccList, Acc) -> ArDist(T1, T2, AccList ++ [math:sqrt(Acc)], 0);
			   	 ArDist([[SH1|ST1]|T1], [[SH2|ST2]|T2], AccList, Acc) -> 
						ArDist([ST1|T1], [ST2|T2], AccList, Acc + (SH1 - SH2)*(SH1 - SH2)) end,

	ArDist(Points1, Points2, [], 0).

mapfoldl(Fun, L) ->

	MapFoldl = fun MapFoldl(Fun1, L1) -> 
		[Fun1(lists:foldl(fun erlang:'*'/2, 1, [X])) || X <- L1] end,

	MapFoldl(Fun, L).

filterfoldl(Pred, L) ->

	FilterFoldl = fun FilterFoldl(Pred1, L1) -> 
		[lists:foldl(fun erlang:'*'/2, 1, [X]) || X <- L1, Pred1(X)=:=true] end,

	FilterFoldl(Pred, L).
