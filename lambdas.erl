-module(lambdas).
-export([recursivearith/3, distance/2, ardist/2, mapfoldl/2]).

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

	MapFoldl = fun MapFoldl(Operation, LIn) -> [lists:foldl(Operation, 0, [X]) || X <- LIn] end,

	MapFoldl(Fun, L).

	% Лямбда принимает операцию над одним аргументом, а фолдл - над двумя. Это проблема