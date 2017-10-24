-module(lazy).
-export([lazy_list/3, map/2, foldl/3, filter/3]).

lazy_list(Begin, End, Step) when Begin =< End, Step > 0 ->
   fun() ->
       [Begin|lazy_list(Begin + Step, End, Step)]
   end;
lazy_list(_, _, _) ->
   fun() ->
        []
   end.

map(Operation, [H|T]) when is_function(T) ->
	fun() ->
		[Operation(H) | map(Operation, T())]
	end;
map(_, _) ->
	fun() ->
		[]
	end.

foldl(Operation, Acc0, [H|T]) when is_function(T) ->
	foldl(Operation, Operation(Acc0, H), T());
foldl(Operation, Acc0, []) -> Acc0.

filter(Pred, Acc0, [H|T]) when is_function(T) ->
	case Pred(H) of
		true -> 
			fun() ->
				[Acc0 ++ [H] | filter(Pred, Acc0 ++ H, T)]
			end;
		false -> 
			fun() ->
				[Acc0 | filter(Pred, Acc0, T)]
			end
	end;
filter(Pred, Acc0, H) ->
	case Pred(H) of
		true ->
			fun() ->
				Acc0 ++ [H]
			end;
		false ->
			fun() ->
				Acc0
			end
	end.