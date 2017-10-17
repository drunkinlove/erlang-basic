-module(db).
-export([new/0, new/1, write/3, delete/2, read/2, match/2, destroy/1, batch_delete/2,
	batch_read/2, append/3, valueof/2]).


new() -> [].

new(Parameters) -> Parameters.

write(Key, Element, Db) -> 
	[{Key, Element}] ++ Db -- [{Key, valueof(Key, Db)}].
% удаляет существующую запись по введенному ключу, если она есть.

delete(Key, Db) -> 
	[{X, Y} || {X, Y} <- Db, X =/= Key].

read(Key, Db) ->
	case [Y || {X, Y} <- Db, X =:= Key] of
		[] -> {error, instance};
		[Element] -> {ok, Element}
	end.

match(Element, Db) ->
	[X || {X, Y} <- Db, Y =:= Element].

destroy(Db) when is_list(Db) =:= true -> file:delete(Db);
destroy(Db) -> [].

valueof(Key, Db) ->
	case [Y || {X, Y} <- Db, X =:= Key] of
		[Element] -> Element;
		[] -> []
	end.

exists([H|T], Db) -> exists([H|T], Db, []).
exists(_, [], Acc) -> false;
exists([], _, Acc) when Acc =:= [] -> false;
exists([], _, Acc) -> Acc;
exists([H|T], Db, Acc) ->
	case oneexists(H, Db) of
		true -> exists(T, Db, Acc ++ [H]);
		false -> exists(T, Db, Acc)
	end.

oneexists(Key, Db) ->
	case [Y || {X, Y} <- Db, X =:= Key] of
		[] -> false;
		[_] -> true
	end.

batch_delete(KeyList, Db) -> 
	case length(KeyList) > valueof(batch, Db) of
		true -> {error, batch_limit};
		false -> Db -- [{Key, valueof(Key, Db)} || Key <- KeyList]
	end.
% если параметр максимального размера batch не задан, 
% любое число будет больше []
	
batch_read(KeyList, Db) ->
	case length(KeyList) > valueof(batch, Db) of 
		true -> {error, batch_limit};
		false -> [{Key, valueof(Key, Db)} || Key <- exists(KeyList, Db) ]
	end.

append(Key, Element, Db) -> 
	case valueof(append, Db) of
		deny -> {error, forbidden};
		allow -> Db ++ [{Key, Element}];
		[] -> Db ++ [{Key, Element}]
	end.
