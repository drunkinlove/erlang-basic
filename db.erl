-module(db).
-export([new/0, new/1, write/3, delete/2, read/2, match/2, destroy/1, batch_delete/2,
	batch_read/2, append/3]).


new() -> {[], []}.

new(Parameters) ->
	case validate(Parameters) of
		ok -> {Parameters, []};
		_ -> {error, bad_parameters}
	end.

write(Key, Element, {P, Db}) -> 
	case oneexists(Key, Db) of
		true -> 
			{P, replaceone(Key, Element, Db)};
		false -> 
			{P, [{Key, Element} | Db]}
	end.
% удаляет существующую запись по введенному ключу, если она есть.

delete(Key, {P, Db}) ->
	case oneexists(Key, Db) of
		true ->
			{P, [{X, Y} || {X, Y} <- Db, X =/= Key]};
		false ->
			{error, instance}
	end.

read(Key, {P, Db}) ->
	case [Y || {X, Y} <- Db, X =:= Key] of
		[] -> {error, instance};
		[Element] -> {ok, Element}
	end.

match(Element, {P, Db}) ->
	[X || {X, Y} <- Db, Y =:= Element].

destroy(Db) when is_list(Db) =:= true ->
	case file:delete(Db) of
		ok -> {ok, filedeleted};
	    _ -> {error, filenotfound}
	end;
destroy(Db) ->
	[],
	ok.

batch_delete(KeyList, {P, Db}) -> 
	% если параметр максимального размера batch не задан, 
	% любое число будет больше []
	case length(KeyList) =< valueof(batch, P) of
		true ->
			case length(exists(KeyList, Db)) =:= length(KeyList) of
				true -> {P, Db -- [{Key, valueof(Key, Db)} || Key <- KeyList]};
				false -> {error, instance}
			end;
		false ->
			{error, batch_limit}
	end.
	
batch_read(KeyList, {P, Db}) -> 
	case length(KeyList) =< valueof(batch, P) of
		true ->
			case length(exists(KeyList, Db)) =:= length(KeyList) of
				true -> {P, [{Key, valueof(Key, Db)} || Key <- exists(KeyList, Db)]};
				false -> {error, instance}
			end;
		false ->
			{error, batch_limit}
	end.

append(Key, Element, {P, Db}) -> 
	case valueof(append, P) of
		deny -> {error, forbidden};
		allow -> {P, [{Key, Element}|Db]};
		[] -> {P, [{Key, Element}|Db]}
	end.

%%% далее идут вспомогательные функции

valueof(Key, Db) ->
	case [Y || {X, Y} <- Db, X =:= Key] of
		[Element] -> Element;
		[] -> []
	end.

exists([H|T], Db) -> exists([H|T], Db, []).
% по списку ключей возвращает список элементов, которые в базе существуют
exists(_, [], Acc) -> Acc;
exists([], _, Acc) -> Acc;
exists([H|T], Db, Acc) ->
	case oneexists(H, Db) of
		true -> exists(T, Db, [H|Acc]);
		false -> exists(T, Db, Acc)
	end.

oneexists(Key, Db) ->
	case [Y || {X, Y} <- Db, X =:= Key] of
		[] -> false;
		[_] -> true
	end.

replaceone(Key, Value, Db) -> replaceone(Key, Value, Db, []).
replaceone(Key, Value, [], Acc) -> Acc;
replaceone(Key, Value, [{X, Y}|T], Acc) when X =:= Key ->
	replaceone(Key, Value, T, [{X, Value}|Acc]);
replaceone(Key, Value, [H|T], Acc) -> 
	replaceone(Key, Value, T, [H|Acc]).

validate(Parameters) ->
	case [N || N <- [is_integer(Y) || {X, Y} <- Parameters, X =:= batch, Y >= 0], N =:= false] of
		[] ->
			case [Y || {X, Y} <- Parameters, X =:= append, Y =/= allow, Y =/= deny] of
				[] -> ok;
				_ -> error
			end;
		_ -> error
	end.
