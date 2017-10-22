-module(db).
-export([new/0, new/1, write/3, delete/2, read/2, match/2, destroy/1, batch_delete/2,
	batch_read/2, append/3, valueof/2, exists/2, oneexists/2, listexists/2, replaceone/3]).


new() -> {[], []}.

new(Parameters) ->
	case is_list(Parameters) of
		true ->
			case length(Parameters) of
				2 ->
					case {checkappend(Parameters), checkbatch(Parameters)} of
						{ok, ok} -> {Parameters, []};
						_ -> {error, bad_parameters}
					end;
				1 ->
					case oneexists(append, Parameters) of
						true ->
							case checkappend(Parameters) of
								ok -> {Parameters, []};
								_ -> {error, bad_parameters}
							end;
						false ->
							case checkbatch(Parameters) of
								ok -> {Parameters, []};
								_ -> {error, bad_parameters}
							end
					end;
				_ -> {error, bad_parameters}
			end;
		false ->
			{error, badarg}
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
	case length(KeyList) =< valueof(batch, P) of
		true ->
			case length(exists(KeyList, Db)) =:= length(KeyList) of
				true -> {P, Db -- [{Key, valueof(Key, Db)} || Key <- KeyList]};
				false -> {error, instance}
			end;
		false ->
			{error, batch_limit}
	end.
% если параметр максимального размера batch не задан, 
% любое число будет больше []
	
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

% далее идут вспомогательные функции

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

listexists([H|T], Db) ->
	case [Y || {X, Y} <- Db, X =:= H] of
		[] -> false;
		[_|_] when []=:=T -> true;
		[_|_] -> listexists(T, Db)
	end.

replaceone(Key, Value, Db) -> replaceone(Key, Value, Db, []).
replaceone(Key, Value, [], Acc) -> Acc;
replaceone(Key, Value, [{X, Y}|T], Acc) when X =:= Key ->
	replaceone(Key, Value, T, [{X, Value}|Acc]);
replaceone(Key, Value, [H|T], Acc) -> 
	replaceone(Key, Value, T, [H|Acc]).

checkappend(Parameters) ->
	case valueof(append, Parameters) of
		allow -> ok;
		deny -> ok;
		_ -> error
	end.
checkbatch(Parameters) ->
	case is_integer(valueof(batch, Parameters)) of
		true ->
			case valueof(batch, Parameters) >= 0 of
				true -> ok;
				_ -> error
			end;
		_ -> error
	end.
