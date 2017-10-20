-module(dbp).
-export([new/2, loop/2]).

new(MsgReceiver, Parameters) -> 
	spawn(?MODULE, loop, [MsgReceiver, {Parameters, []}]).

loop(MsgReceiver, {P, Db}) ->
	receive
		{write, Key, Value} ->
			MsgReceiver ! {ok, Value},
			loop(MsgReceiver, db:write(Key, Value, {P, Db}));
		destroy -> 
			MsgReceiver ! ok;
		{delete, Key} ->
			case db:delete(Key, {P, Db}) of
				{error, instance} ->
					MsgReceiver ! {error, instance},
					loop(MsgReceiver, {P, Db});
				{P, NewDb} ->
					MsgReceiver ! ok,
					loop(MsgReceiver, {P, NewDb})
			end;
		{read, Key} ->
			case db:read(Key, {P, Db}) of
				{error, instance} ->
					MsgReceiver ! {error, instance};
				{ok, Element} ->
					MsgReceiver ! {ok, Element}
			end,
			loop(MsgReceiver, {P, Db});
		{match, Key} ->
			MsgReceiver ! {ok, db:match(Key, {P, Db})},
			loop(MsgReceiver, {P, Db});
		{batch_delete, KeyList} ->
			case db:batch_delete(KeyList, {P, Db}) of
				{error, instance} ->
					MsgReceiver ! {error, instance},
					loop(MsgReceiver, {P, Db});
				{error, batch_limit} ->
					MsgReceiver ! {error, batch_limit},
					loop(MsgReceiver, {P, Db});
				{P, NewDb} ->
					MsgReceiver ! ok,
					loop(MsgReceiver, {P, NewDb})
			end;
		{batch_read, KeyList} ->
			case db:batch_read(KeyList, {P, Db}) of
				{error, instance} ->
					MsgReceiver ! {error, instance};
				{error, batch_limit} ->
					MsgReceiver ! {error, batch_limit};
				{P, Db} ->
					MsgReceiver ! {ok, Db}
			end,
			loop(MsgReceiver, {P, Db});
		{append, Key, Element} ->
			case db:append(Key, Element, {P, Db}) of
				{error, forbidden} ->
					MsgReceiver ! {error, forbidden},
					loop(MsgReceiver, {P, Db});
				{P, NewDb} ->
					MsgReceiver ! ok,
					loop(MsgReceiver, {P, NewDb})
			end;
		_ ->
			loop(MsgReceiver, {P, Db})
	end.