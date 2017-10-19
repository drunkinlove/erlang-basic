-module(dbp).
-export([]).

new() -> spawn(?MODULE, dbloop, [])

dbloop(MsgReceiver, Db) ->
	receive
		{write, Key, Value} ->
			db:write(Key, Value, Db),
			MsgReceiver ! {ok, Value};
		destroy -> 
			MsgReceiver ! ok;
		{delete, Key} ->
			case db:delete(Key, Db) of
				{error, instance} ->
					MsgReceiver ! {error, instance};
				{_} ->
					MsgReceiver ! ok
			end;
		{read, Key} ->
			case db:read(Key, Db) of
				{error, instance} ->
					MsgReceiver ! {error, instance};
				{ok, Element} ->
					MsgReceiver ! {ok, Element}
			end;
		{match, Key} ->
			{ok, db:match(Key, Db)};
		{batch_delete, KeyList} ->
			case db:batch_delete(Key, Db) of
				{error, instance} ->
					MsgReceiver ! {error, instance};
				{error, batch_limit} ->
					MsgReceiver ! {error, batch_limit};
				_ ->
					MsgReceiver ! ok
			end;
		{batch_read, KeyList} ->
			case db:batch_read(Key, Db) of
				{error, instance} ->
					MsgReceiver ! {error, instance};
				{error, batch_limit} ->
					MsgReceiver ! {error, batch_limit};
				{P, Db} ->
					MsgReceiver ! {ok, Db}
			end;
		_ ->
			