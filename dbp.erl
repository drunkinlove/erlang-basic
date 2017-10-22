-module(dbp).
-export([new_receiver/0, msg_receiver/0, new/2, write/3, destroy/1, delete/2,
	read/2, match/2, batch_delete/2, batch_read/2, append/3, loop/2]).

new_receiver() -> 
	spawn(?MODULE, msg_receiver, []).

msg_receiver() ->
	receive
		{Msg} ->
			io:format("Database says ~p~n",[Msg]),
			msg_receiver();
		{Msg1, Msg2} ->
			io:format("Database says ~p~n",[{Msg1, Msg2}]),
			msg_receiver();
		_ ->
			msg_receiver()
	end.

new(MsgReceiver, Parameters) ->
	case db:new(Parameters) of
		{error, bad_parameters} -> 
			{error, bad_parameters};
		{error, badarg} -> 
			{error, badarg};
		_ ->
			spawn(?MODULE, loop, [MsgReceiver, {Parameters, []}])
	end.

write(Key, Value, Db) ->
	case is_process_alive(Db) of
		true -> Db ! {write, Key, Value};
		false -> {error, dead}
	end.

destroy(Db) ->
	case is_process_alive(Db) of
		true -> exit(Db, exit),
			ok;
		false -> {error, dead}
	end.

delete(Key, Db) ->
	case is_process_alive(Db) of
		true -> Db ! {delete, Key};
		false -> {error, dead}
	end.

read(Key, Db) ->
	case is_process_alive(Db) of
		true -> Db ! {read, Key};
		false -> {error, dead}
	end.

match(Key, Db) ->
	case is_process_alive(Db) of
		true -> Db ! {match, Key};
		false -> {error, dead}
	end.

batch_delete(KeyList, Db) ->
	case is_process_alive(Db) of
		true -> Db ! {batch_delete, KeyList};
		false -> {error, dead}
	end.

batch_read(KeyList, Db) ->
	case is_process_alive(Db) of
		true -> Db ! {batch_read, KeyList};
		false -> {error, dead}
	end.

append(Key, Element, Db) ->
	case is_process_alive(Db) of
		true -> Db ! {append, Key, Element};
		false-> {error, dead}
	end.

loop(MsgReceiver, {P, Db}) ->
	receive
		{write, Key, Value} ->
			MsgReceiver ! {ok,Value},
			loop(MsgReceiver, db:write(Key, Value, {P, Db}));
		{delete, Key} ->
			case db:delete(Key, {P, Db}) of
				{error, instance} ->
					MsgReceiver ! {error, instance},
					loop(MsgReceiver, {P, Db});
				{P, NewDb} ->
					MsgReceiver ! {ok},
					loop(MsgReceiver, {P, NewDb})
			end;
		{read, Key} ->
			case db:read(Key, {P, Db}) of
				{error, instance} ->
					MsgReceiver ! {error, instance};
				{ok,Element} ->
					MsgReceiver ! {ok,Element}
			end,
			loop(MsgReceiver, {P, Db});
		{match, Key} ->
			MsgReceiver ! {ok,db:match(Key, {P, Db})},
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
					MsgReceiver ! {ok},
					loop(MsgReceiver, {P, NewDb})
			end;
		{batch_read, KeyList} ->
			case db:batch_read(KeyList, {P, Db}) of
				{error, instance} ->
					MsgReceiver ! {error, instance},
					loop(MsgReceiver, {P, Db});
				{error, batch_limit} ->
					MsgReceiver ! {error, batch_limit},
					loop(MsgReceiver, {P, Db});
				{P, NewDb} ->
					MsgReceiver ! {ok, NewDb},
					loop(MsgReceiver, {P, Db})
			end;
		{append, Key, Element} ->
			case db:append(Key, Element, {P, Db}) of
				{error, forbidden} ->
					MsgReceiver ! {error, forbidden},
					loop(MsgReceiver, {P, Db});
				{P, NewDb} ->
					MsgReceiver ! {ok},
					loop(MsgReceiver, {P, NewDb})
			end;
		_ ->
			loop(MsgReceiver, {P, Db})
	end.