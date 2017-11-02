-module(db_otp).
-behavior(gen_server).
-compile(export_all).


new(Name) -> 
	case gen_server:start_link(?MODULE, [], []) of
		{ok, Pid} ->
			case register(Name, Pid) of
				true -> ok;
				{error, E} -> {error, E}
			end;
		{error, E} -> {error, E}
	end.

delete(Name) ->
	case is_up(Name) of
		true -> gen_server:cast(Name, delete);
		_ -> {error, dead}
	end.

delete(Name, Key) ->
	case is_up(Name) of
		true -> gen_server:cast(Name, {delete, Key});
		_ -> {error, dead}
	end.
	
delete_all_objects(Name) ->
	case is_up(Name) of
		true -> gen_server:cast(Name, delete_all_objects);
		_ -> {error, dead}
	end.

insert(Name, Key, Value) ->
	case is_up(Name) of
		true -> gen_server:cast(Name, {insert, Key, Value});
		_ -> {error, dead}
	end.

find(Name, Value) ->
	case is_up(Name) of
		true -> gen_server:call(Name, {find, Value});
		_ -> {error, dead}
	end.


%%% Server functions

init([]) -> {ok, []}.

handle_cast(delete, Db) ->
	gen_server:terminate(normal, Db);
handle_cast({delete, Key}, Db) ->
	{noreply, [{X, Y} || {X, Y} <- Db, X =/= Key]};
handle_cast(delete_all_objects, Db) ->
	{noreply, []};
handle_cast({insert, Key, Value}, Db) -> 
	case [Y || {X, Y} <- Db, X =:= Key] of
		[] -> {noreply, [{Key, Value} | Db]};
		[Y] -> {noreply, [{Key, Value} | Db -- [{Key, Y}]]};
		_ -> {reply, error}
	end.

handle_call({find, Value}, _From, Db) ->
	case [Y || {X, Y} <- Db, Y =:= Value] of
		[] -> {reply, not_found, Db};
		[Z] -> {reply, {ok, Z}, Db};
		Z -> {reply, {ok, Z}, Db}
	end;
handle_call(dump, _From, Db) ->
	{reply, Db, Db}.


%%% Auxillary functions

is_up(Name) ->
	case whereis(Name) of
		Pid -> true;
		undefined -> false
	end.


%%% For testing

dump(Name) ->
	gen_server:call(Name, dump).
