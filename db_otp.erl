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
	case is_process_alive(whereis(Name)) of
		true -> gen_server:cast(Name, delete);
		_ -> {error, dead}
	end.

delete(Name, Key) ->
	case is_process_alive(whereis(Name)) of
		true -> gen_server:cast(Name, {delete, Key});
		_ -> {error, dead}
	end.
	
delete_all_objects(Name) ->
	case is_process_alive(whereis(Name)) of
		true -> gen_server:cast(Name, delete_all_objects);
		_ -> {error, dead}
	end.

insert(Name, Key, Value) ->
	case is_process_alive(whereis(Name)) of
		true -> gen_server:cast(Name, {insert, Key, Value});
		_ -> {error, dead}
	end.

find(Name, Value) ->
	case is_process_alive(whereis(Name)) of
		true ->
			gen_server:call(Name, {find, Value}),
			receive
				{ok, Value} ->
					{ok, Value};
				_ ->
					not_found
			after 5000 ->
				erlang:error(timeout)
			end;
		_ -> {error, dead}
	end.


%%% Server functions

init([]) -> {ok, []}.

handle_cast(delete, Name) ->
	gen_server:terminate(normal, Name);

handle_cast({delete, Key}, Name) ->
	{noreply, [{X, Y} || {X, Y} <- Name, X =/= Key]};

handle_cast(delete_all_objects, Name) ->
	{noreply, []};
	
handle_cast({insert, Key, Value}, Name) -> 
	{noreply, [{Key, Value} | Name]}.

handle_call({find, Value}, _From, Name) ->
	case [Y || {X, Y} <- Name, Y =:= Value] of
		[] -> {reply, not_found, Name};
		Z -> {reply, {ok, Z}, Name}
	end.
	


