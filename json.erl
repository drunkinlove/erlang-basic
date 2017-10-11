-module(json).
-export([new/2, read/2, write/3]).


new(Key, Value) -> 
	#{Key => Value}.		

read(Key, JsonObj) -> 
	try #{Key := _} = JsonObj of
		_ -> 
		#{Key := Value} = JsonObj,
		{ok, Value}
	catch
		error:{badmatch, _} -> {error, not_found}
	end.
	
write(Key, Value, JsonObj) -> 
	try JsonObj#{Key := Value} of
		_ -> JsonObj
	catch
		error:{badkey, _} -> {error, not_found}
	end.
