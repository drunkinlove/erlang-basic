-module(json).
-export([new/2, read/2, write/3]).


new(Key, Value) -> 
	#{Key => Value}.		

read(Key, JsonObj) ->
	case JsonObj of
  		#{Key := Value} -> {ok, Value};
  		#{} -> {error, not_found}
 	end.
	
write(Key, Value, JsonObj) ->
 	case JsonObj of
  		#{Key := _} -> JsonObj#{Key := Value};
  		#{} -> {error, not_found}
 	end.
