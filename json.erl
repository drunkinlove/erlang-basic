-module(json).


-export([new/2, read/1]).


new(Key, Value) -> 
	Jsons = #{},
	Jsons#{Key => Value}.

read(Key) -> 
	#{Key := X} = Jsons,
	X.