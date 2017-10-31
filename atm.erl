-module(atm).

start_link([{CardNo, Pin, Balance}]) ->
	case gen_server:start_link(?MODULE, [], [])) of
		{ok, Pid} ->
			ok;
		{error, E} ->
			{error, E}
	end.

insert_card(CardNo) ->
	gen_server:call(Atm, {inserted, CardNo}).

push_button(Button) ->
	gen_server:call(Atm, {pressed, Button}).

%%% Server functions

init(Cards) -> {ok, Cards]}.

receive() -> string.

handle_call({inserted, CardNo}, _From, Cards) ->
	case [receive()] of
		[Password || {CardNo, Password, _} <- Cards] -> 
			case [Sum - receive() || {CardNo, _, Sum} <- Cards] of
				[NewSum] when NewSum >= 0 ->
					{reply, {"here's your money"}, [{CardNo, Password, NewSum}] ++ Cards -- [{CardNo, Password, Sum}]};
				[NewSum] ->
					{reply, {"not allowed"}, Cards};
				{error, E} ->
					{reply, E, Cards}
		[cancel] ->
			{reply, "canceled", Cards};
		[_] ->
			{reply, "invalid password", Cards};
		{error, E} ->
			{reply, E, Cards} 
	end.

handle_call({pressed, Button}, _From, Cards) ->
	case Button of
		cancel ->
			{reply, Cancel, Cards};
		Number when Number >= 0 andalso Number <= 9:
			