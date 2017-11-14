-module(atm).
-behavior(gen_statem).
-compile(export_all).


callback_mode() -> state_functions.

name() -> atm.



%%% API

start_link(Cards) ->
	case gen_statem:start_link(?MODULE, {Cards, none, []}, []) of
		{ok, Pid} ->
			case register(name(), Pid) of
				true -> ok;
				{error, E} -> {error, E}
			end;
		{error, E} ->
			{error, E}
	end.

insert_card(CardNo) ->
	gen_statem:call(name(), {inserted, CardNo}).

push_button(Button) ->
	gen_statem:call(name(), {pressed, Button}).



%%% Callback functions


init({Cards, CurrentNo, Input}) -> 
	State = wait,
	io:fwrite("This ATM is currently operable.~n"),
	{ok, State, {Cards, CurrentNo, Input}}.

wait({call, From}, {inserted, CardNo}, {Cards, CurrentNo, Input}) ->
	{next_state, check_card, {Cards, CardNo, Input}}.

check_card(internal, check_card, {Cards, CurrentNo, Input}) ->
	case [X || {X, _, _} <- Cards, X =:= CurrentNo] of
		[X] -> {next_state, wait_for_pin, {Cards, CurrentNo, Input}};
		[] ->
			io:fwrite("Sorry, unrecognized card. Returning to initial screen...~n"),
			{next_state, wait, {Cards, none, []}};
		_ ->
			io:fwrite("An error has occured. Returning to initial screen...~n"),
			{next_state, wait, {Cards, none, []}}
	end.

wait_for_pin(internal, wait_for_pin, {Cards, CurrentNo, Input}) ->
	io:fwrite("Waiting for input...~n"),
	{next_state, wait_for_pin, {Cards, CurrentNo, Input}};
wait_for_pin({call, From}, {pressed, Button}, {Cards, CurrentNo, Input}) ->
	case lists:member(Button, ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']) of
		true ->
			{next_state, wait_for_pin, {Cards, Input ++ atom_to_list(Button)}};
		false when Button =:= enter ->
			{next_state, check_pin, {Cards, string:to_integer(Input)}};
		false when Button =:= cancel ->
			io:fwrite("Canceled by user. Returning to initial screen...~n"),
			{next_state, wait, {Cards, none, []}}
	end.

check_pin({call, From}, {inserted, CardNo}, {Cards, CurrentNo, Input}) ->
	case [Input] =:= [Y || {X, Y, _} <- Cards, X =:= CardNo] of
		true ->
			io:fwrite("Correct pin. Enter sum you want to withdraw:~n"),
			{next_state, wait_for_sum, {Cards, []}};
		_ ->
			io:fwrite("Wrong pin, sorry. Try again~n"),
			{next_state, wait_for_pin, {Cards, CurrentNo, []}}
	end.

wait_for_sum(internal, wait_for_sum, {Cards, CurrentNo, Input}) ->
	io:fwrite("Waiting for input...~n"),
	{next_state, wait_for_sum, {Cards, CurrentNo, Input}};
wait_for_sum({call, From}, {pressed, Button}, {Cards, CurrentNo, Input}) ->
	case lists:member(Button, ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']) of
		true ->
			{next_state, wait_for_sum, {Cards, CurrentNo, Input ++ atom_to_list(Button)}};
		false when Button =:= enter ->
			{next_state, check_sum, {Cards, CurrentNo, string:to_integer(Input)}};
		false when Button =:= cancel ->
			io:fwrite("Canceled by user. Returning to initial screen...~n"),
			{next_state, wait, {Cards, none, []}}
	end.

check_sum(internal, check_sum, {Cards, CurrentNo, Input}) ->
	case [Z - Input || {X, Y, Z} <- Cards, X =:= CurrentNo] of
		[NewSum] when NewSum >= 0 ->
			io:fwrite("Here's your money. Thank you~n"),
			[{Pin, OldSum}] = [{Y, Z} || {X, Y, Z} <- Cards, X =:= CurrentNo],
			{next_state, wait, {[{CurrentNo, Pin, NewSum}] ++ Cards -- [{CurrentNo, Pin, OldSum}], none, []}};
		[NewSum] ->
			io:fwrite("Insufficient balance, sorry. Try again~n"),
			{next_state, wait_for_sum, {Cards, CurrentNo, []}};
		_ ->
			io:fwrite("An error has occured. Returning to initial screen...~n"),
			{next_state, wait, {Cards, none, []}}	
		end.



%%% Dummy functions

return_card() -> ok.
