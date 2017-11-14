-module(atm).
-behavior(gen_statem).
-compile(export_all).
-define(NAME, atm).


callback_mode() -> state_functions.


% Cards :: [{No :: integer(), Pin :: integer(), Balance :: integer()}].

%%% API

start_link(Cards) ->
	case gen_statem:start_link({local, ?NAME}, ?MODULE, {Cards, none, []}, []) of
		{ok, _} ->
			ok;
		{error, E} ->
			{error, E}
	end.

insert_card(CardNo) ->
	gen_statem:call(?NAME, {inserted, CardNo}).

push_button(Button) ->
	gen_statem:call(?NAME, {pressed, Button}).



%%% callback functions


init({Cards, CurrentNo, Input}) -> 
	State = wait,
	io:fwrite("This ATM is currently operable.~n"),
	{ok, State, {Cards, CurrentNo, Input}}.

wait({call, From}, {inserted, CardNo}, {Cards, none, Input}) ->
	case [X || {X, _, _} <- Cards, X =:= CardNo] of
		[X] -> {next_state, wait, {Cards, CardNo, []}, [{reply, From, card_ok}]};
		[] ->
			io:fwrite("Sorry, unrecognized card. Returning to initial screen...~n"),
			{next_state, wait, {Cards, none, []}, [{reply, From, returning}]}
	end;
wait({call, From}, {pressed, Button}, {Cards, none, Input}) ->
	{next_state, wait, {Cards, none, []}, [{reply, no_card}]};
wait({call, From}, {pressed, Button}, {Cards, CardNo, Input}) ->
	case lists:member(Button, ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']) of
		true ->
			{next_state, wait, {Cards, CardNo, Input ++ atom_to_list(Button)}, [{reply, From, {ok, Input ++ atom_to_list(Button)}}]};
		false when Button =:= enter ->
			case check_pin(Cards, CardNo, Input) of
				true ->
					{next_state, wait_for_sum, {Cards, CardNo, []}, [{reply, From, pin_ok}]};
				false ->
					io:fwrite("Bad pin, sorry. Try again~n"),
					{next_state, wait, {Cards, CardNo, []}, [{reply, From, try_again}]}
			end;
		false when Button =:= cancel ->
			io:fwrite("Canceled by user. Returning to initial screen...~n"),
			{next_state, wait, {Cards, none, []}, [{reply, From, returning}]};
		_ ->
			io:fwrite("Unrecognized input. Try again~n"),
			{next_state, wait, {Cards, CardNo, []}, [{reply, From, try_again}]}
	end.

wait_for_sum({call, From}, {pressed, Button}, {Cards, CardNo, Input}) ->
	case lists:member(Button, ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']) of
		true ->
			{next_state, wait_for_sum, {Cards, CardNo, Input ++ atom_to_list(Button)}, [{reply, From, {ok, Input ++ atom_to_list(Button)}}]};
		false when Button =:= enter ->
			case check_sum(Cards, CardNo, Input) of
				insufficient_balance ->
					io:fwrite("Insufficent balance, sorry. Try again~n"),
					{next_state, wait_for_sum, {Cards, CardNo, []}, [{reply, From, insufficient_balance}]};
				UpdatedCards ->
					io:fwrite("Here's your money. Thank you~n"),
					give_money(),
					return_card(),
					{next_state, wait, {UpdatedCards, none, []}, [{reply, From, trans_complete}]}
			end;
		false when Button =:= cancel ->
			io:fwrite("Canceled by user. Returning to initial screen...~n"),
			{next_state, wait, {Cards, none, []}};
		_ ->
			io:fwrite("Unrecognized input. Try again~n"),
			{next_state, wait, {Cards, CardNo, []}, [{reply, From, try_again}]}
	end.



%%% Aux functions

return_card() -> ok.

give_money() -> ok.

check_pin(Cards, CardNo, Input) ->
	[list_to_integer(Input)] =:= [Y || {X, Y, _} <- Cards, X =:= CardNo].

check_sum(Cards, CardNo, Input) ->
	case [Z - list_to_integer(Input) || {X, Y, Z} <- Cards, X =:= CardNo] of
		[NewSum] when NewSum >= 0 -> 
			[{Pin, OldSum}] = [{Y, Z} || {X, Y, Z} <- Cards, X =:= CardNo],
			[{CardNo, Pin, NewSum}] ++ Cards -- [{CardNo, Pin, OldSum}];
		[NewSum] ->
			insufficient_balance
	end.
