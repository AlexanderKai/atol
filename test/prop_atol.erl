-module(prop_atol).

-include_lib("proper/include/proper.hrl").
-compile(export_all).
-export([command/1, initial_state/0, next_state/3,
		          precondition/2, postcondition/3, prop_test/0]).


prop_test() ->
	?FORALL(Cmds, commands(?MODULE),
		begin
			setup(),
			{History, State, Result} = run_commands(?MODULE, Cmds),
			teardown(),
			?WHENFAIL(io:format("=======~n"
				"Failing command: ~p~n"
				"At state: ~p~n"
				"=======~n"
				"Result: ~p~n"
				"History: ~p~n",
				[lists:nth(length(History), Cmds),
				State,Result,History]),
				aggregate(command_names(Cmds), Result =:= ok))
		end).

setup() ->
	io:format("setup~n", []),
	meck:new(hackney, [passthrough]),
	ets:new(hackney_requests, []),
	Request = fun(post, Site, _Headers, Body, Options) ->
		UUID = l2:uuid(),
		Atol = binary:match(Site, <<"https://online.atol.ru/possystem/">>) =/= nomatch,
		Sell = binary:match(Site, <<"sell">>) =/= nomatch,
		SellRefund = binary:match(Site, <<"sell_refund">>) =/= nomatch,
		case [Atol, Sell, SellRefund] of
			[true, true, false] ->
		   		[];
			_ ->
				[]
		end,		
		ets:insert(hackney_requests, [{UUID, [{body, <<"">>} ] }]),
		{ok, [], [], l2:uuid()}		
	end,
	Body = fun(Ref) ->
		{UUID, PropList} = ets:lookup(hackney_requests, Ref),
		{ok, proplists:get_value(body, PropList)}
	end,
	%Res = application:start(atol),
	atol:start(review),
	meck:expect(hackney, request, Request),
	meck:expect(hackney, body, Body),
	%io:format("setup~n", [Res]),
	%{ok, _} = application:ensure_all_started(atol).
	[].
%crud_db:setup().

teardown() ->
	atol:stop(),
	io:format("teardown~n", []),
	meck:unload(hackney),
	ets:delete(hackney_requests),
	io:format("teardown~n", []),
	[].
%crud_db:teardown().

%% Initial model value at system start. Should be deterministic.
initial_state() -> #{}.

command(_State) -> 
	oneof([
		{call, atol, sell, 
			[
				pos_integer(), 
				[{email,email()},{phone,phone()}],
				%resize(
					%frequency([{1,5}, {1,4}, {2,3}, {2,2}, {3,1}]),
				%	list([{name, goods()}, {price, price()}, {quantity, quantity()}, {tax, 0}])
				%)
					list([{name, goods()}, {price, price()}, {quantity, quantity()}, {tax, 0}])
			]
		},
		{call, atol, verify_status, []}
	]).	

email() ->
	?LET(Email, fixed_list([resize(10,range($a, $z)),<<"@">> ,resize(10,range($a,$z)), <<".com">>]), l:l2b(Email)).

phone() ->
	?LET(Phone,range(89155550000, 89155559999), l:i2b(Phone)).

goods() ->
	?LET(Goods,resize(15,range($a, $z)), Goods).

price() ->
	number().

quantity() ->
	pos_integer().

%% Picks whether a command should be valid under the current state.
precondition(_State, {call, _Mod, _Fun, _Args}) -> 
	true.

%% Given the state `State' *prior* to the call `{call, Mod, Fun, Args}',
%% determine whether the result `Res' (coming from the actual system)
%% makes sense.
postcondition(_State, {call, _Mod, _Fun, _Args}, _Res) ->
	true.

%% Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state(State, _Res, {call, _Mod, _Fun, _Args}) ->
	NewState = State,
	NewState.


