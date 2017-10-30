-module(prop_atol).

-include_lib("proper/include/proper.hrl").
-compile(export_all).
-export([command/1, initial_state/0, next_state/3,
		          precondition/2, postcondition/3, prop_test/0]).


prop_test() ->
	Res = 
	?SETUP(fun () ->       % setup function
           setup(),
    	   fun () ->   % finalize function
				teardown(),
				ok
           end
 		end,
		?FORALL(Cmds, commands(?MODULE),
		begin
			%setup(),
			{History, State, Result} = run_commands(?MODULE, Cmds),
			%teardown(),
			?WHENFAIL(io:format("=======~n"
				"Failing command: ~p~n"
				"At state: ~p~n"
				"=======~n"
				"Result: ~p~n"
				"History: ~p~n",
				[Cmds,
					State,Result,History]),
				aggregate(command_names(Cmds), Result =:= ok))
		end)),
	Res.

sprop_test() ->
	setup(),
	Res = ?FORALL(Cmds, commands(?MODULE),
		begin
			{History, State, Result} = run_commands(?MODULE, Cmds),
			?WHENFAIL(io:format("=======~n"
				"Failing command: ~p~n"
				"At state: ~p~n"
				"=======~n"
				"Result: ~p~n"
				"History: ~p~n",
				[Cmds,
					State,Result,History]),
				aggregate(command_names(Cmds), Result =:= ok))
		end),
	teardown(),
	Res.


setup() ->
	%ets:delete(hackney_requests),
	application:set_env(chronos, jobs, []),
	application:set_env(chronos, pools,
            [
                {chronos_worker,
                    [
                        {size, 40},
                        {max_overflow, 60}
                    ],
                    [
                    ]
                },
                {chronos_queue,
                    [
                        {size, 1},
                        {max_overflow, 1}
                    ],
                    [
                    ]
                }
            ]),
	application:set_env(chronos, settings,
            [
                {debug_print, on},
                {email, <<"">>},
                {email_server, <<"">>},
                {email_login, <<"">>},
                {email_password, <<"">>},
                {send_email_on_crash, on}
            ]),
        	

	application:start(chronos),
	application:set_env(atol, settings, [
            {api, <<"v3">>},
            {login, <<"test-ru">>},
            {password, <<"test">>},
            {group, <<"test">>},
            {inn, <<"113101111163">>},
            {payment_address, <<"test.ru">>},
            {callback_url, <<"">>},
            {atol_workers, [
                {atol_worker,
                    [
                        {size, 10},
                        {max_overflow, 20}
                    ],
                    [
                    ]
                }
            ]}
            ]),
	io:format("setup~n", []),
	meck:new(hackney, [passthrough]),
	ets:new(atol_hackney_requests, [named_table, set, public]),
	io:format("in main ~p~n", [pid_to_list(self())]),
	Request = fun(Type, Site, _Headers, Body, Options) ->
		UUID = l2:uuid(),
		Atol = binary:match(Site, <<"https://online.atol.ru/possystem/">>) =/= nomatch,
		Auth = binary:match(Site, <<"getToken">>) =/= nomatch,
		Sell = binary:match(Site, <<"sell">>) =/= nomatch,
		SellRefund = binary:match(Site, <<"sell_refund">>) =/= nomatch,
		[APIFun, Answer] = if
			Auth == true ->
				[<<"auth">>, [
					{<<"code">>, 0},
					{<<"text">>, null},
					{<<"token">>, l2:uuid()}
				]];
			Sell == true orelse SellRefund == true ->
				[<<"sell">>,[
				 	{<<"uuid">>, l2:uuid()},
					{<<"timestamp">>, <<"">>},
					{<<"error">>, null},
					{<<"status">>, <<"wait">>}
				]];
			true ->
				[<<"error">>, <<"">>]
		end,		
		io:format("ETS hackney requests~n~p~n", [ets:info(atol_hackney_requests)]),
		io:format("in fun ~p~n", [pid_to_list(self())]),
		ets:insert(atol_hackney_requests, {UUID, [{api, APIFun}, {answer, Answer} ] }),
		{ok, [], [], UUID}		
	end,
	Body = fun(Ref) ->
		[{Ref, PropList}] = ets:take(atol_hackney_requests, Ref),
		case proplists:get_value(api, PropList) of
			<<"sell">> ->
				Answer = 
			 	[
					{status,<<"done">>},
					{error, null},
					{uuid, Ref}
				],
				ets:insert(atol_hackney_requests, {Ref, [{api, <<"sell_2">>}, {answer,Answer}]});
			_ ->
				[]
		end,
		Body = proplists:get_value(answer, PropList),
		{ok, jsone:encode(Body)}
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
	application:stop(atol),
	application:stop(chronos),
	io:format("teardown~n", []),
	meck:unload(hackney),
	ets:delete(atol_hackney_requests),
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
	?LET(Goods,non_empty(fixed_list([resize(15,range($a, $z))])), l:l2b(Goods)).

price() ->
	non_neg_integer().

quantity() ->
	non_neg_integer().

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


