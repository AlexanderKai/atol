-module(atol_worker).
-compile(export_all).

-define(ETS, atol_ets).
-define(TRANSACTIONS, atol_transactions).

-export[sell/4, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2].

auth() ->
	[{_,API}] = ets:lookup(?ETS, api),
	[{_,Login}] = ets:lookup(?ETS, login),
	[{_,Password}] = ets:lookup(?ETS, password),
	Body = <<>>,
	T = l:l2b([<<"login="/utf8>>,Login,<<"&pass="/utf8>>, Password]),
 	Site = l:l2b([<<"https://online.atol.ru/possystem/">>,API,<<"/getToken?">>, T]),
	Headers = [],
	{ok, _, _, Ref} = hackney:request(get, Site, Headers, Body, []),
	{ok, Answer} = hackney:body(Ref),
	JSON = jsone:decode(Answer, [{object_format, proplist}]),
	io:format("JSON ~p~n", [JSON]),
	case proplists:get_value(<<"token">>, JSON) of
		<<>> -> {error, Answer};
		Token -> 
			ets:insert(?ETS, {token, Token}),
			ets:insert(?ETS, {token_time, l:current_time()}),
			ok
	end.


sell(Type, Id, Attributes, Items) ->

	case atol:check_token() of
		ok ->
			try	
				do_sell(Type, Id, Attributes, Items),
				ok
			catch
				E1:E2 -> 
					{error, io:format("~p~n~p~n~p~n", [E1, E2, erlang:get_stacktrace()])}
			end;

		Error ->
			Error
	end.


do_sell(Type, Id, Attributes, Items) ->
	[{_,Token}] = ets:lookup(?ETS, token),
	[{_,API}] = ets:lookup(?ETS, api),
	[{_,INN}] = ets:lookup(?ETS, inn),
	[{_,Group}] = ets:lookup(?ETS, group),
	Email = proplists:get_value(email, Attributes, <<"">>),
	Phone = proplists:get_value(phone, Attributes, <<"">>),
	%CallbackUrl =proplists:get_value(callback_url, State),
	{_,Payment_Address} = ets:lookup(?ETS, payment_address),
	Operation = l:a2b(Type),

	Sum = lists:sum(
		[
			proplists:get_value(price, A) * proplists:get_value(quantity, A)
		||
		A <- Items
		]
	),
	SumFormat = l:l2b(io_lib:format("~.2f",[Sum])),
	Date = l:current_time(),
	%UnixTime = qdate:to_unixtime(Date),
	TimeStamp = l:l2b(qdate:to_string("d.m.Y H:i:s", Date)),
	Data = l:l2b(
	[
		{<<"external_id">>, Id},
		{<<"receipt">>, 
			[
				{<<"attributes">>,
					[
						{<<"email">>, Email},
						{<<"phone">>, Phone}
					]
				},
				{<<"items">>,
					[
						{
							[
								{<<"name">>, proplists:get_value(name, I)},
								{<<"price">>,l:l2b(io_lib:format("~.2f",[proplists:get_value(price, I)]))},
								{<<"quantity">>, proplists:get_value(quantity, I)},
								{<<"sum">>,l:l2b(io_lib:format("~.2f",[proplists:get_value(quantity, I) * proplists:get_value(price, I)]))},
								{<<"tax">>, proplists:get_value(tax, I)}
							]
						}
					||
					I <- Items
					]
				},
				{<<"payments">>,
					[
						{<<"type">>, 1},
						{<<"sum">>, SumFormat}

					]
				},
				{<<"total">>, SumFormat}
			]
		},
		{<<"service">>,
			[
				%{<<"callback_url">>, CallbackUrl},
				{<<"inn">>, INN},
				{<<"payment_address">>, Payment_Address}
			]
		},
		{<<"timestamp">>, TimeStamp}
	]),
	io:format("~s\n", [jsone:encode(Data, [{indent, 1}, {space, 2}])]),
	JSON = jsone:encode(Data),
	%io:format("atol sell json~n~p~n", [JSON]),
	Site = l:l2b([<<"https://online.atol.ru/possystem/">>, API, <<"/">>, Group,<<"/">>,Operation,<<"?tokenid=">>,Token]),
	Headers = [],
	{ok, _, _, Ref} = hackney:request(post, Site, Headers, JSON, []),
	{ok, Answer} = hackney:body(Ref),
	Result = jsone:decode(Answer, [{object_format, proplist}]),
	Status = proplists:get_value(<<"status">>, Result),
	UUID = proplists:get_value(<<"uuid">>, Result),
	case Status of
		<<"fail">> -> 
			Error = proplists:get_value(<<"error">>, Result),
			TextError = proplists:get_value(<<"text">>, Error),
			{error, TextError};
		_ ->
			ets:insert(?TRANSACTIONS, {UUID, {Status, Id, Attributes, Items}}),
			ok
	end.

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
	process_flag(trap_exit, true),
	{ok, Args}.

handle_call({sell, Id, Attributes, Items}, {_From,_}, State) ->
    Result = sell(sell, Id, Attributes, Items),
	{reply, Result, State};

handle_call({sell_refund, Id, Attributes, Items}, {_From,_}, State) ->
    Result = sell(sell_refund, Id, Attributes, Items),
	{reply, Result, State};


handle_call({auth}, {_From,_}, State) ->
	%Login = proplists:get_value(login, S),
	%Password = proplists:get_value(password, State),
	Result = auth(),
	{reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ok.


