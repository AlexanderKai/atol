-module(atol).
-behaviour(supervisor).
-behaviour(application).
-export([get_status/1, init/1, start/1, start/2, stop/1, sell/3, auth/0, verify_status/0, check_token/0]).

-define(ETS, atol_ets).
-define(TRANSACTIONS, atol_transactions).

start(Application)    -> start(normal,[Application]).
start(_Type, Args) -> supervisor:start_link({local,?MODULE},?MODULE, Args).
stop(_)    -> ok.


init([Application]) -> 
	process_flag(trap_exit, true),
	try
	%{ok, AtolSettings} = application:get_env(Application, atol),
	io:format("~nAll settings~n~p~n", [application:get_all_env()]),
	{ok, AtolSettings} = application:get_env(atol, settings),
	ets:new(?ETS, [{write_concurrency, true}, {read_concurrency, true}, public, set, named_table]),
	ets:insert(?ETS, AtolSettings),
	ets:new(?TRANSACTIONS, [{write_concurrency, true}, {read_concurrency, true}, public, set, named_table]),
	io:format("=============== ATOL ===================~n~p\n",[AtolSettings]),
	Pools = proplists:get_value(atol_workers, AtolSettings),
	io:format("=============== ATOL ===================~n~p\n",[Pools]),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
		Worker = atol_worker,
        PoolArgs = [{name, {local, Name}},
                    {worker_module, Worker}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),
	io:format("=============== ATOL ===================\n",[]),
    io:format("~p~n", [PoolSpecs]),

	chronos:add_jobs(
		[
            {"atol_job",
                [
                    {time,  {"0","*","*","*","*","*","*"}}, 
                    {period_restart, 0},
                    {mfa, {atol,verify_status,[]}}
                ]
            }
		]		
	),

    {ok, {{one_for_one, 10, 10}, PoolSpecs}}
	catch
		E1:E2 ->
			io:format("~p~n~p~n~p~n", [E1, E2, erlang:get_stacktrace()])
	end.

check_token() ->
	Var = case ets:lookup(?ETS, token_time) of
		[] -> 
			check;
		[{_, TokenTime}] ->
			case qdate:compare(l:current_time(), qdate:to_now(qdate:add_minutes(30, TokenTime))) of
				1 -> 
					check;
				_ ->
					ok
			end
	end,
	case Var of
		check -> 
			Auth = auth(),
			case Auth of
				{error, Answer} -> {error, Answer};
				ok -> 
					ok
			end;
		ok ->
			ok
	end.


verify_status() ->
	FirstKey = ets:first(?TRANSACTIONS),
	DeleteKeys = verify_status(FirstKey),
	delete_keys(DeleteKeys).

verify_status(Key) ->
	verify_status(Key, []).

verify_status(Key, List) ->
	case Key of
		'$end_of_table' -> List;
		UUID -> 
			Next = ets:next(?TRANSACTIONS, Key),
			case atol:get_status(UUID) of
				done -> 		
					verify_status(Next, [Key|List]);
				wait ->
					verify_status(Next, List);
				fail ->
					verify_status(Next, List);
				_ ->
					verify_status(Next, List)
			end
	end.

delete_keys([]) ->
	[];

delete_keys([H|T]) ->
	ets:delete(?TRANSACTIONS, H),
	delete_keys(T).

get_status(UUID) ->
	case check_token() of
		ok ->
			do_get(UUID);
		Error ->
			Error
	end.

do_get(UUID) ->
	[{_,Group}] = ets:lookup(?ETS, group),
	[{_,Token}] = ets:lookup(?ETS, token),
	[{_,API}] = ets:lookup(?ETS, api),
	Body = <<>>,
	Site = l:l2b([<<"https://online.atol.ru/possystem/">>,
		API,<<"/">>, Group, <<"/report/">>, UUID, <<"?tokenid=">>, Token ]),
	Headers = [],
	{ok, _, _, Ref} = hackney:request(get, Site, Headers, Body, []),
	{ok, Answer} = hackney:body(Ref),
	JSON = jsone:decode(Answer, [{object_format, proplist}]),
	io:format("Body JSON~n~p~n", [JSON]),
	Status = proplists:get_value(<<"status">>, JSON),
	case Status of
		<<"done">> -> 
			ets:delete(?ETS, UUID),
			done;
		<<"wait">> -> wait;
		<<"fail">> -> fail
	end.



auth() ->
	Worker = poolboy:checkout(atol_worker),
	gen_server:call(Worker, {auth}),
	poolboy:checkin(atol_worker, Worker).

sell(Id, Attributes, Items) ->
	Worker = poolboy:checkout(atol_worker),
	gen_server:call(Worker, {sell, Id, Attributes, Items}),
	poolboy:checkin(atol_worker, Worker).


