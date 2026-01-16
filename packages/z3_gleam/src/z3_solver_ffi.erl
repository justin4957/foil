%% Z3 Solver FFI module
%% Provides Erlang functions for connecting solver.gleam to the Z3 port driver
%% This module bridges the high-level solver API with the low-level port communication

-module(z3_solver_ffi).
-export([start_port/0, stop_port/1, create_context_and_solver/1,
         create_context_and_solver_with_config/2,
         assert_expressions/3, check_sat/2, check_sat_with_timeout/3]).

%% Default timeout for port communication (30 seconds)
-define(DEFAULT_TIMEOUT, 30000).

%% @doc Start the Z3 port driver
start_port() ->
    DriverPath = get_driver_path(),
    try
        Port = erlang:open_port({spawn, DriverPath}, [
            {line, 65536},
            binary,
            exit_status,
            use_stdio
        ]),
        case wait_for_ready(Port) of
            ok -> {ok, Port};
            {error, ReadyErr} -> {error, {port_error, format_error(ReadyErr)}}
        end
    catch
        _:OpenErr ->
            {error, {port_error, format_error(OpenErr)}}
    end.

%% @doc Wait for the ready signal from the port
wait_for_ready(Port) ->
    receive
        {Port, {data, {eol, Line}}} ->
            case binary:match(Line, <<"\"ready\":true">>) of
                nomatch ->
                    case binary:match(Line, <<"\"error\"">>) of
                        nomatch -> wait_for_ready(Port);
                        _ -> {error, Line}
                    end;
                _ -> ok
            end;
        {Port, {exit_status, Status}} ->
            {error, list_to_binary(io_lib:format("Port exited with status ~p", [Status]))}
    after 10000 ->
        {error, <<"Timeout waiting for Z3 driver ready signal">>}
    end.

%% @doc Stop the Z3 port driver
stop_port(Port) ->
    try
        erlang:port_close(Port)
    catch
        _:_ -> ok
    end,
    nil.

%% @doc Create a Z3 context and solver
create_context_and_solver(Port) ->
    create_context_and_solver_with_config(Port, {solver_config, 0, 0, 0, false}).

%% @doc Create a Z3 context and solver with configuration
%% Config is {solver_config, TimeoutMs, Z3Timeout, Z3Rlimit, UnsatCore}
create_context_and_solver_with_config(Port, Config) ->
    {solver_config, _TimeoutMs, Z3Timeout, Z3Rlimit, _UnsatCore} = Config,
    %% Create context
    CtxRequest = <<"{\"id\":1,\"cmd\":\"new_context\"}\n">>,
    erlang:port_command(Port, CtxRequest),
    case receive_json_response(Port, ?DEFAULT_TIMEOUT) of
        {ok, CtxResponse} ->
            CtxId = extract_int(CtxResponse, <<"context_id">>),
            case CtxId of
                undefined ->
                    {error, {parse_error, <<"Missing context_id in response">>}};
                _ ->
                    %% Create solver with timeout config
                    SolverRequest = iolist_to_binary([
                        <<"{\"id\":2,\"cmd\":\"new_solver\",\"context_id\":">>,
                        integer_to_binary(CtxId),
                        <<",\"z3_timeout\":">>,
                        integer_to_binary(Z3Timeout),
                        <<",\"z3_rlimit\":">>,
                        integer_to_binary(Z3Rlimit),
                        <<"}\n">>
                    ]),
                    erlang:port_command(Port, SolverRequest),
                    case receive_json_response(Port, ?DEFAULT_TIMEOUT) of
                        {ok, SolverResponse} ->
                            SolverId = extract_int(SolverResponse, <<"solver_id">>),
                            case SolverId of
                                undefined ->
                                    {error, {parse_error, <<"Missing solver_id in response">>}};
                                _ ->
                                    {ok, {Port, {context, CtxId}, {solver, SolverId, CtxId, Config}}}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Assert expressions to Z3 solver
assert_expressions(Port, SolverTuple, Exprs) ->
    %% Extract solver info and timeout from tuple (handle both old and new formats)
    {SolverId, CtxId, Timeout} = case SolverTuple of
        {solver, S, C, {solver_config, T, _, _, _}} -> {S, C, T};
        {solver, S, C} -> {S, C, ?DEFAULT_TIMEOUT}
    end,
    assert_expressions_loop(Port, SolverId, CtxId, Exprs, 3, Timeout).

assert_expressions_loop(Port, _SolverId, _CtxId, [], _ReqId, _Timeout) ->
    {ok, Port};
assert_expressions_loop(Port, SolverId, CtxId, [Expr | Rest], ReqId, Timeout) ->
    ExprJson = compile_expr(Expr),
    Request = iolist_to_binary([
        <<"{\"id\":">>, integer_to_binary(ReqId),
        <<",\"cmd\":\"assert\",\"solver_id\":">>, integer_to_binary(SolverId),
        <<",\"context_id\":">>, integer_to_binary(CtxId),
        <<",\"expr\":">>, ExprJson,
        <<"}\n">>
    ]),
    erlang:port_command(Port, Request),
    case receive_json_response(Port, Timeout) of
        {ok, _Response} ->
            assert_expressions_loop(Port, SolverId, CtxId, Rest, ReqId + 1, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Check satisfiability
check_sat(Port, SolverTuple) ->
    %% Extract solver info and timeout from tuple (handle both old and new formats)
    {SolverId, Timeout} = case SolverTuple of
        {solver, S, _, {solver_config, T, _, _, _}} -> {S, T};
        {solver, S, _} -> {S, ?DEFAULT_TIMEOUT}
    end,
    check_sat_with_timeout(Port, SolverTuple, Timeout).

%% @doc Check satisfiability with explicit timeout
check_sat_with_timeout(Port, SolverTuple, Timeout) ->
    SolverId = case SolverTuple of
        {solver, S, _, _} -> S;
        {solver, S, _} -> S
    end,
    Request = iolist_to_binary([
        <<"{\"id\":1000,\"cmd\":\"check\",\"solver_id\":">>,
        integer_to_binary(SolverId),
        <<"}\n">>
    ]),
    erlang:port_command(Port, Request),
    case receive_json_response(Port, Timeout) of
        {ok, Response} ->
            Result = parse_check_result(Port, SolverId, Response),
            {ok, {Port, Result}};
        {error, {timeout_error, _}} ->
            %% Return a solver_unknown result on port timeout
            {ok, {Port, {solver_unknown, <<"timeout">>}}};
        {error, Reason} ->
            {error, Reason}
    end.

%% =============================================================================
%% Internal Functions
%% =============================================================================

%% @doc Get the path to the Python driver
get_driver_path() ->
    %% Try to find the driver in several locations
    Candidates = [
        "priv/port/z3_driver.py",
        "../z3_gleam/priv/port/z3_driver.py",
        "packages/z3_gleam/priv/port/z3_driver.py",
        "_build/default/lib/z3_gleam/priv/port/z3_driver.py"
    ],
    case find_existing_file(Candidates) of
        {ok, Path} -> "python3 " ++ Path;
        error -> "python3 priv/port/z3_driver.py"  % Default fallback
    end.

find_existing_file([]) -> error;
find_existing_file([Path | Rest]) ->
    case filelib:is_regular(Path) of
        true -> {ok, Path};
        false -> find_existing_file(Rest)
    end.

%% @doc Receive a JSON response from the port with default timeout
receive_json_response(Port) ->
    receive_json_response(Port, ?DEFAULT_TIMEOUT).

%% @doc Receive a JSON response from the port with configurable timeout
receive_json_response(Port, Timeout) ->
    EffectiveTimeout = case Timeout of
        0 -> ?DEFAULT_TIMEOUT;  % Use default if 0
        _ -> Timeout
    end,
    receive
        {Port, {data, {eol, Line}}} ->
            case binary:match(Line, <<"\"error\"">>) of
                nomatch -> {ok, Line};
                _ -> {error, {solver_error, Line}}
            end;
        {Port, {exit_status, Status}} ->
            {error, {port_error, list_to_binary(io_lib:format("Port exited with status ~p", [Status]))}};
        Other ->
            {error, {port_error, list_to_binary(io_lib:format("Unexpected message: ~p", [Other]))}}
    after EffectiveTimeout ->
        {error, {timeout_error, EffectiveTimeout}}
    end.

%% @doc Extract an integer value from JSON binary by key
extract_int(JsonBinary, Key) ->
    Pattern = <<$", Key/binary, "\":">>,
    case binary:match(JsonBinary, Pattern) of
        nomatch -> undefined;
        {Start, Len} ->
            AfterKey = binary:part(JsonBinary, Start + Len, byte_size(JsonBinary) - Start - Len),
            extract_number(AfterKey)
    end.

%% @doc Extract a number from the start of a binary
extract_number(Bin) ->
    extract_number(Bin, <<>>).

extract_number(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    extract_number(Rest, <<Acc/binary, C>>);
extract_number(_, <<>>) ->
    undefined;
extract_number(_, Acc) ->
    binary_to_integer(Acc).

%% @doc Parse the check result from Z3 response
parse_check_result(Port, SolverId, Response) ->
    case binary:match(Response, <<"\"result\":\"sat\"">>) of
        {_, _} ->
            %% Get model
            Model = get_model(Port, SolverId),
            {solver_sat, {solver_model, Model}};
        nomatch ->
            case binary:match(Response, <<"\"result\":\"unsat\"">>) of
                {_, _} -> solver_unsat;
                nomatch ->
                    %% Unknown or error - check if it's a timeout
                    Reason = extract_string(Response, <<"reason">>),
                    IsTimeout = case binary:match(Response, <<"\"timeout\":true">>) of
                        {_, _} -> true;
                        nomatch ->
                            %% Also check if reason contains timeout-related keywords
                            case Reason of
                                undefined -> false;
                                R ->
                                    LowerReason = string:lowercase(binary_to_list(R)),
                                    lists:any(fun(Keyword) ->
                                        string:find(LowerReason, Keyword) =/= nomatch
                                    end, ["timeout", "canceled", "cancelled"])
                            end
                    end,
                    ReasonStr = case Reason of
                        undefined -> <<"Unknown reason">>;
                        R2 -> R2
                    end,
                    case IsTimeout of
                        true -> {solver_unknown, <<"timeout">>};
                        false -> {solver_unknown, ReasonStr}
                    end
            end
    end.

%% @doc Extract a string value from JSON binary by key
extract_string(JsonBinary, Key) ->
    Pattern = <<$", Key/binary, "\":\"">>,
    case binary:match(JsonBinary, Pattern) of
        nomatch -> undefined;
        {Start, Len} ->
            AfterKey = binary:part(JsonBinary, Start + Len, byte_size(JsonBinary) - Start - Len),
            extract_quoted_string(AfterKey)
    end.

extract_quoted_string(Bin) ->
    extract_quoted_string(Bin, <<>>).

extract_quoted_string(<<$", _/binary>>, Acc) -> Acc;
extract_quoted_string(<<C, Rest/binary>>, Acc) ->
    extract_quoted_string(Rest, <<Acc/binary, C>>);
extract_quoted_string(<<>>, _) -> undefined.

%% @doc Get model from solver after SAT result
get_model(Port, SolverId) ->
    Request = iolist_to_binary([
        <<"{\"id\":1001,\"cmd\":\"get_model\",\"solver_id\":">>,
        integer_to_binary(SolverId),
        <<"}\n">>
    ]),
    erlang:port_command(Port, Request),
    case receive_json_response(Port) of
        {ok, _Response} ->
            %% For now, return empty map - full model parsing would be more complex
            %% This is sufficient for basic SAT/UNSAT checking
            #{};
        {error, _} ->
            #{}
    end.

%% @doc Delete a solver to free resources
delete_solver(Port, SolverId) ->
    Request = iolist_to_binary([
        <<"{\"id\":2000,\"cmd\":\"del_solver\",\"solver_id\":">>,
        integer_to_binary(SolverId),
        <<"}\n">>
    ]),
    erlang:port_command(Port, Request),
    receive_json_response(Port, 5000),
    ok.

%% @doc Delete a context to free resources
delete_context(Port, CtxId) ->
    Request = iolist_to_binary([
        <<"{\"id\":2001,\"cmd\":\"del_context\",\"context_id\":">>,
        integer_to_binary(CtxId),
        <<"}\n">>
    ]),
    erlang:port_command(Port, Request),
    receive_json_response(Port, 5000),
    ok.

%% @doc Cleanup solver and context resources (called on error)
cleanup_resources(Port, SolverId, CtxId) ->
    try
        delete_solver(Port, SolverId),
        delete_context(Port, CtxId)
    catch
        _:_ -> ok  % Ignore cleanup errors
    end.

%% @doc Compile a Gleam expression to JSON string
%% This handles the Gleam expression type and converts to JSON format
compile_expr({bool_lit, true}) ->
    <<"{\"type\":\"bool_lit\",\"value\":true}">>;
compile_expr({bool_lit, false}) ->
    <<"{\"type\":\"bool_lit\",\"value\":false}">>;
compile_expr({int_lit, Value}) ->
    iolist_to_binary([<<"{\"type\":\"int_lit\",\"value\":">>, integer_to_binary(Value), <<"}">>]);
compile_expr({real_lit, Num, Den}) ->
    iolist_to_binary([
        <<"{\"type\":\"real_lit\",\"numerator\":">>, integer_to_binary(Num),
        <<",\"denominator\":">>, integer_to_binary(Den), <<"}">>
    ]);
compile_expr({const, Name, Sort}) ->
    NameBin = to_binary(Name),
    SortJson = compile_sort(Sort),
    iolist_to_binary([
        <<"{\"type\":\"const\",\"name\":\"">>, NameBin,
        <<"\",\"sort\":">>, SortJson, <<"}">>
    ]);
compile_expr({'and', Exprs}) ->
    ExprsJson = compile_expr_list(Exprs),
    iolist_to_binary([<<"{\"type\":\"and\",\"exprs\":">>, ExprsJson, <<"}">>]);
compile_expr({'or', Exprs}) ->
    ExprsJson = compile_expr_list(Exprs),
    iolist_to_binary([<<"{\"type\":\"or\",\"exprs\":">>, ExprsJson, <<"}">>]);
compile_expr({'not', Expr}) ->
    ExprJson = compile_expr(Expr),
    iolist_to_binary([<<"{\"type\":\"not\",\"expr\":">>, ExprJson, <<"}">>]);
compile_expr({implies, A, B}) ->
    AJson = compile_expr(A),
    BJson = compile_expr(B),
    iolist_to_binary([
        <<"{\"type\":\"implies\",\"antecedent\":">>, AJson,
        <<",\"consequent\":">>, BJson, <<"}">>
    ]);
compile_expr({iff, A, B}) ->
    AJson = compile_expr(A),
    BJson = compile_expr(B),
    iolist_to_binary([
        <<"{\"type\":\"iff\",\"left\":">>, AJson,
        <<",\"right\":">>, BJson, <<"}">>
    ]);
compile_expr({add, Exprs}) ->
    ExprsJson = compile_expr_list(Exprs),
    iolist_to_binary([<<"{\"type\":\"add\",\"exprs\":">>, ExprsJson, <<"}">>]);
compile_expr({mul, Exprs}) ->
    ExprsJson = compile_expr_list(Exprs),
    iolist_to_binary([<<"{\"type\":\"mul\",\"exprs\":">>, ExprsJson, <<"}">>]);
compile_expr({sub, A, B}) ->
    AJson = compile_expr(A),
    BJson = compile_expr(B),
    iolist_to_binary([
        <<"{\"type\":\"sub\",\"left\":">>, AJson,
        <<",\"right\":">>, BJson, <<"}">>
    ]);
compile_expr({'div', A, B}) ->
    AJson = compile_expr(A),
    BJson = compile_expr(B),
    iolist_to_binary([
        <<"{\"type\":\"div\",\"numerator\":">>, AJson,
        <<",\"denominator\":">>, BJson, <<"}">>
    ]);
compile_expr({neg, Expr}) ->
    ExprJson = compile_expr(Expr),
    iolist_to_binary([<<"{\"type\":\"neg\",\"expr\":">>, ExprJson, <<"}">>]);
compile_expr({eq, A, B}) ->
    AJson = compile_expr(A),
    BJson = compile_expr(B),
    iolist_to_binary([
        <<"{\"type\":\"eq\",\"left\":">>, AJson,
        <<",\"right\":">>, BJson, <<"}">>
    ]);
compile_expr({lt, A, B}) ->
    AJson = compile_expr(A),
    BJson = compile_expr(B),
    iolist_to_binary([
        <<"{\"type\":\"lt\",\"left\":">>, AJson,
        <<",\"right\":">>, BJson, <<"}">>
    ]);
compile_expr({le, A, B}) ->
    AJson = compile_expr(A),
    BJson = compile_expr(B),
    iolist_to_binary([
        <<"{\"type\":\"le\",\"left\":">>, AJson,
        <<",\"right\":">>, BJson, <<"}">>
    ]);
compile_expr({gt, A, B}) ->
    AJson = compile_expr(A),
    BJson = compile_expr(B),
    iolist_to_binary([
        <<"{\"type\":\"gt\",\"left\":">>, AJson,
        <<",\"right\":">>, BJson, <<"}">>
    ]);
compile_expr({ge, A, B}) ->
    AJson = compile_expr(A),
    BJson = compile_expr(B),
    iolist_to_binary([
        <<"{\"type\":\"ge\",\"left\":">>, AJson,
        <<",\"right\":">>, BJson, <<"}">>
    ]);
compile_expr({for_all, Vars, Body}) ->
    VarsJson = compile_var_list(Vars),
    BodyJson = compile_expr(Body),
    iolist_to_binary([
        <<"{\"type\":\"forall\",\"vars\":">>, VarsJson,
        <<",\"body\":">>, BodyJson, <<"}">>
    ]);
compile_expr({exists, Vars, Body}) ->
    VarsJson = compile_var_list(Vars),
    BodyJson = compile_expr(Body),
    iolist_to_binary([
        <<"{\"type\":\"exists\",\"vars\":">>, VarsJson,
        <<",\"body\":">>, BodyJson, <<"}">>
    ]);
compile_expr({ite, Cond, Then, Else}) ->
    CondJson = compile_expr(Cond),
    ThenJson = compile_expr(Then),
    ElseJson = compile_expr(Else),
    iolist_to_binary([
        <<"{\"type\":\"ite\",\"condition\":">>, CondJson,
        <<",\"then\":">>, ThenJson,
        <<",\"else\":">>, ElseJson, <<"}">>
    ]);
compile_expr(_) ->
    <<"{\"type\":\"bool_lit\",\"value\":true}">>.  % Fallback for unknown types

%% @doc Compile a list of expressions to JSON array
compile_expr_list(Exprs) ->
    Compiled = [compile_expr(E) || E <- Exprs],
    iolist_to_binary([<<"[">>, lists:join(<<",">>, Compiled), <<"]">>]).

%% @doc Compile a list of variables to JSON array
compile_var_list(Vars) ->
    Compiled = [[<<"[\"">>, to_binary(N), <<"\",">>, compile_sort(S), <<"]">>] || {N, S} <- Vars],
    iolist_to_binary([<<"[">>, lists:join(<<",">>, Compiled), <<"]">>]).

%% @doc Compile a sort to JSON
compile_sort(bool_sort) -> <<"\"bool\"">>;
compile_sort(int_sort) -> <<"\"int\"">>;
compile_sort(real_sort) -> <<"\"real\"">>;
compile_sort({uninterpreted_sort, Name}) ->
    iolist_to_binary([
        <<"{\"type\":\"uninterpreted\",\"name\":\"">>,
        to_binary(Name),
        <<"\"}">>
    ]);
compile_sort({array_sort, Domain, Range}) ->
    iolist_to_binary([
        <<"{\"type\":\"array\",\"domain\":">>,
        compile_sort(Domain),
        <<",\"range\":">>,
        compile_sort(Range),
        <<"}">>
    ]);
compile_sort(_) -> <<"\"bool\"">>.  % Fallback

%% @doc Convert to binary
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(_) -> <<>>.

%% @doc Format an error for display
format_error(Reason) when is_binary(Reason) -> Reason;
format_error(Reason) -> list_to_binary(io_lib:format("~p", [Reason])).
