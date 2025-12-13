%% Z3 Port FFI module
%% Provides Erlang functions for port communication with the Z3 Python driver

-module(z3_port_ffi).
-export([open_port/1, close_port/1, send_request/2, receive_response/1, receive_ready/1,
         get_int_field/2, get_string_field/2]).

%% @doc Open a port to the Z3 driver
open_port(Command) ->
    try
        CommandList = binary_to_list(Command),
        Port = erlang:open_port({spawn, CommandList}, [
            {line, 65536},
            binary,
            exit_status,
            use_stdio
        ]),
        {ok, Port}
    catch
        _:Reason ->
            {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% @doc Close the port
close_port(Port) ->
    try
        erlang:port_close(Port),
        nil
    catch
        _:_ -> nil
    end.

%% @doc Send a JSON request to the port
send_request(Port, JsonRequest) ->
    try
        JsonBinary = gleam_json:encode(JsonRequest),
        Message = <<JsonBinary/binary, "\n">>,
        erlang:port_command(Port, Message),
        {ok, nil}
    catch
        _:Reason ->
            {error, {port_error, list_to_binary(io_lib:format("Send failed: ~p", [Reason]))}}
    end.

%% @doc Receive a JSON response from the port
receive_response(Port) ->
    receive
        {Port, {data, {eol, Line}}} ->
            try
                Decoded = gleam_json:decode(Line),
                case Decoded of
                    {ok, Json} ->
                        case maps:get(<<"error">>, Json, undefined) of
                            undefined -> {ok, Json};
                            Error -> {error, {solver_error, Error}}
                        end;
                    {error, _} ->
                        {error, {parse_error, <<"Failed to parse JSON response">>}}
                end
            catch
                _:_ ->
                    {error, {parse_error, <<"Failed to decode response">>}}
            end;
        {Port, {exit_status, Status}} ->
            {error, {port_error, list_to_binary(io_lib:format("Port exited with status ~p", [Status]))}};
        Other ->
            {error, {port_error, list_to_binary(io_lib:format("Unexpected message: ~p", [Other]))}}
    after 30000 ->
        {error, {timeout_error, 30000}}
    end.

%% @doc Wait for the ready signal from the port
receive_ready(Port) ->
    receive
        {Port, {data, {eol, Line}}} ->
            try
                Decoded = gleam_json:decode(Line),
                case Decoded of
                    {ok, #{<<"ready">> := true}} -> {ok, nil};
                    {ok, #{<<"error">> := Error}} -> {error, {port_error, Error}};
                    _ -> {error, {port_error, <<"Unexpected ready response">>}}
                end
            catch
                _:_ ->
                    {error, {parse_error, <<"Failed to parse ready response">>}}
            end;
        {Port, {exit_status, Status}} ->
            {error, {port_error, list_to_binary(io_lib:format("Port exited with status ~p", [Status]))}}
    after 10000 ->
        {error, {timeout_error, 10000}}
    end.

%% @doc Get an integer field from a map (for dynamic field access)
get_int_field(Map, Field) when is_map(Map), is_binary(Field) ->
    case maps:get(Field, Map, undefined) of
        undefined -> {error, nil};
        Value when is_integer(Value) -> {ok, Value};
        _ -> {error, nil}
    end;
get_int_field(_, _) ->
    {error, nil}.

%% @doc Get a string field from a map (for dynamic field access)
get_string_field(Map, Field) when is_map(Map), is_binary(Field) ->
    case maps:get(Field, Map, undefined) of
        undefined -> {error, nil};
        Value when is_binary(Value) -> {ok, Value};
        Value when is_list(Value) -> {ok, list_to_binary(Value)};
        _ -> {error, nil}
    end;
get_string_field(_, _) ->
    {error, nil}.
