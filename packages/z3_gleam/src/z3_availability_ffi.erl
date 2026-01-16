%% Z3 Availability FFI module
%% Provides Erlang functions for checking Z3 availability

-module(z3_availability_ffi).
-export([check_z3/0, check_z3_detailed/0]).

%% @doc Check if Z3 is available, returns version or error
check_z3() ->
    case start_z3_probe() of
        {ok, Version} -> {ok, Version};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Check Z3 with detailed information
check_z3_detailed() ->
    case start_z3_probe_detailed() of
        {ok, Version, PythonVersion, DriverPath} ->
            {ok, {Version, PythonVersion, DriverPath}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Start a probe to check Z3 availability
start_z3_probe() ->
    DriverPath = get_driver_path(),
    Command = "python3 -c \"
import sys
try:
    from z3 import get_version_string
    print('OK:' + get_version_string())
except ImportError as e:
    print('ERROR:Z3 Python bindings not installed. Run: pip3 install z3-solver')
    sys.exit(1)
except Exception as e:
    print('ERROR:' + str(e))
    sys.exit(1)
\"",
    case run_command(Command) of
        {ok, Output} ->
            case parse_probe_output(Output) of
                {ok, Version} -> {ok, Version};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Start a detailed probe
start_z3_probe_detailed() ->
    DriverPath = get_driver_path(),
    Command = "python3 -c \"
import sys
try:
    from z3 import get_version_string
    version = get_version_string()
    python_version = sys.version.split()[0]
    print('OK:' + version + ':' + python_version)
except ImportError:
    print('ERROR:Z3 Python bindings not installed')
    sys.exit(1)
except Exception as e:
    print('ERROR:' + str(e))
    sys.exit(1)
\"",
    case run_command(Command) of
        {ok, Output} ->
            case parse_detailed_output(Output, DriverPath) of
                {ok, Version, PythonVersion} ->
                    {ok, Version, PythonVersion, list_to_binary(DriverPath)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get the path to the Python driver
get_driver_path() ->
    Candidates = [
        "priv/port/z3_driver.py",
        "../z3_gleam/priv/port/z3_driver.py",
        "packages/z3_gleam/priv/port/z3_driver.py",
        "_build/default/lib/z3_gleam/priv/port/z3_driver.py"
    ],
    case find_existing_file(Candidates) of
        {ok, Path} -> Path;
        error -> "priv/port/z3_driver.py"
    end.

find_existing_file([]) -> error;
find_existing_file([Path | Rest]) ->
    case filelib:is_regular(Path) of
        true -> {ok, Path};
        false -> find_existing_file(Rest)
    end.

%% @doc Run a shell command and capture output
run_command(Command) ->
    try
        Port = erlang:open_port({spawn, Command}, [
            exit_status,
            stderr_to_stdout,
            binary,
            {line, 1024}
        ]),
        collect_output(Port, <<>>)
    catch
        _:Reason ->
            {error, list_to_binary(io_lib:format("Failed to run command: ~p", [Reason]))}
    end.

%% @doc Collect output from port
collect_output(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            collect_output(Port, <<Acc/binary, Line/binary, "\n">>);
        {Port, {data, {noeol, Line}}} ->
            collect_output(Port, <<Acc/binary, Line/binary>>);
        {Port, {exit_status, 0}} ->
            {ok, Acc};
        {Port, {exit_status, Status}} ->
            case Acc of
                <<>> ->
                    {error, list_to_binary(io_lib:format("Command failed with status ~p", [Status]))};
                _ ->
                    %% Try to parse error from output
                    case parse_probe_output(Acc) of
                        {error, Reason} -> {error, Reason};
                        _ -> {error, Acc}
                    end
            end
    after 10000 ->
        catch erlang:port_close(Port),
        {error, <<"Timeout waiting for Python command">>}
    end.

%% @doc Parse probe output
parse_probe_output(Output) ->
    OutputStr = binary_to_list(string:trim(Output)),
    case string:prefix(OutputStr, "OK:") of
        nomatch ->
            case string:prefix(OutputStr, "ERROR:") of
                nomatch -> {error, list_to_binary(OutputStr)};
                ErrorMsg -> {error, list_to_binary(ErrorMsg)}
            end;
        Version ->
            {ok, list_to_binary(Version)}
    end.

%% @doc Parse detailed output
parse_detailed_output(Output, _DriverPath) ->
    OutputStr = binary_to_list(string:trim(Output)),
    case string:prefix(OutputStr, "OK:") of
        nomatch ->
            case string:prefix(OutputStr, "ERROR:") of
                nomatch -> {error, list_to_binary(OutputStr)};
                ErrorMsg -> {error, list_to_binary(ErrorMsg)}
            end;
        Rest ->
            case string:split(Rest, ":") of
                [Version, PythonVersion | _] ->
                    {ok, list_to_binary(Version), list_to_binary(PythonVersion)};
                [Version] ->
                    {ok, list_to_binary(Version), <<"unknown">>}
            end
    end.
