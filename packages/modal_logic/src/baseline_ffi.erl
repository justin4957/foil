-module(baseline_ffi).
-export([read_file/1, write_file/2, ensure_dir/1]).

read_file(Path) ->
    file:read_file(Path).

write_file(Path, Data) ->
    case file:write_file(Path, Data) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.

ensure_dir(Path) ->
    case filelib:ensure_dir(Path) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.
