%% Z3 NIF Module
%% This module loads the native Z3 NIF library and exposes its functions to Gleam.
%% The NIF library provides direct bindings to libz3.

-module(z3_nif).
-export([
    is_available/0,
    version/0,
    mk_context/0,
    del_context/1,
    mk_solver/1,
    solver_check/1,
    solver_get_model/1,
    solver_push/1,
    solver_pop/2,
    solver_reset/1,
    model_to_string/1
]).

-on_load(init/0).

%% Load the NIF library on module load
init() ->
    PrivDir = case code:priv_dir(z3_gleam) of
        {error, _} ->
            %% Fallback for development
            "priv";
        Dir ->
            Dir
    end,
    NifPath = filename:join(PrivDir, "z3_nif"),
    case erlang:load_nif(NifPath, 0) of
        ok -> ok;
        {error, {reload, _}} -> ok;
        {error, _Reason} ->
            %% NIF not available - functions will return z3_not_available
            ok
    end.

%% NIF stubs - these are replaced by the native code when loaded
is_available() -> false.
version() -> z3_not_available.
mk_context() -> {error, z3_not_available}.
del_context(_Context) -> {error, z3_not_available}.
mk_solver(_Context) -> {error, z3_not_available}.
solver_check(_Solver) -> {error, z3_not_available}.
solver_get_model(_Solver) -> {error, z3_not_available}.
solver_push(_Solver) -> {error, z3_not_available}.
solver_pop(_Solver, _N) -> {error, z3_not_available}.
solver_reset(_Solver) -> {error, z3_not_available}.
model_to_string(_Model) -> {error, z3_not_available}.
