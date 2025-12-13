/**
 * Z3 NIF Implementation
 *
 * This file provides Native Implemented Functions (NIFs) for Z3 integration.
 * It links directly against libz3 and provides a typed interface to Gleam.
 *
 * Build requirements:
 * - Z3 library installed (libz3.so/dylib/dll)
 * - Z3 headers (z3.h)
 * - Erlang NIF headers (erl_nif.h)
 *
 * Build command (example):
 *   gcc -shared -fPIC -o priv/z3_nif.so c_src/z3_nif.c \
 *       -I$ERL_INCLUDE_PATH -lz3
 */

#include <string.h>
#include <stdio.h>
#include "erl_nif.h"

#ifdef Z3_AVAILABLE
#include <z3.h>
#endif

/* Resource types for Z3 objects */
static ErlNifResourceType* CONTEXT_RESOURCE;
static ErlNifResourceType* SOLVER_RESOURCE;
static ErlNifResourceType* MODEL_RESOURCE;

/* Wrapper structs for Z3 handles */
typedef struct {
#ifdef Z3_AVAILABLE
    Z3_context ctx;
#else
    void* ctx;
#endif
} ContextWrapper;

typedef struct {
#ifdef Z3_AVAILABLE
    Z3_solver solver;
    Z3_context ctx;  /* Reference to parent context */
#else
    void* solver;
    void* ctx;
#endif
} SolverWrapper;

typedef struct {
#ifdef Z3_AVAILABLE
    Z3_model model;
    Z3_context ctx;
#else
    void* model;
    void* ctx;
#endif
} ModelWrapper;

/* Atoms */
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_SAT;
static ERL_NIF_TERM ATOM_UNSAT;
static ERL_NIF_TERM ATOM_UNKNOWN;
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_NIL;
static ERL_NIF_TERM ATOM_NOT_AVAILABLE;

/* Resource destructors */
static void context_destructor(ErlNifEnv* env, void* obj) {
#ifdef Z3_AVAILABLE
    ContextWrapper* wrapper = (ContextWrapper*)obj;
    if (wrapper->ctx) {
        Z3_del_context(wrapper->ctx);
        wrapper->ctx = NULL;
    }
#endif
}

static void solver_destructor(ErlNifEnv* env, void* obj) {
#ifdef Z3_AVAILABLE
    SolverWrapper* wrapper = (SolverWrapper*)obj;
    if (wrapper->solver && wrapper->ctx) {
        Z3_solver_dec_ref(wrapper->ctx, wrapper->solver);
        wrapper->solver = NULL;
    }
#endif
}

static void model_destructor(ErlNifEnv* env, void* obj) {
#ifdef Z3_AVAILABLE
    ModelWrapper* wrapper = (ModelWrapper*)obj;
    if (wrapper->model && wrapper->ctx) {
        Z3_model_dec_ref(wrapper->ctx, wrapper->model);
        wrapper->model = NULL;
    }
#endif
}

/* NIF: Check if Z3 is available */
static ERL_NIF_TERM nif_is_available(ErlNifEnv* env, int argc,
                                      const ERL_NIF_TERM argv[]) {
#ifdef Z3_AVAILABLE
    return ATOM_TRUE;
#else
    return ATOM_FALSE;
#endif
}

/* NIF: Get Z3 version */
static ERL_NIF_TERM nif_version(ErlNifEnv* env, int argc,
                                 const ERL_NIF_TERM argv[]) {
#ifdef Z3_AVAILABLE
    unsigned major, minor, build, revision;
    Z3_get_version(&major, &minor, &build, &revision);

    char version[64];
    snprintf(version, sizeof(version), "%u.%u.%u.%u",
             major, minor, build, revision);

    return enif_make_string(env, version, ERL_NIF_LATIN1);
#else
    return ATOM_NOT_AVAILABLE;
#endif
}

/* NIF: Create a new context */
static ERL_NIF_TERM nif_mk_context(ErlNifEnv* env, int argc,
                                    const ERL_NIF_TERM argv[]) {
#ifdef Z3_AVAILABLE
    /* Create Z3 context with default config */
    Z3_config cfg = Z3_mk_config();
    Z3_context ctx = Z3_mk_context(cfg);
    Z3_del_config(cfg);

    if (!ctx) {
        return enif_make_tuple2(env, ATOM_ERROR,
            enif_make_string(env, "Failed to create context", ERL_NIF_LATIN1));
    }

    /* Wrap in resource */
    ContextWrapper* wrapper = enif_alloc_resource(CONTEXT_RESOURCE,
                                                   sizeof(ContextWrapper));
    wrapper->ctx = ctx;

    ERL_NIF_TERM resource = enif_make_resource(env, wrapper);
    enif_release_resource(wrapper);

    return enif_make_tuple2(env, ATOM_OK, resource);
#else
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_NOT_AVAILABLE);
#endif
}

/* NIF: Delete a context */
static ERL_NIF_TERM nif_del_context(ErlNifEnv* env, int argc,
                                     const ERL_NIF_TERM argv[]) {
#ifdef Z3_AVAILABLE
    ContextWrapper* wrapper;
    if (!enif_get_resource(env, argv[0], CONTEXT_RESOURCE, (void**)&wrapper)) {
        return enif_make_tuple2(env, ATOM_ERROR,
            enif_make_string(env, "Invalid context resource", ERL_NIF_LATIN1));
    }

    if (wrapper->ctx) {
        Z3_del_context(wrapper->ctx);
        wrapper->ctx = NULL;
    }

    return ATOM_OK;
#else
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_NOT_AVAILABLE);
#endif
}

/* NIF: Create a solver */
static ERL_NIF_TERM nif_mk_solver(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
#ifdef Z3_AVAILABLE
    ContextWrapper* ctx_wrapper;
    if (!enif_get_resource(env, argv[0], CONTEXT_RESOURCE, (void**)&ctx_wrapper)) {
        return enif_make_tuple2(env, ATOM_ERROR,
            enif_make_string(env, "Invalid context resource", ERL_NIF_LATIN1));
    }

    Z3_solver solver = Z3_mk_solver(ctx_wrapper->ctx);
    Z3_solver_inc_ref(ctx_wrapper->ctx, solver);

    SolverWrapper* wrapper = enif_alloc_resource(SOLVER_RESOURCE,
                                                  sizeof(SolverWrapper));
    wrapper->solver = solver;
    wrapper->ctx = ctx_wrapper->ctx;

    ERL_NIF_TERM resource = enif_make_resource(env, wrapper);
    enif_release_resource(wrapper);

    return enif_make_tuple2(env, ATOM_OK, resource);
#else
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_NOT_AVAILABLE);
#endif
}

/* NIF: Check satisfiability */
static ERL_NIF_TERM nif_solver_check(ErlNifEnv* env, int argc,
                                      const ERL_NIF_TERM argv[]) {
#ifdef Z3_AVAILABLE
    SolverWrapper* wrapper;
    if (!enif_get_resource(env, argv[0], SOLVER_RESOURCE, (void**)&wrapper)) {
        return enif_make_tuple2(env, ATOM_ERROR,
            enif_make_string(env, "Invalid solver resource", ERL_NIF_LATIN1));
    }

    Z3_lbool result = Z3_solver_check(wrapper->ctx, wrapper->solver);

    switch (result) {
        case Z3_L_TRUE:
            return enif_make_tuple2(env, ATOM_OK, ATOM_SAT);
        case Z3_L_FALSE:
            return enif_make_tuple2(env, ATOM_OK, ATOM_UNSAT);
        default:
            return enif_make_tuple2(env, ATOM_OK, ATOM_UNKNOWN);
    }
#else
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_NOT_AVAILABLE);
#endif
}

/* NIF: Get model from solver */
static ERL_NIF_TERM nif_solver_get_model(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[]) {
#ifdef Z3_AVAILABLE
    SolverWrapper* wrapper;
    if (!enif_get_resource(env, argv[0], SOLVER_RESOURCE, (void**)&wrapper)) {
        return enif_make_tuple2(env, ATOM_ERROR,
            enif_make_string(env, "Invalid solver resource", ERL_NIF_LATIN1));
    }

    Z3_model model = Z3_solver_get_model(wrapper->ctx, wrapper->solver);
    if (!model) {
        return enif_make_tuple2(env, ATOM_ERROR,
            enif_make_string(env, "No model available", ERL_NIF_LATIN1));
    }

    Z3_model_inc_ref(wrapper->ctx, model);

    ModelWrapper* model_wrapper = enif_alloc_resource(MODEL_RESOURCE,
                                                       sizeof(ModelWrapper));
    model_wrapper->model = model;
    model_wrapper->ctx = wrapper->ctx;

    ERL_NIF_TERM resource = enif_make_resource(env, model_wrapper);
    enif_release_resource(model_wrapper);

    return enif_make_tuple2(env, ATOM_OK, resource);
#else
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_NOT_AVAILABLE);
#endif
}

/* NIF: Push solver scope */
static ERL_NIF_TERM nif_solver_push(ErlNifEnv* env, int argc,
                                     const ERL_NIF_TERM argv[]) {
#ifdef Z3_AVAILABLE
    SolverWrapper* wrapper;
    if (!enif_get_resource(env, argv[0], SOLVER_RESOURCE, (void**)&wrapper)) {
        return enif_make_tuple2(env, ATOM_ERROR,
            enif_make_string(env, "Invalid solver resource", ERL_NIF_LATIN1));
    }

    Z3_solver_push(wrapper->ctx, wrapper->solver);
    return ATOM_OK;
#else
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_NOT_AVAILABLE);
#endif
}

/* NIF: Pop solver scope */
static ERL_NIF_TERM nif_solver_pop(ErlNifEnv* env, int argc,
                                    const ERL_NIF_TERM argv[]) {
#ifdef Z3_AVAILABLE
    SolverWrapper* wrapper;
    unsigned int n;

    if (!enif_get_resource(env, argv[0], SOLVER_RESOURCE, (void**)&wrapper)) {
        return enif_make_tuple2(env, ATOM_ERROR,
            enif_make_string(env, "Invalid solver resource", ERL_NIF_LATIN1));
    }

    if (!enif_get_uint(env, argv[1], &n)) {
        return enif_make_tuple2(env, ATOM_ERROR,
            enif_make_string(env, "Invalid pop count", ERL_NIF_LATIN1));
    }

    Z3_solver_pop(wrapper->ctx, wrapper->solver, n);
    return ATOM_OK;
#else
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_NOT_AVAILABLE);
#endif
}

/* NIF: Reset solver */
static ERL_NIF_TERM nif_solver_reset(ErlNifEnv* env, int argc,
                                      const ERL_NIF_TERM argv[]) {
#ifdef Z3_AVAILABLE
    SolverWrapper* wrapper;
    if (!enif_get_resource(env, argv[0], SOLVER_RESOURCE, (void**)&wrapper)) {
        return enif_make_tuple2(env, ATOM_ERROR,
            enif_make_string(env, "Invalid solver resource", ERL_NIF_LATIN1));
    }

    Z3_solver_reset(wrapper->ctx, wrapper->solver);
    return ATOM_OK;
#else
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_NOT_AVAILABLE);
#endif
}

/* NIF: Model to string */
static ERL_NIF_TERM nif_model_to_string(ErlNifEnv* env, int argc,
                                         const ERL_NIF_TERM argv[]) {
#ifdef Z3_AVAILABLE
    ModelWrapper* wrapper;
    if (!enif_get_resource(env, argv[0], MODEL_RESOURCE, (void**)&wrapper)) {
        return enif_make_tuple2(env, ATOM_ERROR,
            enif_make_string(env, "Invalid model resource", ERL_NIF_LATIN1));
    }

    const char* str = Z3_model_to_string(wrapper->ctx, wrapper->model);
    return enif_make_tuple2(env, ATOM_OK,
        enif_make_string(env, str, ERL_NIF_LATIN1));
#else
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_NOT_AVAILABLE);
#endif
}

/* NIF function table */
static ErlNifFunc nif_funcs[] = {
    {"is_available", 0, nif_is_available},
    {"version", 0, nif_version},
    {"mk_context", 0, nif_mk_context},
    {"del_context", 1, nif_del_context},
    {"mk_solver", 1, nif_mk_solver},
    {"solver_check", 1, nif_solver_check},
    {"solver_get_model", 1, nif_solver_get_model},
    {"solver_push", 1, nif_solver_push},
    {"solver_pop", 2, nif_solver_pop},
    {"solver_reset", 1, nif_solver_reset},
    {"model_to_string", 1, nif_model_to_string},
};

/* Module load callback */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    /* Create atoms */
    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_ERROR = enif_make_atom(env, "error");
    ATOM_SAT = enif_make_atom(env, "sat");
    ATOM_UNSAT = enif_make_atom(env, "unsat");
    ATOM_UNKNOWN = enif_make_atom(env, "unknown");
    ATOM_TRUE = enif_make_atom(env, "true");
    ATOM_FALSE = enif_make_atom(env, "false");
    ATOM_NIL = enif_make_atom(env, "nil");
    ATOM_NOT_AVAILABLE = enif_make_atom(env, "z3_not_available");

    /* Create resource types */
    CONTEXT_RESOURCE = enif_open_resource_type(
        env, NULL, "z3_context", context_destructor,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    SOLVER_RESOURCE = enif_open_resource_type(
        env, NULL, "z3_solver", solver_destructor,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    MODEL_RESOURCE = enif_open_resource_type(
        env, NULL, "z3_model", model_destructor,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    return 0;
}

/* NIF module initialization */
ERL_NIF_INIT(z3_nif, nif_funcs, load, NULL, NULL, NULL)
