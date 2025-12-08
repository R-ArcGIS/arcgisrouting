#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Forward declarations
SEXP decode_compressed_geometry_(SEXP geometry_str);

// Register native routines
static const R_CallMethodDef CallEntries[] = {
    {"decode_compressed_geometry_", (DL_FUNC) &decode_compressed_geometry_, 1},
    {NULL, NULL, 0}
};

void R_init_arcgisrouting(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
