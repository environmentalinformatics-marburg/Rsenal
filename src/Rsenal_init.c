#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP Rsenal_findudC(SEXP);
extern SEXP Rsenal_ioaC(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"Rsenal_findudC", (DL_FUNC) &Rsenal_findudC, 1},
    {"Rsenal_ioaC",    (DL_FUNC) &Rsenal_ioaC,    2},
    {NULL, NULL, 0}
};

void R_init_Rsenal(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
