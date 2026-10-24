/* Define to the version of this package. */
#define PACKAGE_VERSION "7.1.2"

#ifdef HAVE_FORTRAN
#include "Libxc_MANGLE.h"
#define FC_FUNC FC_FUNC_GLOBAL
#define FC_FUNC_ FC_FUNC_GLOBAL_
#endif

/* #undef ENABLE_CUDA */
#ifdef ENABLE_CUDA
#define HAVE_CUDA 1
#endif

/* #undef ENABLE_HIP */
#ifdef ENABLE_HIP
#define HAVE_HIP 1
#define HAVE_CUDA 1
#endif

/* #undef XC_CHECK_NUMERICS */
/* #undef XC_PROFILING */

#include <stdio.h>

