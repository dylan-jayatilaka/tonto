########################################################################
# Determine the appropriate flags for this compiler for each build type.
# For each option type, a list of possible flags is given that work
# for various compilers.  The first flag that works is chosen.
# If none of the flags work, nothing is added (unless the REQUIRED
# flag is given in the call).  This way unknown compiles are supported.
#######################################################################

SET(GNUNATIVE "-mtune=native")
SET(GNUGENERIC "-mtune=generic")

# Architecture tuning.
#
# -march=native, -mtune=native and Intel's -xHost all tune for the machine
# running the COMPILER. Under cross-compilation -- e.g. an HPC login node
# building for compute nodes -- that is the wrong machine: at best the target's
# ISA extensions go unused, at worst the binary dies with SIGILL on the target.
# So native tuning is switched off automatically when CMAKE_CROSSCOMPILING is
# set, and must be stated explicitly for the target instead.
#
#   -DTONTO_ARCH_FLAG=auto                     (default) tune for the build host
#   -DTONTO_ARCH_FLAG=none                     no architecture tuning at all
#   -DTONTO_ARCH_FLAG="-march=znver3"          explicit flags for the target CPU
#
set(TONTO_ARCH_FLAG "auto" CACHE STRING
    "Architecture tuning: 'auto' (tune for build host), 'none', or explicit flags for the target CPU")

set(TONTO_ARCH_EXPLICIT "")
set(TONTO_ARCH_NATIVE OFF)
if(TONTO_ARCH_FLAG STREQUAL "auto")
    if(CMAKE_CROSSCOMPILING)
        message(STATUS
            "Cross-compiling: native architecture tuning disabled (it would tune for the "
            "build host, not the target). Pass -DTONTO_ARCH_FLAG=\"<flags>\" to tune for "
            "the target CPU.")
    else()
        set(TONTO_ARCH_NATIVE ON)
    endif()
elseif(TONTO_ARCH_FLAG STREQUAL "none")
    message(STATUS "Architecture tuning disabled (TONTO_ARCH_FLAG=none)")
else()
    set(TONTO_ARCH_EXPLICIT "${TONTO_ARCH_FLAG}")
    message(STATUS "Architecture tuning (explicit): ${TONTO_ARCH_EXPLICIT}")
endif()
if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
    set(COMPILER "Intel_ifort")
    if(WIN32)
        set(HOST_FLAG "")
        set(DEBUG_FLAGS "/Od /warn:all /traceback /check:bounds -DUSE_PRECONDITIONS -DDEBUG")
        set(RELEASE_FLAGS "/O2 /libs:static /Qunroll /warn:none -DUSE_ERROR_MANAGEMENT")
    else()
        # -xHost tunes for the build host; see TONTO_ARCH_FLAG above.
        if(TONTO_ARCH_NATIVE)
            set(HOST_FLAG "-xHost")
        else()
            set(HOST_FLAG "${TONTO_ARCH_EXPLICIT}")
        endif()
        set(DEBUG_FLAGS "-g -warn all -traceback -check all -debug -DDEBUG")
        set(RELEASE_FLAGS "-O2 -warn none -traceback -DUSE_ERROR_MANAGEMENT")
    endif()
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "PGI")
    set(COMPILER      "PGI_pgfortran")
    set(HOST_FLAG     "-ta=host")
    set(DEBUG_FLAGS   "-O0 -traceback -Mbounds -DUSE_PRECONDITIONS -DDEBUG")
    set(RELEASE_FLAGS "-fast -Mipa=fast,inline -Munroll -Minline -Mvect -DUSE_ERROR_MANAGEMENT")
    set(FAST_FLAGS    "-fast -Mipa=fast,inline -Munroll -Minline -Mvect")
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
    set(COMPILER      "GNU_gfortran")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-sign-zero -ffree-line-length-none -fallow-invalid-boz")
    # Architecture-tuning flag; see TONTO_ARCH_FLAG above. gfortran on Apple
    # Silicon (arm64 macOS) does not accept the x86-style -march=native reliably,
    # so use the CPU-specific -mcpu flag there.
    if(TONTO_ARCH_NATIVE)
        if(APPLE AND CMAKE_SYSTEM_PROCESSOR MATCHES "arm64|aarch64")
            set(ARCH_FLAG "-mcpu=apple-m2")
        else()
            set(ARCH_FLAG "-march=native")
        endif()
        set(HOST_FLAG ${GNUNATIVE})
    else()
        set(ARCH_FLAG "${TONTO_ARCH_EXPLICIT}")
        set(HOST_FLAG "")
    endif()
    # WORKAROUND (2026-07-28): gfortran 14.3 on arm64 macOS miscompiles
    # shell1quartet.F90 at -O2/-Ofast when the pre-register-allocation
    # instruction scheduler is on. The two-electron integrals come out slightly
    # too small and every downstream quantity silently inherits it -- the oxygen
    # atom converged to -77.6178 Ha, ~2.8 Ha *below* the variational limit
    # (virial -V/T = 1.957 instead of 2.000), and rgbi/BN's Roby populations were
    # wrong. Bisected to -fschedule-insns: -O2 is wrong, -O2 -fno-schedule-insns,
    # -O1 and -O0 are all correct, with -fno-fast-math already in force (so this
    # is not FP semantics). Disabled for the whole arm64-macOS build rather than
    # just shell1quartet.F90: one file is proven miscompiled, but nothing shows
    # it is the only one, and a silent wrong-answer bug is worth the small cost.
    # Must be appended AFTER the -O flags (see below) or -O2 re-enables it.
    if(APPLE AND CMAKE_SYSTEM_PROCESSOR MATCHES "arm64|aarch64")
        set(WORKAROUND_FLAGS "-fno-schedule-insns")
        message(STATUS "arm64 macOS: adding -fno-schedule-insns "
                       "(works around a gfortran miscompilation of shell1quartet.F90)")
    endif()
    set(DEBUG_FLAGS   "-Wall -g -fbacktrace -fcheck=bounds -Wno-maybe-uninitialized -Wno-uninitialized -DUSE_PRECONDITIONS -DDEBUG=1")
    set(RELEASE_FLAGS "-Ofast ${ARCH_FLAG} -DUSE_ERROR_MANAGEMENT")
    set(FAST_FLAGS    "-Ofast -faggressive-loop-optimizations -fstrict-aliasing ${ARCH_FLAG} -DUSE_ERROR_MANAGEMENT")
  # set(FAST_FLAGS    "-Ofast -faggressive-loop-optimizations ${ARCH_FLAG}")
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "NAG")
    set(COMPILER      "NAG_nagfor")
    set(HOST_FLAG     "-dusty -kind=byte -maxcontin=1023")
    set(RELEASE_FLAGS "-no_underflow_warning -w -O4 -DUSE_ERROR_MANAGEMENT")
    set(FAST_FLAGS    "-no_underflow_warning -w -O4")
else()
    set(COMPILER "Unknown")
    message(STATUS "Unknown Fortran compiler, just trying -O2 for RELEASE and -g for debug")
    set(DEBUG_FLAGS   "-g  -DUSE_PRECONDITIONS -DDEBUG")
    set(RELEASE_FLAGS "-O2 -DUSE_ERROR_MANAGEMENT")
    set(FAST_FLAGS    "-O2")
endif()

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -D${COMPILER} -D${COMPILER}_on_${CMAKE_SYSTEM_NAME} ${HOST_FLAG}")
# Make sure the build type is uppercase
string(TOUPPER "${CMAKE_BUILD_TYPE}" BT)

if(BT STREQUAL "RELEASE")
    set(CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are DEBUG, RELEASE, or TESTING."
      FORCE)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${RELEASE_FLAGS}")
elseif(BT STREQUAL "RELEASE-STATIC")
    set(CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are DEBUG, RELEASE, or TESTING."
      FORCE)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${RELEASE_FLAGS} -static")
elseif(BT STREQUAL "DEBUG")
    set (CMAKE_BUILD_TYPE DEBUG CACHE STRING
      "Choose the type of build, options are DEBUG, RELEASE, or TESTING."
      FORCE)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${DEBUG_FLAGS}")
ELSEIF(BT STREQUAL "TESTING")
    SET (CMAKE_BUILD_TYPE TESTING CACHE STRING
      "Choose the type of build, options are DEBUG, RELEASE, or TESTING."
      FORCE)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${DEBUG_FLAGS}")
ELSEIF(BT STREQUAL "FAST")
    SET (CMAKE_BUILD_TYPE TESTING CACHE STRING
        "Choose the type of build, options are DEBUG, RELEASE, FAST, or TESTING."
      FORCE)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${FAST_FLAGS}")
ELSEIF(NOT BT)
    SET(CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are DEBUG, RELEASE, or TESTING."
      FORCE)
    MESSAGE(STATUS "CMAKE_BUILD_TYPE not provided, default: RELEASE")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${RELEASE_FLAGS}")
ELSE()
    MESSAGE(FATAL_ERROR "CMAKE_BUILD_TYPE not valid, choices are DEBUG, RELEASE, or TESTING")
ENDIF(BT STREQUAL "RELEASE")

# Set default macros
# These are the default kinds from the current build, should be a better way
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DINT_KIND=4 -DBIN_KIND=4 \
-DREAL_KIND=8 -DCPX_KIND=8 -DFLUSH")

if(WITH_MPI)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DMPI=1")
endif()

# Codegen workarounds must come AFTER every -O flag: gcc applies options left to
# right, so a -fno-X placed before -O2 is simply re-enabled by it. CMake appends
# CMAKE_Fortran_FLAGS_<CONFIG> after CMAKE_Fortran_FLAGS, so append there.
if(WORKAROUND_FLAGS)
    foreach(_cfg RELEASE DEBUG TESTING FAST RELWITHDEBINFO MINSIZEREL)
        set(CMAKE_Fortran_FLAGS_${_cfg} "${CMAKE_Fortran_FLAGS_${_cfg}} ${WORKAROUND_FLAGS}")
    endforeach()
endif()
