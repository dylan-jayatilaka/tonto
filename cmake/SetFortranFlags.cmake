########################################################################
# Determine the appropriate flags for this compiler for each build type.
# For each option type, a list of possible flags is given that work
# for various compilers.  The first flag that works is chosen.
# If none of the flags work, nothing is added (unless the REQUIRED
# flag is given in the call).  This way unknown compiles are supported.
#######################################################################

SET(GNUNATIVE "-mtune=native")
SET(GNUGENERIC "-mtune=generic")
if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
    set(COMPILER "Intel_ifort")
    if(WIN32)
        set(HOST_FLAG "")
        set(DEBUG_FLAGS "/Od /warn:all /traceback /check:bounds -DUSE_PRECONDITIONS -DDEBUG")
        set(RELEASE_FLAGS "/O2 /libs:static /Qunroll /warn:none -DUSE_ERROR_MANAGEMENT")
    else()
        set(HOST_FLAG "-xHost")
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
  # check_compiler_flag(Fortran -fallow-invalid-boz HAS_BOZ)
    execute_process(COMMAND gfortran -dumpversion OUTPUT_VARIABLE GVERSION)
  # if(GVERSION STRGREATER_EQUAL 9) 
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-sign-zero -ffree-line-length-none -fallow-invalid-boz")
  # else()
  # set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-sign-zero -ffree-line-length-none")
  # endif()
    set(HOST_FLAG ${GNUNATIVE})
    set(DEBUG_FLAGS   "-Og -Wall -g -fbacktrace -fcheck=bounds -Wno-maybe-uninitialized -Wno-uninitialized -DUSE_PRECONDITIONS -DDEBUG")
    set(RELEASE_FLAGS "-O -fbacktrace -DUSE_ERROR_MANAGEMENT")
    set(FAST_FLAGS    "-Ofast -faggressive-loop-optimizations -fstrict-aliasing -march=native")
  # set(FAST_FLAGS    "-Ofast -faggressive-loop-optimizations -march=native")
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
