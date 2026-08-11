# CMake
cmake_minimum_required(VERSION 3.20)
cmake_policy(VERSION 3.20)

# Allow arbitrary characters in test names
cmake_policy(SET CMP0110 NEW)

# Tonto version info and build date
string(TIMESTAMP VERSION_MAJOR "%y")
string(TIMESTAMP VERSION_MINOR "%m")
string(TIMESTAMP VERSION_MINOR_MINOR "%d")
string(TIMESTAMP TONTO_BUILD_DATE "%Y.%m.%d %H:%M")

# Tonto
set(TONTO_VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_MINOR_MINOR}")
project(tonto
    VERSION ${TONTO_VERSION}
    LANGUAGES Fortran C)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

# WSL (Windows Subsystem for Linux). No-op on every other host. This MUST come
# before any find_package/find_program: its main job is to take the Windows
# directories off PATH so that no tool search can resolve to a .exe. See
# cmake/WSL.cmake and docs/BUILDING_ON_WINDOWS.md.
include(WSL)
tonto_wsl_preflight()

# Install where?
set(CMAKE_INSTALL_PREFIX /usr/local CACHE PATH "Install prefix")

# Tonto specifics
set(BASIS_SETS ${CMAKE_CURRENT_SOURCE_DIR}/basis_sets 
    CACHE PATH "Basis sets directory")

option(MPI "Use MPI" OFF)
option(COMPILE_LAPACK "Use the reference implementation of lapack provided" OFF)

# Java and ANTLR4
find_package(Java REQUIRED COMPONENTS Runtime Development)
# Under WSL, reject a Windows JDK: it cannot read Linux paths and wants ';' as
# its classpath separator, while the translator below is invoked with ':'.
tonto_wsl_finalize()
# The ANTLR4 tool is a single "complete" jar. It is a release artefact (NOT part
# of the antlr4 source repo), so we cannot get it from a git submodule. If it is
# not already present, download it into external/ (git-ignored) and verify its
# checksum. A system copy or a manually-placed jar is used in preference.
set(ANTLR4_VERSION 4.13.2)
set(ANTLR4_JAR_NAME antlr-${ANTLR4_VERSION}-complete.jar)
set(ANTLR4_JAR_CACHE ${CMAKE_CURRENT_SOURCE_DIR}/external/${ANTLR4_JAR_NAME})
set(ANTLR4_JAR_SHA256 eae2dfa119a64327444672aff63e9ec35a20180dc5b8090b7a6ab85125df4d76)
set(ANTLR4_JAR_CANDIDATES
    ${ANTLR4_JAR_CACHE}
    ${CMAKE_CURRENT_SOURCE_DIR}/external/antlr4/${ANTLR4_JAR_NAME}
    /usr/local/lib/${ANTLR4_JAR_NAME}
)
unset(ANTLR4_JAR CACHE)
foreach(ANTLR4_JAR_CANDIDATE ${ANTLR4_JAR_CANDIDATES})
    if(EXISTS ${ANTLR4_JAR_CANDIDATE})
        set(ANTLR4_JAR ${ANTLR4_JAR_CANDIDATE})
        break()
    endif()
endforeach()
if(NOT ANTLR4_JAR)
    message(STATUS "ANTLR4 jar not found locally; downloading ${ANTLR4_JAR_NAME} into external/ ...")
    file(DOWNLOAD
        https://www.antlr.org/download/${ANTLR4_JAR_NAME}
        ${ANTLR4_JAR_CACHE}
        SHOW_PROGRESS
        EXPECTED_HASH SHA256=${ANTLR4_JAR_SHA256}
        STATUS ANTLR4_DL_STATUS)
    list(GET ANTLR4_DL_STATUS 0 ANTLR4_DL_CODE)
    if(NOT ANTLR4_DL_CODE EQUAL 0)
        file(REMOVE ${ANTLR4_JAR_CACHE})
        list(GET ANTLR4_DL_STATUS 1 ANTLR4_DL_MSG)
        message(FATAL_ERROR "Failed to download ANTLR4 jar (${ANTLR4_DL_MSG}). "
            "Place ${ANTLR4_JAR_NAME} in external/ manually and re-run cmake.")
    endif()
    set(ANTLR4_JAR ${ANTLR4_JAR_CACHE})
endif()
message(STATUS "Using ANTLR4 jar: ${ANTLR4_JAR}")

# Git
find_package(Git)
if(${GIT_FOUND})
    execute_process(COMMAND
        "${GIT_EXECUTABLE}" log "-n 1" "--pretty=%h"
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        OUTPUT_VARIABLE TONTO_GIT_REVISION
        OUTPUT_STRIP_TRAILING_WHITESPACE)
else()
    set(TONTO_GIT_REVISION "Unknown git revision")
endif()

# Testing?
enable_testing()

# BLAS
#set(BLA_VENDOR NVPL)
#set(BLA_VENDOR OpenBLAS)

# On macOS, find_package(LAPACK) picks Apple's Accelerate framework, whose
# Fortran entry points (dsyev_ etc.) are frozen at LAPACK 3.2.1 (2009) for ABI
# compatibility -- Apple's newer LAPACK is only reachable via $NEWLAPACK-suffixed
# symbols, which is not usable from Fortran. That 2009 dsyev differs materially
# from a modern one (verified 2026-07-28 on M2/Tahoe: it alone flipped
# short/h2o_rhf_6-31G(d)_normal_mode_analysis). So prefer Homebrew's OpenBLAS
# (LAPACK 3.12.x) when it is installed; fall back to whatever find_package finds.
# Override by configuring with an explicit -DBLA_VENDOR=... (e.g. Accelerate).
if(APPLE AND NOT DEFINED BLA_VENDOR)
    foreach(_brew_prefix /opt/homebrew/opt/openblas /usr/local/opt/openblas)
        if(EXISTS ${_brew_prefix})
            set(BLA_VENDOR OpenBLAS)
            list(APPEND CMAKE_PREFIX_PATH ${_brew_prefix})
            message(STATUS "macOS: preferring OpenBLAS at ${_brew_prefix} over Accelerate "
                           "(Accelerate's Fortran LAPACK is frozen at 3.2.1)")
            break()
        endif()
    endforeach()
    if(NOT DEFINED BLA_VENDOR)
        message(WARNING
            "macOS: OpenBLAS not found, falling back to Accelerate's LAPACK 3.2.1 (2009). "
            "Install a modern LAPACK with:  brew install openblas")
    endif()
endif()

# LAPACK
# The bundled reference LAPACK (external/lapack-release, netlib 3.8.0) declares an
# ancient cmake_minimum_required (2.8.x) that CMake >= 4 rejects. Permit configuring
# such old subprojects without editing the pinned submodule.
set(CMAKE_POLICY_VERSION_MINIMUM 3.5 CACHE STRING "min policy for old subprojects" FORCE)
set(ENV{CMAKE_POLICY_VERSION_MINIMUM} 3.5)
string(TOUPPER "${CMAKE_BUILD_TYPE}" TEMP)
if(TEMP STREQUAL "RELEASE-STATIC")
  set(COMPILE_LAPACK "ON")
endif()
if(COMPILE_LAPACK)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/external/lapack-release)
    set(LAPACK_LIBRARIES
        ${PROJECT_BINARY_DIR}/external/lapack-release/lib/liblapack.a
        ${PROJECT_BINARY_DIR}/external/lapack-release/lib/libblas.a
    )
    message(STATUS "Compiling lapack: ${LAPACK_LIBRARIES}")
else()
  find_package(LAPACK)
  if(${LAPACK_FOUND})
  else()
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/external/lapack-release)
    set(LAPACK_LIBRARIES
        ${PROJECT_BINARY_DIR}/external/lapack-release/lib/liblapack.a
        ${PROJECT_BINARY_DIR}/external/lapack-release/lib/libblas.a
    )
    message(STATUS "Compiling lapack: ${LAPACK_LIBRARIES}")
    set(COMPILE_LAPACK "ON")
  endif()
endif()

# Report which LAPACK version is actually linked, and warn if it is ancient.
# find_package(LAPACK) reports only "found", not "found, but from 2009" -- which
# is how Accelerate's 3.2.1 was silently used on macOS for a long time. Query the
# library itself via ILAVER (present since LAPACK 3.2) so the version is on the
# record in every configure log, and comparable across platforms.
if(NOT COMPILE_LAPACK AND NOT CMAKE_CROSSCOMPILING)
    set(_ilaver_src ${CMAKE_BINARY_DIR}/CMakeFiles/lapack_version.f90)
    file(WRITE ${_ilaver_src}
         "program v\n"
         "  integer :: a,b,c\n"
         "  call ilaver(a,b,c)\n"
         "  print *, a, b, c\n"
         "end program\n")
    try_run(_ilaver_run _ilaver_build
            ${CMAKE_BINARY_DIR}/CMakeFiles/lapack_version ${_ilaver_src}
            LINK_LIBRARIES ${LAPACK_LIBRARIES}
            RUN_OUTPUT_VARIABLE _ilaver_out)
    if(_ilaver_build AND _ilaver_run EQUAL 0)
        string(REGEX MATCHALL "[0-9]+" _v "${_ilaver_out}")
        list(GET _v 0 _v_major)
        list(GET _v 1 _v_minor)
        list(GET _v 2 _v_patch)
        set(LAPACK_VERSION "${_v_major}.${_v_minor}.${_v_patch}")
        message(STATUS "LAPACK version in use: ${LAPACK_VERSION}")
        if(LAPACK_VERSION VERSION_LESS 3.9)
            message(WARNING
                "LAPACK ${LAPACK_VERSION} is very old (3.9 dates from 2019). Numerical "
                "results -- particularly eigenvectors from dsyev -- may differ noticeably "
                "from those produced with a current LAPACK. Consider installing OpenBLAS.")
        endif()
    else()
        message(STATUS "LAPACK version in use: unknown (ILAVER probe did not run)")
    endif()
endif()

# MPI
if(MPI)
    message(STATUS "Building with MPI enabled")
    if(MSMPI)
       message(STATUS "Building with MSMPI")
       set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -I${msmpi-linux-home}/Include -L${msmpi-linux-home}/Lib/x64 -lmsmpi -D _WIN32")
       set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -I${msmpi-linux-home}/Include -L${msmpi-linux-home}/Lib/x64 -lmsmpi")
       set(CMAKE_C_FLAGS "${CMAKE_CXX_FLAGS} -I${msmpi-linux-home}/Include -L${msmpi-linux-home}/Lib/x64 -lmsmpi")
       set(CMAKE_Fortran_FLAGS "${MPI_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS}")
       set(WITH_MPI ON CACHE STRING "" FORCE)
       set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lmsmpi")
       set(MPI_Fortran_INCLUDE_PATH "${msmpi-linux-home}/Include")
       include_directories("${msmpi-linux-home}/Include")
       set(MPI_LIBRARIES "${msmpi-linux-home}/Lib/x64/libmsmpi.a")
    else()
       # REQUIRED, and with an explicit Fortran component: the old
       # `include(FindMPI)` + `if(MPI_FOUND)` reported a miss as a mere STATUS
       # line, left WITH_MPI unset, and then happily built a SERIAL binary that
       # looked like it had honoured -DMPI=1. -DMPI=1 must never silently
       # produce a serial tonto.
       find_package(MPI REQUIRED COMPONENTS Fortran)

       message(STATUS "MPI found.")
       message(STATUS "MPI Fortran compiler: ${MPI_Fortran_COMPILER}")
       message(STATUS "MPI include: ${MPI_Fortran_INCLUDE_PATH}")
       message(STATUS "MPI libs:${MPI_Fortran_LIBRARIES}")
       # NOTE: modern FindMPI never sets MPI_Fortran_FLAGS (it sets
       # MPI_Fortran_COMPILE_OPTIONS), so this assigns "" -- and it is an
       # assignment, not an append, so it also wipes any user -DCMAKE_Fortran_FLAGS.
       # Survivable only because SetFortranFlags rebuilds the flags afterwards.
       # See DEFERRED.md.
       set(CMAKE_Fortran_FLAGS "${MPI_Fortran_FLAGS}")
       set(WITH_MPI ON CACHE STRING "" FORCE)
       include_directories(${MPI_Fortran_INCLUDE_PATH})

       # Tonto does `USE mpi` (foofiles/parallel.foo) and needs MPI_ADDRESS_KIND
       # from that module (include/macros.in), so it needs MPI Fortran *module*
       # files built by THIS gfortran. Fortran .mod files are compiler-version
       # specific, so an MPI installed against a different gcc fails with
       # "Cannot read module file ... created by a different version of GNU
       # Fortran" -- hundreds of files into the build, long after configure said
       # everything was fine. Catch it here instead.
       include(CheckFortranSourceCompiles)
       # Give the probe the same includes/libs the real build gets, so a failure
       # means "modules unreadable", not "modules not found".
       set(_tonto_saved_req_libs "${CMAKE_REQUIRED_LIBRARIES}")
       set(_tonto_saved_req_incs "${CMAKE_REQUIRED_INCLUDES}")
       set(CMAKE_REQUIRED_LIBRARIES MPI::MPI_Fortran)
       set(CMAKE_REQUIRED_INCLUDES ${MPI_Fortran_INCLUDE_DIRS} ${MPI_Fortran_INCLUDE_PATH})
       check_fortran_source_compiles("
       program t
          use mpi
          implicit none
          integer :: ierr
          call MPI_Init(ierr)
          call MPI_Finalize(ierr)
       end program t
       " TONTO_MPI_MODULE_USABLE SRC_EXT f90)
       set(CMAKE_REQUIRED_LIBRARIES "${_tonto_saved_req_libs}")
       set(CMAKE_REQUIRED_INCLUDES "${_tonto_saved_req_incs}")
       if(NOT TONTO_MPI_MODULE_USABLE)
           message(FATAL_ERROR
             "The MPI installation provides Fortran modules this compiler cannot "
             "read.\n"
             "  Fortran compiler : ${CMAKE_Fortran_COMPILER_ID} "
             "${CMAKE_Fortran_COMPILER_VERSION}\n"
             "  MPI Fortran      : ${MPI_Fortran_COMPILER}\n"
             "Tonto does `USE mpi`, and Fortran .mod files are compiler-version "
             "specific. Rebuild MPI against this compiler (e.g. configure Open MPI "
             "with FC=${CMAKE_Fortran_COMPILER}), or point CMAKE_Fortran_COMPILER "
             "at the wrapper whose MPI modules match. See docs/TONTO_AND_MPI.md.")
       endif()
    endif()
endif()


# If desirable we can use .F03/.f03 by appending to the below variable
# message(STATUS "Allowed fortran suffixes: ${CMAKE_Fortran_SOURCE_FILE_EXTENSIONS}")

# Set the fortran flags
include(SetFortranFlags)
message(STATUS "Fortran Compiler flags: ${CMAKE_Fortran_FLAGS}")
message(STATUS "CXX Compiler Flags: ${CMAKE_CXX_FLAGS}")
message(STATUS "C Compiler Flags: ${CMAKE_C_FLAGS}")

# Provenance stamped into the run banner (see molecule.main.foo). Recording the
# compiler and the LAPACK version makes every stdout self-documenting, so a
# cross-platform comparison does not have to guess what produced each side. This
# was added after a numerical discrepancy between macOS and Linux turned out to
# be a compiler miscompilation, while the two boxes were also unknowingly using
# different gfortran versions AND different LAPACKs -- none of it visible in the
# output. NOTE: scripts/test.py must ignore these lines (prefixes_to_ignore) or
# every reference comparison breaks.
set(TONTO_COMPILER "${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}")
if(DEFINED LAPACK_VERSION)
    set(TONTO_LAPACK "${LAPACK_VERSION}")
elseif(COMPILE_LAPACK)
    set(TONTO_LAPACK "bundled reference (netlib)")
else()
    set(TONTO_LAPACK "unknown")
endif()
message(STATUS "Provenance: compiler ${TONTO_COMPILER}, LAPACK ${TONTO_LAPACK}")

# Macros
set(MACROS ${PROJECT_BINARY_DIR}/macros)
configure_file(
    ${PROJECT_SOURCE_DIR}/include/macros.in
    ${MACROS}
)


# Foo source files
set(FOO_DIR ${CMAKE_CURRENT_SOURCE_DIR}/foofiles)
set(FOOGRAMMAR_DIR ${CMAKE_CURRENT_SOURCE_DIR}/foogrammar)
set(FOO_TYPES_FILE ${FOO_DIR}/types.foo)
set(FOO_ANTLR4_GRAMMAR ${FOOGRAMMAR_DIR}/Foo.g4)
set(FORTRAN_TYPES_FILE types.F90)
set(FOO_SRC
    ${FOO_DIR}/types.foo
    ${FOO_DIR}/object.foo
    ${FOO_DIR}/intrinsic.foo
    ${FOO_DIR}/int.foo
    ${FOO_DIR}/bin.foo
    ${FOO_DIR}/str.foo
    ${FOO_DIR}/real.foo
    ${FOO_DIR}/cpx.foo
    ${FOO_DIR}/vec{object}.foo
    ${FOO_DIR}/vec{intrinsic}.foo
    ${FOO_DIR}/vec{bin}.foo
    ${FOO_DIR}/vec{int}.foo
    ${FOO_DIR}/vec{str}.foo
    ${FOO_DIR}/vec{real}.foo
    ${FOO_DIR}/vec{cpx}.foo
    ${FOO_DIR}/mat{object}.foo
    ${FOO_DIR}/mat{intrinsic}.foo
    ${FOO_DIR}/mat{bin}.foo
    ${FOO_DIR}/mat{int}.foo
    ${FOO_DIR}/mat{str}.foo
    ${FOO_DIR}/mat{real}.foo
    ${FOO_DIR}/mat{cpx}.foo
    ${FOO_DIR}/mat3{intrinsic}.foo
    ${FOO_DIR}/mat3{bin}.foo
    ${FOO_DIR}/mat3{int}.foo
    ${FOO_DIR}/mat3{real}.foo
    ${FOO_DIR}/mat3{cpx}.foo
    ${FOO_DIR}/mat4{object}.foo
    ${FOO_DIR}/mat4{intrinsic}.foo
    ${FOO_DIR}/mat4{int}.foo
    ${FOO_DIR}/mat4{real}.foo
    ${FOO_DIR}/mat4{cpx}.foo
    ${FOO_DIR}/mat5{intrinsic}.foo
    ${FOO_DIR}/mat5{int}.foo
    ${FOO_DIR}/mat5{real}.foo
    ${FOO_DIR}/mat5{cpx}.foo
    ${FOO_DIR}/vec{evec{intrinsic}}.foo
    ${FOO_DIR}/vec{evec{bin}}.foo
    ${FOO_DIR}/vec{evec{int}}.foo
    ${FOO_DIR}/vec{evec{real}}.foo
    ${FOO_DIR}/vec{evec{str}}.foo
    ${FOO_DIR}/mat{evec{int}}.foo
    ${FOO_DIR}/mat{evec{real}}.foo
    ${FOO_DIR}/mat3{evec{int}}.foo
    ${FOO_DIR}/vec{emat{intrinsic}}.foo
    ${FOO_DIR}/vec{emat{bin}}.foo
    ${FOO_DIR}/vec{emat{int}}.foo
    ${FOO_DIR}/vec{emat{real}}.foo
    ${FOO_DIR}/mat{emat{real}}.foo
    ${FOO_DIR}/vec{emat3{intrinsic}}.foo
    ${FOO_DIR}/vec{emat3{real}}.foo
    ${FOO_DIR}/vec{emat4{intrinsic}}.foo
    ${FOO_DIR}/vec{emat4{real}}.foo
    ${FOO_DIR}/vec{evec{evec{intrinsic}}}.foo
    ${FOO_DIR}/vec{evec{evec{int}}}.foo
    ${FOO_DIR}/vec{evec{evec{real}}}.foo
    ${FOO_DIR}/map{vec{key},vec{val}}.foo
    ${FOO_DIR}/map{int,int}.foo
    ${FOO_DIR}/map{key,val}.foo
    ${FOO_DIR}/map{str,int}.foo
    ${FOO_DIR}/map{vec{int},vec{int}}.foo
    ${FOO_DIR}/time.foo
    ${FOO_DIR}/command_line.foo
    ${FOO_DIR}/system.foo
    ${FOO_DIR}/atom.foo
    ${FOO_DIR}/basis.foo
    ${FOO_DIR}/becke_grid.foo
#   ${FOO_DIR}/breakdown_data.foo
    ${FOO_DIR}/buffer.foo
    ${FOO_DIR}/output_style.foo
    ${FOO_DIR}/textfile.foo
#   ${FOO_DIR}/json.foo
    ${FOO_DIR}/file.foo
    ${FOO_DIR}/archive.foo
    ${FOO_DIR}/cif.foo
    ${FOO_DIR}/cluster.foo
    ${FOO_DIR}/colour.foo
    ${FOO_DIR}/colour_function.foo
    ${FOO_DIR}/coppensbasis.foo
    ${FOO_DIR}/coppensorbital.foo
    ${FOO_DIR}/csf_to_sd.foo
    ${FOO_DIR}/dft_functional.foo
    ${FOO_DIR}/diffraction_data.inq.foo
    ${FOO_DIR}/diffraction_data.set.foo
    ${FOO_DIR}/diffraction_data.read.foo
    ${FOO_DIR}/diffraction_data.put.foo
    ${FOO_DIR}/diis.foo
    ${FOO_DIR}/rys.foo
    ${FOO_DIR}/rms_indices.foo
    ${FOO_DIR}/rms2_indices.foo
    ${FOO_DIR}/mat{rms_indices}.foo
    ${FOO_DIR}/mat{rms2_indices}.foo
    ${FOO_DIR}/mat4{rms2_indices}.foo
    ${FOO_DIR}/gaussian_data.foo
    ${FOO_DIR}/gaussian.foo
    ${FOO_DIR}/gaussian2.foo
    ${FOO_DIR}/gaussian4.foo
    ${FOO_DIR}/interpolator.foo
    ${FOO_DIR}/l_bfgs.foo
    ${FOO_DIR}/marchingcube.foo
    ${FOO_DIR}/capping_square.foo
    ${FOO_DIR}/isosurface.foo
    ${FOO_DIR}/plot_grid.foo
    ${FOO_DIR}/quadrature.foo
    ${FOO_DIR}/lebedev.foo
    ${FOO_DIR}/womersley.foo
    ${FOO_DIR}/roby.foo
    ${FOO_DIR}/scf_data.foo
    ${FOO_DIR}/shell.foo
    ${FOO_DIR}/shell1.foo
    ${FOO_DIR}/shell1pair.foo
    ${FOO_DIR}/mat{shell1pair}.foo
    ${FOO_DIR}/shell1quartet.foo
    ${FOO_DIR}/shell2.foo
    ${FOO_DIR}/shell4.foo
    ${FOO_DIR}/slaterbasis.foo
    ${FOO_DIR}/slatershell.foo
    ${FOO_DIR}/unit_cell.foo
    ${FOO_DIR}/irrep.foo
    ${FOO_DIR}/pointgroup.foo
    ${FOO_DIR}/spacegroup.foo
    ${FOO_DIR}/reflection.foo
    ${FOO_DIR}/crystal.foo
    ${FOO_DIR}/crystal23.foo
    ${FOO_DIR}/spherical_harmonic.foo
    ${FOO_DIR}/t_tensor.foo
    ${FOO_DIR}/table_column.foo
    ${FOO_DIR}/td_data.foo
   #${FOO_DIR}/gsd_tree.foo
   #${FOO_DIR}/ci_data.foo
    ${FOO_DIR}/atom_group.foo
    ${FOO_DIR}/vec{atom_group}.foo
    ${FOO_DIR}/vec{atom}.foo
    ${FOO_DIR}/vec{basis}.foo
    ${FOO_DIR}/vec{coppensbasis}.foo
    ${FOO_DIR}/vec{coppensorbital}.foo
    ${FOO_DIR}/vec{diis}.foo
   #${FOO_DIR}/vec{gsd_tree}.foo
    ${FOO_DIR}/vec{interpolator}.foo
    ${FOO_DIR}/vec{irrep}.foo
    ${FOO_DIR}/vec{pair_energy}.foo
    ${FOO_DIR}/vec{quadrature}.foo
   #${FOO_DIR}/vec{rys}.foo
    ${FOO_DIR}/vec{reflection}.foo
    ${FOO_DIR}/vec{shell}.foo
    ${FOO_DIR}/vec{slaterbasis}.foo
    ${FOO_DIR}/vec{slatershell}.foo
    ${FOO_DIR}/vec{table_column}.foo
    ${FOO_DIR}/vec{unit_cell}.foo
    ${FOO_DIR}/opmatrix.foo
    ${FOO_DIR}/opvector.foo
    ${FOO_DIR}/mo_localiser.foo
    ${FOO_DIR}/pair_energy.foo
    ${FOO_DIR}/molecule.inq.foo
    ${FOO_DIR}/molecule.set.foo
    ${FOO_DIR}/molecule.read.foo
    ${FOO_DIR}/molecule.put.foo
    ${FOO_DIR}/molecule.base.foo
#   ${FOO_DIR}/molecule.rel.foo ! for now
    ${FOO_DIR}/molecule.rho.foo
    ${FOO_DIR}/molecule.grid.foo
    ${FOO_DIR}/molecule.xtal.foo
    ${FOO_DIR}/molecule.ce.foo
    ${FOO_DIR}/molecule.ints.foo
    ${FOO_DIR}/molecule.fock.foo
#   ${FOO_DIR}/molecule.nomo.foo ! for now
    ${FOO_DIR}/molecule.scf.foo
    ${FOO_DIR}/molecule.prop.foo
    ${FOO_DIR}/molecule.plot.foo
    ${FOO_DIR}/molecule.cp.foo
    ${FOO_DIR}/molecule.td.foo
    ${FOO_DIR}/molecule.har.foo
    ${FOO_DIR}/molecule.misc.foo
    ${FOO_DIR}/molecule.main.foo
    ${FOO_DIR}/test_parallel.foo
)

# Fortran source files used by run_molecule/run_har
set(FORTRAN_SRC
    ${CMAKE_CURRENT_BINARY_DIR}/archive.F90
    ${CMAKE_CURRENT_BINARY_DIR}/atom.F90
    ${CMAKE_CURRENT_BINARY_DIR}/atom_group.F90
    ${CMAKE_CURRENT_BINARY_DIR}/basis.F90
    ${CMAKE_CURRENT_BINARY_DIR}/becke_grid.F90
    ${CMAKE_CURRENT_BINARY_DIR}/bin.F90
#   ${CMAKE_CURRENT_BINARY_DIR}/breakdown_data.F90
    ${CMAKE_CURRENT_BINARY_DIR}/buffer.F90
    ${CMAKE_CURRENT_BINARY_DIR}/capping_square.F90
    ${CMAKE_CURRENT_BINARY_DIR}/cif.F90
    ${CMAKE_CURRENT_BINARY_DIR}/cluster.F90
    ${CMAKE_CURRENT_BINARY_DIR}/colour.F90
    ${CMAKE_CURRENT_BINARY_DIR}/colour_function.F90
    ${CMAKE_CURRENT_BINARY_DIR}/command_line.F90
    ${CMAKE_CURRENT_BINARY_DIR}/coppensbasis.F90
    ${CMAKE_CURRENT_BINARY_DIR}/coppensorbital.F90
    ${CMAKE_CURRENT_BINARY_DIR}/csf_to_sd.F90
    ${CMAKE_CURRENT_BINARY_DIR}/cpx.F90
    ${CMAKE_CURRENT_BINARY_DIR}/crystal.F90
    ${CMAKE_CURRENT_BINARY_DIR}/crystal23.F90
#   ${CMAKE_CURRENT_BINARY_DIR}/datafile.F90
    ${CMAKE_CURRENT_BINARY_DIR}/dft_functional.F90
    ${CMAKE_CURRENT_BINARY_DIR}/diffraction_data.inq.F90
    ${CMAKE_CURRENT_BINARY_DIR}/diffraction_data.set.F90
    ${CMAKE_CURRENT_BINARY_DIR}/diffraction_data.read.F90
    ${CMAKE_CURRENT_BINARY_DIR}/diffraction_data.put.F90
    ${CMAKE_CURRENT_BINARY_DIR}/diis.F90
    ${CMAKE_CURRENT_BINARY_DIR}/file.F90
    ${CMAKE_CURRENT_BINARY_DIR}/gaussian.F90
    ${CMAKE_CURRENT_BINARY_DIR}/gaussian2.F90
    ${CMAKE_CURRENT_BINARY_DIR}/gaussian4.F90
    ${CMAKE_CURRENT_BINARY_DIR}/gaussian_data.F90
    ${CMAKE_CURRENT_BINARY_DIR}/int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/interpolator.F90
    ${CMAKE_CURRENT_BINARY_DIR}/irrep.F90
    ${CMAKE_CURRENT_BINARY_DIR}/isosurface.F90
    ${CMAKE_CURRENT_BINARY_DIR}/l_bfgs.F90
    ${CMAKE_CURRENT_BINARY_DIR}/lebedev.F90
    ${CMAKE_CURRENT_BINARY_DIR}/map_int_int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/map_vec_int_vec_int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/marchingcube.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat3_cpx.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat3_int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat3_real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat3_evec_int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat4_cpx.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat4_int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat4_real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat5_cpx.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat5_real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_bin.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_cpx.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_evec_int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_evec_real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_emat_real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_rms2_indices.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_rms_indices.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_shell1pair.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_str.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mo_localiser.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.inq.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.set.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.read.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.put.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.base.F90
 #  ${CMAKE_CURRENT_BINARY_DIR}/molecule.rel.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.rho.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.grid.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.xtal.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.ce.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.ints.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.fock.F90
 #  ${CMAKE_CURRENT_BINARY_DIR}/molecule.nomo.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.scf.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.prop.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.plot.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.cp.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.td.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.har.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.misc.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.main.F90
    ${CMAKE_CURRENT_BINARY_DIR}/output_style.F90
    ${CMAKE_CURRENT_BINARY_DIR}/opmatrix.F90
    ${CMAKE_CURRENT_BINARY_DIR}/opvector.F90
    ${CMAKE_CURRENT_BINARY_DIR}/plot_grid.F90
    ${CMAKE_CURRENT_BINARY_DIR}/pair_energy.F90
    ${CMAKE_CURRENT_BINARY_DIR}/pointgroup.F90
    ${CMAKE_CURRENT_BINARY_DIR}/quadrature.F90
    ${CMAKE_CURRENT_BINARY_DIR}/real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/reflection.F90
    ${CMAKE_CURRENT_BINARY_DIR}/rms2_indices.F90
    ${CMAKE_CURRENT_BINARY_DIR}/rms_indices.F90
    ${CMAKE_CURRENT_BINARY_DIR}/roby.F90
    ${CMAKE_CURRENT_BINARY_DIR}/rys.F90
    ${CMAKE_CURRENT_BINARY_DIR}/scf_data.F90
    ${CMAKE_CURRENT_BINARY_DIR}/shell.F90
    ${CMAKE_CURRENT_BINARY_DIR}/shell1.F90
    ${CMAKE_CURRENT_BINARY_DIR}/shell1pair.F90
    ${CMAKE_CURRENT_BINARY_DIR}/shell1quartet.F90
    ${CMAKE_CURRENT_BINARY_DIR}/shell2.F90
    ${CMAKE_CURRENT_BINARY_DIR}/shell4.F90
    ${CMAKE_CURRENT_BINARY_DIR}/slaterbasis.F90
    ${CMAKE_CURRENT_BINARY_DIR}/slatershell.F90
    ${CMAKE_CURRENT_BINARY_DIR}/spacegroup.F90
    ${CMAKE_CURRENT_BINARY_DIR}/spherical_harmonic.F90
    ${CMAKE_CURRENT_BINARY_DIR}/str.F90
    ${CMAKE_CURRENT_BINARY_DIR}/system.F90
    ${CMAKE_CURRENT_BINARY_DIR}/table_column.F90
    ${CMAKE_CURRENT_BINARY_DIR}/textfile.F90
    ${CMAKE_CURRENT_BINARY_DIR}/time.F90
    ${CMAKE_CURRENT_BINARY_DIR}/types.F90
    ${CMAKE_CURRENT_BINARY_DIR}/t_tensor.F90
    ${CMAKE_CURRENT_BINARY_DIR}/td_data.F90
    ${CMAKE_CURRENT_BINARY_DIR}/unit_cell.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_atom_group.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_atom.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_basis.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_bin.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_coppensbasis.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_coppensorbital.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_cpx.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_diis.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_interpolator.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_irrep.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_emat_bin.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_emat_int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_emat_real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_emat3_real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_pair_energy.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_quadrature.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_real.F90
    #${CMAKE_CURRENT_BINARY_DIR}/vec_rys.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_reflection.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_shell.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_slaterbasis.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_slatershell.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_str.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_table_column.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_evec_bin.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_evec_int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_evec_real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_evec_str.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_evec_evec_int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/womersley.F90
    ${CMAKE_CURRENT_BINARY_DIR}/test_parallel.F90
)

# Set up run files
set(RUNFILE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/runfiles)
set(RUN_SRC 
    ${RUNFILE_DIR}/run_molecule.foo
    ${RUNFILE_DIR}/run_buffer.foo
    ${RUNFILE_DIR}/run_dc.foo
    ${RUNFILE_DIR}/run_dc_derivs.foo
    ${RUNFILE_DIR}/run_sf.foo
    ${RUNFILE_DIR}/run_sf_derivs.foo
    ${RUNFILE_DIR}/run_compare_cifs.foo
    ${RUNFILE_DIR}/run_dispersion.foo
    ${RUNFILE_DIR}/run_gaussian_data.foo
    ${RUNFILE_DIR}/run_har.foo
    ${RUNFILE_DIR}/run_himz.foo
    ${RUNFILE_DIR}/run_mat{real}.foo
    ${RUNFILE_DIR}/run_mat{cpx}.foo
    ${RUNFILE_DIR}/run_mp2.foo
#   ${RUNFILE_DIR}/run_mpi_matmul.foo
    ${RUNFILE_DIR}/run_mpi_pi.foo
    ${RUNFILE_DIR}/run_qr.foo
    ${RUNFILE_DIR}/run_shell2.foo
    ${RUNFILE_DIR}/run_real.foo
    ${RUNFILE_DIR}/run_rgbi.foo
    ${RUNFILE_DIR}/run_least_squares.foo
    ${RUNFILE_DIR}/run_textfile.foo
    ${RUNFILE_DIR}/run_mpi_test.foo
    ${RUNFILE_DIR}/run_mpi_test_complete.foo
    ${RUNFILE_DIR}/run_system_commands.foo
    ${RUNFILE_DIR}/run_vec{str}.foo
)


# Processing foo into fortran source
message(STATUS "Rules to process .foo -> .F90 using ANTLR4 two-pass translator")
set(SRC ${FOO_SRC} ${RUN_SRC})
set(ANTLR4_VALIDATION_DIR ${CMAKE_CURRENT_BINARY_DIR}/antlr4-validation)
set(ANTLR4_CLASSES_DIR ${CMAKE_CURRENT_BINARY_DIR}/antlr4-classes)
set(ANTLR4_VALIDATION_STAMP ${ANTLR4_VALIDATION_DIR}/FooParser.stamp)
set(ANTLR4_TRANSLATOR_STAMP ${ANTLR4_CLASSES_DIR}/FooToFortran.stamp)
set(FOO_TRANSLATOR_JAVA ${FOOGRAMMAR_DIR}/FooToFortran.java)

# Optional dead-code purge (phase B). Set to a run-file stem (e.g. run_molecule) in a
# SEPARATE build tree — `cmake .. -DPURGE_DEAD_CODE=run_molecule` — to emit only the
# procedures reachable from that one executable's entry point, dropping the rest. The
# normal build (empty PURGE_DEAD_CODE) is unaffected. Per-executable by design: code
# dead for run_molecule may be live for another program, so never share the build dir.
set(PURGE_DEAD_CODE "" CACHE STRING
    "If set to a run-file stem (e.g. run_molecule), emit only code reachable from that executable")

# Cap the translator JVM's heap. WITHOUT THIS A PARALLEL BUILD IS AN OOM MACHINE.
#
# A JVM given no -Xmx sets its maximum heap to 1/4 of physical RAM -- 3.7 GB on a
# 16 GB box -- and the collector treats that as a budget to spend: it lets garbage
# accumulate rather than pay to collect it, so resident memory drifts up toward the
# ceiling no matter how little live data there is. One translation of the 4 KB
# mat{str}.foo was measured at 2.1 GB RSS for exactly this reason. `make -j8` then
# runs eight of them and the kernel starts killing processes (Error 137), which is
# how this was found on 2026-08-11.
#
# Neither of make's throttles can prevent that: -j caps process COUNT, and -l is a
# load-average trigger -- a runqueue metric that knows nothing about memory and lags
# besides. The heap cap is the only lever that works.
#
# 512m is about double what is needed. Measured on the largest file in the tree,
# vec{atom}.foo (21k lines), translation succeeds at -Xmx256m with 389 MB peak RSS,
# and the wall time is flat across 256m/512m/1g (~29 s), so capping costs nothing.
# Peak RSS runs above the cap because metaspace, code cache and thread stacks sit
# outside the heap. If a future file genuinely needs more, the failure is now a
# clean Java OutOfMemoryError naming the file, not a random SIGKILL.
set(FOO_TRANSLATOR_XMX "512m" CACHE STRING
    "Max heap for each per-file Foo translator JVM (many run in parallel under -j)")
# The whole-library modes (purge, call graphs) hold every file's parse data in ONE
# JVM, so they need more -- but only one of them ever runs at a time.
set(FOO_ANALYSIS_XMX "2g" CACHE STRING
    "Max heap for whole-library translator runs (--purge-dead-code, --call-graph-report)")

add_custom_command(
    OUTPUT ${ANTLR4_VALIDATION_STAMP}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${ANTLR4_VALIDATION_DIR}
    COMMAND ${Java_JAVA_EXECUTABLE} -jar ${ANTLR4_JAR} -Dlanguage=Java -visitor -listener -o ${ANTLR4_VALIDATION_DIR} Foo.g4
    COMMAND ${CMAKE_COMMAND} -E touch ${ANTLR4_VALIDATION_STAMP}
    DEPENDS ${FOO_ANTLR4_GRAMMAR}
    WORKING_DIRECTORY ${FOOGRAMMAR_DIR}
    COMMENT "Generating ANTLR4 parser sources from Foo.g4"
    VERBATIM
)

add_custom_target(antlr4-grammar DEPENDS ${ANTLR4_VALIDATION_STAMP})

add_custom_command(
    OUTPUT ${ANTLR4_TRANSLATOR_STAMP}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${ANTLR4_CLASSES_DIR}
    COMMAND ${Java_JAVAC_EXECUTABLE}
        -cp ${ANTLR4_JAR}:${ANTLR4_VALIDATION_DIR}
        -d ${ANTLR4_CLASSES_DIR}
        ${ANTLR4_VALIDATION_DIR}/FooLexer.java
        ${ANTLR4_VALIDATION_DIR}/FooParser.java
        ${ANTLR4_VALIDATION_DIR}/FooListener.java
        ${ANTLR4_VALIDATION_DIR}/FooBaseListener.java
        ${ANTLR4_VALIDATION_DIR}/FooVisitor.java
        ${ANTLR4_VALIDATION_DIR}/FooBaseVisitor.java
        ${FOO_TRANSLATOR_JAVA}
    COMMAND ${CMAKE_COMMAND} -E touch ${ANTLR4_TRANSLATOR_STAMP}
    DEPENDS ${ANTLR4_VALIDATION_STAMP} ${FOO_TRANSLATOR_JAVA}
    COMMENT "Compiling ANTLR4 Foo translator"
    VERBATIM
)

add_custom_target(antlr4-translator DEPENDS ${ANTLR4_TRANSLATOR_STAMP})

set(FOO_GENERATED_F90_OUTPUTS "")
set(ALL_F90_OUTPUTS "")
foreach(foopath ${SRC})
    # Virtual (get_from template) modules - e.g. object.foo, vec{intrinsic}.foo,
    # map{key,val}.foo - are inlined into their heirs and never compiled. Skip
    # them so nothing is generated for them and they are never handed to the
    # Fortran compiler. (They are also absent from FORTRAN_SRC.)
    file(STRINGS "${foopath}" _foo_virtual REGEX "^[ \t]*virtual[ \t]+module")
    if(_foo_virtual)
        continue()
    endif()
    get_filename_component(foofile ${foopath} NAME)
    string(REGEX REPLACE ".foo\$" ".F90" F90file ${foofile})
    # This is just so Make doesn't panic about unescaped { or ,
    # and the end result is the filenames in FORTRAN_SRC
    string(REGEX REPLACE "{|}|," "_" F90file ${F90file})
    string(REGEX REPLACE "___" "_" F90file ${F90file})
    string(REGEX REPLACE "__" "_" F90file ${F90file})
    string(REGEX REPLACE "_.F90" ".F90" F90file ${F90file})
    list(APPEND ALL_F90_OUTPUTS ${CMAKE_CURRENT_BINARY_DIR}/${F90file})
    if(foopath MATCHES "^${FOO_DIR}/")
        list(APPEND FOO_GENERATED_F90_OUTPUTS ${CMAKE_CURRENT_BINARY_DIR}/${F90file})
    endif()
    # Per-file generation (normal build). In purge mode a single batch command
    # (below) supersedes these — cross-file reachability can't be computed one file
    # at a time — so skip creating the per-file rules.
    # NB: no VERBATIM here. Foo file names contain shell brace-expansion syntax
    # (e.g. map{int,int}.foo, map{vec{key},vec{val}}.foo). VERBATIM would leave
    # the path unquoted and /bin/sh (bash) would brace-expand it into multiple
    # args. Emit explicit double quotes around the paths so the shell treats each
    # as a single literal argument.
    if(NOT PURGE_DEAD_CODE)
        add_custom_command(
            OUTPUT ${F90file}
            COMMAND ${Java_JAVA_EXECUTABLE}
            -Xmx${FOO_TRANSLATOR_XMX}
            -cp "${ANTLR4_CLASSES_DIR}:${ANTLR4_JAR}"
            FooToFortran
            --types \"${FOO_TYPES_FILE}\"
            --foo \"${foopath}\"
            --out-dir \"${CMAKE_CURRENT_BINARY_DIR}\"
            DEPENDS ${foopath} ${FOO_TYPES_FILE} ${ANTLR4_TRANSLATOR_STAMP}
            # DEPFILE picks up the get_from DONORS this file inlines, which the
            # DEPENDS list above cannot know. A donor is not in FOO_SRC and emits
            # no .F90 of its own, so without this it has NO dependency edge at all:
            # editing it rebuilds nothing and you silently test a stale binary.
            # foofiles/parallel.foo is the worst case -- the entire MPI layer, 9865
            # lines, inlined into SYSTEM. That trap is how a working MPI_BARRIER fix
            # came to be switched off unnoticed in 2021, and it invalidated an
            # experiment here on 2026-08-02 before being caught. The translator
            # writes <stem>.F90.d listing exactly what it read. Needs CMake >= 3.20
            # for the Makefile generators (we require 3.20 at the top).
            DEPFILE ${CMAKE_CURRENT_BINARY_DIR}/${F90file}.d
        )
    endif()
endforeach(foopath)

# Purge mode: a single batch command emits EVERY .F90/.int/.use with procedures
# unreachable from ${PURGE_DEAD_CODE} dropped. It depends on all sources (global
# reachability), and produces the whole ALL_F90_OUTPUTS set at once.
if(PURGE_DEAD_CODE)
    message(STATUS "Dead-code purge ACTIVE: emitting only code reachable from ${PURGE_DEAD_CODE}")
    add_custom_command(
        OUTPUT ${ALL_F90_OUTPUTS}
        COMMAND ${Java_JAVA_EXECUTABLE}
        -Xmx${FOO_ANALYSIS_XMX}
        -cp "${ANTLR4_CLASSES_DIR}:${ANTLR4_JAR}"
        FooToFortran
        --types \"${FOO_TYPES_FILE}\"
        --purge-dead-code \"${RUNFILE_DIR}/${PURGE_DEAD_CODE}.foo\"
        --out-dir \"${CMAKE_CURRENT_BINARY_DIR}\"
        DEPENDS ${SRC} ${FOO_TYPES_FILE} ${ANTLR4_TRANSLATOR_STAMP}
    )
endif()

add_custom_target(antlr4-fortran
    DEPENDS ${ANTLR4_TRANSLATOR_STAMP} ${FOO_GENERATED_F90_OUTPUTS}
)

add_dependencies(antlr4-fortran antlr4-grammar)
add_dependencies(antlr4-fortran antlr4-translator)

# On-demand documentation graphs: `make callgraphs` regenerates the Graphviz DOT
# files (procedure call graph; module use graph with submodules collapsed to their
# parent; expanded submodule use graph) plus a dead-code reachability report for
# run_molecule, into <build-dir>/callgraphs/ (the current build tree). If Graphviz `dot` is present, the two small
# use graphs are also rendered to SVG.
find_program(DOT_EXE dot)
add_custom_target(callgraphs
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/callgraphs
    COMMAND ${Java_JAVA_EXECUTABLE}
        -Xmx${FOO_ANALYSIS_XMX}
        -cp "${ANTLR4_CLASSES_DIR}:${ANTLR4_JAR}"
        FooToFortran
        --types \"${FOO_TYPES_FILE}\"
        --dead-code-report \"${RUNFILE_DIR}/run_molecule.foo\"
        --call-graph-report
        --out-dir \"${CMAKE_CURRENT_BINARY_DIR}/callgraphs\"
    DEPENDS ${ANTLR4_TRANSLATOR_STAMP}
    COMMENT "Generating Foo call/use graphs (DOT) + dead-code report -> ${CMAKE_CURRENT_BINARY_DIR}/callgraphs/"
)
if(DOT_EXE)
    add_custom_command(TARGET callgraphs POST_BUILD
        COMMAND ${DOT_EXE} -Tsvg ${CMAKE_CURRENT_BINARY_DIR}/callgraphs/module_use.dot
                -o ${CMAKE_CURRENT_BINARY_DIR}/callgraphs/module_use.svg
        COMMAND ${DOT_EXE} -Tsvg ${CMAKE_CURRENT_BINARY_DIR}/callgraphs/submodule_use.dot
                -o ${CMAKE_CURRENT_BINARY_DIR}/callgraphs/submodule_use.svg
        COMMENT "Rendering module/submodule use graphs to SVG")
endif()
add_dependencies(callgraphs antlr4-translator)

# Target libtonto
add_library(tonto
   ${FORTRAN_SRC}
   ${MACROS}
)

# types.F90 contains no executable code of its own -- only the ~90 derived-type
# definitions. But those types have ~585 allocatable/pointer components, so
# gfortran auto-generates a deep-copy helper (__copy_types_module_<TYPE>) for each
# one: ~8.9 MB of __TEXT from 5.7 k lines of pure declarations. Handing all of that
# to the optimiser is what makes types.F90 the slowest single compile in the build,
# and it buys nothing -- the generated helpers are memcpy-shaped. Measured on
# M2/Tahoe, gfortran-14: parse+.mod 0.01 s, -O0 29 s, -O1 45 s, full release
# flags 839 s (~14 min) -- so -O1 is an ~18x saving on the slowest compile.
# Compile it at -O1 instead. (See DEFERRED.md for the follow-up: splitting
# types.foo into several modules so they compile in parallel.)
set_source_files_properties(${CMAKE_CURRENT_BINARY_DIR}/types.F90
    PROPERTIES COMPILE_OPTIONS "-O1")

# textfile.F90 -- disable tail-call ("sibling call") optimisation.
#
# At -O2 exactly, gfortran 14.2 on x86_64 Linux miscompiles this file badly
# enough to desynchronise MPI: four CIF-reading tests abort at >=2 ranks with
# MPI_ERR_TRUNCATE in MPI_Bcast (CLAUDE.md milestone 7, docs/TONTO_AND_MPI.md Finding 6).
# The ranks agree for 8481 broadcasts and then rank 0 issues one surplus
# integer broadcast, offsetting the streams until a 1-integer receive meets a
# 256-character send.
#
# Bisected to a single flag with a 7-second per-file harness: of the 45 flags
# that -O2 enables over -O1, -foptimize-sibling-calls is the ONLY one whose
# removal fixes it. That also explains why the bug hid from every attempt to
# look at it -- a `write` probe or -fcheck=all placed after a call stops that
# call being a tail call, so instrumenting the code made it pass (5/5).
#
# It is an interaction, not a lone bad pass: -O1 plus ALL 45 flags (sibling
# calls included) passes, and so do -O3 and -Ofast, which are supersets of -O2.
# So the SHIPPED release build (-Ofast) is not affected -- but the
# -O2 -fno-fast-math control build is, and that is the build docs/TONTO_AND_MPI.md uses to
# separate floating-point reassociation from genuine MPI defects, so it has to
# work. Whether the underlying fault is a gcc bug or latent UB in the Foo
# sources that only tail calls expose is NOT yet established; see Finding 6.
#
# The cost is nil: this is line-oriented I/O, never a hot path.
set_source_files_properties(${CMAKE_CURRENT_BINARY_DIR}/textfile.F90
    PROPERTIES COMPILE_OPTIONS "-fno-optimize-sibling-calls")

# WORKAROUND (2026-07-28): gfortran 14.3 on arm64 macOS miscompiles
# shell1quartet.F90 -- the two-electron integral code -- at -O3. The integrals
# come out slightly too small and everything downstream silently inherits it:
# the oxygen atom converged to -77.45 Ha, about 2.8 Ha *below* the variational
# limit (virial -V/T = 1.959 instead of 2.000), boron's atomic SCF diverged, and
# rgbi/BN's Roby populations were wrong. Pinning this one file to -O2 restores
# exact agreement with the Linux reference (E, V_ee, V_eN, T and the virial all
# match to the printed digits, and BN reproduces the reference numbers exactly).
#
# Two distinct triggers were found, which is why the level is pinned rather than
# a single pass disabled: at -O2 with strict FP the culprit bisects to
# -fschedule-insns (the pre-register-allocation scheduler); at -O3 there is a
# further trigger that -fno-schedule-insns does NOT cover. Belt and braces here.
#
# NOT yet established whether this is a GCC backend bug or latent undefined
# behaviour in the emitted code that optimisation exposes -- see
# DEFERRED.md. If it is UB, this only masks it.
# Set -DTONTO_SKIP_ARM64_WORKAROUNDS=ON to build WITHOUT this (and the matching
# flag in cmake/SetFortranFlags.cmake), so a newer gfortran can be tested for the
# miscompilation: configure with it ON, build, then run
# scripts/check_spherical_cartesian.sh. If that passes, the compiler is clean and
# the workarounds can be retired (or gated on the compiler version).
option(TONTO_SKIP_ARM64_WORKAROUNDS
       "Skip the arm64-macOS gfortran miscompilation workarounds (to test a newer compiler)" OFF)

if(APPLE AND CMAKE_SYSTEM_PROCESSOR MATCHES "arm64|aarch64"
   AND "${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU"
   AND NOT TONTO_SKIP_ARM64_WORKAROUNDS)
    set_source_files_properties(${CMAKE_CURRENT_BINARY_DIR}/shell1quartet.F90
        PROPERTIES COMPILE_OPTIONS "-O2;-fno-schedule-insns")
    message(STATUS "arm64 macOS: pinning shell1quartet.F90 to -O2 "
                   "(gfortran miscompilation workaround)")
endif()

# Generate ALL Foo->Fortran output (every .F90 and its co-generated .int/.use)
# before compiling any object. The .use files carry the inter-module `use`
# statements (e.g. system.use has `use TYPES_MODULE`) and are pulled in via
# `#include "<mod>.use"`. CMake's Fortran dependency scanner can only follow
# that include - and thus discover that system.F90 must compile after types.F90 -
# if the .use file already exists on disk when the scan runs. Forcing the whole
# antlr4-fortran generation step first guarantees every include is present, so the
# module compile order is derived correctly instead of falling back to list order
# (where system.F90 precedes types.F90 and type(SYSTEM_TYPE) is undefined).
add_dependencies(tonto antlr4-fortran)

if(COMPILE_LAPACK)
	target_link_libraries(tonto lapack blas ${MPI_Fortran_LIBRARIES})
else()
	target_link_libraries(tonto ${MPI_Fortran_LIBRARIES})
endif()

# tonto's generated Fortran (.F90/.int/.use) and .mod files all live in the build
# root, which is the only include path the compile needs. A former
# ${CMAKE_CURRENT_BINARY_DIR}/external entry was vestigial: no external dependency
# puts includable files there (system LAPACK is linked as a library; a subdir
# lapack-release build lands in build/external/lapack-release, and reference LAPACK
# has no Fortran modules anyway). That entry only made gfortran warn about a
# nonexistent include directory when the deps were system-provided (so build/external
# was never created) -- removed rather than papered over with an empty directory.
target_include_directories(tonto PUBLIC ${PROJECT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR})


# Standard executables
add_executable(run_molecule      ${CMAKE_CURRENT_BINARY_DIR}/run_molecule.F90)
add_executable(run_buffer        EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_buffer.F90)
add_executable(run_dc            EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_dc.F90)
add_executable(run_dc_derivs     EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_dc_derivs.F90)
add_executable(run_sf            EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_sf.F90)
add_executable(run_sf_derivs     EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_sf_derivs.F90)
add_executable(run_compare_cifs  EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_compare_cifs.F90)
add_executable(run_dispersion    EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_dispersion.F90)
add_executable(run_gaussian_data EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_gaussian_data.F90)
add_executable(run_har           ${CMAKE_CURRENT_BINARY_DIR}/run_har.F90)
add_executable(run_himz          EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_himz.F90)
add_executable(run_shell2        EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_shell2.F90)
add_executable(run_mat_real      EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_mat_real.F90)
add_executable(run_mat_cpx       EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_mat_cpx.F90)
add_executable(run_mp2           ${CMAKE_CURRENT_BINARY_DIR}/run_mp2.F90)
#add_executable(run_mpi_matmul    EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_mpi_matmul.F90)
# run_mpi_pi is the MPI smoke test: one `parallel do` + one PARALLEL_SUM, with a
# known answer that must not depend on the rank count. Registered as the
# `mpi_pi_rank_invariant` ctest (scripts/check_mpi_pi.sh). Not EXCLUDE_FROM_ALL,
# so an MPI build always produces something that can be checked.
add_executable(run_mpi_pi         ${CMAKE_CURRENT_BINARY_DIR}/run_mpi_pi.F90)
add_executable(run_qr            EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_qr.F90)
add_executable(run_real          EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_real.F90)
add_executable(run_rgbi          ${CMAKE_CURRENT_BINARY_DIR}/run_rgbi.F90)
add_executable(run_vec_str       EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_vec_str.F90)
add_executable(run_least_squares EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_least_squares.F90)
add_executable(run_textfile      EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_textfile.F90)
# run_system_commands exercises the SYSTEM_COMMAND virtual module (shell
# execution, used to render plots with gnuplot at the end of a job). Not
# EXCLUDE_FROM_ALL: it is registered as the `system_commands` ctest, and shelling
# out is exactly the kind of thing that silently does nothing if it breaks.
add_executable(run_system_commands ${CMAKE_CURRENT_BINARY_DIR}/run_system_commands.F90)
add_executable(run_mpi_test      ${CMAKE_CURRENT_BINARY_DIR}/run_mpi_test.F90)
add_executable(run_mpi_test_complete ${CMAKE_CURRENT_BINARY_DIR}/run_mpi_test_complete.F90)

# Link exectables
target_link_libraries(run_molecule      tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_buffer        tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_dc            tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_dc_derivs     tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_sf            tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_sf_derivs     tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_compare_cifs  tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_dispersion    tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_gaussian_data tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_har           tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_himz          tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_mat_real      tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_mat_cpx       tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_mp2           tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
#target_link_libraries(run_mpi_matmul    tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_mpi_pi         tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_qr            tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_real          tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_rgbi          tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_vec_str       tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_least_squares tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_textfile      tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_system_commands tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_mpi_test      tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_mpi_test_complete tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_shell2        tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})


# Make non-standard-named executables
set_target_properties(run_molecule PROPERTIES OUTPUT_NAME tonto)
set_target_properties(run_har      PROPERTIES OUTPUT_NAME hart)
set_target_properties(run_rgbi     PROPERTIES OUTPUT_NAME rgbi)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/tests)

install(TARGETS run_molecule DESTINATION bin)
install(TARGETS run_har      DESTINATION bin)
install(TARGETS run_rgbi     DESTINATION bin)
install(TARGETS run_mpi_test DESTINATION bin)
install(TARGETS run_mpi_test_complete DESTINATION bin)

# Packages
install(DIRECTORY ${BASIS_SETS} DESTINATION share/tonto)

# The RGBI picture pipeline: two driver scripts and the doctor go on PATH; the
# LaTeX templates they consume go beside the basis sets.
#
# This is what makes an INSTALLED tonto usable with no repository present --
# until now the scripts copied their templates from ~/bin, so rgbi-scripts/ was
# the source of truth on no machine but the author's, and the two copies had
# already drifted. scripts/rgbi_doctor.sh --print-template-dir performs the
# search (override, checkout, this share directory, ~/bin), and the drivers ask
# it rather than keeping a second copy of the logic.
#
# PROGRAMS, not FILES: the exec bit matters, and make-rgbi-pic invokes
# make-rgbi-dials as a sibling.
install(PROGRAMS
    ${CMAKE_CURRENT_SOURCE_DIR}/rgbi-scripts/make-rgbi-pic
    ${CMAKE_CURRENT_SOURCE_DIR}/rgbi-scripts/make-rgbi-dials
    ${CMAKE_CURRENT_SOURCE_DIR}/scripts/rgbi_doctor.sh
    DESTINATION bin)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/rgbi-scripts/
    DESTINATION share/tonto/rgbi-scripts
    FILES_MATCHING PATTERN "*.tex" PATTERN "*.sty")

set(CPACK_SET_DESTDIR ON)
set(CPACK_PACKAGE_NAME ${PROJECT_NAME})
set(CPACK_PACKAGE_VERSION ${TONTO_VERSION})
set(CPACK_PACKAGE_CONTACT "Dylan Jayatilaka <dylan.jayatilaka@gmail.com>")
set(CPACK_GENERATOR TGZ)
set(CPACK_STRIP_FILES ON)

include(CPack)
