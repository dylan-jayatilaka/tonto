# CMake
cmake_minimum_required(VERSION 3.0.0)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

# Install where?
set(CMAKE_INSTALL_PREFIX /usr/local CACHE PATH "Install prefix")

# Tonto version info and build date
string(TIMESTAMP VERSION_MAJOR "%y")
string(TIMESTAMP VERSION_MINOR "%m")
string(TIMESTAMP VERSION_MINOR_MINOR "%d")
set(TONTO_VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_MINOR_MINOR}")

# Tonto speicifics
string(TIMESTAMP TONTO_BUILD_DATE "%Y-%m-%d %H:%M")
set(BASIS_SETS ${CMAKE_CURRENT_SOURCE_DIR}/basis_sets 
    CACHE PATH "Basis sets directory")

option(MPI "Use MPI" OFF)

# Perl
find_package(Perl REQUIRED)

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


# Tonto
project(tonto
    LANGUAGES Fortran C
    VERSION ${TONTO_VERSION})

if(MPI)
    message(STATUS "Building with MPI enabled")
    if(MSMPI)
       set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -I${msmpi-linux-home}/Include -L${msmpi-linux-home}/Lib/x64 -lmsmpi")
       set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -I${msmpi-linux-home}/Include -L${msmpi-linux-home}/Lib/x64 -lmsmpi")
       set(CMAKE_Fortran_FLAGS "${MPI_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS}")
       set(WITH_MPI ON CACHE STRING "" FORCE)
       set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lmsmpi")
       set(MPI_Fortran_INCLUDE_PATH "${msmpi-linux-home}/Include")
       include_directories("${msmpi-linux-home}/Include")
       set(MPI_LIBRARIES "${msmpi-linux-home}/Lib/x64/libmsmpi.a")
    else()
       include(FindMPI)
       # MPI
       if(MPI_FOUND)
           message(STATUS "MPI Fortran compiler: ${MPI_Fortran_COMPILER}")
           message(STATUS "MPI include: ${MPI_Fortran_INCLUDE_PATH}")
           message(STATUS "MPI libs:${MPI_Fortran_LIBRARIES}")
           set(CMAKE_Fortran_FLAGS "${MPI_Fortran_FLAGS}")
           set(WITH_MPI ON CACHE STRING "" FORCE)
           include_directories(${MPI_Fortran_INCLUDE_PATH})
       else()
           message(STATUS "MPI_Fortran not found: ${MPI_Fortran_FOUND}")
       endif()
    endif()
endif()


# If desirable we can use .F03/.f03 by appending to the below variable
# message(STATUS "Allowed fortran suffixes: ${CMAKE_Fortran_SOURCE_FILE_EXTENSIONS}")


# Set the fortran flags
include(SetFortranFlags)
message(STATUS "Fortran Compiler flags: ${CMAKE_Fortran_FLAGS}")
message(STATUS "CXX Compiler Flags: ${CMAKE_CXX_FLAGS}")

# Macros
set(MACROS ${PROJECT_BINARY_DIR}/macros)
configure_file(
    ${PROJECT_SOURCE_DIR}/include/macros.in
    ${MACROS}
)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/external/sbf)
include_directories(${SBF_MODULE_DIRECTORY})

# Build directory
include_directories(${PROJECT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR})

# BLAS/LAPACK dependency
if(NOT USE_PACKAGED_LAPACK)
    find_package(LAPACK QUIET)
    if(NOT ${LAPACK_FOUND})
        message(STATUS "No lapack found, using packaged libs")
        add_library(lapack STATIC ${CMAKE_CURRENT_SOURCE_DIR}/lapack/blas.F90 ${CMAKE_CURRENT_SOURCE_DIR}/lapack/lapack.F90 ${MACROS})
        set(LAPACK_LIBRARIES lapack)
    endif()
else()
    message(STATUS "Using packaged libs")
    add_library(lapack STATIC ${CMAKE_CURRENT_SOURCE_DIR}/lapack/blas.F90 ${CMAKE_CURRENT_SOURCE_DIR}/lapack/lapack.F90 ${MACROS})
    set(LAPACK_LIBRARIES lapack)
endif()


# Foo source files
set(FOO_DIR ${CMAKE_CURRENT_SOURCE_DIR}/foofiles)
set(FOO_TYPES_FILE ${FOO_DIR}/types.foo)
set(FORTRAN_TYPES_FILE types.F90)
set(FOO_SRC
    ${FOO_DIR}/archive.foo
    ${FOO_DIR}/atom.foo
    ${FOO_DIR}/atom_group.foo
    ${FOO_DIR}/basis.foo
    ${FOO_DIR}/becke_grid.foo
    ${FOO_DIR}/bin.foo
    ${FOO_DIR}/buffer.foo
    ${FOO_DIR}/capping_square.foo
    ${FOO_DIR}/cif.foo
    ${FOO_DIR}/cluster.foo
    ${FOO_DIR}/colour.foo
    ${FOO_DIR}/colour_function.foo
    ${FOO_DIR}/command_line.foo
    ${FOO_DIR}/coppensbasis.foo
    ${FOO_DIR}/coppensorbital.foo
    ${FOO_DIR}/cpx.foo
    ${FOO_DIR}/crystal.foo
    ${FOO_DIR}/debug.foo
    ${FOO_DIR}/datafile.foo
    ${FOO_DIR}/dft_functional.foo
    ${FOO_DIR}/diffraction_data.foo
    ${FOO_DIR}/diis.foo
    ${FOO_DIR}/file.foo
    ${FOO_DIR}/gaussian.foo
    ${FOO_DIR}/gaussian2.foo
    ${FOO_DIR}/gaussian4.foo
    ${FOO_DIR}/gaussian_data.foo
    ${FOO_DIR}/geminal_mf_scheme.foo
    ${FOO_DIR}/geminal_mf_spectrum.foo
    ${FOO_DIR}/int.foo
    ${FOO_DIR}/interpolator.foo
    ${FOO_DIR}/intrinsic.foo
    ${FOO_DIR}/irrep.foo
    ${FOO_DIR}/isosurface.foo
    ${FOO_DIR}/json.foo
    ${FOO_DIR}/l_bfgs.foo
    ${FOO_DIR}/lebedev.foo
    ${FOO_DIR}/map{int,int}.foo
    ${FOO_DIR}/map{key,val}.foo
    ${FOO_DIR}/map{str,int}.foo
    ${FOO_DIR}/map{vec{int},vec{int}}.foo
    ${FOO_DIR}/map{vec{key},vec{val}}.foo
    ${FOO_DIR}/marchingcube.foo
    ${FOO_DIR}/mat3{bin}.foo
    ${FOO_DIR}/mat3{cpx}.foo
    ${FOO_DIR}/mat3{intrinsic}.foo
    ${FOO_DIR}/mat3{int}.foo
    ${FOO_DIR}/mat3{real}.foo
    ${FOO_DIR}/mat3{vec_{bin}}.foo
    ${FOO_DIR}/mat3{vec_{intrinsic}}.foo
    ${FOO_DIR}/mat3{vec_{int}}.foo
    ${FOO_DIR}/mat3{vec_{real}}.foo
    ${FOO_DIR}/mat4{cpx}.foo
    ${FOO_DIR}/mat4{intrinsic}.foo
    ${FOO_DIR}/mat4{int}.foo
    ${FOO_DIR}/mat4{object}.foo
    ${FOO_DIR}/mat4{real}.foo
    ${FOO_DIR}/mat4{rms2_indices}.foo
    ${FOO_DIR}/mat5{cpx}.foo
    ${FOO_DIR}/mat5{intrinsic}.foo
    ${FOO_DIR}/mat5{int}.foo
    ${FOO_DIR}/mat5{real}.foo
    ${FOO_DIR}/mat{bin}.foo
    ${FOO_DIR}/mat{cpx}.foo
    ${FOO_DIR}/mat{intrinsic}.foo
    ${FOO_DIR}/mat{int}.foo
    ${FOO_DIR}/mat{mat_{intrinsic}}.foo
    ${FOO_DIR}/mat{mat_{real}}.foo
    ${FOO_DIR}/mat{object}.foo
    ${FOO_DIR}/mat{real}.foo
    ${FOO_DIR}/mat{rms2_indices}.foo
    ${FOO_DIR}/mat{rms_indices}.foo
    ${FOO_DIR}/mat{shell1pair}.foo
    ${FOO_DIR}/mat{str}.foo
    ${FOO_DIR}/mo_localiser.foo
    ${FOO_DIR}/molecule.base.foo
    ${FOO_DIR}/molecule.ce.foo
    ${FOO_DIR}/molecule.cp.foo
    ${FOO_DIR}/molecule.fock.foo
    ${FOO_DIR}/molecule.gem.foo
    ${FOO_DIR}/molecule.grid.foo
    ${FOO_DIR}/molecule.ints.foo
    ${FOO_DIR}/molecule.main.foo
    ${FOO_DIR}/molecule.misc.foo
    ${FOO_DIR}/molecule.plot.foo
    ${FOO_DIR}/molecule.prop.foo
    ${FOO_DIR}/molecule.rel.foo
    ${FOO_DIR}/molecule.scf.foo
    ${FOO_DIR}/molecule.tad.foo
    ${FOO_DIR}/molecule.xtal.foo
    ${FOO_DIR}/object.foo
    ${FOO_DIR}/opmatrix.foo
    ${FOO_DIR}/opvector.foo
    ${FOO_DIR}/parallel.foo
    ${FOO_DIR}/plot_grid.foo
    ${FOO_DIR}/pointgroup.foo
    ${FOO_DIR}/quadrature.foo
    ${FOO_DIR}/real.foo
    ${FOO_DIR}/reflection.foo
    ${FOO_DIR}/rms2_indices.foo
    ${FOO_DIR}/rms_indices.foo
    ${FOO_DIR}/roby.foo
    ${FOO_DIR}/rys.foo
    ${FOO_DIR}/sasaki_form_factors.foo
    ${FOO_DIR}/scfdata.foo
    ${FOO_DIR}/shell.foo
    ${FOO_DIR}/shell1.foo
    ${FOO_DIR}/shell1pair.foo
    ${FOO_DIR}/shell1quartet.foo
    ${FOO_DIR}/shell2.foo
    ${FOO_DIR}/shell4.foo
    ${FOO_DIR}/slaterbasis.foo
    ${FOO_DIR}/slatershell.foo
    ${FOO_DIR}/spacegroup.foo
    ${FOO_DIR}/spherical.foo
    ${FOO_DIR}/str.foo
    ${FOO_DIR}/system.foo
    ${FOO_DIR}/system_command.foo
    ${FOO_DIR}/table_column.foo
    ${FOO_DIR}/textfile.foo
    ${FOO_DIR}/time.foo
    ${FOO_DIR}/types.foo
    ${FOO_DIR}/t_tensor.foo
    ${FOO_DIR}/unit_cell.foo
    ${FOO_DIR}/unit_number.foo
    ${FOO_DIR}/vec{atom_group}.foo
    ${FOO_DIR}/vec{atom}.foo
    ${FOO_DIR}/vec{basis}.foo
    ${FOO_DIR}/vec{bin}.foo
    ${FOO_DIR}/vec{coppensbasis}.foo
    ${FOO_DIR}/vec{coppensorbital}.foo
    ${FOO_DIR}/vec{cpx}.foo
    ${FOO_DIR}/vec{diis}.foo
    ${FOO_DIR}/vec{interpolator}.foo
    ${FOO_DIR}/vec{intrinsic}.foo
    ${FOO_DIR}/vec{int}.foo
    ${FOO_DIR}/vec{irrep}.foo
    ${FOO_DIR}/vec{mat3_{intrinsic}}.foo
    ${FOO_DIR}/vec{mat3_{real}}.foo
    ${FOO_DIR}/vec{mat4_{intrinsic}}.foo
    ${FOO_DIR}/vec{mat4_{real}}.foo
    ${FOO_DIR}/vec{mat_{intrinsic}}.foo
    ${FOO_DIR}/vec{mat_{real}}.foo
    ${FOO_DIR}/vec{object}.foo
    ${FOO_DIR}/vec{quadrature}.foo
    ${FOO_DIR}/vec{real}.foo
    #${FOO_DIR}/vec{rys}.foo
    ${FOO_DIR}/vec{reflection}.foo
    ${FOO_DIR}/vec{shell}.foo
    ${FOO_DIR}/vec{slaterbasis}.foo
    ${FOO_DIR}/vec{slatershell}.foo
    ${FOO_DIR}/vec{str}.foo
    ${FOO_DIR}/vec{table_column}.foo
    ${FOO_DIR}/vec{unit_cell}.foo
    ${FOO_DIR}/vec{vec_{bin}}.foo
    ${FOO_DIR}/vec{vec_{intrinsic}}.foo
    ${FOO_DIR}/vec{vec_{int}}.foo
    ${FOO_DIR}/vec{vec_{real}}.foo
    ${FOO_DIR}/vec{vec_{str}}.foo
    ${FOO_DIR}/vec{vec_{vec_{intrinsic}}}.foo
    ${FOO_DIR}/vec{vec_{vec_{int}}}.foo
    ${FOO_DIR}/vec{vec_{vec_{real}}}.foo
)

# Fortran source files used by run_molecule/run_har
set(FORTRAN_SRC
    ${CMAKE_CURRENT_BINARY_DIR}/archive.F90
    ${CMAKE_CURRENT_BINARY_DIR}/atom.F90
    ${CMAKE_CURRENT_BINARY_DIR}/atom_group.F90
    ${CMAKE_CURRENT_BINARY_DIR}/basis.F90
    ${CMAKE_CURRENT_BINARY_DIR}/becke_grid.F90
    ${CMAKE_CURRENT_BINARY_DIR}/bin.F90
    ${CMAKE_CURRENT_BINARY_DIR}/buffer.F90
    ${CMAKE_CURRENT_BINARY_DIR}/capping_square.F90
    ${CMAKE_CURRENT_BINARY_DIR}/cif.F90
    ${CMAKE_CURRENT_BINARY_DIR}/cluster.F90
    ${CMAKE_CURRENT_BINARY_DIR}/colour.F90
    ${CMAKE_CURRENT_BINARY_DIR}/colour_function.F90
    ${CMAKE_CURRENT_BINARY_DIR}/command_line.F90
    ${CMAKE_CURRENT_BINARY_DIR}/coppensbasis.F90
    ${CMAKE_CURRENT_BINARY_DIR}/coppensorbital.F90
    ${CMAKE_CURRENT_BINARY_DIR}/cpx.F90
    ${CMAKE_CURRENT_BINARY_DIR}/crystal.F90
    ${CMAKE_CURRENT_BINARY_DIR}/datafile.F90
    ${CMAKE_CURRENT_BINARY_DIR}/dft_functional.F90
    ${CMAKE_CURRENT_BINARY_DIR}/diffraction_data.F90
    ${CMAKE_CURRENT_BINARY_DIR}/diis.F90
    ${CMAKE_CURRENT_BINARY_DIR}/file.F90
    ${CMAKE_CURRENT_BINARY_DIR}/gaussian.F90
    ${CMAKE_CURRENT_BINARY_DIR}/gaussian2.F90
    ${CMAKE_CURRENT_BINARY_DIR}/gaussian4.F90
    ${CMAKE_CURRENT_BINARY_DIR}/gaussian_data.F90
    ${CMAKE_CURRENT_BINARY_DIR}/geminal_mf_scheme.F90
    ${CMAKE_CURRENT_BINARY_DIR}/geminal_mf_spectrum.F90
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
    ${CMAKE_CURRENT_BINARY_DIR}/mat3_vec_int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat4_cpx.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat4_int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat4_real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat5_cpx.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat5_real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_bin.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_cpx.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_rms2_indices.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_rms_indices.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_shell1pair.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mat_str.F90
    ${CMAKE_CURRENT_BINARY_DIR}/mo_localiser.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.base.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.ce.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.cp.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.fock.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.gem.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.grid.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.ints.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.main.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.misc.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.plot.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.prop.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.rel.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.scf.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.tad.F90
    ${CMAKE_CURRENT_BINARY_DIR}/molecule.xtal.F90
    ${CMAKE_CURRENT_BINARY_DIR}/opmatrix.F90
    ${CMAKE_CURRENT_BINARY_DIR}/opvector.F90
    ${CMAKE_CURRENT_BINARY_DIR}/plot_grid.F90
    ${CMAKE_CURRENT_BINARY_DIR}/pointgroup.F90
    ${CMAKE_CURRENT_BINARY_DIR}/quadrature.F90
    ${CMAKE_CURRENT_BINARY_DIR}/real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/reflection.F90
    ${CMAKE_CURRENT_BINARY_DIR}/rms2_indices.F90
    ${CMAKE_CURRENT_BINARY_DIR}/rms_indices.F90
    ${CMAKE_CURRENT_BINARY_DIR}/roby.F90
    ${CMAKE_CURRENT_BINARY_DIR}/rys.F90
    ${CMAKE_CURRENT_BINARY_DIR}/sasaki_form_factors.F90
    ${CMAKE_CURRENT_BINARY_DIR}/scfdata.F90
    ${CMAKE_CURRENT_BINARY_DIR}/shell.F90
    ${CMAKE_CURRENT_BINARY_DIR}/shell1.F90
    ${CMAKE_CURRENT_BINARY_DIR}/shell1pair.F90
    ${CMAKE_CURRENT_BINARY_DIR}/shell1quartet.F90
    ${CMAKE_CURRENT_BINARY_DIR}/shell2.F90
    ${CMAKE_CURRENT_BINARY_DIR}/shell4.F90
    ${CMAKE_CURRENT_BINARY_DIR}/slaterbasis.F90
    ${CMAKE_CURRENT_BINARY_DIR}/slatershell.F90
    ${CMAKE_CURRENT_BINARY_DIR}/spacegroup.F90
    ${CMAKE_CURRENT_BINARY_DIR}/spherical.F90
    ${CMAKE_CURRENT_BINARY_DIR}/str.F90
    ${CMAKE_CURRENT_BINARY_DIR}/system.F90
    ${CMAKE_CURRENT_BINARY_DIR}/table_column.F90
    ${CMAKE_CURRENT_BINARY_DIR}/textfile.F90
    ${CMAKE_CURRENT_BINARY_DIR}/time.F90
    ${CMAKE_CURRENT_BINARY_DIR}/types.F90
    ${CMAKE_CURRENT_BINARY_DIR}/t_tensor.F90
    ${CMAKE_CURRENT_BINARY_DIR}/unit_cell.F90
    ${CMAKE_CURRENT_BINARY_DIR}/unit_number.F90
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
    ${CMAKE_CURRENT_BINARY_DIR}/vec_mat3_real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_mat_real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_quadrature.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_real.F90
    #${CMAKE_CURRENT_BINARY_DIR}/vec_rys.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_reflection.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_shell.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_slaterbasis.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_slatershell.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_str.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_table_column.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_vec_bin.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_vec_int.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_vec_real.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_vec_str.F90
    ${CMAKE_CURRENT_BINARY_DIR}/vec_vec_vec_int.F90
)

# Set up run files
set(RUNFILE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/runfiles)
set(RUN_SRC 
    ${RUNFILE_DIR}/run_molecule.foo
    ${RUNFILE_DIR}/run_buffer.foo
    ${RUNFILE_DIR}/run_dispersion.foo
    ${RUNFILE_DIR}/run_gaussian_data.foo
    ${RUNFILE_DIR}/run_har.foo
    ${RUNFILE_DIR}/run_himz.foo
    ${RUNFILE_DIR}/run_mat{real}.foo
    ${RUNFILE_DIR}/run_mat{cpx}.foo
    ${RUNFILE_DIR}/run_qr.foo
    ${RUNFILE_DIR}/run_shell2.foo
    ${RUNFILE_DIR}/run_real.foo
    ${RUNFILE_DIR}/run_least_squares.foo
    ${RUNFILE_DIR}/run_vec{str}.foo
)


# Processing foo into fortran source
message(STATUS "Rules to process .foo -> .F90 using ${PERL_EXECUTABLE}")
set(FOO_SCRIPT "${CMAKE_CURRENT_SOURCE_DIR}/scripts/foo.pl")
set(FOO_ARGS "")
set(SRC ${FOO_SRC} ${RUN_SRC})
foreach(foopath ${SRC})
    get_filename_component(foofile ${foopath} NAME)
    string(REGEX REPLACE ".foo\$" ".F90" F90file ${foofile})
    # This is just so Make doesn't panic about unescaped { or ,
    # and the end result is the filenames in FORTRAN_SRC
    string(REGEX REPLACE "{|}|," "_" F90file ${F90file})
    string(REGEX REPLACE "___" "_" F90file ${F90file})
    string(REGEX REPLACE "__" "_" F90file ${F90file})
    string(REGEX REPLACE "_.F90" ".F90" F90file ${F90file})
    add_custom_command(
        OUTPUT ${F90file}
        COMMAND ${PERL_EXECUTABLE}
        ARGS ${FOO_SCRIPT} ${FOO_ARGS}
        -fortran "${CMAKE_CURRENT_BINARY_DIR}/${F90file}"
        -types ${FOO_TYPES_FILE}
        \"${foopath}\"
        DEPENDS ${foopath}
    )
endforeach(foopath)

# Target libtonto
add_library(tonto
    ${FORTRAN_SRC}
    ${MACROS}
)
target_link_libraries(tonto sbf_fortran ${MPI_Fortran_LIBRARIES})

# Standard executables
add_executable(run_molecule      ${CMAKE_CURRENT_BINARY_DIR}/run_molecule.F90)
add_executable(run_buffer        EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_buffer.F90)
add_executable(run_dispersion    EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_dispersion.F90)
add_executable(run_gaussian_data EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_gaussian_data.F90)
add_executable(run_har           ${CMAKE_CURRENT_BINARY_DIR}/run_har.F90)
add_executable(run_himz          EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_himz.F90)
add_executable(run_shell2        EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_shell2.F90)
add_executable(run_mat_real      EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_mat_real.F90)
add_executable(run_mat_cpx       EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_mat_cpx.F90)
add_executable(run_qr            EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_qr.F90)
add_executable(run_real          EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_real.F90)
add_executable(run_vec_str       EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_vec_str.F90)
add_executable(run_least_squares EXCLUDE_FROM_ALL ${CMAKE_CURRENT_BINARY_DIR}/run_least_squares.F90)

# Link exectables
target_link_libraries(run_molecule      tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_buffer        tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_dispersion    tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_gaussian_data tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_har           tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_himz          tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_mat_real      tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_mat_cpx       tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_qr            tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_real          tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_vec_str       tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_least_squares tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})
target_link_libraries(run_shell2        tonto ${LAPACK_LIBRARIES} ${MPI_LIBRARIES})

# Make non-standard-named executables
set_target_properties(run_molecule PROPERTIES OUTPUT_NAME tonto)
set_target_properties(run_har      PROPERTIES OUTPUT_NAME hart)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/tests)

install(TARGETS run_molecule DESTINATION bin)
install(TARGETS run_har      DESTINATION bin)

# Packages
install(DIRECTORY ${BASIS_SETS} DESTINATION share/tonto)

set(CPACK_SET_DESTDIR ON)
set(CPACK_PACKAGE_NAME ${PROJECT_NAME})
set(CPACK_PACKAGE_VERSION ${TONTO_VERSION})
set(CPACK_PACKAGE_CONTACT "Dylan Jayatilaka <dylan.jayatilaka@gmail.com>")
set(CPACK_GENERATOR TGZ)
set(CPACK_STRIP_FILES ON)

include(CPack)
