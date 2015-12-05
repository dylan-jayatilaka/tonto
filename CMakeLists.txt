cmake_minimum_required(VERSION 3.1.0)
set(VERSION_MAJOR 3)
set(VERSION_MINOR 6)
set(VERSION_MINOR_MINOR 0)
set(TONTO_VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_MINOR_MINOR}")
string(TIMESTAMP TONTO_BUILD_DATE "%Y-%m-%d %H:%M")

find_package(Perl REQUIRED)
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

project(tonto
    LANGUAGES Fortran
    VERSION ${TONTO_VERSION})


# Add preprocessor flags etc
set(CMAKE_Fortran_FLAGS "-cpp -Wall -fno-sign-zero \
-ffree-line-length-none -O3 \
-DINT_KIND=4 -DBIN_KIND=4 \
-DREAL_KIND=8 -DCPX_KIND=8 \
-DGNU_gfortran \
-DGNU_gfortran_on_DARWIN \
-DNO_GENERIC_NAMES -DUSE_PRECONDITIONS -DFLUSH"
)

set(MACROS ${PROJECT_BINARY_DIR}/macros)
configure_file(
    ${PROJECT_SOURCE_DIR}/include/macros.in
    ${MACROS}
)
include_directories(${PROJECT_BINARY_DIR})

find_package(LAPACK)
if(NOT ${LAPACK_FOUND})
    message(STATUS "No lapack found, using packaged libs")
    add_library(lapack STATIC ${CMAKE_SOURCE_DIR}/lapack/blas.f90 ${CMAKE_SOURCE_DIR}/lapack/lapack.f90 ${MACROS})
    set(LAPACK_LIBRARIES lapack)
endif()


set(FOO_DIR ${CMAKE_SOURCE_DIR}/foofiles)
set(FOO_TYPES_FILE ${FOO_DIR}/types.foo)
set(FORTRAN_TYPES_FILE types.f90)
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

set(FORTRAN_SRC
    ${CMAKE_BINARY_DIR}/archive.f90
    ${CMAKE_BINARY_DIR}/atom.f90
    ${CMAKE_BINARY_DIR}/atom_group.f90
    ${CMAKE_BINARY_DIR}/basis.f90
    ${CMAKE_BINARY_DIR}/becke_grid.f90
    ${CMAKE_BINARY_DIR}/bin.f90
    ${CMAKE_BINARY_DIR}/buffer.f90
    ${CMAKE_BINARY_DIR}/capping_square.f90
    ${CMAKE_BINARY_DIR}/cif.f90
    ${CMAKE_BINARY_DIR}/cluster.f90
    ${CMAKE_BINARY_DIR}/colour.f90
    ${CMAKE_BINARY_DIR}/colour_function.f90
    ${CMAKE_BINARY_DIR}/command_line.f90
    ${CMAKE_BINARY_DIR}/coppensbasis.f90
    ${CMAKE_BINARY_DIR}/coppensorbital.f90
    ${CMAKE_BINARY_DIR}/cpx.f90
    ${CMAKE_BINARY_DIR}/crystal.f90
    ${CMAKE_BINARY_DIR}/debug.f90
    ${CMAKE_BINARY_DIR}/dft_functional.f90
    ${CMAKE_BINARY_DIR}/diffraction_data.f90
    ${CMAKE_BINARY_DIR}/diis.f90
    ${CMAKE_BINARY_DIR}/file.f90
    ${CMAKE_BINARY_DIR}/gaussian.f90
    ${CMAKE_BINARY_DIR}/gaussian2.f90
    ${CMAKE_BINARY_DIR}/gaussian4.f90
    ${CMAKE_BINARY_DIR}/gaussian_data.f90
    ${CMAKE_BINARY_DIR}/geminal_mf_scheme.f90
    ${CMAKE_BINARY_DIR}/geminal_mf_spectrum.f90
    ${CMAKE_BINARY_DIR}/int.f90
    ${CMAKE_BINARY_DIR}/interpolator.f90
    ${CMAKE_BINARY_DIR}/intrinsic.f90
    ${CMAKE_BINARY_DIR}/irrep.f90
    ${CMAKE_BINARY_DIR}/isosurface.f90
    ${CMAKE_BINARY_DIR}/json.f90
    ${CMAKE_BINARY_DIR}/lebedev.f90
    ${CMAKE_BINARY_DIR}/map_int_int.f90
    ${CMAKE_BINARY_DIR}/map_key_val.f90
    ${CMAKE_BINARY_DIR}/map_str_int.f90
    ${CMAKE_BINARY_DIR}/map_vec_int_vec_int.f90
    ${CMAKE_BINARY_DIR}/map_vec_key_vec_val.f90
    ${CMAKE_BINARY_DIR}/marchingcube.f90
    ${CMAKE_BINARY_DIR}/mat3_bin.f90
    ${CMAKE_BINARY_DIR}/mat3_cpx.f90
    ${CMAKE_BINARY_DIR}/mat3_intrinsic.f90
    ${CMAKE_BINARY_DIR}/mat3_int.f90
    ${CMAKE_BINARY_DIR}/mat3_real.f90
    ${CMAKE_BINARY_DIR}/mat3_vec_bin.f90
    ${CMAKE_BINARY_DIR}/mat3_vec_intrinsic.f90
    ${CMAKE_BINARY_DIR}/mat3_vec_int.f90
    ${CMAKE_BINARY_DIR}/mat3_vec_real.f90
    ${CMAKE_BINARY_DIR}/mat4_cpx.f90
    ${CMAKE_BINARY_DIR}/mat4_intrinsic.f90
    ${CMAKE_BINARY_DIR}/mat4_int.f90
    ${CMAKE_BINARY_DIR}/mat4_object.f90
    ${CMAKE_BINARY_DIR}/mat4_real.f90
    ${CMAKE_BINARY_DIR}/mat4_rms2_indices.f90
    ${CMAKE_BINARY_DIR}/mat5_cpx.f90
    ${CMAKE_BINARY_DIR}/mat5_intrinsic.f90
    ${CMAKE_BINARY_DIR}/mat5_int.f90
    ${CMAKE_BINARY_DIR}/mat5_real.f90
    ${CMAKE_BINARY_DIR}/mat_bin.f90
    ${CMAKE_BINARY_DIR}/mat_cpx.f90
    ${CMAKE_BINARY_DIR}/mat_intrinsic.f90
    ${CMAKE_BINARY_DIR}/mat_int.f90
    ${CMAKE_BINARY_DIR}/mat_mat_intrinsic.f90
    ${CMAKE_BINARY_DIR}/mat_mat_real.f90
    ${CMAKE_BINARY_DIR}/mat_object.f90
    ${CMAKE_BINARY_DIR}/mat_real.f90
    ${CMAKE_BINARY_DIR}/mat_rms2_indices.f90
    ${CMAKE_BINARY_DIR}/mat_rms_indices.f90
    ${CMAKE_BINARY_DIR}/mat_shell1pair.f90
    ${CMAKE_BINARY_DIR}/mat_str.f90
    ${CMAKE_BINARY_DIR}/mo_localiser.f90
    ${CMAKE_BINARY_DIR}/molecule.base.f90
    ${CMAKE_BINARY_DIR}/molecule.ce.f90
    ${CMAKE_BINARY_DIR}/molecule.cp.f90
    ${CMAKE_BINARY_DIR}/molecule.fock.f90
    ${CMAKE_BINARY_DIR}/molecule.gem.f90
    ${CMAKE_BINARY_DIR}/molecule.grid.f90
    ${CMAKE_BINARY_DIR}/molecule.ints.f90
    ${CMAKE_BINARY_DIR}/molecule.main.f90
    ${CMAKE_BINARY_DIR}/molecule.misc.f90
    ${CMAKE_BINARY_DIR}/molecule.plot.f90
    ${CMAKE_BINARY_DIR}/molecule.prop.f90
    ${CMAKE_BINARY_DIR}/molecule.rel.f90
    ${CMAKE_BINARY_DIR}/molecule.scf.f90
    ${CMAKE_BINARY_DIR}/molecule.tad.f90
    ${CMAKE_BINARY_DIR}/molecule.xtal.f90
    ${CMAKE_BINARY_DIR}/object.f90
    ${CMAKE_BINARY_DIR}/opmatrix.f90
    ${CMAKE_BINARY_DIR}/opvector.f90
    ${CMAKE_BINARY_DIR}/parallel.f90
    ${CMAKE_BINARY_DIR}/plot_grid.f90
    ${CMAKE_BINARY_DIR}/pointgroup.f90
    ${CMAKE_BINARY_DIR}/quadrature.f90
    ${CMAKE_BINARY_DIR}/real.f90
    ${CMAKE_BINARY_DIR}/reflection.f90
    ${CMAKE_BINARY_DIR}/rms2_indices.f90
    ${CMAKE_BINARY_DIR}/rms_indices.f90
    ${CMAKE_BINARY_DIR}/roby.f90
    ${CMAKE_BINARY_DIR}/rys.f90
    ${CMAKE_BINARY_DIR}/scfdata.f90
    ${CMAKE_BINARY_DIR}/shell.f90
    ${CMAKE_BINARY_DIR}/shell1.f90
    ${CMAKE_BINARY_DIR}/shell1pair.f90
    ${CMAKE_BINARY_DIR}/shell1quartet.f90
    ${CMAKE_BINARY_DIR}/shell2.f90
    ${CMAKE_BINARY_DIR}/shell4.f90
    ${CMAKE_BINARY_DIR}/slaterbasis.f90
    ${CMAKE_BINARY_DIR}/slatershell.f90
    ${CMAKE_BINARY_DIR}/spacegroup.f90
    ${CMAKE_BINARY_DIR}/spherical.f90
    ${CMAKE_BINARY_DIR}/str.f90
    ${CMAKE_BINARY_DIR}/system.f90
    ${CMAKE_BINARY_DIR}/system_command.f90
    ${CMAKE_BINARY_DIR}/table_column.f90
    ${CMAKE_BINARY_DIR}/textfile.f90
    ${CMAKE_BINARY_DIR}/time.f90
    ${CMAKE_BINARY_DIR}/types.f90
    ${CMAKE_BINARY_DIR}/unit_cell.f90
    ${CMAKE_BINARY_DIR}/unit_number.f90
    ${CMAKE_BINARY_DIR}/vec_atom_group.f90
    ${CMAKE_BINARY_DIR}/vec_atom.f90
    ${CMAKE_BINARY_DIR}/vec_basis.f90
    ${CMAKE_BINARY_DIR}/vec_bin.f90
    ${CMAKE_BINARY_DIR}/vec_coppensbasis.f90
    ${CMAKE_BINARY_DIR}/vec_coppensorbital.f90
    ${CMAKE_BINARY_DIR}/vec_cpx.f90
    ${CMAKE_BINARY_DIR}/vec_diis.f90
    ${CMAKE_BINARY_DIR}/vec_interpolator.f90
    ${CMAKE_BINARY_DIR}/vec_intrinsic.f90
    ${CMAKE_BINARY_DIR}/vec_int.f90
    ${CMAKE_BINARY_DIR}/vec_irrep.f90
    ${CMAKE_BINARY_DIR}/vec_mat3_intrinsic.f90
    ${CMAKE_BINARY_DIR}/vec_mat3_real.f90
    ${CMAKE_BINARY_DIR}/vec_mat4_intrinsic.f90
    ${CMAKE_BINARY_DIR}/vec_mat4_real.f90
    ${CMAKE_BINARY_DIR}/vec_mat_intrinsic.f90
    ${CMAKE_BINARY_DIR}/vec_mat_real.f90
    ${CMAKE_BINARY_DIR}/vec_object.f90
    ${CMAKE_BINARY_DIR}/vec_quadrature.f90
    ${CMAKE_BINARY_DIR}/vec_real.f90
    ${CMAKE_BINARY_DIR}/vec_reflection.f90
    ${CMAKE_BINARY_DIR}/vec_shell.f90
    ${CMAKE_BINARY_DIR}/vec_slaterbasis.f90
    ${CMAKE_BINARY_DIR}/vec_slatershell.f90
    ${CMAKE_BINARY_DIR}/vec_str.f90
    ${CMAKE_BINARY_DIR}/vec_table_column.f90
    ${CMAKE_BINARY_DIR}/vec_unit_cell.f90
    ${CMAKE_BINARY_DIR}/vec_vec_bin.f90
    ${CMAKE_BINARY_DIR}/vec_vec_intrinsic.f90
    ${CMAKE_BINARY_DIR}/vec_vec_int.f90
    ${CMAKE_BINARY_DIR}/vec_vec_real.f90
    ${CMAKE_BINARY_DIR}/vec_vec_str.f90
    ${CMAKE_BINARY_DIR}/vec_vec_vec_intrinsic.f90
    ${CMAKE_BINARY_DIR}/vec_vec_vec_int.f90
    ${CMAKE_BINARY_DIR}/vec_vec_vec_real.f90
)
set(RUNFILE_DIR ${CMAKE_SOURCE_DIR}/runfiles)
set(RUN_SRC ${RUNFILE_DIR}/run_molecule.foo ${RUNFILE_DIR}/run_har.foo)


# lib tonto files
message(STATUS "Rules to process .foo -> .f90 using ${PERL_EXECUTABLE}")
set(FOO_SCRIPT "${CMAKE_SOURCE_DIR}/scripts/foo.pl")
set(FOO_ARGS "-no_generic -no_unknown")
foreach(foopath ${FOO_SRC})
    get_filename_component(foofile ${foopath} NAME)
    string(REGEX REPLACE ".foo\$" ".f90" f90file ${foofile})
    # This is just so Make doesn't panic about unescaped { or ,
    string(REGEX REPLACE "{|}|," "_" f90file ${f90file})
    string(REGEX REPLACE "___" "_" f90file ${f90file})
    string(REGEX REPLACE "__" "_" f90file ${f90file})
    string(REGEX REPLACE "_.f90" ".f90" f90file ${f90file})
    add_custom_command(
        OUTPUT ${f90file}
        COMMAND ${PERL_EXECUTABLE}
        ARGS -w ${FOO_SCRIPT} ${FOO_ARGS}
        -fortran "${CMAKE_BINARY_DIR}/${f90file}"
        -types ${FOO_TYPES_FILE}
        "'${foopath}'"
        DEPENDS ${foopath}
    )
endforeach(foopath)
# Run files
foreach(foopath ${RUN_SRC})
    get_filename_component(foofile ${foopath} NAME)
    string(REGEX REPLACE ".foo\$" ".f90" f90file ${foofile})
    string(REGEX REPLACE "{|}|," "_" f90file ${f90file})
    string(REGEX REPLACE "___" "_" f90file ${f90file})
    string(REGEX REPLACE "__" "_" f90file ${f90file})
    add_custom_command(
        OUTPUT ${f90file}
        COMMAND ${PERL_EXECUTABLE}
        ARGS -w ${FOO_SCRIPT} ${FOO_ARGS}
        -fortran "${CMAKE_BINARY_DIR}/${f90file}"
        -types ${FOO_TYPES_FILE}
        "'${foopath}'"
        DEPENDS "${foopath}"
    )
endforeach(foopath)

add_library(tonto
    ${FORTRAN_SRC}
    ${MACROS}
)


add_executable(run_molecule ${CMAKE_BINARY_DIR}/run_molecule.f90)

add_executable(run_har ${CMAKE_BINARY_DIR}/run_har.f90)

target_link_libraries(run_molecule tonto ${LAPACK_LIBRARIES})
target_link_libraries(run_har tonto ${LAPACK_LIBRARIES})
