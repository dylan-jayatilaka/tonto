cmake_minimum_required(VERSION 3.1.0)
set(VERSION_MAJOR 3)
set(VERSION_MINOR 6)
set(VERSION_MINOR_MINOR 1)
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
# Add preprocessor
#set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008ts -cpp -ffree-line-length-none")
set(CMAKE_Fortran_FLAGS "-cpp -Wall -fno-sign-zero  \
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


set(FOO_TYPES_FILE foofiles/types.foo)
set(FORTRAN_TYPES_FILE types.f90)
set(FOO_SRC
    foofiles/archive.foo
    foofiles/atom.foo
    foofiles/atom_group.foo
    foofiles/basis.foo
    foofiles/becke_grid.foo
    foofiles/bin.foo
    foofiles/buffer.foo
    foofiles/capping_square.foo
    foofiles/cif.foo
    foofiles/cluster.foo
    foofiles/colour.foo
    foofiles/colour_function.foo
    foofiles/command_line.foo
    foofiles/coppensbasis.foo
    foofiles/coppensorbital.foo
    foofiles/cpx.foo
    foofiles/crystal.foo
    foofiles/debug.foo
    foofiles/dft_functional.foo
    foofiles/diffraction_data.foo
    foofiles/diis.foo
    foofiles/file.foo
    foofiles/gaussian.foo
    foofiles/gaussian2.foo
    foofiles/gaussian4.foo
    foofiles/gaussian_data.foo
    foofiles/geminal_mf_scheme.foo
    foofiles/geminal_mf_spectrum.foo
    foofiles/int.foo
    foofiles/interpolator.foo
    foofiles/intrinsic.foo
    foofiles/irrep.foo
    foofiles/isosurface.foo
    foofiles/json.foo
    foofiles/lebedev.foo
    foofiles/map{int,int}.foo
    foofiles/map{key,val}.foo
    foofiles/map{str,int}.foo
    foofiles/map{vec{int},vec{int}}.foo
    foofiles/map{vec{key},vec{val}}.foo
    foofiles/marchingcube.foo
    foofiles/mat3{bin}.foo
    foofiles/mat3{cpx}.foo
    foofiles/mat3{intrinsic}.foo
    foofiles/mat3{int}.foo
    foofiles/mat3{real}.foo
    foofiles/mat3{vec_{bin}}.foo
    foofiles/mat3{vec_{intrinsic}}.foo
    foofiles/mat3{vec_{int}}.foo
    foofiles/mat3{vec_{real}}.foo
    foofiles/mat4{cpx}.foo
    foofiles/mat4{intrinsic}.foo
    foofiles/mat4{int}.foo
    foofiles/mat4{object}.foo
    foofiles/mat4{real}.foo
    foofiles/mat4{rms2_indices}.foo
    foofiles/mat5{cpx}.foo
    foofiles/mat5{intrinsic}.foo
    foofiles/mat5{int}.foo
    foofiles/mat5{real}.foo
    foofiles/mat{bin}.foo
    foofiles/mat{cpx}.foo
    foofiles/mat{intrinsic}.foo
    foofiles/mat{int}.foo
    foofiles/mat{mat_{intrinsic}}.foo
    foofiles/mat{mat_{real}}.foo
    foofiles/mat{object}.foo
    foofiles/mat{real}.foo
    foofiles/mat{rms2_indices}.foo
    foofiles/mat{rms_indices}.foo
    foofiles/mat{shell1pair}.foo
    foofiles/mat{str}.foo
    foofiles/mo_localiser.foo
    foofiles/molecule.base.foo
    foofiles/molecule.ce.foo
    foofiles/molecule.cp.foo
    foofiles/molecule.fock.foo
    foofiles/molecule.gem.foo
    foofiles/molecule.grid.foo
    foofiles/molecule.ints.foo
    foofiles/molecule.main.foo
    foofiles/molecule.misc.foo
    foofiles/molecule.plot.foo
    foofiles/molecule.prop.foo
    foofiles/molecule.rel.foo
    foofiles/molecule.scf.foo
    foofiles/molecule.tad.foo
    foofiles/molecule.xtal.foo
    foofiles/object.foo
    foofiles/opmatrix.foo
    foofiles/opvector.foo
    foofiles/parallel.foo
    foofiles/plot_grid.foo
    foofiles/pointgroup.foo
    foofiles/quadrature.foo
    foofiles/real.foo
    foofiles/reflection.foo
    foofiles/rms2_indices.foo
    foofiles/rms_indices.foo
    foofiles/roby.foo
    foofiles/rys.foo
    foofiles/scfdata.foo
    foofiles/shell.foo
    foofiles/shell1.foo
    foofiles/shell1pair.foo
    foofiles/shell1quartet.foo
    foofiles/shell2.foo
    foofiles/shell4.foo
    foofiles/slaterbasis.foo
    foofiles/slatershell.foo
    foofiles/spacegroup.foo
    foofiles/spherical.foo
    foofiles/str.foo
    foofiles/system.foo
    foofiles/system_command.foo
    foofiles/table_column.foo
    foofiles/textfile.foo
    foofiles/time.foo
    foofiles/types.foo
    foofiles/unit_cell.foo
    foofiles/unit_number.foo
    foofiles/vec{atom_group}.foo
    foofiles/vec{atom}.foo
    foofiles/vec{basis}.foo
    foofiles/vec{bin}.foo
    foofiles/vec{coppensbasis}.foo
    foofiles/vec{coppensorbital}.foo
    foofiles/vec{cpx}.foo
    foofiles/vec{diis}.foo
    foofiles/vec{interpolator}.foo
    foofiles/vec{intrinsic}.foo
    foofiles/vec{int}.foo
    foofiles/vec{irrep}.foo
    foofiles/vec{mat3_{intrinsic}}.foo
    foofiles/vec{mat3_{real}}.foo
    foofiles/vec{mat4_{intrinsic}}.foo
    foofiles/vec{mat4_{real}}.foo
    foofiles/vec{mat_{intrinsic}}.foo
    foofiles/vec{mat_{real}}.foo
    foofiles/vec{object}.foo
    foofiles/vec{quadrature}.foo
    foofiles/vec{real}.foo
    foofiles/vec{reflection}.foo
    foofiles/vec{shell}.foo
    foofiles/vec{slaterbasis}.foo
    foofiles/vec{slatershell}.foo
    foofiles/vec{str}.foo
    foofiles/vec{table_column}.foo
    foofiles/vec{unit_cell}.foo
    foofiles/vec{vec_{bin}}.foo
    foofiles/vec{vec_{intrinsic}}.foo
    foofiles/vec{vec_{int}}.foo
    foofiles/vec{vec_{real}}.foo
    foofiles/vec{vec_{str}}.foo
    foofiles/vec{vec_{vec_{intrinsic}}}.foo
    foofiles/vec{vec_{vec_{int}}}.foo
    foofiles/vec{vec_{vec_{real}}}.foo
)

set(FORTRAN_SRC
    archive.f90
    atom.f90
    atom_group.f90
    basis.f90
    becke_grid.f90
    bin.f90
    buffer.f90
    capping_square.f90
    cif.f90
    cluster.f90
    colour.f90
    colour_function.f90
    command_line.f90
    coppensbasis.f90
    coppensorbital.f90
    cpx.f90
    crystal.f90
    debug.f90
    dft_functional.f90
    diffraction_data.f90
    diis.f90
    file.f90
    gaussian.f90
    gaussian2.f90
    gaussian4.f90
    gaussian_data.f90
    geminal_mf_scheme.f90
    geminal_mf_spectrum.f90
    int.f90
    interpolator.f90
    intrinsic.f90
    irrep.f90
    isosurface.f90
    json.f90
    lebedev.f90
    map_int_int.f90
    map_key_val.f90
    map_str_int.f90
    map_vec_int_vec_int.f90
    map_vec_key_vec_val.f90
    marchingcube.f90
    mat3_bin.f90
    mat3_cpx.f90
    mat3_intrinsic.f90
    mat3_int.f90
    mat3_real.f90
    mat3_vec_bin.f90
    mat3_vec_intrinsic.f90
    mat3_vec_int.f90
    mat3_vec_real.f90
    mat4_cpx.f90
    mat4_intrinsic.f90
    mat4_int.f90
    mat4_object.f90
    mat4_real.f90
    mat4_rms2_indices.f90
    mat5_cpx.f90
    mat5_intrinsic.f90
    mat5_int.f90
    mat5_real.f90
    mat_bin.f90
    mat_cpx.f90
    mat_intrinsic.f90
    mat_int.f90
    mat_mat_intrinsic.f90
    mat_mat_real.f90
    mat_object.f90
    mat_real.f90
    mat_rms2_indices.f90
    mat_rms_indices.f90
    mat_shell1pair.f90
    mat_str.f90
    mo_localiser.f90
    molecule.base.f90
    molecule.ce.f90
    molecule.cp.f90
    molecule.fock.f90
    molecule.gem.f90
    molecule.grid.f90
    molecule.ints.f90
    molecule.main.f90
    molecule.misc.f90
    molecule.plot.f90
    molecule.prop.f90
    molecule.rel.f90
    molecule.scf.f90
    molecule.tad.f90
    molecule.xtal.f90
    object.f90
    opmatrix.f90
    opvector.f90
    parallel.f90
    plot_grid.f90
    pointgroup.f90
    quadrature.f90
    real.f90
    reflection.f90
    rms2_indices.f90
    rms_indices.f90
    roby.f90
    rys.f90
    scfdata.f90
    shell.f90
    shell1.f90
    shell1pair.f90
    shell1quartet.f90
    shell2.f90
    shell4.f90
    slaterbasis.f90
    slatershell.f90
    spacegroup.f90
    spherical.f90
    str.f90
    system.f90
    system_command.f90
    table_column.f90
    textfile.f90
    time.f90
    types.f90
    unit_cell.f90
    unit_number.f90
    vec_atom_group.f90
    vec_atom.f90
    vec_basis.f90
    vec_bin.f90
    vec_coppensbasis.f90
    vec_coppensorbital.f90
    vec_cpx.f90
    vec_diis.f90
    vec_interpolator.f90
    vec_intrinsic.f90
    vec_int.f90
    vec_irrep.f90
    vec_mat3_intrinsic.f90
    vec_mat3_real.f90
    vec_mat4_intrinsic.f90
    vec_mat4_real.f90
    vec_mat_intrinsic.f90
    vec_mat_real.f90
    vec_object.f90
    vec_quadrature.f90
    vec_real.f90
    vec_reflection.f90
    vec_shell.f90
    vec_slaterbasis.f90
    vec_slatershell.f90
    vec_str.f90
    vec_table_column.f90
    vec_unit_cell.f90
    vec_vec_bin.f90
    vec_vec_intrinsic.f90
    vec_vec_int.f90
    vec_vec_real.f90
    vec_vec_str.f90
    vec_vec_vec_intrinsic.f90
    vec_vec_vec_int.f90
    vec_vec_vec_real.f90
)

set(RUN_SRC runfiles/run_molecule.foo runfiles/run_har.foo)


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
        -fortran ${f90file} 
        -types ${CMAKE_SOURCE_DIR}/${FOO_TYPES_FILE}
        ${CMAKE_SOURCE_DIR}/${foopath}
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
        -fortran ${f90file} 
        -types ${CMAKE_SOURCE_DIR}/${FOO_TYPES_FILE}
        ${CMAKE_SOURCE_DIR}/${foopath}
        DEPENDS ${foopath}
    )
endforeach(foopath)

add_library(tonto
    ${FORTRAN_SRC}
    ${MACROS}
)

find_package(LAPACK)

add_executable(run_molecule
    run_molecule.f90
    ${MACROS}
    ${FORTRAN_TYPES_FILE}
)

add_executable(run_har
    run_har.f90
    ${MACROS}
    ${FORTRAN_TYPES_FILE}
)

target_link_libraries(run_molecule tonto ${LAPACK_LIBRARIES})
target_link_libraries(run_har tonto ${LAPACK_LIBRARIES})
