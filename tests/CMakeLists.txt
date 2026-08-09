# Numdiff
set(TEST_SCRIPT "${PROJECT_SOURCE_DIR}/scripts/test.py"
    CACHE FILEPATH "Script to perform tests in a directory")

set(TONTO_EXE "${PROJECT_BINARY_DIR}/tonto"
    CACHE FILEPATH "Test executable")

# The hart tests still pass TONTO_EXE as --program: test.py resolves a test's
# "program:" key as a sibling of it, so both binaries must live side by side
# (they do -- run_molecule and run_har both land in PROJECT_BINARY_DIR).
# HART_EXE is only for the hand-registered invariant check below.
set(HART_EXE "${PROJECT_BINARY_DIR}/hart"
    CACHE FILEPATH "Hirshfeld atom refinement executable")

set(BASIS_SETS "${PROJECT_SOURCE_DIR}/basis_sets"
    CACHE PATH "Basis sets directory")

macro(subdirlist result curdir)
  file(GLOB children RELATIVE ${curdir} ${curdir}/*)
  set(dirlist "")
  foreach(child ${children})
    if(IS_DIRECTORY ${curdir}/${child})
        list(APPEND dirlist ${child})
    endif()
  endforeach()
  set(${result} ${dirlist})
endmacro()

if(WITH_MPI)
    set(MPI_FLAG "--mpi" CACHE STRING "Flag for test.py" FORCE)
endif()

function(add_all_tests)
    # This expects a list of test directories
    # followed by LABELS then a list of labels
    # e.g. add_all_tests(${DIRS} LABELS "foo")
    set(DIRS)
    set(LABELS)
    set(current_list)
    foreach(arg ${ARGN})
        if(arg STREQUAL "LABELS")
            list(APPEND DIRS ${current_list})
            set(current_list)
        else()
            list(APPEND current_list ${arg})
        endif()
    endforeach()
    list(APPEND LABELS ${current_list})
    unset(current_list)
    foreach(D ${DIRS})
        get_filename_component(N "${D}" NAME)
        add_test(NAME "${N}"
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMAND python3  ${TEST_SCRIPT}
            --test-directory "${CMAKE_CURRENT_SOURCE_DIR}/${D}"
            --basis-sets     "${BASIS_SETS}"
            --program        "${TONTO_EXE}"
            # WARNING (not DEBUG): keeps test.py's captured output to just the
            # one-line AGREEMENT row (+ worst-offender notes), which test.py prints
            # regardless of log level. So `ctest --output-on-failure` / `ctest -V`
            # show a concise per-test agreement line instead of dumping the full
            # numeric diff. (Plain `ctest` is still pass/fail-only -- unavoidable.)
            --log-level=WARNING
            ${MPI_FLAG})
        set_tests_properties("${N}" PROPERTIES
            LABELS ${LABELS})
    endforeach()
endfunction()

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/short)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/hart)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/rgbi)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/long)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/cx)

# Invariant test -- no reference output involved.
#
# Below d functions the spherical and cartesian basis sets are mathematically
# identical, but use_spherical_basis selects different two-electron code
# (make_r_JK_direct vs make_r_JK_engine), so the two settings must give the same
# energy on an s/p-only basis. Unlike the reference-diff tests this one cannot be
# silently blessed by regenerating a reference on a broken build.
#
# Added after a gfortran 14.3 miscompilation of shell1quartet.F90 on arm64 macOS
# made the engine path return two-electron energies that were slightly too small
# -- the oxygen atom converged ~2.8 Ha *below* the variational limit -- and no
# test in the suite compared the two paths. Runs in a couple of seconds.
add_test(NAME "spherical_vs_cartesian"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND sh ${PROJECT_SOURCE_DIR}/scripts/check_spherical_cartesian.sh
            ${TONTO_EXE} ${BASIS_SETS})
set_tests_properties("spherical_vs_cartesian" PROPERTIES LABELS "short")

# MPI invariant test -- no reference output involved.
#
# pi by Riemann integration: one `parallel do` plus one PARALLEL_SUM, i.e. the
# whole macro surface every parallel routine depends on, against an answer that
# is known in advance and must not change with the rank count.
#
# Added because NOTHING asserted that MPI worked at all -- no MPI build had ever
# been configured, in CI or locally, and four reductions in molecule.grid.foo
# were dead code that silently returned 1/n_ranks of the answer. Like
# spherical_vs_cartesian this compares the program against a known truth rather
# than a stored reference, so it cannot be blessed away.
#
# Only meaningful in an MPI build; in a serial build the rank sweep is
# degenerate, so register it only when WITH_MPI.
if(WITH_MPI)
    add_test(NAME "mpi_pi_rank_invariant"
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND sh ${PROJECT_SOURCE_DIR}/scripts/check_mpi_pi.sh
                ${PROJECT_BINARY_DIR}/run_mpi_pi ${MPIEXEC_EXECUTABLE} 1 2 4)
    set_tests_properties("mpi_pi_rank_invariant" PROPERTIES LABELS "mpi")
endif()

# Invariant test -- no reference output involved.
#
# hart is command-line driven, so its `--help` text is its interface
# documentation, and nothing keeps that text in step with the option `case`
# labels in runfiles/run_har.foo. It had already drifted: --disk-sfs was fully
# documented while its case label was commented out, so the documented option
# killed the run. This compares the two sets directly, and also asserts that
# hart's documented failure modes exit non-zero. Runs in a couple of seconds.
add_test(NAME "hart_options"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND sh ${PROJECT_SOURCE_DIR}/scripts/check_hart_options.sh
            ${HART_EXE} ${PROJECT_SOURCE_DIR}/runfiles/run_har.foo ${BASIS_SETS})
set_tests_properties("hart_options" PROPERTIES LABELS "hart")

# Invariant test -- no reference output involved.
#
# The SYSTEM_COMMAND exerciser. It shells out for real: it writes a file with
# `echo`, reads the contents back, and asserts that a failing command is
# reported as failing. Registered because shell execution is the classic silent
# failure -- a broken implementation returns "0, no error" and simply does
# nothing, which is precisely how the eight MPI reductions and the per-rank-IO
# flag hid for years (docs/TONTO_AND_MPI.md). The binary self-checks and exits non-zero,
# so no reference file can be blessed to make it pass. Runs in well under a
# second.
add_test(NAME "system_commands"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND ${PROJECT_BINARY_DIR}/run_system_commands)
set_tests_properties("system_commands" PROPERTIES LABELS "short")

# Invariant test -- no reference output involved.
#
# A Foo procedure that takes arguments is a library routine: it can be called
# by the argv-driven programs (hart, rgbi), which have no job file. So it must
# not touch stdin. A no-argument keyword handler may, since only the keyword
# dispatcher reaches it. Merging the two roles is what segfaulted hart on its
# second refinement cycle -- MOLECULE.READ:read_archive took arguments and
# still peeked at stdin for a trailing qualifier. Instant; no build needed.
add_test(NAME "library_stdin"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND python3 ${PROJECT_SOURCE_DIR}/scripts/check_library_stdin.py
            ${PROJECT_SOURCE_DIR}/foofiles)
set_tests_properties("library_stdin" PROPERTIES LABELS "short")

# Invariant test -- no reference output involved.
#
# Two MPI hazards that inspection cannot find, both from milestone 4:
#   . a collective inside a `parallel do` body. LOCK_PARALLEL_DO is the first
#     statement in the loop and DO_IN_PARALLEL is false while it is held, so a
#     PARALLEL_SUM there is dead code that looks correct -- four sites in
#     molecule.grid.foo each returned 1/n_ranks of the answer. A broadcast or
#     barrier there is worse: it executes, but a different number of times per
#     rank, so the job hangs.
#   . a raw write/read on a `.unit` outside file/textfile/buffer, reaching
#     around the IO_IS_ALLOWED guard. The loud version crashed
#     DWGN_lamaGOET_NBO_file_47; the silent version interleaves output from
#     every rank via preconnected unit 6.
# Source-level and instant; no build and no MPI needed, which is the point --
# these are invisible in the serial runs everyone actually does.
add_test(NAME "parallel_lint"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND python3 ${PROJECT_SOURCE_DIR}/scripts/check_parallel_lint.py
            ${PROJECT_SOURCE_DIR}/foofiles)
set_tests_properties("parallel_lint" PROPERTIES LABELS "short")

# Invariant test -- no reference output involved.
#
# scripts/rgbi_doctor.sh reports what is missing from the RGBI picture
# toolchain. On a developer machine with everything installed it will always
# say "you are fine", so it proves nothing about itself; this drives it against
# synthetic environments and asserts that each check FIRES.
#
# Note the DOCTOR is deliberately not a ctest: CI has none of the arcane
# software (Open Babel, Indigo, mol2chemfig) and would be permanently red. The
# SELF-TEST is, because it needs nothing but bash and coreutils -- it builds a
# minimal PATH so that "absent" really means absent, rather than stubbing
# anything inside the doctor.
#
# The case that matters is "installed but cannot run": on 2026-08-09 this
# machine had a mol2chemfig that `command -v` found happily and whose pipx
# virtualenv pointed at a python the OS had removed. The pipeline's only
# symptom was a picture that silently did not appear. Runs in a second or two.
add_test(NAME "rgbi_doctor_selftest"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND bash ${PROJECT_SOURCE_DIR}/scripts/rgbi_selftest.sh)
set_tests_properties("rgbi_doctor_selftest" PROPERTIES LABELS "short")

# Grouped per-suite agreement report.
#
# `ctest` gives a flat pass/fail list and hides each test's stdout (the concise
# AGREEMENT line printed by test.py). This target instead runs
# scripts/suite_report.py over every suite and writes a compact
# per-suite table (exact / loose / last-digit + worst rel% and ulp) to
# tests.log in the build tree -- one line per test, not the whole run.
#
#   cmake --build . --target report        (or:  make report)
#
# It builds the tonto executable first, then reports.  tests.log is written
# fully before the driver exits; the driver's exit code is non-zero if any test
# failed the loose criterion (so `make report` mirrors ctest's pass/fail status).
add_custom_target(report
    # -u keeps the report streaming when make's output is redirected or piped
    # (a tty is line-buffered, a pipe is 8 KB-buffered -- which is the whole
    # report, so it would all appear only at the end and look like a hang).
    COMMAND python3 -u ${PROJECT_SOURCE_DIR}/scripts/suite_report.py
            --program    ${TONTO_EXE}
            --tests-dir  ${CMAKE_CURRENT_SOURCE_DIR}
            --basis-sets ${BASIS_SETS}
            --log        ${PROJECT_BINARY_DIR}/tests.log
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
    USES_TERMINAL
    COMMENT "Tonto per-suite agreement report -> tests.log")
add_dependencies(report run_molecule run_har)
