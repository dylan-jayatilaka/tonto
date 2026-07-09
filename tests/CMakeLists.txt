# Numdiff
set(TEST_SCRIPT "${PROJECT_SOURCE_DIR}/scripts/test.py"
    CACHE FILEPATH "Script to perform tests in a directory")

set(TONTO_EXE "${PROJECT_BINARY_DIR}/tonto"
    CACHE FILEPATH "Test executable")

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
        add_test(NAME \"${N}\"
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
        set_tests_properties(\"${N}\" PROPERTIES 
            LABELS ${LABELS})
        set_tests_properties(\"${N}\" PROPERTIES 
            DEPENDS run_test)
    endforeach()
endfunction()

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/short)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/rgbi)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/long)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/cx)

# Grouped per-suite agreement report.
#
# `ctest` gives a flat pass/fail list and hides each test's stdout (the concise
# AGREEMENT line printed by test.py). This target instead runs
# scripts/compare_test_outputs.py over every suite and writes a compact
# per-suite table (exact / loose / last-digit + worst rel% and ulp) to
# tests.log in the build tree -- one line per test, not the whole run.
#
#   cmake --build . --target report        (or:  make report)
#
# It builds the tonto executable first, then reports.  tests.log is written
# fully before the driver exits; the driver's exit code is non-zero if any test
# failed the loose criterion (so `make report` mirrors ctest's pass/fail status).
add_custom_target(report
    COMMAND python3 ${PROJECT_SOURCE_DIR}/scripts/compare_test_outputs.py
            --program    ${TONTO_EXE}
            --tests-dir  ${CMAKE_CURRENT_SOURCE_DIR}
            --basis-sets ${BASIS_SETS}
            --log        ${PROJECT_BINARY_DIR}/tests.log
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
    USES_TERMINAL
    COMMENT "Tonto per-suite agreement report -> tests.log")
add_dependencies(report run_molecule)
