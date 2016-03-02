set(TEST_SCRIPT "${CMAKE_SOURCE_DIR}/scripts/test.pl"
    CACHE FILEPATH "Script to perform tests in a directory")
set(CMP_SCRIPT "${CMAKE_SOURCE_DIR}/scripts/ndcmp"
    CACHE FILEPATH "Script to compare tonto outputs")
set(TONTO_EXE "${CMAKE_BINARY_DIR}/run_test"
    CACHE FILEPATH "Test executable")
set(BASIS_SETS "${PROJECT_SOURCE_DIR}/basis_sets"
    CACHE PATH "Basis sets directory")

if(MPI)
    set(TEST_EXE "${CMAKE_BINARY_DIR}/tontompi")
    configure_file(
        ${CMAKE_SOURCE_DIR}/scripts/tontompi.in
        ${TEST_EXE}
    )
else()
    set(TEST_EXE "${TONTO_EXE}")
endif()
message(STATUS "Testing with ${TEST_EXE}")

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
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
            COMMAND ${PERL_EXECUTABLE} ${TEST_SCRIPT}
            -testdir "${CMAKE_CURRENT_SOURCE_DIR}/${D}"
            -program "${TEST_EXE}"
            -cmp "${CMP_SCRIPT}"
            -basis "${BASIS_SETS}")
        set_tests_properties(\"${N}\" PROPERTIES 
            LABELS ${LABELS})
        set_tests_properties(\"${N}\" PROPERTIES 
            DEPENDS run_test)
    endforeach()
endfunction()

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/short)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/long)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/cx)
