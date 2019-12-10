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
            COMMAND python3 ${TEST_SCRIPT}
            -t "${CMAKE_CURRENT_SOURCE_DIR}/${D}"
            -b "${BASIS_SETS}"
            -p "${TONTO_EXE}"
            --log-level=DEBUG
            ${MPI_FLAG})
        set_tests_properties(\"${N}\" PROPERTIES 
            LABELS ${LABELS})
        set_tests_properties(\"${N}\" PROPERTIES 
            DEPENDS run_test)
    endforeach()
endfunction()

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/short)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/long)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/cx)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/relativistic)
