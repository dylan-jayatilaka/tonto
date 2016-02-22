set(TEST_SCRIPT "${CMAKE_SOURCE_DIR}/scripts/test.pl"
    CACHE FILEPATH "Script to perform tests in a directory")
set(CMP_SCRIPT "${CMAKE_SOURCE_DIR}/scripts/ndcmp"
    CACHE FILEPATH "Script to compare tonto outputs")
set(TONTO_EXE "${CMAKE_BINARY_DIR}/run_molecule"
    CACHE FILEPATH "Test executable")

if(MPI)
    set(TEST_EXE "${CMAKE_BINARY_DIR}/tontompi")
    configure_file(
        ${CMAKE_SOURCE_DIR}/scripts/tontompi.in
        ${TEST_EXE}
    )
else()
    set(TEST_EXE "${TONTO_EXE}")
endif()


MACRO(SUBDIRLIST result curdir)
  FILE(GLOB children RELATIVE ${curdir} ${curdir}/*)
  SET(dirlist "")
  FOREACH(child ${children})
    IF(IS_DIRECTORY ${curdir}/${child})
        LIST(APPEND dirlist ${child})
    ENDIF()
  ENDFOREACH()
  SET(${result} ${dirlist})
ENDMACRO()


add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/short)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/long)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/cx)
