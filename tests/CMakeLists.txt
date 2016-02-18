set(TEST_SCRIPT "${CMAKE_SOURCE_DIR}/scripts/perform_tests.pl"
    CACHE FILEPATH "Script to perform tests in a directory")
set(CMP_SCRIPT "${CMAKE_SOURCE_DIR}/scripts/compare_output.pl"
    CACHE FILEPATH "Script to compare tonto outputs")
set(TONTO_EXE "${CMAKE_BINARY_DIR}/run_molecule"
    CACHE FILEPATH "Test executable")

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


add_subdirectory("short")
add_subdirectory("long")
add_subdirectory("cx")
