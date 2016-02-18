set(TEST_SCRIPT "${CMAKE_SOURCE_DIR}/scripts/perform_tests.pl")
set(CMP_SCRIPT "${CMAKE_SOURCE_DIR}/scripts/compare_output.pl")

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


SUBDIRLIST(TESTDIRS "${CMAKE_CURRENT_SOURCE_DIR}")
foreach(D ${TESTDIRS})
    add_subdirectory(${D})
endforeach()
