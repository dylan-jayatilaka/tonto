SUBDIRLIST(TESTDIRS "${CMAKE_CURRENT_SOURCE_DIR}")
foreach(D ${TESTDIRS})
    get_filename_component(N ${D} NAME)
    add_test(NAME \"${N}\"
             WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
             COMMAND ${PERL_EXECUTABLE} ${TEST_SCRIPT}
             -testdir "${CMAKE_CURRENT_SOURCE_DIR}/${N}"
             -program "${TONTO_EXE}"
             -cmp "${CMP_SCRIPT}")
     set_tests_properties(\"${N}\" PROPERTIES LABELS "cx")
endforeach()
