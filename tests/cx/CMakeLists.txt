SUBDIRLIST(TESTDIRS "${CMAKE_CURRENT_SOURCE_DIR}")
foreach(D ${TESTDIRS})
    get_filename_component(N ${D} NAME)
    add_test(NAME \"${N}\"
             WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
             COMMAND ${PERL_EXECUTABLE} ${TEST_SCRIPT}
             -testdir "${CMAKE_CURRENT_SOURCE_DIR}/${N}"
             -program "${CMAKE_BINARY_DIR}/run_molecule"
             -cmp "${CMP_SCRIPT}")
endforeach()
