subdirlist(TESTDIRS "${CMAKE_CURRENT_SOURCE_DIR}")
add_all_tests(${TESTDIRS} LABELS "short")
