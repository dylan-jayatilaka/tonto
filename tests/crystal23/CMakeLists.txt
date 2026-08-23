# Refinements against an imported periodic wavefunction: CRYSTAL23 XML, and
# CP2K densities read through the same reader. Kept in its own directory
# because these jobs need a large externally generated wavefunction file that
# is NOT committed -- see docs/LOLO_CP2K_PORT.md for how to make one.
#
# Labelled "long" as well as "crystal23", so that moving these jobs out of
# tests/long did not quietly remove them from the long suite.
subdirlist(TESTDIRS "${CMAKE_CURRENT_SOURCE_DIR}")
add_all_tests(${TESTDIRS} LABELS "crystal23" "long")
