# Write the build stamp: the version, the git commit and the build date that
# Tonto prints in its output header and CIF files. Run by `cmake -P` on every
# build (the build_stamp target), so the stamp names the code actually built.
#
#   -DSOURCE_DIR=<source tree>   -DOUTPUT=<file to write>
#
# The commit is marked "-dirty-" plus a short fingerprint of the uncommitted
# changes when there are any, so two different uncommitted states get
# different stamps. OUTPUT is rewritten only when that changes; the date is
# the moment it changed. Only the files that print the stamp include OUTPUT,
# so a new stamp recompiles those, not the whole library.

set(rev "unknown")
find_package(Git QUIET)
if(GIT_FOUND AND EXISTS "${SOURCE_DIR}/.git")
    execute_process(COMMAND "${GIT_EXECUTABLE}" rev-parse --short=8 HEAD
        WORKING_DIRECTORY "${SOURCE_DIR}"
        OUTPUT_VARIABLE rev OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET)
    execute_process(COMMAND "${GIT_EXECUTABLE}" diff HEAD
        WORKING_DIRECTORY "${SOURCE_DIR}"
        OUTPUT_VARIABLE diff ERROR_QUIET)
    if(NOT "${diff}" STREQUAL "")
        string(SHA1 fingerprint "${diff}")
        string(SUBSTRING "${fingerprint}" 0 7 fingerprint)
        set(rev "${rev}-dirty-${fingerprint}")
    endif()
endif()

# Unchanged commit and changes: keep the stamp, date and all.
if(EXISTS "${OUTPUT}")
    file(READ "${OUTPUT}" old)
    if("${old}" MATCHES "GIT_VERSION +\"${rev}\"")
        return()
    endif()
endif()

string(TIMESTAMP date "%Y.%m.%d %H:%M")
string(TIMESTAMP version "%y.%m.%d")
file(WRITE "${OUTPUT}"
"! The build stamp, written by cmake/BuildStamp.cmake on every build.
# define TONTO_VERSION     \"${version}\"
# define GIT_VERSION       \"${rev}\"
# define TONTO_BUILD_DATE  \"${date}\"
")
