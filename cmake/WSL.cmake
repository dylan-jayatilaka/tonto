########################################################################
# WSL (Windows Subsystem for Linux) support.
#
# WSL looks like Ubuntu to CMake, so the ordinary Linux build "works" -- right
# up to the point where it does not, in ways whose error messages point nowhere
# near the cause. Four of them, all of which have cost real hours:
#
#   1. Windows PATH interop. WSL appends the Windows PATH to the Linux one, so
#      find_package(Java) happily resolves to /mnt/c/Program Files/.../java.exe.
#      That JDK cannot open /home/... paths, and it wants ';' -- not ':' -- as
#      its classpath separator, while CMakeLists.txt hardcodes ':' when it runs
#      the Foo translator. The failure surfaces as an ANTLR/classpath error.
#   2. Building on /mnt/c (DrvFs). 10-50x slower than the Linux filesystem, with
#      unreliable exec bits and no symlinks. Translating and compiling 184 .foo
#      files is close to the worst possible workload for it.
#   3. CRLF. A repo cloned by *Windows* git with core.autocrlf=true and then
#      built from WSL carries \r into foofiles/*.foo and include/macros.in.
#   4. WSL2 memory. The build spawns ONE JVM PER .foo FILE. `make -j$(nproc)`
#      inside a WSL2 VM capped at half the host's RAM OOMs where the identical
#      command is fine on bare Linux.
#
# This module detects WSL, removes cause 1 outright (by sanitising PATH before
# any find_* runs), and hard-errors on the rest with the fix in the message.
#
# Usage from CMakeLists.txt -- two calls, because the Java check can only happen
# after find_package(Java), while the PATH sanitisation must happen before it:
#
#     include(WSL)
#     tonto_wsl_preflight()      # detect, sanitise PATH, run the file checks
#     find_package(Java ...)
#     tonto_wsl_finalize()       # check the tools that were just found; report
#
# Options
#   -DTONTO_WSL=AUTO|ON|OFF        detection override (default AUTO)
#   -DTONTO_WSL_STRICT=OFF         downgrade every WSL error to a warning
#   -DTONTO_WSL_KEEP_WINDOWS_PATH=ON   do not strip /mnt/* from PATH
#
# Non-WSL hosts are untouched: every function below returns immediately and
# nothing is printed.
########################################################################

set(TONTO_WSL "AUTO" CACHE STRING
    "WSL handling: AUTO (detect), ON (force on -- for testing), OFF (disable)")
set_property(CACHE TONTO_WSL PROPERTY STRINGS AUTO ON OFF)

option(TONTO_WSL_STRICT
    "Treat WSL environment problems as errors (OFF = warn and continue)" ON)

option(TONTO_WSL_KEEP_WINDOWS_PATH
    "Keep Windows (/mnt/*) directories on PATH when searching for tools" OFF)

# Test hook: pretend the build tree is somewhere else, so the DrvFs guard can be
# exercised on an ordinary Linux box (and in CI) without a Windows machine.
set(TONTO_WSL_TEST_BINARY_DIR "" CACHE STRING
    "Testing only: path used in place of CMAKE_BINARY_DIR by the DrvFs check")
mark_as_advanced(TONTO_WSL_TEST_BINARY_DIR)

# Problems are accumulated and raised together at the end of each phase, rather
# than on the spot, so that one configure run reports every problem it can see --
# otherwise fixing four things costs four configure cycles.
set(TONTO_WSL_PROBLEMS "" CACHE INTERNAL "" FORCE)

macro(_tonto_wsl_problem _msg)
    # Guard the empty case: appending to "" would leave a blank first element,
    # which the report below would print as an empty bullet.
    if(TONTO_WSL_PROBLEMS)
        set(TONTO_WSL_PROBLEMS "${TONTO_WSL_PROBLEMS}" "${_msg}" CACHE INTERNAL "" FORCE)
    else()
        set(TONTO_WSL_PROBLEMS "${_msg}" CACHE INTERNAL "" FORCE)
    endif()
endmacro()

# Raise everything collected so far as one message, then clear the list so a
# later call does not repeat it. Called at the end of each phase.
function(_tonto_wsl_report)
    if(NOT TONTO_WSL_PROBLEMS)
        return()
    endif()
    set(_report "")
    foreach(_p IN LISTS TONTO_WSL_PROBLEMS)
        string(APPEND _report "\n  *  ${_p}\n")
    endforeach()
    set(TONTO_WSL_PROBLEMS "" CACHE INTERNAL "" FORCE)
    if(TONTO_WSL_STRICT)
        message(FATAL_ERROR
            "WSL environment problems (see docs/BUILD_WSL.md):\n${_report}\n"
            "Configure with -DTONTO_WSL_STRICT=OFF to downgrade these to warnings.")
    else()
        message(WARNING
            "WSL environment problems, ignored because TONTO_WSL_STRICT=OFF "
            "(see docs/BUILD_WSL.md):\n${_report}")
    endif()
endfunction()

# True when <dir> lives on a Windows drive mounted into WSL. The path test is
# the reliable one (WSL always mounts drives under /mnt/<letter>); findmnt, when
# present, additionally catches drives mounted elsewhere. WSL1 reports drvfs,
# WSL2 reports 9p or virtiofs depending on the kernel.
function(_tonto_wsl_is_drvfs _dir _out)
    set(${_out} FALSE PARENT_SCOPE)
    if(_dir MATCHES "^/mnt/[A-Za-z]/" OR _dir MATCHES "^/mnt/[A-Za-z]$")
        set(${_out} TRUE PARENT_SCOPE)
        return()
    endif()
    find_program(_TONTO_FINDMNT findmnt)
    if(_TONTO_FINDMNT)
        execute_process(
            COMMAND ${_TONTO_FINDMNT} -n -o FSTYPE --target "${_dir}"
            OUTPUT_VARIABLE _fstype
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET)
        if(_fstype MATCHES "drvfs|9p|virtiofs")
            set(${_out} TRUE PARENT_SCOPE)
        endif()
    endif()
endfunction()


########################################################################
# Phase 1 -- detect, sanitise PATH, check the source tree.
# Must be called BEFORE any find_package/find_program.
########################################################################
function(tonto_wsl_preflight)
    set(TONTO_WSL_ACTIVE OFF PARENT_SCOPE)

    if(TONTO_WSL STREQUAL "OFF")
        return()
    endif()

    # -- Detection ------------------------------------------------------
    # WSL1 osrelease: 4.4.0-19041-Microsoft
    # WSL2 osrelease: 5.15.153.1-microsoft-standard-WSL2
    set(_osrelease "")
    if(EXISTS /proc/sys/kernel/osrelease)
        file(READ /proc/sys/kernel/osrelease _osrelease)
        string(STRIP "${_osrelease}" _osrelease)
    endif()

    set(_is_wsl OFF)
    if(TONTO_WSL STREQUAL "ON")
        set(_is_wsl ON)
    elseif(_osrelease MATCHES "[Mm]icrosoft|WSL" OR NOT "$ENV{WSL_DISTRO_NAME}" STREQUAL "")
        set(_is_wsl ON)
    endif()

    if(NOT _is_wsl)
        return()
    endif()

    # WSL2 has a real Linux kernel and sets WSL_INTEROP; WSL1 has neither.
    if(_osrelease MATCHES "WSL2" OR NOT "$ENV{WSL_INTEROP}" STREQUAL "")
        set(_wsl_version 2)
    elseif(_osrelease MATCHES "[Mm]icrosoft")
        set(_wsl_version 1)
    else()
        set(_wsl_version 0)   # forced on for testing; not really WSL
    endif()

    set(_distro "$ENV{WSL_DISTRO_NAME}")
    if(_distro STREQUAL "")
        set(_distro "unknown")
    endif()

    set(TONTO_WSL_ACTIVE  ON             PARENT_SCOPE)
    set(TONTO_WSL_VERSION ${_wsl_version} PARENT_SCOPE)
    set(TONTO_WSL_DISTRO  "${_distro}"   PARENT_SCOPE)
    set(TONTO_WSL_KERNEL  "${_osrelease}" PARENT_SCOPE)

    # -- Check A: strip Windows directories from PATH --------------------
    # This is the fix for cause 1, and it fixes it for EVERY find_* in the
    # project at once -- find_package(Java), find_program(DOT_EXE dot),
    # find_package(LAPACK) -- rather than one guard per tool. Only this CMake
    # process and its children are affected; the user's interactive PATH is not
    # touched.
    set(_dropped 0)
    if(NOT TONTO_WSL_KEEP_WINDOWS_PATH)
        string(REPLACE ":" ";" _path_list "$ENV{PATH}")
        set(_kept "")
        foreach(_p IN LISTS _path_list)
            if(_p MATCHES "^/mnt/[A-Za-z]/")
                math(EXPR _dropped "${_dropped}+1")
            else()
                list(APPEND _kept "${_p}")
            endif()
        endforeach()
        string(REPLACE ";" ":" _new_path "${_kept}")
        set(ENV{PATH} "${_new_path}")
    endif()
    set(TONTO_WSL_PATH_DROPPED ${_dropped} PARENT_SCOPE)

    # -- Check B0: is there a Linux JDK at all? ---------------------------
    # Once check A has removed the Windows directories, a machine whose only JDK
    # is the Windows one has no javac left to find. find_package(Java REQUIRED)
    # would then abort with a generic "Could NOT find Java" that says nothing
    # about WSL. Probe first so the message names the actual situation.
    find_program(_TONTO_WSL_JAVAC javac)
    if(NOT _TONTO_WSL_JAVAC)
        _tonto_wsl_problem(
"No Linux javac found inside WSL.
     A JDK installed on the *Windows* side does not count: it cannot read Linux
     paths, and CMake was told to ignore /mnt/* when searching for tools.
     FIX: sudo apt install default-jdk")
    endif()
    unset(_TONTO_WSL_JAVAC CACHE)

    # -- Check C: build tree on a Windows drive --------------------------
    set(_bindir "${CMAKE_BINARY_DIR}")
    if(NOT TONTO_WSL_TEST_BINARY_DIR STREQUAL "")
        set(_bindir "${TONTO_WSL_TEST_BINARY_DIR}")
    endif()
    _tonto_wsl_is_drvfs("${_bindir}" _bin_on_drvfs)
    if(_bin_on_drvfs)
        _tonto_wsl_problem(
"The build tree is on a Windows drive (${_bindir}).
     Compiling there is 10-50x slower and the exec bit is unreliable.
     FIX: build inside the Linux filesystem instead, e.g.
          mkdir -p ~/tonto-build && cd ~/tonto-build && cmake <path-to-source>")
    endif()

    # -- Check D: source tree on a Windows drive (warn only) -------------
    # Tonto's tracked paths are NTFS-safe (no : * ? \" < > | characters, no
    # case-collisions), so a /mnt/c checkout does work -- it is just slow.
    _tonto_wsl_is_drvfs("${CMAKE_SOURCE_DIR}" _src_on_drvfs)
    if(_src_on_drvfs)
        message(WARNING
            "WSL: the source tree is on a Windows drive (${CMAKE_SOURCE_DIR}). "
            "This works, but every file read crosses the Windows/Linux boundary and the "
            "build will be far slower. Consider cloning into your Linux home directory.")
    endif()

    # -- Check E: CRLF line endings --------------------------------------
    # Read as hex so the test does not depend on how CMake handles \\r itself.
    foreach(_f "${CMAKE_SOURCE_DIR}/foofiles/types.foo"
               "${CMAKE_SOURCE_DIR}/include/macros.in")
        if(EXISTS "${_f}")
            file(READ "${_f}" _hex LIMIT 4096 HEX)
            if(_hex MATCHES "0d0a")
                _tonto_wsl_problem(
"${_f} has Windows (CRLF) line endings.
     The Foo translator and the Fortran compiler both need LF. This happens when
     the repo is cloned by *Windows* git with core.autocrlf=true and then built
     from WSL.
     FIX: re-checkout with Unix endings, from inside WSL:
          git config core.autocrlf input && git rm --cached -r . && git reset --hard")
                break()
            endif()
        endif()
    endforeach()

    # -- Check F: WSL1 ----------------------------------------------------
    if(_wsl_version EQUAL 1)
        message(WARNING
            "WSL: this is WSL 1. Its filesystem and process creation are much slower than "
            "WSL 2's, which matters here because the build spawns one JVM per .foo file. "
            "Upgrade with:  wsl --set-version ${_distro} 2")
    endif()

    # -- Check G: case-insensitive build directory -------------------------
    set(_probe "${CMAKE_BINARY_DIR}/_TontoWSLCaseProbe")
    file(WRITE "${_probe}" "probe")
    if(EXISTS "${CMAKE_BINARY_DIR}/_tontowslcaseprobe")
        message(WARNING
            "WSL: the build directory is on a case-INSENSITIVE filesystem. The Fortran build "
            "distinguishes e.g. types.F90 from types.f90; unexplained 'file not found' or "
            "duplicate-module errors are likely to start here.")
    endif()
    file(REMOVE "${_probe}")

    # -- Check H: memory vs parallelism ------------------------------------
    # Translation runs `java` once per .foo file; each JVM wants roughly 0.5-1 GB.
    # WSL2 defaults its VM to half the host's RAM, so nproc alone over-commits.
    include(ProcessorCount)
    ProcessorCount(_ncpu)
    set(_safe_jobs ${_ncpu})
    if(EXISTS /proc/meminfo)
        file(STRINGS /proc/meminfo _memtotal REGEX "^MemTotal:")
        if(_memtotal MATCHES "([0-9]+)")
            math(EXPR _mem_gb "${CMAKE_MATCH_1}/1048576")
            math(EXPR _mem_jobs "${_mem_gb}/2")
            if(_mem_jobs LESS 1)
                set(_mem_jobs 1)
            endif()
            if(_mem_jobs LESS _safe_jobs)
                set(_safe_jobs ${_mem_jobs})
            endif()
            set(TONTO_WSL_MEM_GB ${_mem_gb} PARENT_SCOPE)
        endif()
    endif()
    if(_safe_jobs LESS 1)
        set(_safe_jobs 1)
    endif()
    set(TONTO_WSL_SAFE_JOBS ${_safe_jobs} PARENT_SCOPE)
    set(TONTO_WSL_NCPU      ${_ncpu}      PARENT_SCOPE)

    # -- Summary ----------------------------------------------------------
    # Printed in every configure log, for the same reason the LAPACK version and
    # the compiler provenance are: so that a cross-platform comparison never has
    # to guess what produced each side.
    message(STATUS "WSL: detected -- distro ${_distro}, WSL ${_wsl_version}, kernel ${_osrelease}")
    if(DEFINED _mem_gb)
        message(STATUS "WSL: ${_ncpu} CPUs, ${_mem_gb} GB RAM visible to the VM")
    endif()
    if(TONTO_WSL_KEEP_WINDOWS_PATH)
        message(STATUS "WSL: Windows PATH entries KEPT (TONTO_WSL_KEEP_WINDOWS_PATH=ON)")
    else()
        message(STATUS "WSL: dropped ${_dropped} Windows PATH entries before searching for tools")
    endif()
    message(STATUS "WSL: build with  make -j${_safe_jobs}  "
                   "(translation runs one JVM per .foo file; higher -j risks the OOM killer)")

    # Raise the environment problems now rather than in tonto_wsl_finalize():
    # find_package(Java REQUIRED) runs in between, and if the only JDK is the
    # Windows one it would abort first with a generic "Could NOT find Java".
    _tonto_wsl_report()
endfunction()


########################################################################
# Phase 2 -- check the tools that find_package() just resolved, then print
# the summary and raise anything collected. Call AFTER find_package(Java).
########################################################################
function(tonto_wsl_finalize)
    if(NOT TONTO_WSL_ACTIVE)
        return()
    endif()

    # -- Check B: a Windows JDK ------------------------------------------
    # Check A normally makes this unreachable. It is a backstop for a stale
    # CMakeCache, an explicit -DJava_JAVA_EXECUTABLE=, or
    # TONTO_WSL_KEEP_WINDOWS_PATH=ON.
    foreach(_var Java_JAVA_EXECUTABLE Java_JAVAC_EXECUTABLE)
        if(DEFINED ${_var} AND (${_var} MATCHES "^/mnt/" OR ${_var} MATCHES "\\.exe$"))
            _tonto_wsl_problem(
"${_var} is a *Windows* JDK: ${${_var}}
     It cannot read Linux paths such as ${CMAKE_SOURCE_DIR}, and it needs ';'
     rather than ':' between classpath entries -- but the Foo translator is
     invoked with ':'. Translation would fail with an obscure ANTLR error.
     FIX: install a Linux JDK inside WSL:  sudo apt install default-jdk")
        endif()
    endforeach()

    _tonto_wsl_report()
endfunction()
