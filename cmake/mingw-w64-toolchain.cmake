# Cross-compile a Windows executable from Linux, with MinGW-w64.
#
#   cmake -DCMAKE_TOOLCHAIN_FILE=cmake/mingw-w64-toolchain.cmake \
#         -DCMAKE_BUILD_TYPE=release -DCOMPILE_LAPACK=ON ..
#
# The Foo->Fortran translator is a Java program and runs on the *host*, so only
# the Fortran and C compilation is cross-targeted. LAPACK must be built from the
# bundled source (-DCOMPILE_LAPACK=ON): there is no Windows BLAS to find.
#
# EXPERIMENTAL. Native Windows has never been a tested platform for Tonto; the
# supported route on Windows is WSL. See docs/BUILDING_ON_WINDOWS.md.

set(CMAKE_SYSTEM_NAME Windows)
set(CMAKE_SYSTEM_PROCESSOR x86_64)

set(TOOLCHAIN_PREFIX x86_64-w64-mingw32)

set(CMAKE_C_COMPILER       ${TOOLCHAIN_PREFIX}-gcc)
set(CMAKE_CXX_COMPILER     ${TOOLCHAIN_PREFIX}-g++)
set(CMAKE_Fortran_COMPILER ${TOOLCHAIN_PREFIX}-gfortran)
set(CMAKE_RC_COMPILER      ${TOOLCHAIN_PREFIX}-windres)

set(CMAKE_FIND_ROOT_PATH /usr/${TOOLCHAIN_PREFIX})

# Look for programs on the host (java, javac), and for libraries and headers
# only in the target sysroot.
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)

# One self-contained .exe: no libgfortran/libgcc/libwinpthread DLLs to ship.
set(CMAKE_EXE_LINKER_FLAGS_INIT "-static -static-libgfortran -static-libgcc")
