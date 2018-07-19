cmake_minimum_required(VERSION 3.0.0)
project(dftd3)

set(DFTD3_SRC
    "${CMAKE_CURRENT_SOURCE_DIR}/dftd3-lib/lib/api.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/dftd3-lib/lib/common.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/dftd3-lib/lib/core.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/dftd3-lib/lib/pars.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/dftd3-lib/lib/sizes.f90"
)

add_library(dftd3 STATIC ${DFTD3_SRC})
target_include_directories(dftd3 PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
set(DFTD3_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} PARENT_SCOPE)
