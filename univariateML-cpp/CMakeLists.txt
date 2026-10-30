# cppunivariate — header-only C++17 library for univariate
# density estimation. The R package in bindings/r/ is a downstream consumer
# of these headers; the planned Python package will be another.
#
# Builds nothing on its own; consumers link against the INTERFACE
# target `cppunivariate`.
#
# Boost.Math (header-only) is discovered in this order:
#   1. find_package(Boost) — system Boost (apt: libboost-dev, dnf: boost-devel)
#   2. fallback: BH package in an R install — `Rscript -e "system.file('include', 'BH')"`
# If neither is available, the user must install one.

cmake_minimum_required(VERSION 3.16)
project(cppunivariate CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# --- Boost.Math discovery --------------------------------------------------
find_package(Boost CONFIG QUIET)
if(NOT Boost_FOUND)
  find_program(R_EXECUTABLE R)
  if(R_EXECUTABLE)
    execute_process(
      COMMAND ${R_EXECUTABLE} --no-save --silent --no-echo
              -e "cat(system.file('include', package = 'BH'))"
      OUTPUT_VARIABLE BH_INCLUDE_DIR
      OUTPUT_STRIP_TRAILING_WHITESPACE
      RESULT_VARIABLE BH_RESULT)
    if(BH_RESULT EQUAL 0 AND BH_INCLUDE_DIR AND IS_DIRECTORY "${BH_INCLUDE_DIR}")
      set(Boost_INCLUDE_DIRS "${BH_INCLUDE_DIR}")
      message(STATUS "Boost.Math: using BH headers at ${BH_INCLUDE_DIR}")
    endif()
  endif()
  if(NOT Boost_INCLUDE_DIRS)
    message(FATAL_ERROR
      "Boost not found. Install system Boost (libboost-dev / boost-devel) "
      "or ensure the R 'BH' package is installed and Rscript is on PATH.")
  endif()
else()
  message(STATUS "Boost.Math: using find_package result")
endif()

# --- the cppunivariate INTERFACE library ----------------------------------
add_library(cppunivariate INTERFACE)
target_include_directories(cppunivariate INTERFACE
  ${CMAKE_CURRENT_SOURCE_DIR}/include
  ${Boost_INCLUDE_DIRS})
target_compile_features(cppunivariate INTERFACE cxx_std_17)

# --- tests -----------------------------------------------------------------
enable_testing()
add_subdirectory(tests)
