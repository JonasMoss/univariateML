# Standalone build of the C++ test harness for the mlcore estimators.
#
# Goal: `cmake --build && ctest` runs golden-regression tests against
# the same JSON files the R-side test_golden_regression.R consumes.
#
# Math backend (current): system Rmath via the R install on PATH.
# `R CMD config` gives us the include + library flags. WS3 task #4
# replaces this with a vendored standalone Rmath build so that no R
# install is required.

cmake_minimum_required(VERSION 3.16)
project(mlcore_tests CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# --- locate R / Rmath via `R CMD config` ----------------------------------
find_program(R_EXECUTABLE R REQUIRED)

execute_process(
  COMMAND ${R_EXECUTABLE} CMD config --cppflags
  OUTPUT_VARIABLE R_CPPFLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE)

execute_process(
  COMMAND ${R_EXECUTABLE} CMD config --ldflags
  OUTPUT_VARIABLE R_LDFLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE)

separate_arguments(R_CPPFLAGS_LIST UNIX_COMMAND "${R_CPPFLAGS}")
separate_arguments(R_LDFLAGS_LIST UNIX_COMMAND "${R_LDFLAGS}")

# --- the test binary -------------------------------------------------------
add_executable(mlcore_tests test_estimators.cpp)
set_target_properties(mlcore_tests PROPERTIES POSITION_INDEPENDENT_CODE ON)

# Headers from the package source tree (mlcore is header-only).
target_include_directories(mlcore_tests PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../../src)

target_compile_options(mlcore_tests PRIVATE
  -Wall -Wextra -Wpedantic ${R_CPPFLAGS_LIST})

target_link_options(mlcore_tests PRIVATE ${R_LDFLAGS_LIST})

# --- ctest registration ----------------------------------------------------
enable_testing()
add_test(
  NAME mlcore_tests
  COMMAND mlcore_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
set_tests_properties(mlcore_tests PROPERTIES
  ENVIRONMENT "UMVL_GOLDEN_DIR=${CMAKE_CURRENT_SOURCE_DIR}/../golden")
