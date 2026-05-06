# Standalone build of the C++ test harness for the mlcore estimators.
#
# Goal: `cmake --build && ctest` runs golden-regression tests against
# the same JSON files the R-side test_golden_regression.R consumes —
# without requiring an R install on the box.
#
# mlcore is header-only except for the polygamma port (digamma /
# trigamma / tetragamma; see src/ml/special/polygamma.cpp). That .cpp
# is compiled into mlcore_tests below; bfgs_tests doesn't need it
# (the BFGS smoke test only includes core/* headers).

cmake_minimum_required(VERSION 3.16)
project(mlcore_tests CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# --- the test binary -------------------------------------------------------
add_executable(mlcore_tests
  test_estimators.cpp
  ../../src/ml/special/polygamma.cpp)
set_target_properties(mlcore_tests PROPERTIES POSITION_INDEPENDENT_CODE ON)

target_include_directories(mlcore_tests PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../../src)

target_compile_options(mlcore_tests PRIVATE
  -Wall -Wextra -Wpedantic)

# --- the BFGS smoke-test binary -------------------------------------------
add_executable(bfgs_tests test_bfgs.cpp)
set_target_properties(bfgs_tests PROPERTIES POSITION_INDEPENDENT_CODE ON)

target_include_directories(bfgs_tests PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../../src)

target_compile_options(bfgs_tests PRIVATE
  -Wall -Wextra -Wpedantic)

# --- ctest registration ----------------------------------------------------
enable_testing()
add_test(
  NAME mlcore_tests
  COMMAND mlcore_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
set_tests_properties(mlcore_tests PROPERTIES
  ENVIRONMENT "UMVL_GOLDEN_DIR=${CMAKE_CURRENT_SOURCE_DIR}/../golden")

add_test(
  NAME bfgs_tests
  COMMAND bfgs_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
