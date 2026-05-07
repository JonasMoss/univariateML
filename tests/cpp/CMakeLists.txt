# Standalone build of the C++ test harness for the mlcore estimators.
#
# Goal: `cmake --build && ctest` runs golden-regression tests against
# the same JSON files the R-side test_golden_regression.R consumes.
#
# mlcore is header-only. The math backend (special functions, dpqr) is
# Boost.Math, which is header-only. We discover Boost via:
#   1. find_package(Boost) — system Boost (apt: libboost-dev, dnf: boost-devel)
#   2. fallback: BH package in an R install — `Rscript -e "system.file('include', 'BH')"`
# If neither is available, the user must install one.

cmake_minimum_required(VERSION 3.16)
project(mlcore_tests CXX)

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

# --- the test binary -------------------------------------------------------
add_executable(mlcore_tests test_estimators.cpp)
set_target_properties(mlcore_tests PROPERTIES POSITION_INDEPENDENT_CODE ON)

target_include_directories(mlcore_tests PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../../src
  ${Boost_INCLUDE_DIRS})

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

# --- the newton_md smoke-test binary --------------------------------------
add_executable(newton_md_tests test_newton_md.cpp)
set_target_properties(newton_md_tests PROPERTIES POSITION_INDEPENDENT_CODE ON)

target_include_directories(newton_md_tests PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../../src)

target_compile_options(newton_md_tests PRIVATE
  -Wall -Wextra -Wpedantic)

# --- the per-estimator Hessian tests (analytical vs FD vs Fisher) --------
add_executable(hessian_tests test_hessians.cpp)
set_target_properties(hessian_tests PROPERTIES POSITION_INDEPENDENT_CODE ON)

target_include_directories(hessian_tests PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../../src
  ${Boost_INCLUDE_DIRS})

target_compile_options(hessian_tests PRIVATE
  -Wall -Wextra -Wpedantic)

# --- the sampling-test binary ---------------------------------------------
add_executable(sample_tests test_sampling.cpp)
set_target_properties(sample_tests PROPERTIES POSITION_INDEPENDENT_CODE ON)

target_include_directories(sample_tests PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../../src
  ${Boost_INCLUDE_DIRS})

target_compile_options(sample_tests PRIVATE
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

add_test(
  NAME newton_md_tests
  COMMAND newton_md_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_test(
  NAME hessian_tests
  COMMAND hessian_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
set_tests_properties(hessian_tests PROPERTIES
  ENVIRONMENT "UMVL_GOLDEN_DIR=${CMAKE_CURRENT_SOURCE_DIR}/../golden")

add_test(
  NAME sample_tests
  COMMAND sample_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
