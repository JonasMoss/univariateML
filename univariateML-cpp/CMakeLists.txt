# cppunivariate — header-only C++17 library for univariate
# density estimation. Language bindings are downstream consumers of these
# headers and are not required to configure, build or test this artifact.
#
# Builds nothing on its own; consumers link against the INTERFACE
# target `cppunivariate`.
#
# Boost.Math is discovered without R; explicit plain header paths are supported.

cmake_minimum_required(VERSION 3.16)
project(cppunivariate CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
# R's shipped C++17 compilation uses GNU extensions (gnu++17).
set(CMAKE_CXX_EXTENSIONS ON)

# --- Boost.Math discovery --------------------------------------------------
if(Boost_INCLUDE_DIR)
  set(Boost_INCLUDE_DIRS "${Boost_INCLUDE_DIR}")
else()
  find_package(Boost CONFIG QUIET)
  if(NOT Boost_FOUND)
    find_package(Boost MODULE QUIET)
  endif()
endif()
if(NOT Boost_INCLUDE_DIRS OR NOT EXISTS "${Boost_INCLUDE_DIRS}/boost/version.hpp")
  message(FATAL_ERROR
    "Boost headers not found. Install Boost or configure with "
    "-DBoost_INCLUDE_DIR=/path/to/headers or -DBOOST_ROOT=/path/to/boost.")
endif()
message(STATUS "Boost.Math headers: ${Boost_INCLUDE_DIRS}")

# --- the cppunivariate INTERFACE library ----------------------------------
add_library(cppunivariate INTERFACE)
target_include_directories(cppunivariate INTERFACE
  ${CMAKE_CURRENT_SOURCE_DIR}/include
  ${Boost_INCLUDE_DIRS})
target_compile_features(cppunivariate INTERFACE cxx_std_17)

# --- tests -----------------------------------------------------------------
enable_testing()
add_subdirectory(tests)
