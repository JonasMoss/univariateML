# Test binaries for cppunivariate. Consumes the parent INTERFACE
# target. Per-distribution tests and goldens live in
# include/cppunivariate/distributions/<id>/ (distribution_tests).


set(_CPPUNIV_TEST_FLAGS -Wall -Wextra -Wpedantic)

function(cppuniv_test target source)
  add_executable(${target} ${source})
  set_target_properties(${target} PROPERTIES POSITION_INDEPENDENT_CODE ON)
  target_link_libraries(${target} PRIVATE cppunivariate)
  target_include_directories(${target} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
  target_compile_options(${target} PRIVATE ${_CPPUNIV_TEST_FLAGS})
endfunction()

cppuniv_test(bfgs_tests       test_bfgs.cpp)
cppuniv_test(sample_tests     test_sampling.cpp)

add_test(NAME bfgs_tests COMMAND bfgs_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})


add_test(NAME sample_tests COMMAND sample_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})


cppuniv_test(no_throw_tests test_no_throw.cpp)

# Per-distribution tests: every distributions/<id>/test.cpp, one executable.
set(DISTRIBUTIONS_DIR
  "${CMAKE_SOURCE_DIR}/include/cppunivariate/distributions")
file(GLOB DISTRIBUTION_TEST_SOURCES CONFIGURE_DEPENDS
  "${DISTRIBUTIONS_DIR}/*/test.cpp")
add_executable(distribution_tests distribution_tests_main.cpp
  ${DISTRIBUTION_TEST_SOURCES})
target_link_libraries(distribution_tests PRIVATE cppunivariate)
target_include_directories(distribution_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_options(distribution_tests PRIVATE ${_CPPUNIV_TEST_FLAGS})
target_compile_definitions(distribution_tests PRIVATE
  CPPUNIV_DISTRIBUTIONS_DIR="${DISTRIBUTIONS_DIR}")
# Shard by source file and complementary expensive-case filters. No case is
# dropped: full selects both shards, smaller tiers select the core shard.
set(_slow_cases "*oracle*,*multistart*,*replication*,*large sample*,*large-sample*,*every held golden subset*,*slow*reference*,*240 seeded*")
foreach(source IN LISTS DISTRIBUTION_TEST_SOURCES)
  get_filename_component(folder "${source}" DIRECTORY)
  get_filename_component(id "${folder}" NAME)
  add_test(NAME distribution_${id} COMMAND distribution_tests
    "--source-file=*distributions/${id}/test.cpp" "--test-case-exclude=${_slow_cases}")
  set_tests_properties(distribution_${id} PROPERTIES
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" LABELS "cran;dev;full")
  file(READ "${source}" contents)
  string(TOLOWER "${contents}" contents)
  if(contents MATCHES "test_case\\(\"[^\"]*(oracle|multistart|replication|large.sample|every held golden subset|slow[^\"]*reference|240 seeded)")
    add_test(NAME distribution_${id}_slow COMMAND distribution_tests
      "--source-file=*distributions/${id}/test.cpp" "--test-case=${_slow_cases}")
    set_tests_properties(distribution_${id}_slow PROPERTIES
      WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" LABELS "full")
  endif()
endforeach()
add_test(NAME distribution_support COMMAND distribution_tests
  "--source-file=*distribution_tests_main.cpp")
set_tests_properties(distribution_support PROPERTIES LABELS "cran;dev;full")

# Benchmark switch: compare clean builds with compiler caching disabled.
option(CPPUNIV_TEST_PCH "Precompile common distribution test headers" ON)
if(CPPUNIV_TEST_PCH)
  set_source_files_properties(distribution_tests_main.cpp PROPERTIES SKIP_PRECOMPILE_HEADERS ON)
  target_precompile_headers(distribution_tests PRIVATE
    "${CMAKE_CURRENT_SOURCE_DIR}/test_support.hpp"
    <cppunivariate/special/special.hpp>)
  # CMake supplies -include; ccache additionally requires these PCH settings.
  # Test sources use no time macros or header-configuration defines.
  if(CMAKE_CXX_COMPILER_LAUNCHER STREQUAL "ccache")
    set_property(TARGET distribution_tests PROPERTY CXX_COMPILER_LAUNCHER
      "${CMAKE_COMMAND};-E;env;CCACHE_SLOPPINESS=pch_defines,time_macros;ccache")
  endif()
endif()

add_test(NAME no_throw_tests COMMAND no_throw_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

cppuniv_test(cmp_tests test_cmp.cpp)
add_test(NAME cmp_tests COMMAND cmp_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

cppuniv_test(tails_tests test_tails.cpp)
add_test(NAME tails_tests COMMAND tails_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

set_tests_properties(bfgs_tests sample_tests no_throw_tests cmp_tests tails_tests
  PROPERTIES LABELS "cran;dev;full")

# The monorepo audit is generated from binding call routing, but its checked-in
# C++ source depends only on the standalone core. Do not require bindings or
# Python when building the independently distributed C++ tree.
set(_diagnostics_source "${CMAKE_SOURCE_DIR}/../dev/validation/diagnostics-audit.cpp")
if(EXISTS "${_diagnostics_source}")
  cppuniv_test(diagnostics_tests "${_diagnostics_source}")
  target_compile_options(diagnostics_tests PRIVATE -O2)
  add_test(NAME diagnostics_tests COMMAND diagnostics_tests)
  set_tests_properties(diagnostics_tests PROPERTIES LABELS "dev;full" TIMEOUT 60)
endif()
