# Test binaries for cppunivariate. Consumes the parent INTERFACE
# target. Per-distribution tests and goldens live in
# include/cppunivariate/distributions/<id>/ (distribution_tests).


# Exercise R's optimization and assertion mode without Release's -O3.
# This applies only to our executables, never to downstream header consumers.
option(CPPUNIV_TEST_OPTIMIZE "Build tests with R's -g -O2 -DNDEBUG flags" ON)
set(_CPPUNIV_TEST_FLAGS -Wall -Wextra -Wpedantic -g)
if(CPPUNIV_TEST_OPTIMIZE)
  list(APPEND _CPPUNIV_TEST_FLAGS -O2 -DNDEBUG)
else()
  list(APPEND _CPPUNIV_TEST_FLAGS -O0)
endif()

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
file(GLOB MODIFIER_TEST_SOURCES CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/modifiers/*.cpp")
add_executable(distribution_tests distribution_tests_main.cpp
  ${MODIFIER_TEST_SOURCES}
  ${DISTRIBUTION_TEST_SOURCES})
target_link_libraries(distribution_tests PRIVATE cppunivariate)
target_include_directories(distribution_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_options(distribution_tests PRIVATE ${_CPPUNIV_TEST_FLAGS})
target_compile_definitions(distribution_tests PRIVATE
  CPPUNIV_DISTRIBUTIONS_DIR="${DISTRIBUTIONS_DIR}"
  CPPUNIV_MODIFIER_FIXTURES_DIR="${CMAKE_CURRENT_SOURCE_DIR}/modifiers/fixtures")
# Shard by source file and complementary expensive-case filters. No case is
# dropped: full selects both shards, smaller tiers select the core shard.
set(_slow_cases "*oracle*,*multistart*,*replication*,*large sample*,*large-sample*,*every held golden subset*,*slow*reference*,*240 seeded*")

# Each pair gives an approximate CPU cost in seconds and an exact case name.
# Keep the executable shared so scheduling cases independently adds no link work.
set(distribution_genbeta_shards
  60 "genbeta: exact gamma and effective-endpoint power face comparisons"
  49 "genbeta all-free certificate: a shallow gamma-adjacent proposal retains its uncertified legacy fit"
  46 "genbeta all-free: incomplete gamma certificates preserve the legacy payload"
  33 "genbeta: structured face payloads"
  27 "genbeta all-free: signed squared-power certificate selects a global gamma face")
set(distribution_genbeta_slow_shards
  87 "genbeta: large replication preserves conditional coefficient accuracy")
set(distribution_bhatt_shards
  24 "bhatt: free fit is affine equivariant"
  10 "bhatt failed enclosure preserves the successful existing search as uncertified"
  11 "bhatt: central density preserves the golden fit")
set(distribution_bnbinom_shards
  15 "bnbinom: insufficient certificate work preserves a successful finite legacy search"
  10 "bnbinom: every held subset, scores and mirror fits"
  10 "bnbinom: all-free proposals pass a parameter-uniform global inner certificate"
   9 "bnbinom: all-free numerical candidates retain both labels and a diagonal witness")
set(distribution_cmb_shards
  96 "cmb: free-size endpoints, invalid controls and work exhaustion"
  11 "cmb: million-count searches exhaust a shared cell ledger with witnesses")
set(distribution_dgamma_shards
  42 "dgamma: ordinary starts keep their allowance despite remote shape probes")
set(distribution_hyper_shards
  16 "hyper: fully free search, faces and work limit"
   9 "hyper: held population joint finite grid")
set(distribution_sged_shards
  30 "sged: half-GED payload retains base scale and original-unit likelihood"
  14 "sged constrained faces and shapes"
  11 "sged translated exponential faces")
set(distribution_st_shards
  14 "st: native golden fit"
  12 "st near-normal 55-digit coefficient reference is affine equivariant")
set(distribution_trgamma_shards
  29 "trgamma: seeded samples agree with a dense-grid profile reference"
  11 "trgamma: interior close to the lognormal face and scale range")
# Prioritize long indivisible cases and the remaining work in split targets.
set(distribution_pig_cost 98)
set(distribution_tlambda_slow_cost 56)
set(distribution_pois_slow_cost 59)
set(distribution_cmp_slow_cost 62)
set(distribution_bhatt_cost 28)
set(distribution_bnbinom_cost 32)
set(distribution_sged_cost 27)
set(modifier_ztpois_cost 36)

function(cppuniv_distribution_shards name source labels included excluded)
  set(remainder "${excluded}")
  set(shards "${${name}_shards}")
  set(part 0)
  while(shards)
    list(POP_FRONT shards cost case)
    math(EXPR part "${part} + 1")
    # doctest separates patterns at unescaped commas.
    string(REPLACE "," "\\," pattern "${case}")
    add_test(NAME ${name}_part${part} COMMAND distribution_tests
      "--source-file=*${source}" "--test-case=${pattern}")
    set_tests_properties(${name}_part${part} PROPERTIES
      WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" LABELS "${labels}" COST "${cost}")
    if(remainder)
      string(APPEND remainder ",")
    endif()
    string(APPEND remainder "${pattern}")
  endwhile()
  add_test(NAME ${name} COMMAND distribution_tests "--source-file=*${source}"
    "--test-case=${included}" "--test-case-exclude=${remainder}")
  set_tests_properties(${name} PROPERTIES
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" LABELS "${labels}")
  if(DEFINED ${name}_cost)
    set_tests_properties(${name} PROPERTIES COST "${${name}_cost}")
  endif()
endfunction()

foreach(source IN LISTS DISTRIBUTION_TEST_SOURCES)
  get_filename_component(folder "${source}" DIRECTORY)
  get_filename_component(id "${folder}" NAME)
  cppuniv_distribution_shards(distribution_${id} "distributions/${id}/test.cpp"
    "cran;dev;full" "" "${_slow_cases}")
  file(READ "${source}" contents)
  string(TOLOWER "${contents}" contents)
  if(contents MATCHES "test_case\\(\"[^\"]*(oracle|multistart|replication|large.sample|every held golden subset|slow[^\"]*reference|240 seeded)")
    cppuniv_distribution_shards(distribution_${id}_slow "distributions/${id}/test.cpp"
      "full" "${_slow_cases}" "")
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

cppuniv_test(comparison_tests test_comparison.cpp)
add_test(NAME comparison_tests COMMAND comparison_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

cppuniv_test(maximizer_set_tests test_maximizer_set.cpp)
add_test(NAME maximizer_set_tests COMMAND maximizer_set_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

set_tests_properties(bfgs_tests sample_tests no_throw_tests cmp_tests tails_tests comparison_tests maximizer_set_tests
  PROPERTIES LABELS "cran;dev;full")

# The monorepo audit is generated from binding call routing, but its checked-in
# C++ source depends only on the standalone core. Do not require bindings or
# Python when building the independently distributed C++ tree.
set(_diagnostics_source "${CMAKE_SOURCE_DIR}/../dev/validation/diagnostics-audit.cpp")
if(EXISTS "${_diagnostics_source}")
  cppuniv_test(diagnostics_tests "${_diagnostics_source}")
  add_test(NAME diagnostics_tests COMMAND diagnostics_tests)
  set_tests_properties(diagnostics_tests PROPERTIES LABELS "dev;full" TIMEOUT 60)
endif()

foreach(source IN LISTS MODIFIER_TEST_SOURCES)
  get_filename_component(id "${source}" NAME_WE)
  cppuniv_distribution_shards(modifier_${id} "modifiers/${id}.cpp"
    "cran;dev;full" "" "${_slow_cases}")
  file(READ "${source}" contents)
  string(TOLOWER "${contents}" contents)
  if(contents MATCHES "test_case\\(\"[^\"]*(oracle|multistart|replication|large.sample|every held golden subset|slow[^\"]*reference|240 seeded)")
    cppuniv_distribution_shards(modifier_${id}_slow "modifiers/${id}.cpp"
      "full" "${_slow_cases}" "")
  endif()
endforeach()
