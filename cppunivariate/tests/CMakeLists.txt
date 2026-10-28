# Test binaries for cppunivariate. Consumes the parent INTERFACE
# target. Per-distribution tests and goldens live in
# include/cppunivariate/distributions/<id>/ (distribution_tests).

set(VALIDATION_DIR "${CMAKE_SOURCE_DIR}/../quality/derived")

set(_CPPUNIV_TEST_FLAGS -Wall -Wextra -Wpedantic)

function(cppuniv_test target source)
  add_executable(${target} ${source})
  set_target_properties(${target} PROPERTIES POSITION_INDEPENDENT_CODE ON)
  target_link_libraries(${target} PRIVATE cppunivariate)
  target_include_directories(${target} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
  target_compile_options(${target} PRIVATE ${_CPPUNIV_TEST_FLAGS})
endfunction()

cppuniv_test(bfgs_tests       test_bfgs.cpp)
cppuniv_test(newton_md_tests  test_newton_md.cpp)
cppuniv_test(sample_tests     test_sampling.cpp)
cppuniv_test(validation_tests test_validation.cpp)

add_test(NAME bfgs_tests COMMAND bfgs_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_test(NAME newton_md_tests COMMAND newton_md_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_test(NAME sample_tests COMMAND sample_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_test(NAME validation_tests COMMAND validation_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
set_tests_properties(validation_tests PROPERTIES
  ENVIRONMENT "UMVL_VALIDATION_DIR=${VALIDATION_DIR}")

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
add_test(NAME distribution_tests COMMAND distribution_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME no_throw_tests COMMAND no_throw_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
