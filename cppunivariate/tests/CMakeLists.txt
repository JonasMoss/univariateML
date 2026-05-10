# Test binaries for cppunivariate. Consumes the parent INTERFACE
# target. Goldens live at quality/golden/ at the repo root and are
# located via the UMVL_GOLDEN_DIR env var.

set(GOLDEN_DIR "${CMAKE_SOURCE_DIR}/../quality/golden")
set(VALIDATION_DIR "${CMAKE_SOURCE_DIR}/../quality/derived")

set(_CPPUNIV_TEST_FLAGS -Wall -Wextra -Wpedantic)

function(cppuniv_test target source)
  add_executable(${target} ${source})
  set_target_properties(${target} PROPERTIES POSITION_INDEPENDENT_CODE ON)
  target_link_libraries(${target} PRIVATE cppunivariate)
  target_include_directories(${target} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
  target_compile_options(${target} PRIVATE ${_CPPUNIV_TEST_FLAGS})
endfunction()

cppuniv_test(estimator_tests  test_estimators.cpp)
cppuniv_test(bfgs_tests       test_bfgs.cpp)
cppuniv_test(newton_md_tests  test_newton_md.cpp)
cppuniv_test(hessian_tests    test_hessians.cpp)
cppuniv_test(sample_tests     test_sampling.cpp)
cppuniv_test(metadata_tests   test_metadata.cpp)
cppuniv_test(validation_tests test_validation.cpp)

add_test(NAME estimator_tests COMMAND estimator_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
set_tests_properties(estimator_tests PROPERTIES
  ENVIRONMENT "UMVL_GOLDEN_DIR=${GOLDEN_DIR}")

add_test(NAME bfgs_tests COMMAND bfgs_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_test(NAME newton_md_tests COMMAND newton_md_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_test(NAME hessian_tests COMMAND hessian_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
set_tests_properties(hessian_tests PROPERTIES
  ENVIRONMENT "UMVL_GOLDEN_DIR=${GOLDEN_DIR}")

add_test(NAME sample_tests COMMAND sample_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_test(NAME metadata_tests COMMAND metadata_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_test(NAME validation_tests COMMAND validation_tests
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
set_tests_properties(validation_tests PROPERTIES
  ENVIRONMENT "UMVL_VALIDATION_DIR=${VALIDATION_DIR}")
