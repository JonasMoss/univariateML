// Shared helpers for the per-distribution doctest files
// (cppunivariate/include/cppunivariate/distributions/<id>/test.cpp), which
// CMake globs into the distribution_tests executable.

#pragma once

#include "doctest.h"
#include "json_loader.hpp"

#include <string>

#ifndef CPPUNIV_DISTRIBUTIONS_DIR
#error "CPPUNIV_DISTRIBUTIONS_DIR must be defined by the build (tests/CMakeLists.txt)"
#endif

namespace mltest {

// Relative tolerance against goldens. The finite-difference BFGS fitters
// move by ~1e-6 across compilers and Boost versions, hence kFdTol for them.
constexpr double kTol = 1e-10;
constexpr double kFdTol = 1e-6;

// Defaults of the R adapters (reltol / gradtol / abstol, iterlim).
constexpr double kFitTol = 1.220703e-4;
constexpr int kFitMaxit = 100;

inline Golden load_distribution_golden(const std::string &id) {
  return load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/" + id + "/golden.json");
}

}  // namespace mltest
