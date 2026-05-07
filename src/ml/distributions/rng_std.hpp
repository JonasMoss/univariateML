// Standalone std::random RNG adapter, used by tests/cpp/test_sampling.cpp
// and (later) by the Python/standalone build paths. Exposes the same
// duck-typed interface as src/distributions/rng_r.h's RRng:
//
//   double runif(double a, double b);
//   double rnorm();                                    // N(0, 1)
//   double rgamma(double shape, double scale = 1.0);
//   double rt(double df);
//
// The portable samplers under src/ml/distributions/ are
// `template <typename Rng> double r<dist>(Rng& rng, ...)` and are
// instantiated with whichever adapter the host environment provides
// (RRng for R, this StdRng for standalone, NumpyRng later for Python).

#pragma once

#include <random>

namespace mlcore {
namespace distributions {

template <typename Generator = std::mt19937_64>
struct StdRng {
  Generator gen;
  std::normal_distribution<> norm_{0.0, 1.0};

  explicit StdRng(typename Generator::result_type seed) : gen(seed) {}

  inline double runif(double a, double b) {
    return std::uniform_real_distribution<>(a, b)(gen);
  }
  inline double rnorm() { return norm_(gen); }
  inline double rgamma(double shape, double scale = 1.0) {
    return std::gamma_distribution<>(shape, scale)(gen);
  }
  inline double rt(double df) {
    return std::student_t_distribution<>(df)(gen);
  }
};

} // namespace distributions
} // namespace mlcore
