// Standalone std::random RNG adapter, used by tests/cpp/test_sampling.cpp
// and (later) by the Python/standalone build paths. Exposes the same
// duck-typed interface as univariateML/src/distributions/rng_r.h's RRng:
//
//   double runif(double a, double b);
//   double rnorm();                                    // N(0, 1)
//   double rgamma(double shape, double scale = 1.0);
//   double rt(double df);
//
// The portable samplers under cppunivariate/include/cppunivariate/distributions/ are
// `template <typename Rng> double r<dist>(Rng& rng, ...)` and are
// instantiated with whichever adapter the host environment provides
// (RRng for R, this StdRng for standalone, NumpyRng later for Python).

#pragma once

#include <random>
#include <cmath>
#include <cstdint>
#include <limits>

namespace cppunivariate {
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
  inline double rpois(double lambda) {
    if(!(lambda>=0) || !std::isfinite(lambda))
      return lambda==std::numeric_limits<double>::infinity() ? lambda : std::numeric_limits<double>::quiet_NaN();
    // Preserve the established seeded stream throughout the old safe range.
    if(lambda<=static_cast<double>(std::numeric_limits<int>::max())-1e6)
      return static_cast<double>(std::poisson_distribution<int>(lambda)(gen));
    if(lambda<1e18)
      return static_cast<double>(std::poisson_distribution<std::int64_t>(lambda)(gen));
    // Hoermann PTRS transformed rejection, in double count coordinates.
    // Evaluate the Poisson log mass by a deviance series and Stirling;
    // subtracting lgamma(k+1) from k*log(lambda) loses all digits here.
    const double sl=std::sqrt(lambda), b=.931+2.53*sl;
    const double a=-.059+.02483*b, inv_alpha=1.1239+1.1328/(b-3.4);
    const double vr=.9277-3.6224/(b-2);
    for(;;) {
      const double u=runif(0,1)-.5, v=runif(0,1), us=.5-std::abs(u);
      if(us==0) continue;
      const double offset=(2*a/us+b)*u+.43;
      const double k=std::floor(lambda+offset);
      if(k<0 || !std::isfinite(k)) continue;
      if(us>=.07 && v<=vr) return k;
      if(us<.013 && v>us) continue;
      const double d=k-lambda, t=d/lambda;
      // k log(k/lambda)-k+lambda = lambda*(t^2/2-t^3/6+...).
      const double dev=(d/sl)*(d/sl)*(.5-t/6+t*t/12);
      const double lm=-dev-.5*(std::log(2*3.14159265358979323846)+std::log(k))-1/(12*k);
      if(std::log(v*inv_alpha/(a/(us*us)+b))<=lm) return k;
    }
  }
};

} // namespace distributions
} // namespace cppunivariate
