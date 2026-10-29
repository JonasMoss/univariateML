#include "test_support.hpp"
#include "cppunivariate/distributions/pois/mle.hpp"
#include "cppunivariate/distributions/pois/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace dist = cppunivariate::distributions;
#define D_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define D_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)
namespace {
std::vector<int> as_ints(const std::vector<double> &v) {
  std::vector<int> out;
  for (double x : v) out.push_back(static_cast<int>(x));
  return out;
}
cppunivariate::ispan as_ispan(const std::vector<int> &v) { return {v.data(), v.size()}; }
}
TEST_CASE("pois") {
  auto g = mltest::load_distribution_golden("pois");
  auto v = as_ints(g.data);
  auto r = cppunivariate::pois_mle(as_ispan(v));
  CHECK(r.lambda == D_APPROX(g.estimates.at("lambda")));
  CHECK(r.logLik == D_APPROX(g.logLik));
}

TEST_CASE("pois all zeros") {
  const std::vector<int> v{0, 0, 0};
  const auto r = cppunivariate::pois_mle(as_ispan(v));
  CHECK(r.lambda == 0.0);
  CHECK(r.logLik == 0.0);
}

TEST_CASE("pois dpqr: pmf and CDF tail") {
  CHECK(dist::dpois(0.0, 1.0, false) == doctest::Approx(std::exp(-1.0)));
  CHECK(dist::ppois(0.0, 1.0, true, false) == doctest::Approx(std::exp(-1.0)));
  // qpois(p) is the smallest k with cdf >= p.
  const double k = dist::qpois(0.95, 4.0, true, false);
  CHECK(dist::ppois(k, 4.0, true, false) >= 0.95);
  CHECK(dist::ppois(k - 1.0, 4.0, true, false) < 0.95);
}

// Independent high-precision oracle; see quality/truth.py.
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("pois: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("pois",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dpois(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ppois(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qpois(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rpois(rng, p[0]); });
}

TEST_CASE("pois: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // l=mpf(2.0)
  // d=lambda k:exp(-l)*l**k/factorial(k); S=lambda k:gammainc(k+1,0,l)/gamma(k+1)
  // print(log(d(mpf(1e+100)))); print(log(S(mpf(110.0))))
  // lo,hi=0,1
  // while log(S(hi))>-10000: hi*=2
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::dpois(1e+100, 2.0, true) == doctest::Approx(-2.2856536211884465e+102).epsilon(1e-10));
  CHECK(cppunivariate::distributions::ppois(110.0, 2.0, false, true) == doctest::Approx(-340.07495410418113).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qpois(-10000, 2.0, false, true) == 1733.0);
  CHECK(cppunivariate::distributions::dpois(0.5, 2.0, false) == 0);
}

TEST_CASE("pois: adjacent noninteger at zero has no mass") {
  // mp.dps=100; x=mpf(float.fromhex("0x0.0000000000001p-1022")); print(x==floor(x))
  CHECK(cppunivariate::distributions::dpois(std::nextafter(0.0,1.0),2,false)==0);
}
