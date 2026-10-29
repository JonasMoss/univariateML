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
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("pois: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("pois",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dpois(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ppois(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qpois(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rpois(rng, p[0]); });
}

TEST_CASE("pois: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("pois");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<2;++mask) {
   const auto r=cppunivariate::pois_mle({x.data(),x.size()},(mask & 1)?g.estimates.at("lambda"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.lambda==g.estimates.at("lambda"));
 }
}

TEST_CASE("pois: P2 held boundary likelihood and domain") {
 const std::vector<int> x{0,2},same{0,0};
 const auto incompatible=cppunivariate::pois_mle({x.data(),x.size()},0);
 CHECK_FALSE(std::isfinite(incompatible.logLik));
 const auto valid=cppunivariate::pois_mle({same.data(),same.size()},0);
 CHECK(valid.lambda==0);
 CHECK(std::isfinite(valid.logLik));
 const auto invalid=cppunivariate::pois_mle({x.data(),x.size()},-1);
 CHECK_FALSE(std::isfinite(invalid.logLik));
}
