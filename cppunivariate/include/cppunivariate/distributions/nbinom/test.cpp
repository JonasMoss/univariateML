#include "test_support.hpp"
#include "cppunivariate/distributions/nbinom/mle.hpp"
#include "cppunivariate/distributions/nbinom/dpqr.hpp"
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
 TEST_CASE("nbinom") {
  auto g = mltest::load_distribution_golden("nbinom");
  auto v = as_ints(g.data);
  auto r = cppunivariate::nbinom_mle(as_ispan(v), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.size == D_APPROX(g.estimates.at("size")));
  CHECK(r.prob == D_APPROX(g.estimates.at("prob")));
  CHECK(r.logLik == D_APPROX(g.logLik));
}

TEST_CASE("nbinom all zeros") {
  const std::vector<int> v{0, 0, 0};
  const auto r = cppunivariate::nbinom_mle(as_ispan(v), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.size == 1.0);
  CHECK(r.prob == 1.0);
  CHECK(r.logLik == 0.0);
  CHECK(cppunivariate::special::dnbinom(0.0, r.size, r.prob, true) == 0.0);
  CHECK(cppunivariate::special::dnbinom(1.0, r.size, r.prob, true) ==
        -std::numeric_limits<double>::infinity());
}

TEST_CASE("nbinom dpqr: matches dnbinom from special::") {
  CHECK(dist::dnbinom(0.0, 5.0, 0.5, false) > 0.0);
  // CDF round-trip.
  const double k = dist::qnbinom(0.6, 5.0, 0.5, true, false);
  CHECK(dist::pnbinom(k, 5.0, 0.5, true, false) >= 0.6);
  CHECK(dist::pnbinom(k - 1.0, 5.0, 0.5, true, false) < 0.6);
}

TEST_CASE("nbinom: underdispersion status") {
  const std::vector<int> x{1, 1, 1, 1};
  const auto r = cppunivariate::nbinom_mle(as_ispan(x), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::NbinomStatus::Underdispersed);
  CHECK(r.underdispersed);
}
