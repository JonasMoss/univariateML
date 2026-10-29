#include "test_support.hpp"
#include "cppunivariate/distributions/lgser/mle.hpp"
#include "cppunivariate/distributions/lgser/dpqr.hpp"
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
 TEST_CASE("lgser") {
  auto g = mltest::load_distribution_golden("lgser");
  auto v = as_ints(g.data);
  auto r = cppunivariate::lgser_mle(as_ispan(v));
  CHECK(r.theta == D_APPROX(g.estimates.at("theta")));
  CHECK(r.logLik == D_APPROX(g.logLik));
}

TEST_CASE("lgser: mean just above one") {
  const std::vector<int> x{1, 1, 1, 2};
  const auto r = cppunivariate::lgser_mle(cppunivariate::ispan{x.data(), x.size()});
  REQUIRE(!r.all_ones);
  CHECK(r.theta > 0.0);
  CHECK(r.theta < 1.0);
  // mean(x) = 5/4 = -theta / ((1 - theta) log(1 - theta)).
  CHECK(-r.theta / ((1.0 - r.theta) * std::log1p(-r.theta)) == doctest::Approx(1.25).epsilon(1e-12));
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("lgser: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("lgser",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dlgser(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::plgser(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qlgser(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rlgser(rng, p[0]); });
}

TEST_CASE("lgser: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("lgser");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<2;++mask) {
   const auto r=cppunivariate::lgser_mle({x.data(),x.size()},(mask & 1)?g.estimates.at("theta"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.theta==g.estimates.at("theta"));
 }
}

TEST_CASE("lgser: P2 held boundary likelihood and domain") {
 const std::vector<int> x{1,2},same{1,1};
 const auto incompatible=cppunivariate::lgser_mle({x.data(),x.size()},0);
 CHECK_FALSE(std::isfinite(incompatible.logLik));
 const auto valid=cppunivariate::lgser_mle({same.data(),same.size()},0.4);
 CHECK(valid.theta==0.4);
 CHECK(std::isfinite(valid.logLik));
 const auto invalid=cppunivariate::lgser_mle({x.data(),x.size()},-1);
 CHECK_FALSE(std::isfinite(invalid.logLik));
}
