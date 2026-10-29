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
