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
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("lgser: d/p/q/r against the truth table") {
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


TEST_CASE("lgser: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // t=mpf(0.9)
  // d=lambda k:-t**k/(k*log(1-t)); S=lambda k:nsum(lambda j:d(j),[k+1,inf])
  // print(log(d(mpf(1e+100)))); print(log(S(mpf(6486.0))))
  // lo,hi=0,1
  // while log(S(hi))>-10000: hi*=2
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::dlgser(1e+100, 0.9, true) == doctest::Approx(-1.0536051565782628e+99).epsilon(1e-10));
  CHECK(cppunivariate::distributions::plgser(6486.0, 0.9, false, true) == doctest::Approx(-690.7840521798125).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qlgser(-10000, 0.9, false, true) == 94817.0);
  CHECK(cppunivariate::distributions::dlgser(0.5, 0.9, false) == 0);
}

TEST_CASE("lgser: TASK-132 invalid counts and empty input have statuses") {
  const std::vector<int> empty, negative{-1,2}, zero(4,0), one(4,1);
  CHECK(cppunivariate::lgser_mle(cppunivariate::ispan{empty.data(),empty.size()}).status == cppunivariate::LgserStatus::InvalidInput);
  CHECK(cppunivariate::lgser_mle(cppunivariate::ispan{negative.data(),negative.size()}).status == cppunivariate::LgserStatus::InvalidInput);
  CHECK(cppunivariate::lgser_mle(cppunivariate::ispan{one.data(),one.size()}).status == cppunivariate::LgserStatus::PointMass);
}
