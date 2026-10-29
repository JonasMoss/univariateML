#include "test_support.hpp"
#include "cppunivariate/distributions/geom/mle.hpp"
#include "cppunivariate/distributions/geom/dpqr.hpp"
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
 TEST_CASE("geom") {
  auto g = mltest::load_distribution_golden("geom");
  auto v = as_ints(g.data);
  auto r = cppunivariate::geom_mle(as_ispan(v));
  CHECK(r.prob == D_APPROX(g.estimates.at("prob")));
  CHECK(r.logLik == D_APPROX(g.logLik));
}

TEST_CASE("geom all zeros") {
  const std::vector<int> v{0, 0, 0};
  const auto r = cppunivariate::geom_mle(as_ispan(v));
  CHECK(r.prob == 1.0);
  CHECK(r.logLik == 0.0);
  CHECK(cppunivariate::special::dgeom(1.0, 1.0, true) ==
        -std::numeric_limits<double>::infinity());
}

TEST_CASE("geom dpqr: closed-form CDF") {
  // F(0; prob=0.5) = prob.
  CHECK(dist::pgeom(0.0, 0.5, true, false) == doctest::Approx(0.5));
  // P(X = 0) = prob.
  CHECK(dist::dgeom(0.0, 0.5, false) == doctest::Approx(0.5));
  CHECK(dist::qgeom(0.99, 0.5, true, false) >= 0.0);
}

TEST_CASE("geom: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("geom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<2;++mask) {
   const auto r=cppunivariate::geom_mle({x.data(),x.size()},(mask & 1)?g.estimates.at("prob"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.prob==g.estimates.at("prob"));
 }
}

TEST_CASE("geom: P2 held boundary likelihood and domain") {
 const std::vector<int> x{0,2},same{0,0};
 const auto incompatible=cppunivariate::geom_mle({x.data(),x.size()},1);
 CHECK_FALSE(std::isfinite(incompatible.logLik));
 const auto valid=cppunivariate::geom_mle({same.data(),same.size()},1);
 CHECK(valid.prob==1);
 CHECK(std::isfinite(valid.logLik));
 const auto invalid=cppunivariate::geom_mle({x.data(),x.size()},-1);
 CHECK_FALSE(std::isfinite(invalid.logLik));
}
