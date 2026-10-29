#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include <vector>
#include <cmath>
#include <string>
namespace {
#include "../burr/oracle-fixtures.inc"
}
TEST_CASE("burr three parameter golden and reciprocal fit") {
 auto g = mltest::load_distribution_golden("burr");
 auto r = cppunivariate::burr_mle(mltest::as_span(g.data),1e-8,1000);
 REQUIRE(r.status == cppunivariate::BurrStatus::Ok);
 CHECK(r.shape1 == doctest::Approx(g.estimates.at("shape1")).epsilon(1e-12));
 CHECK(r.shape2 == doctest::Approx(g.estimates.at("shape2")).epsilon(1e-12));
 CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(1e-12));
 CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(1e-12));
 // Independent full actuar multistart oracle on this ordinary sample.
 CHECK(r.logLik == doctest::Approx(-613.67979675083461).epsilon(1e-8));
 // Independent full likelihood scores, including the freely fitted scale.
 double g0=-static_cast<double>(g.data.size()), g1=g0, g2=0;
 for (double x : g.data) {
   const double z = r.shape2 * (std::log(x)-std::log(r.scale));
   const double sp = std::max(z,0.0)+std::log1p(std::exp(-std::abs(z)));
   const double sigmoid = z>=0 ? 1/(1+std::exp(-z)) : std::exp(z)/(1+std::exp(z));
   g0 += r.shape1*sp;
   g1 += -z+(r.shape1+1)*z*sigmoid;
   g2 += r.shape2*(1-(r.shape1+1)*sigmoid);
 }
 CHECK(std::sqrt(g0*g0+g1*g1+g2*g2)<1e-3);
 const double factor = 1000.; // All Burr values exceed 1; all inverse values are below 1.
 std::vector<double> reciprocal, scaled;
 double jacobian = 0;
 for (double x : g.data) { reciprocal.push_back(1/x); scaled.push_back(factor*x); jacobian += 2*std::log(x); }
 auto inv = cppunivariate::invburr_mle(mltest::as_span(reciprocal),1e-8,1000);
 CHECK(inv.shape1 == doctest::Approx(r.shape1).epsilon(1e-5));
 CHECK(inv.shape2 == doctest::Approx(r.shape2).epsilon(1e-5));
 CHECK(inv.scale == doctest::Approx(1/r.scale).epsilon(1e-5));
 CHECK(inv.logLik == doctest::Approx(r.logLik+jacobian).epsilon(1e-8));
 auto s = cppunivariate::burr_mle(mltest::as_span(scaled),1e-8,1000);
 CHECK(s.shape1 == doctest::Approx(r.shape1).epsilon(1e-4));
 CHECK(s.scale == doctest::Approx(factor*r.scale).epsilon(1e-4));
 CHECK(s.logLik == doctest::Approx(r.logLik-g.data.size()*std::log(factor)).epsilon(1e-8));
}
TEST_CASE("burr limits and invalid inputs") {
 std::vector<double> w,p;
 for (int i=1;i<=200;++i) {
   double u=(i-0.5)/200.;
   w.push_back(std::pow(-std::log(1-u),0.5));
   p.push_back(std::pow(1-u,-0.5));
 }

 auto a=cppunivariate::burr_mle(mltest::as_span(w),1e-8,1000);
 auto b=cppunivariate::burr_mle(mltest::as_span(p),1e-8,1000);
 CHECK(a.status == cppunivariate::BurrStatus::WeibullBoundary);
 CHECK(b.status == cppunivariate::BurrStatus::ParetoBoundary);
 const std::vector<double> bad = {0,1};
 CHECK(cppunivariate::burr_mle(mltest::as_span(bad),1e-8,1000).status == cppunivariate::BurrStatus::InvalidInit);
}

TEST_CASE("burr n20 actuar multistart oracle fixtures") {
 for (const auto &fixture : burr_oracle_fixtures) {
   std::vector<double> x = fixture.x;
   double jacobian=0;

   auto fit=cppunivariate::burr_mle(mltest::as_span(x),1e-8,1000);
   CHECK(fit.logLik == doctest::Approx(fixture.logLik+jacobian).epsilon(1e-8));
   if (fixture.face=="interior") CHECK(fit.status==cppunivariate::BurrStatus::Ok);
   if (fixture.face=="weibull") CHECK(fit.status==cppunivariate::BurrStatus::WeibullBoundary);
   if (fixture.face=="pareto") CHECK(fit.status==cppunivariate::BurrStatus::ParetoBoundary);
 }
}

TEST_CASE("burr fixed scale historical regression and equivariance") {
 const auto golden = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/burr/fixed-scale-golden.json");
 const auto &x = golden.data;
 auto r = cppunivariate::burr_mle_fixed_scale(mltest::as_span(x),1.0);
 REQUIRE(r.status == cppunivariate::BurrStatus::Ok);
 CHECK(std::abs(r.shape1 - 1.077036666067376) < 1e-10);
 CHECK(std::abs(r.shape2 - 1.9857562895421235) < 1e-10);
 CHECK(std::abs(r.logLik - -614.507368787803) < 1e-10);
 auto y=x; for (double &v : y) v*=3.0;
 auto s = cppunivariate::burr_mle_fixed_scale(mltest::as_span(y),3.0);
 CHECK(s.scale == 3.0);
 CHECK(s.shape1 == doctest::Approx(r.shape1).epsilon(1e-10));
 CHECK(s.shape2 == doctest::Approx(r.shape2).epsilon(1e-10));
 CHECK(s.logLik == doctest::Approx(r.logLik-x.size()*std::log(3.0)).epsilon(1e-10));
 const std::vector<double> escape = { 3,4,5 };
 CHECK(cppunivariate::burr_mle_fixed_scale(mltest::as_span(escape),3).status == cppunivariate::BurrStatus::AllGeOne);
 for (double bad : {0.0,-1.0,std::numeric_limits<double>::infinity()})
   CHECK(cppunivariate::burr_mle_fixed_scale(mltest::as_span(x),bad).status == cppunivariate::BurrStatus::InvalidInit);
}

TEST_CASE("burr fixed scale profile fallback start") {
 std::vector<double> x = {.5,2,3,4,5};

 auto r = cppunivariate::burr_mle_fixed_scale(mltest::as_span(x),1.0,1e-10);
 REQUIRE(r.status == cppunivariate::BurrStatus::Ok);
 CHECK(std::isfinite(r.logLik));
 double theta[2] = {r.shape1,r.shape2}, score[2];
 cppunivariate::BurrCost cost{mltest::as_span(x)};
 cost.gradient(theta,score);
 CHECK(std::hypot(score[0],score[1]) < 1e-8);
}

TEST_CASE("burr: held-scale endpoint spike and nonconstant escape differ") {
  const std::vector<double> spike={1,1,1,1}, mixed={1,1.5,2,3};
  const auto a=cppunivariate::burr_mle_fixed_scale(mltest::as_span(spike),1);
  CHECK(a.status==cppunivariate::BurrStatus::Unbounded);
  CHECK(a.logLik==INFINITY);
  CHECK(cppunivariate::burr_mle_fixed_scale(mltest::as_span(mixed),1).status==cppunivariate::BurrStatus::AllGeOne);
}
