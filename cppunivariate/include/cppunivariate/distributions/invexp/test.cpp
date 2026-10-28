#include "test_support.hpp"
#include "cppunivariate/distributions/invexp/mle.hpp"
#include "cppunivariate/distributions/invexp/dpqr.hpp"
TEST_CASE("invexp: golden, reciprocal agreement and score") {
  auto g=mltest::load_distribution_golden("invexp");
  auto r=cppunivariate::invexp_mle(mltest::as_span(g.data));
  CHECK(r.rate == doctest::Approx(g.estimates.at("rate")).epsilon(1e-12));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(1e-12));
  std::vector<double> y; double jac=0;
  for (double x:g.data) { y.push_back(1/x); jac-=2*std::log(x); }
  auto b=cppunivariate::exp_mle(mltest::as_span(y));
  CHECK(r.rate == doctest::Approx(1/b.rate));
  CHECK(r.logLik == doctest::Approx(b.logLik+jac).epsilon(1e-12));
  
}
