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

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("invexp: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("invexp",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dinvexp(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pinvexp(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qinvexp(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rinvexp(rng, p[0]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// F=lambda x:m.exp(-1/x); Q=lambda l:-1/l
TEST_CASE("invexp: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pinvexp(1e300,1,false,true) == doctest::Approx(-std::log(1e300)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qinvexp(-10000,1,true,true) == doctest::Approx(0.0001).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::dinvexp(INFINITY,1,true) == -INFINITY);
}
