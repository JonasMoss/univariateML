#include <algorithm>
#include <vector>
#include <limits>
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

TEST_CASE("invexp P2 every held subset") {
  const auto g=mltest::load_distribution_golden("invexp");
  std::vector<double> x=g.data;
  const std::vector<double> held{g.estimates.at("rate")};
  { const auto r=cppunivariate::invexp_mle(mltest::as_span(x),held[0]);
    CHECK(r.rate==held[0]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvexp(v,p[0],true);return s;};
    std::vector<double> p{r.rate};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}
