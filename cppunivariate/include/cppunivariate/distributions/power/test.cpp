#include <algorithm>
#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/power/mle.hpp"
#include "cppunivariate/distributions/power/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("rpower samples lie in (0, alpha); mean = alpha*beta/(beta+1)") {
  cppunivariate::distributions::StdRng<> rng(109);
  const int n = 20000;
  const double alpha = 3.0, beta = 2.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rpower(rng, alpha, beta);
    CHECK(x > 0.0);
    CHECK(x < alpha);
    sum += x;
  }
  const double expected = alpha * beta / (beta + 1.0);
  CHECK(std::abs(sum / n - expected) < 0.05);
}

TEST_CASE("power") {
  auto g = mltest::load_distribution_golden("power");
  auto r = cppunivariate::power_mle(mltest::as_span(g.data), 1.490116e-08);
  CHECK(r.alpha == doctest::Approx(g.estimates.at("alpha")).epsilon(mltest::kTol));
  CHECK(r.beta == doctest::Approx(g.estimates.at("beta")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("power: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("power",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dpower(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ppower(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qpower(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rpower(rng, p[0], p[1]); });
}

TEST_CASE("power P2 every held subset") {
  const auto g=mltest::load_distribution_golden("power");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("alpha"),g.estimates.at("beta")};
  { const auto r=cppunivariate::power_mle(mltest::as_span(x),1.490116e-08,held[0],nan);
    CHECK(r.alpha==held[0]);
    CHECK(std::isfinite(r.beta));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dpower(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.alpha,r.beta};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::power_mle(mltest::as_span(x),1.490116e-08,held[0],held[1]);
    CHECK(r.alpha==held[0]);
    CHECK(r.beta==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dpower(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.alpha,r.beta};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("power P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK_FALSE(std::isfinite(cppunivariate::power_mle(mltest::as_span(x),1e-8,2,nan).logLik));
  CHECK(std::isfinite(cppunivariate::power_mle(mltest::as_span(x),1e-8,3,nan).logLik));
  CHECK_FALSE(std::isfinite(cppunivariate::power_mle(mltest::as_span(x),1e-8,nan,2).logLik));
}
