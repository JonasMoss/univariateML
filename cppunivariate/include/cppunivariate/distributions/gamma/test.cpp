#include <algorithm>
#include "test_support.hpp"
#include "cppunivariate/distributions/gamma/mle.hpp"
#include "cppunivariate/distributions/gamma/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>

TEST_CASE("gamma") {
  auto g = mltest::load_distribution_golden("gamma");
  auto r = cppunivariate::gamma_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kTol));
  CHECK(r.rate == doctest::Approx(g.estimates.at("rate")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("gamma dpqr: rate parameterisation round-trip") {
  // mean = shape / rate.
  const double q = cppunivariate::distributions::qgamma(0.5, 2.0, 1.0, true, false);
  CHECK(cppunivariate::distributions::pgamma(q, 2.0, 1.0, true, false) == doctest::Approx(0.5));
  // density at mean is positive and finite.
  CHECK(cppunivariate::distributions::dgamma(2.0, 2.0, 1.0, false) > 0.0);
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("gamma: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("gamma",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dgamma(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pgamma(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qgamma(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rgamma(rng, p[0], p[1]); });
}

TEST_CASE("gamma P2 every held subset") {
  const auto g=mltest::load_distribution_golden("gamma");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("shape"),g.estimates.at("rate")};
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.rate));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dgamma(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.rate};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,nan,held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.rate==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dgamma(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.rate};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,held[0],held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.rate==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dgamma(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.rate};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("gamma P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::gamma_mle(mltest::as_span(x),1e-10,1,nan,2).status==cppunivariate::GammaStatus::IterationFailure);
  CHECK(cppunivariate::gamma_mle(mltest::as_span(x),1e-10,100,2,nan).rate==doctest::Approx(1));
}

TEST_CASE("gamma held shape profiles rate without intermediate overflow") {
  const std::vector<double> x{1e308,1e308};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,100,1e308,nan);
  CHECK(r.shape==1e308);
  CHECK(r.rate==doctest::Approx(1));
  // The likelihood itself is outside the direct lgamma formula's range;
  // a numerical failure is returned rather than a spurious finite fit.
  CHECK(r.status==cppunivariate::GammaStatus::IterationFailure);
}

TEST_CASE("gamma: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("gamma");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::gamma_mle(x,1e-8,1).status==cppunivariate::GammaStatus::IterationFailure);
}
