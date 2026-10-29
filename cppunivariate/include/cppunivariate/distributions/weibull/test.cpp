#include <algorithm>
#include "test_support.hpp"
#include "cppunivariate/distributions/weibull/mle.hpp"
#include "cppunivariate/distributions/weibull/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>

TEST_CASE("weibull") {
  auto g = mltest::load_distribution_golden("weibull");
  auto r = cppunivariate::weibull_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kTol));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("weibull: explicit shape start and automatic default") {
  auto g = mltest::load_distribution_golden("weibull");
  auto a = cppunivariate::weibull_mle(mltest::as_span(g.data), 1e-12, 200);
  auto b = cppunivariate::weibull_mle(mltest::as_span(g.data), 1e-12, 200, 1.2 * a.shape);
  CHECK(b.shape == doctest::Approx(a.shape).epsilon(1e-9));
  CHECK(b.logLik == doctest::Approx(a.logLik).epsilon(1e-10));
}

TEST_CASE("weibull dpqr: known CDF value and quantile inverse") {
  // F(scale; shape, scale) = 1 - exp(-1) for any shape.
  CHECK(cppunivariate::distributions::pweibull(1.0, 2.0, 1.0, true, false) ==
        doctest::Approx(1.0 - std::exp(-1.0)));
  const double q = cppunivariate::distributions::qweibull(0.6, 1.5, 2.0, true, false);
  CHECK(cppunivariate::distributions::pweibull(q, 1.5, 2.0, true, false) == doctest::Approx(0.6));
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("weibull: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("weibull",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dweibull(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pweibull(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qweibull(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rweibull(rng, p[0], p[1]); });
}

TEST_CASE("weibull P2 every held subset") {
  const auto g=mltest::load_distribution_golden("weibull");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("shape"),g.estimates.at("scale")};
  { const auto r=cppunivariate::weibull_mle(mltest::as_span(x),1e-10,200,nan,held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.scale));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dweibull(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::weibull_mle(mltest::as_span(x),1e-10,200,nan,nan,held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dweibull(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::weibull_mle(mltest::as_span(x),1e-10,200,nan,held[0],held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dweibull(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("weibull P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK_FALSE(std::isfinite(cppunivariate::weibull_mle(mltest::as_span(x),1e-10,100,nan,nan,2).logLik));
  CHECK(std::isfinite(cppunivariate::weibull_mle(mltest::as_span(x),1e-10,100,nan,2,nan).logLik));
  CHECK_FALSE(std::isfinite(cppunivariate::weibull_mle(mltest::as_span(x),1e-10,1,nan,nan,3).logLik));
}

TEST_CASE("weibull held tiny shape preserves the geometric-mean limit") {
  const std::vector<double> x{1,4,16};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto r=cppunivariate::weibull_mle(mltest::as_span(x),1e-10,100,nan,1e-200,nan);
  CHECK(r.shape==1e-200);
  CHECK(r.scale==doctest::Approx(4));
}

TEST_CASE("weibull: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("weibull");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::weibull_mle(x,1e-8,1).status==cppunivariate::WeibullStatus::IterationFailure);
}
