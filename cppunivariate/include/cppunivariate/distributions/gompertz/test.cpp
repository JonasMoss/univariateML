#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/gompertz/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

TEST_CASE("gompertz") {
  auto g = mltest::load_distribution_golden("gompertz");
  auto r = cppunivariate::gompertz_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.a == C_APPROX(g.estimates.at("a")));
  CHECK(r.b == C_APPROX(g.estimates.at("b")));
  CHECK(r.logLik == C_APPROX(g.logLik));
}

#undef C_APPROX
#undef C_APPROX_FD

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("gompertz: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("gompertz",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dgompertz(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pgompertz(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qgompertz(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rgompertz(rng, p[0], p[1]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// logS=lambda x:-m.expm1(x); Q=lambda l:m.log1p(-l)
TEST_CASE("gompertz: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pgompertz(1e-300,1,1,true,true) == doctest::Approx(std::log(1e-300)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qgompertz(-10000,1,1,false,true) == doctest::Approx(std::log(10001.0)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::dgompertz(0,1,1,true) == doctest::Approx(0).epsilon(1e-12).scale(1e-300));
}


TEST_CASE("gompertz P2 every held subset") {
  const auto g=mltest::load_distribution_golden("gompertz");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("a"),g.estimates.at("b")};
  { const auto r=cppunivariate::gompertz_mle(mltest::as_span(x),1e-10,200,held[0],nan);
    CHECK(r.a==held[0]);
    CHECK(std::isfinite(r.b));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dgompertz(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.a,r.b};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::gompertz_mle(mltest::as_span(x),1e-10,200,nan,held[1]);
    CHECK(std::isfinite(r.a));
    CHECK(r.b==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dgompertz(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.a,r.b};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::gompertz_mle(mltest::as_span(x),1e-10,200,held[0],held[1]);
    CHECK(r.a==held[0]);
    CHECK(r.b==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dgompertz(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.a,r.b};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("gompertz P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::gompertz_mle(mltest::as_span(x),1e-10,100,1,nan).status==cppunivariate::GompertzStatus::Overdispersed);
  CHECK(std::isfinite(cppunivariate::gompertz_mle(mltest::as_span(x),1e-10,100,nan,1).logLik));
  CHECK(cppunivariate::gompertz_mle(mltest::as_span(x),1e-10,1,.1,nan).status==cppunivariate::GompertzStatus::IterationFailure);
  const std::vector<double> zero{0,0};
  CHECK(cppunivariate::gompertz_mle(mltest::as_span(zero),1e-10,100,2,nan).b==1);
  CHECK_FALSE(std::isfinite(cppunivariate::gompertz_mle(mltest::as_span(zero),1e-10,100,nan,1).logLik));
}
