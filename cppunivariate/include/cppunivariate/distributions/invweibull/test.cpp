#include <algorithm>
#include "test_support.hpp"
#include "cppunivariate/distributions/invweibull/mle.hpp"
#include "cppunivariate/distributions/invweibull/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>

TEST_CASE("rinvweibull samples are positive; CDF round-trip at p=0.5") {
  cppunivariate::distributions::StdRng<> rng(157);
  const int n = 5000;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rinvweibull(rng, 2.0, 3.0);
    CHECK(x > 0.0);
  }
  const double q =
      cppunivariate::distributions::qinvweibull(0.5, 2.0, 3.0, true, false);
  const double back =
      cppunivariate::distributions::pinvweibull(q, 2.0, 3.0, true, false);
  CHECK(std::abs(back - 0.5) < 1e-10);
}

TEST_CASE("invweibull") {
  auto g = mltest::load_distribution_golden("invweibull");
  auto r =
      cppunivariate::invweibull_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kTol));
  CHECK(r.rate == doctest::Approx(g.estimates.at("rate")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("invweibull: explicit shape start and automatic default") {
  auto g = mltest::load_distribution_golden("invweibull");
  auto a = cppunivariate::invweibull_mle(mltest::as_span(g.data), 1e-12, 200);
  auto b = cppunivariate::invweibull_mle(mltest::as_span(g.data), 1e-12, 200, 1.2 * a.shape);
  CHECK(b.shape == doctest::Approx(a.shape).epsilon(1e-9));
  CHECK(b.logLik == doctest::Approx(a.logLik).epsilon(1e-10));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("invweibull: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("invweibull",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dinvweibull(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pinvweibull(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qinvweibull(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rinvweibull(rng, p[0], p[1]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// F=lambda x:m.exp(-1/x**2); Q=lambda l:1/m.sqrt(-l)
TEST_CASE("invweibull: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pinvweibull(1e300,2,1,false,true) == doctest::Approx(-2*std::log(1e300)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qinvweibull(-10000,2,1,true,true) == doctest::Approx(0.01).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::dinvweibull(INFINITY,2,1,true) == -INFINITY);
}


TEST_CASE("invweibull P2 every held subset") {
  const auto g=mltest::load_distribution_golden("invweibull");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("shape"),g.estimates.at("rate")};
  { const auto r=cppunivariate::invweibull_mle(mltest::as_span(x),1e-10,200,nan,held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.rate));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvweibull(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.rate};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::invweibull_mle(mltest::as_span(x),1e-10,200,nan,nan,held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.rate==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvweibull(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.rate};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::invweibull_mle(mltest::as_span(x),1e-10,200,nan,held[0],held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.rate==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvweibull(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.rate};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("invweibull P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK_FALSE(std::isfinite(cppunivariate::invweibull_mle(mltest::as_span(x),1e-10,100,nan,nan,.5).logLik));
  CHECK(std::isfinite(cppunivariate::invweibull_mle(mltest::as_span(x),1e-10,100,nan,2,nan).logLik));
}

TEST_CASE("invweibull: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("invweibull");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::invweibull_mle(x,1e-8,1).status==cppunivariate::InvWeibullStatus::IterationFailure);
}
