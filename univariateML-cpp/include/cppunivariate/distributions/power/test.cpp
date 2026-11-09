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
  CHECK(r.alpha == doctest::Approx(g.estimates.at("alpha")).epsilon(mltest::kGolden_power).scale(0));
  CHECK(r.beta == doctest::Approx(g.estimates.at("beta")).epsilon(mltest::kGolden_power).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_power).scale(0));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("power: d/p/q/r against the truth table") {
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
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=std::log(p[1])-p[1]*std::log(p[0])+(p[1]-1)*std::log(v);return s;};
    std::vector<double> p{r.alpha,r.beta};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::power_mle(mltest::as_span(x),1.490116e-08,held[0],held[1]);
    CHECK(r.alpha==held[0]);
    CHECK(r.beta==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=std::log(p[1])-p[1]*std::log(p[0])+(p[1]-1)*std::log(v);return s;};
    std::vector<double> p{r.alpha,r.beta};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("power P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK_FALSE(std::isfinite(cppunivariate::power_mle(mltest::as_span(x),1e-8,2,nan).logLik));
  CHECK(std::isfinite(cppunivariate::power_mle(mltest::as_span(x),1e-8,3,nan).logLik));
  CHECK(std::isfinite(cppunivariate::power_mle(mltest::as_span(x),1e-8,nan,2).logLik));
}

TEST_CASE("power closed endpoint and constant-sample profiles") {
  const std::vector<double> x{1, 2, 2};
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const auto r = cppunivariate::power_mle(mltest::as_span(x), 0);
  CHECK(r.alpha == 2);
  CHECK(r.beta == doctest::Approx(3 / std::log(2.0)));
  CHECK(cppunivariate::power_mle(mltest::as_span(x), 1).alpha == 2);
  const auto held = cppunivariate::power_mle(mltest::as_span(x), 0, nan, 2);
  CHECK(held.alpha == 2);
  CHECK(held.beta == 2);
  const std::vector<double> constant{2, 2};
  CHECK(cppunivariate::power_mle(mltest::as_span(constant)).status == cppunivariate::PowerStatus::Unbounded);
  CHECK(cppunivariate::power_mle(mltest::as_span(constant), 0, nan, 2).logLik == 0);
}


// mpmath reproduction: import mpmath as mp; mp.mp.dps=1200
// F=lambda x: x**4; Q=lambda l: mp.exp(l/4)
// Evaluate log tails at the stated decimal x; log densities follow mp.log(mp.diff(F,x))
// (use -mp.diff(S,x) for survival definitions); Q uses the indicated log tail.
TEST_CASE("power: deep tails and log inversion regressions") {
  CHECK(cppunivariate::distributions::ppower(1e-200,1,4,true,true) == doctest::Approx(-800*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qpower(-1000,1,4,true,true) == doctest::Approx(std::exp(-250.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qpower(-INFINITY,3,4,false,true) == 3);
  CHECK(std::isnan(cppunivariate::distributions::qpower(1.1,1,4,true,false)));
  CHECK(cppunivariate::distributions::dpower(0,3,.5,true) == INFINITY);
  CHECK(cppunivariate::distributions::dpower(3,3,2,false) == doctest::Approx(2.0/3).scale(0.0));
  // mp.mp.dps=400; a=mp.mpf(float(1e-320)); h=-mp.log1p(-mp.exp(-750))/a
  // Quantile mp.exp(-h); log upper tail at x=1/e tends to mp.log(a).
  CHECK(cppunivariate::distributions::qpower(-750,1,1e-320,false,true) == doctest::Approx(std::exp(-std::exp(-750-std::log(1e-320)))).scale(0.0));
  CHECK(cppunivariate::distributions::ppower(std::exp(-1.0),1,1e-320,false,true) == doctest::Approx(std::log(1e-320)).scale(0.0));

}
