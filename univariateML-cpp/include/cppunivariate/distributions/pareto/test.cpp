#include <algorithm>
#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/pareto/mle.hpp"
#include "cppunivariate/distributions/pareto/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("rpareto samples are >= scale b; mean = a*b/(a-1) for a>1") {
  cppunivariate::distributions::StdRng<> rng(113);
  const int n = 20000;
  const double a = 3.0, b = 2.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rpareto(rng, a, b);
    CHECK(x >= b);
    sum += x;
  }
  const double expected = a * b / (a - 1.0); // = 3.0
  CHECK(std::abs(sum / n - expected) < 0.1);
}

TEST_CASE("pareto") {
  auto g = mltest::load_distribution_golden("pareto");
  auto r = cppunivariate::pareto_mle(mltest::as_span(g.data));
  // golden "a" → ParetoResult.shape, "b" → .min
  CHECK(r.shape == doctest::Approx(g.estimates.at("a")).epsilon(mltest::kTol));
  CHECK(r.min == doctest::Approx(g.estimates.at("b")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("pareto: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("pareto",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dpareto(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ppareto(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qpareto(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rpareto(rng, p[0], p[1]); });
}

TEST_CASE("pareto P2 every held subset") {
  const auto g=mltest::load_distribution_golden("pareto");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("a"),g.estimates.at("b")};
  { const auto r=cppunivariate::pareto_mle(mltest::as_span(x),held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.min));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dpareto(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.min};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
  { const auto r=cppunivariate::pareto_mle(mltest::as_span(x),nan,held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.min==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dpareto(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.min};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::pareto_mle(mltest::as_span(x),held[0],held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.min==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dpareto(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.min};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("pareto P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK_FALSE(std::isfinite(cppunivariate::pareto_mle(mltest::as_span(x),nan,2).logLik));
  CHECK_FALSE(std::isfinite(cppunivariate::pareto_mle(mltest::as_span(x),nan,3).logLik));
  CHECK(std::isfinite(cppunivariate::pareto_mle(mltest::as_span(x),2,nan).logLik));
}


// mpmath reproduction: import mpmath as mp; mp.mp.dps=1200
// S=lambda x: x**-4; Q=lambda l: mp.exp(-l/4)
// Evaluate log tails at the stated decimal x; log densities follow mp.log(mp.diff(F,x))
// (use -mp.diff(S,x) for survival definitions); Q uses the indicated log tail.
TEST_CASE("pareto: deep tails and log inversion regressions") {
  CHECK(cppunivariate::distributions::ppareto(1e200,4,1,false,true) == doctest::Approx(-800*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::dpareto(1e200,4,1,true) == doctest::Approx(std::log(4.0)-1000*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qpareto(-1000,4,1,false,true) == doctest::Approx(std::exp(250.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qpareto(-INFINITY,4,3,true,true) == 3);
  // mp.mp.dps=400; a=mp.mpf(float(1e-320)); h=-mp.log1p(-mp.exp(-750))/a
  // Quantile mp.exp(h); log lower tail at x=e tends to mp.log(a).
  CHECK(cppunivariate::distributions::qpareto(-750,1e-320,1,true,true) == doctest::Approx(std::exp(std::exp(-750-std::log(1e-320)))).scale(0.0));
  CHECK(cppunivariate::distributions::ppareto(std::exp(1.0),1e-320,1,true,true) == doctest::Approx(std::log(1e-320)).scale(0.0));

}

TEST_CASE("pareto: constant positive data have unbounded free shape") {
  const std::vector<double> x(4, 2.0);
  const auto r = cppunivariate::pareto_mle(mltest::as_span(x));
  CHECK(r.status == cppunivariate::ParetoStatus::Unbounded);
  CHECK(r.shape == std::numeric_limits<double>::infinity());
  CHECK(r.min == 2.0);
  CHECK(r.logLik == std::numeric_limits<double>::infinity());
}

TEST_CASE("pareto: TASK-132 invalid data never report success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2.0},std::vector<double>{INFINITY,2.0},std::vector<double>{-INFINITY,2.0},std::vector<double>{-1.0,2.0},std::vector<double>{0.0,2.0}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::pareto_mle(x).status != cppunivariate::ParetoStatus::Ok);
  }
}

TEST_CASE("pareto: TASK-132 unresolved likelihood has numerical failure status") {
  const std::vector<double> data{100.0,200.0};
  const cppunivariate::span x{data.data(),data.size()};
  CHECK(cppunivariate::pareto_mle(x,1e308,1.0).status == cppunivariate::ParetoStatus::IterationFailure);
}
