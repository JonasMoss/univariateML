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
  CHECK(r.a == doctest::Approx(g.estimates.at("a")).epsilon(mltest::kGolden_gompertz).scale(0));
  CHECK(r.b == doctest::Approx(g.estimates.at("b")).epsilon(mltest::kGolden_gompertz).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_gompertz).scale(0));
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

TEST_CASE("gompertz: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("gompertz");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::gompertz_mle(x,1e-8,1).status==cppunivariate::GompertzStatus::IterationFailure);
}

TEST_CASE("gompertz core preserves adapter constant-data rejection") {
  const std::vector<double> x{2,2,2};
  CHECK(cppunivariate::gompertz_mle(mltest::as_span(x),1e-8,100).status==cppunivariate::GompertzStatus::Unbounded);
}

TEST_CASE("gompertz: verified extreme conditional bracket") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  double base=0,rate=0;
  for(double unit:{1.0,1e10}) {
    const std::vector<double> x{1e-300*unit,2e-300*unit};
    const auto r=cppunivariate::gompertz_mle(mltest::as_span(x),1e-10,100,1e-300/unit,nan);
    REQUIRE(r.status==cppunivariate::GompertzStatus::Ok);
    CHECK(r.b*unit>6e302);CHECK(r.logLik+2*std::log(unit)>418);
    if(unit==1){base=r.logLik;rate=r.b;}else {CHECK(r.b*unit==doctest::Approx(rate).epsilon(1e-10));CHECK(r.logLik+2*std::log(unit)==doctest::Approx(base).epsilon(1e-10));}
  }
  const std::vector<double> edge{1e-310,2e-310};
  CHECK(cppunivariate::gompertz_mle(mltest::as_span(edge),1e-10,100,1e-300,nan).status==cppunivariate::GompertzStatus::IterationFailure);
  const std::vector<double> large{1e308,1e308};
  const auto wide=cppunivariate::gompertz_mle(mltest::as_span(large),1e-10,100,1e-308,nan);
  REQUIRE(wide.status==cppunivariate::GompertzStatus::Ok);
  CHECK(wide.b*1e308==doctest::Approx(1).epsilon(1e-9));
  CHECK(wide.logLik+2*std::log(1e308)==doctest::Approx(2-2*std::expm1(1.0)).epsilon(1e-9));
}

TEST_CASE("gompertz: stable sampler at fixed uniforms and ordinary arithmetic") {
  struct FixedUniform {
    double u;
    double runif(double,double) { return u; }
  };
  for (double u:{1e-100,.1,.5,.99}) {
    for (const auto &p:std::vector<std::vector<double>>{{1e-300,7e302},{1e300,1e-300},{1e7,1},{1,1}}) {
      FixedUniform rng{u};
      const double draw=cppunivariate::distributions::rgompertz(rng,p[0],p[1]);
      REQUIRE(std::isfinite(draw));
      CHECK(draw>0);
      CHECK(draw==doctest::Approx(cppunivariate::distributions::qgompertz(std::log(u),p[0],p[1],false,true)).epsilon(1e-12).scale(1e-310));
    }
  }
  cppunivariate::distributions::StdRng<> old_rng(1),new_rng(1);
  for (int i=0;i<200;++i) {
    const double u=old_rng.runif(0,1);
    CHECK(cppunivariate::distributions::rgompertz(new_rng,1,1)==std::log(1-std::log(u)));
  }
}

TEST_CASE("gompertz: structured face payloads") {
  const std::vector<double> x{0,0,1,9};
  for(double a:{static_cast<double>(NAN),.3}) {
    const auto r=cppunivariate::gompertz_mle(mltest::as_span(x),1e-8,100,a,NAN);
    REQUIRE(r.stop_detail.limit_coef.size()==1);
    const double rate=std::isnan(a)?.4:a;
    CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(rate));
    CHECK(r.stop_detail.limit_loglik==doctest::Approx(4*std::log(rate)-10*rate));
  }
}

TEST_CASE("gompertz: coefficients that leave binary64 after rescaling") {
  // TASK-214.3. On (1, 2, 3)*denorm_min the MLE is a = 1.45e322,
  // b = 2.48e323 (references.json): above binary64. It used to return Ok
  // with both coefficients infinite and a finite log likelihood.
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double tiny=std::numeric_limits<double>::denorm_min();
  const std::vector<double> x{tiny,2*tiny,3*tiny};
  const auto r=cppunivariate::gompertz_mle(mltest::as_span(x),mltest::kFitTol,mltest::kFitMaxit,nan,nan);
  CHECK(r.status==cppunivariate::GompertzStatus::NumericalFailure);
  CHECK(std::isnan(r.a));
  CHECK(std::isnan(r.b));
  CHECK(cppunivariate::outcome(r).kind==cppunivariate::OutcomeKind::Failure);
  CHECK(cppunivariate::outcome(r).reason==cppunivariate::OutcomeReason::Evaluation);
  // Representable rescalings stay exact to rounding.
  const std::vector<double> one{1,2,3},small{1e-300,2e-300,3e-300};
  const auto r1=cppunivariate::gompertz_mle(mltest::as_span(one),1e-10,100);
  const auto rs=cppunivariate::gompertz_mle(mltest::as_span(small),1e-10,100);
  REQUIRE(r1.status==cppunivariate::GompertzStatus::Ok);
  REQUIRE(rs.status==cppunivariate::GompertzStatus::Ok);
  CHECK(rs.a*1e-300==doctest::Approx(r1.a).epsilon(1e-13));
  CHECK(rs.b*1e-300==doctest::Approx(r1.b).epsilon(1e-13));
  CHECK(rs.logLik-3*std::log(1e300)==doctest::Approx(r1.logLik).epsilon(1e-13));
}

TEST_CASE("gompertz: empty, non-finite and negative samples are invalid input") {
  // TASK-214.3 (coordinator addition): these ended in IterationFailure
  // (no-convergence), and infinite data in the exponential-face status.
  const double nan=std::numeric_limits<double>::quiet_NaN(),inf=std::numeric_limits<double>::infinity();
  for(const std::vector<double>& x:{std::vector<double>{},{nan,1,2},{inf,1,2},{-inf,1,2},{-1,1,2}}) {
    CAPTURE(x.size());
    const auto r=cppunivariate::gompertz_mle(mltest::as_span(x),mltest::kFitTol,mltest::kFitMaxit,nan,nan);
    CHECK(r.status==cppunivariate::GompertzStatus::InvalidInput);
    CHECK(cppunivariate::outcome(r).kind==cppunivariate::OutcomeKind::Failure);
    CHECK(cppunivariate::outcome(r).reason==cppunivariate::OutcomeReason::InvalidInput);
    CHECK(cppunivariate::gompertz_mle(mltest::as_span(x),1e-10,100,1.0,nan).status==cppunivariate::GompertzStatus::InvalidInput);
    CHECK(cppunivariate::gompertz_mle(mltest::as_span(x),1e-10,100,nan,1.0).status==cppunivariate::GompertzStatus::InvalidInput);
  }
  // All-zero data stay a scale collapse with both coefficients free.
  const std::vector<double> zeros{0,0,0};
  CHECK(cppunivariate::gompertz_mle(mltest::as_span(zeros),mltest::kFitTol,mltest::kFitMaxit,nan,nan).status==
        cppunivariate::GompertzStatus::Unbounded);
}
