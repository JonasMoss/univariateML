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
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kGolden_weibull).scale(0));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_weibull).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_weibull).scale(0));
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
TEST_CASE("weibull: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("weibull",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dweibull(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pweibull(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qweibull(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rweibull(rng, p[0], p[1]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// S=lambda x:m.exp(-x*x); Q=lambda l:m.sqrt(-l)
TEST_CASE("weibull: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pweibull(100,2,1,false,true) == doctest::Approx(-10000).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qweibull(-10000,2,1,false,true) == doctest::Approx(100).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::pweibull(1e-300,2,1,true,true) == doctest::Approx(2*std::log(1e-300)).epsilon(1e-12).scale(1e-300));
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

TEST_CASE("weibull: TASK-132 invalid data never report success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2.0},std::vector<double>{INFINITY,2.0},std::vector<double>{-INFINITY,2.0},std::vector<double>{-1.0,2.0},std::vector<double>{0.0,2.0}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::weibull_mle(x,1e-8,200).status != cppunivariate::WeibullStatus::Ok);
  }
}

TEST_CASE("weibull: TASK-132 preserves invalid Gumbel controls") {
  const std::vector<double> data{1,2,3};
  CHECK(cppunivariate::weibull_mle(cppunivariate::span{data.data(),data.size()},1e-8,0).status == cppunivariate::WeibullStatus::InvalidInput);
}

TEST_CASE("weibull: relative logarithms retain concentrated-sample shape accuracy") {
  const std::vector<double> x{2.890529999994219,2.8905299999971095,2.89053,2.8905300000028906,2.8905300000086716};
  // Independent score root is checked on the actual binary64 inputs by the
  // retained audit replay. Here changing units must preserve the shape.
  auto y=x;for(double& v:y)v*=.5;
  const auto a=cppunivariate::weibull_mle({x.data(),x.size()},1e-10,200);
  const auto b=cppunivariate::weibull_mle({y.data(),y.size()},1e-10,200);
  REQUIRE(a.status==cppunivariate::WeibullStatus::Ok);
  REQUIRE(b.status==cppunivariate::WeibullStatus::Ok);
  CHECK(a.shape==doctest::Approx(b.shape).epsilon(1e-11).scale(0));
}

TEST_CASE("weibull: unrepresentable subnormal coefficient is not Ok") {
  const double u=std::numeric_limits<double>::denorm_min();
  const std::vector<double> x{u,2*u};
  const auto r=cppunivariate::weibull_mle({x.data(),x.size()},1e-10,200);
  CHECK(r.status==cppunivariate::WeibullStatus::AccuracyFailure);
}

TEST_CASE("weibull: held scale retains concentrated-sample shape accuracy") {
  const std::vector<double> x{2.4349403590372,2.434940359039635,2.43494035904207,2.434940359044505,2.434940359049375};
  const auto r=cppunivariate::weibull_mle({x.data(),x.size()},1e-10,100,NAN,NAN,x[0]);
  REQUIRE(r.status==cppunivariate::WeibullStatus::Ok);
  // Independent 100-digit conditional-score root on the binary64 sample.
  CHECK(r.shape==doctest::Approx(263511819158.377827834).epsilon(1e-10));
}

TEST_CASE("weibull: held shape does not hide an unrepresentable scale") {
  const double u=std::numeric_limits<double>::denorm_min();
  const std::vector<double> x{u,2*u};
  CHECK(cppunivariate::weibull_mle({x.data(),x.size()},1e-10,100,NAN,1,NAN).status==cppunivariate::WeibullStatus::AccuracyFailure);
}

// Independent 100-digit mpmath values; priority.py records exact double inputs.
TEST_CASE("weibull: exact-input large-shape displacement") {
  CHECK(cppunivariate::distributions::dweibull(999.9999999999999,1e12,1000.,true) == doctest::Approx(19.72326583048431).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pweibull(999.9999999999999,1e12,1000.,false,true) == doctest::Approx(-0.999886319624382).epsilon(1e-10));
  CHECK(cppunivariate::distributions::dweibull(1000.0000000000001,1e12,1000.,true) == doctest::Approx(19.723265830483818).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pweibull(1000.0000000000001,1e12,1000.,false,true) == doctest::Approx(-1.000113693300315).epsilon(1e-10));
  CHECK(cppunivariate::distributions::dweibull(1000.0000000005,1e12,1000.,true) == doctest::Approx(19.574547996466492).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pweibull(1000.0000000005,1e12,1000.,false,true) == doctest::Approx(-1.6487125527789595).epsilon(1e-10));
}

// Independent 160-digit values, with probabilities rounded before inversion.
TEST_CASE("weibull: small-shape inverse at unit hazard") {
  CHECK(cppunivariate::distributions::qweibull(0.6321205588285577,1e-08,1.0,true,false) == doctest::Approx(0.9999999966215145).epsilon(1e-10).scale(0));
  CHECK(cppunivariate::distributions::qweibull(0.36787944117144233,1e-08,1.0,false,false) == doctest::Approx(0.9999999966215145).epsilon(1e-10).scale(0));
  CHECK(cppunivariate::distributions::qweibull(-0.4586751453870819,1e-08,1.0,true,true) == doctest::Approx(1.0000000031167178).epsilon(1e-10).scale(0));
  CHECK(cppunivariate::distributions::qweibull(0.6321205588285577,1e-12,1.0,true,false) == doctest::Approx(0.9999662157154426).epsilon(1e-10).scale(0));
  CHECK(cppunivariate::distributions::qweibull(0.36787944117144233,1e-12,1.0,false,false) == doctest::Approx(0.9999662157154426).epsilon(1e-10).scale(0));
  CHECK(cppunivariate::distributions::qweibull(-0.4586751453870819,1e-12,1.0,true,true) == doctest::Approx(1.000031167662517).epsilon(1e-10).scale(0));
  CHECK(cppunivariate::distributions::qweibull(0.6321205588285577,1e-16,1.0,true,false) == doctest::Approx(0.7133033076559511).epsilon(1e-10).scale(0));
  CHECK(cppunivariate::distributions::qweibull(0.36787944117144233,1e-16,1.0,false,false) == doctest::Approx(0.7133033076559511).epsilon(1e-10).scale(0));
  CHECK(cppunivariate::distributions::qweibull(-0.4586751453870819,1e-16,1.0,true,true) == doctest::Approx(1.3657063511383019).epsilon(1e-10).scale(0));
}
