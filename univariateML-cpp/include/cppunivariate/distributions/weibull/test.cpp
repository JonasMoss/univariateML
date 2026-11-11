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

#include "cppunivariate/distributions/weibull/censored.hpp"
#include "censored_reference.hpp"
TEST_CASE("weibull: right censoring independent 50-digit reference") {
  for(const auto& c:censored_reference::cases()) {
    if(std::string(c.family)!="weibull")continue;
    CAPTURE(c.label); CAPTURE(c.held);
    const auto& v=c.lower;
    std::vector<double> upper=v;
    for(std::size_t i=0;i<v.size();++i)if(!c.exact[i])upper[i]=INFINITY;
    const cppunivariate::CensoredData data{mltest::as_span(v),mltest::as_span(upper)};
    const auto r=cppunivariate::weibull_censored_mle(data,c.held);
    REQUIRE(r.status==cppunivariate::WeibullCensoredStatus::Ok);
    CHECK(r.scale/c.coefficient==doctest::Approx(1).epsilon(1e-10));
    CHECK(r.logLik==doctest::Approx(c.logLik).epsilon(1e-12));
    if(std::string(c.label)=="0pct" || std::string(c.label)=="single") {
      const auto exact=cppunivariate::weibull_mle(mltest::as_span(v),1e-10,1000,NAN,c.held,NAN);
      CHECK(r.scale/exact.scale==doctest::Approx(1).epsilon(4*std::numeric_limits<double>::epsilon()));
      CHECK(r.logLik==doctest::Approx(exact.logLik).epsilon(2e-13));
    }
  }
}
TEST_CASE("weibull: right censoring classifications and refusals") {
  std::vector<double> lower{1,2},upper{INFINITY,INFINITY};
  cppunivariate::CensoredData data{mltest::as_span(lower),mltest::as_span(upper)};
  const auto c=censored_reference::Case{"","",1.,0,0,{},{}};
  CHECK(cppunivariate::weibull_censored_mle(data,c.held).status==cppunivariate::WeibullCensoredStatus::AllCensored);
  upper[0]=1;
  CHECK(cppunivariate::weibull_censored_mle(data,c.held).status==cppunivariate::WeibullCensoredStatus::Ok);
  lower[0]=-INFINITY;
  CHECK(cppunivariate::weibull_censored_mle(data,c.held).status==cppunivariate::WeibullCensoredStatus::NotImplemented);
  lower[0]=1;upper[0]=1.5;
  CHECK(cppunivariate::weibull_censored_mle(data,c.held).status==cppunivariate::WeibullCensoredStatus::NotImplemented);
  upper[0]=0;
  CHECK(cppunivariate::weibull_censored_mle(data,c.held).status==cppunivariate::WeibullCensoredStatus::InvalidInput);
}
TEST_CASE("weibull: censored hold sets") {
  std::vector<double> v{1,2};
  const cppunivariate::CensoredData x{mltest::as_span(v),mltest::as_span(v)};
  CHECK(cppunivariate::weibull_censored_mle(x,-1).status==cppunivariate::WeibullCensoredStatus::InvalidInput);
  CHECK(cppunivariate::weibull_censored_mle(x,NAN,-1).status==cppunivariate::WeibullCensoredStatus::InvalidInput);
  // All held: evaluation equals the exact-data log likelihood.
  const auto both=cppunivariate::weibull_censored_mle(x,2,1.5);
  REQUIRE(both.status==cppunivariate::WeibullCensoredStatus::Ok);
  const double expected=cppunivariate::distributions::dweibull(1,2,1.5,true)+cppunivariate::distributions::dweibull(2,2,1.5,true);
  CHECK(both.logLik==doctest::Approx(expected).epsilon(1e-14));
}
TEST_CASE("weibull: censored tiny held shape retains logarithmic spread") {
  std::vector<double> v{1,4};
  const cppunivariate::CensoredData x{mltest::as_span(v),mltest::as_span(v)};
  const auto r=cppunivariate::weibull_censored_mle(x,1e-20);
  REQUIRE(r.status==cppunivariate::WeibullCensoredStatus::Ok);
  CHECK(r.scale==doctest::Approx(2).epsilon(1e-14));
}
TEST_CASE("weibull: right censoring spans the binary64 exponent range") {
  std::vector<double> l{1e-200,1e200},u{1e-200,INFINITY};
  const cppunivariate::CensoredData x{mltest::as_span(l),mltest::as_span(u)};
  const auto r=cppunivariate::weibull_censored_mle(x,.5);
  REQUIRE(r.status==cppunivariate::WeibullCensoredStatus::Ok);
  CHECK(r.scale/1e200==doctest::Approx(1).epsilon(1e-14));
  CHECK(std::isfinite(r.logLik));
}
TEST_CASE("weibull: tiny held shape with censoring at zero") {
  std::vector<double> l{1,4,0},u{1,4,INFINITY};
  const cppunivariate::CensoredData x{mltest::as_span(l),mltest::as_span(u)};
  const auto r=cppunivariate::weibull_censored_mle(x,1e-20);
  REQUIRE(r.status==cppunivariate::WeibullCensoredStatus::Ok);
  CHECK(r.scale==doctest::Approx(2).epsilon(1e-14));
}
TEST_CASE("weibull: tiny normalized power and representable rescaled root") {
  std::vector<double> l{1e-200,1e200},u{1e-200,INFINITY};
  const cppunivariate::CensoredData x{mltest::as_span(l),mltest::as_span(u)};
  const auto r=cppunivariate::weibull_censored_mle(x,.02);
  REQUIRE(r.status==cppunivariate::WeibullCensoredStatus::Ok);
  CHECK(r.scale/1e200==doctest::Approx(std::pow(1+1e-8,50)).epsilon(1e-12));
  l={1e-300,1e-300,1e300};u=l;
  const cppunivariate::CensoredData y{mltest::as_span(l),mltest::as_span(u)};
  const auto s=cppunivariate::weibull_censored_mle(y,1e-20);
  REQUIRE(s.status==cppunivariate::WeibullCensoredStatus::Ok);
  CHECK(s.scale/1e-100==doctest::Approx(1).epsilon(1e-12));
}

#include "censored_reference_weibull_free.hpp"
TEST_CASE("weibull: right censoring with free shape, independent 30-digit reference") {
  for(const auto& c:censored_reference_weibull_free::cases()) {
    CAPTURE(c.kind); CAPTURE(c.label); CAPTURE(c.scale);
    std::vector<double> upper=c.lower;
    for(std::size_t i=0;i<upper.size();++i)if(!c.exact[i])upper[i]=INFINITY;
    const cppunivariate::CensoredData data{mltest::as_span(c.lower),mltest::as_span(upper)};
    const auto r=cppunivariate::weibull_censored_mle(data,NAN,c.held);
    REQUIRE(r.status==cppunivariate::WeibullCensoredStatus::Ok);
    CHECK(r.shape/c.shape==doctest::Approx(1).epsilon(1e-10));
    CHECK(r.scale/c.scale==doctest::Approx(1).epsilon(1e-10));
    CHECK(r.logLik==doctest::Approx(c.logLik).epsilon(1e-12));
  }
}
TEST_CASE("weibull: right censoring, free shape agrees with the exact-data fitter") {
  auto g = mltest::load_distribution_golden("weibull");
  const auto x=mltest::as_span(g.data);
  const cppunivariate::CensoredData data{x,x};
  const auto e=cppunivariate::weibull_mle(x,1e-10,1000);
  const auto r=cppunivariate::weibull_censored_mle(data);
  REQUIRE(r.status==cppunivariate::WeibullCensoredStatus::Ok);
  CHECK(r.shape/e.shape==doctest::Approx(1).epsilon(1e-10));
  CHECK(r.scale/e.scale==doctest::Approx(1).epsilon(1e-10));
  CHECK(r.logLik==doctest::Approx(e.logLik).epsilon(1e-12));
  const auto s=cppunivariate::weibull_censored_mle(data,NAN,e.scale);
  const auto es=cppunivariate::weibull_mle(x,1e-10,1000,NAN,NAN,e.scale);
  REQUIRE(s.status==cppunivariate::WeibullCensoredStatus::Ok);
  CHECK(s.shape/es.shape==doctest::Approx(1).epsilon(1e-10));
}
TEST_CASE("weibull: right censoring existence boundaries for a free shape") {
  using S=cppunivariate::WeibullCensoredStatus;
  const auto fit=[](std::vector<double> l,std::vector<double> u,double scale=NAN) {
    return cppunivariate::weibull_censored_mle({mltest::as_span(l),mltest::as_span(u)},NAN,scale).status;
  };
  const double inf=INFINITY;
  // Exact cluster with censoring only at or below it: collapse.
  CHECK(fit({2,2,2,1},{2,2,2,inf})==S::Unbounded);
  CHECK(fit({2,2,2,2},{2,2,2,inf})==S::Unbounded);
  CHECK(fit({2},{2})==S::Unbounded);
  // A censoring time above the cluster bounds the likelihood.
  CHECK(fit({2,2,2,3},{2,2,2,inf})==S::Ok);
  CHECK(fit({2,3},{2,inf})==S::Ok);
  CHECK(fit({2,3},{inf,inf})==S::AllCensored);
  // Scale held: exact times equal to the scale and nothing above it.
  CHECK(fit({2,2,1},{2,2,inf},2)==S::Unbounded);
  CHECK(fit({2,2,3},{2,2,inf},2)==S::Ok);
  CHECK(fit({1,2},{1,2},2)==S::Ok);
  // Times censored at zero carry no information but are valid.
  CHECK(fit({0,1,3},{inf,1,3})==S::Ok);
  CHECK(fit({0,1},{0,1})==S::InvalidInput);
}
