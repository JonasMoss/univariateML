#include <algorithm>
#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/exp/mle.hpp"
#include "cppunivariate/distributions/exp/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("exp") {
  auto g = mltest::load_distribution_golden("exp");
  auto r = cppunivariate::exp_mle(mltest::as_span(g.data));
  CHECK(r.rate == doctest::Approx(g.estimates.at("rate")).epsilon(mltest::kGolden_exp).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_exp).scale(0));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("exp: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("exp",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dexp(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pexp(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qexp(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rexp(rng, p[0]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// S=lambda x:m.exp(-x); Q=lambda l:-l
TEST_CASE("exp: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pexp(1000,1,false,true) == doctest::Approx(-1000).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qexp(-10000,1,false,true) == doctest::Approx(10000).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::pexp(1e-300,1,true,true) == doctest::Approx(std::log(1e-300)).epsilon(1e-12).scale(1e-300));
}


TEST_CASE("exp P2 every held subset") {
  const auto g=mltest::load_distribution_golden("exp");
  std::vector<double> x=g.data;
  const std::vector<double> held{g.estimates.at("rate")};
  { const auto r=cppunivariate::exp_mle(mltest::as_span(x),held[0]);
    CHECK(r.rate==held[0]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dexp(v,p[0],true);return s;};
    std::vector<double> p{r.rate};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("exp: all-zero free rate is unbounded, held rate is finite") {
  const std::vector<double> x(4, 0.0);
  const auto r = cppunivariate::exp_mle(mltest::as_span(x));
  CHECK(r.status == cppunivariate::ExpStatus::Unbounded);
  CHECK(r.rate == std::numeric_limits<double>::infinity());
  CHECK(r.logLik == std::numeric_limits<double>::infinity());
  const auto delegated = cppunivariate::exp_mle(mltest::as_span(x),
      std::numeric_limits<double>::quiet_NaN());
  CHECK(delegated.status == cppunivariate::ExpStatus::Unbounded);
  const auto held = cppunivariate::exp_mle(mltest::as_span(x), 2.0);
  CHECK(held.status == cppunivariate::ExpStatus::Ok);
  CHECK(held.logLik == doctest::Approx(4 * std::log(2.0)));
}

TEST_CASE("exp: TASK-132 invalid data never report success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2.0},std::vector<double>{INFINITY,2.0},std::vector<double>{-INFINITY,2.0},std::vector<double>{-1.0,2.0}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::exp_mle(x).status != cppunivariate::ExpStatus::Ok);
  }
}

TEST_CASE("exp: TASK-132 a zero mixed with positive data is valid") {
  const std::vector<double> data{0,2};
  CHECK(cppunivariate::exp_mle(cppunivariate::span{data.data(),data.size()}).status == cppunivariate::ExpStatus::Ok);
}

#include "cppunivariate/distributions/exp/inference.hpp"

TEST_CASE("exp: TASK-102.5 inference analytic and invalid-input checks") {

  const std::vector<double> x{2};
  const auto ci=cppunivariate::exp_rate_exact_interval(mltest::as_span(x));
  REQUIRE(ci.status==cppunivariate::InferenceStatus::Ok);
  CHECK(ci.lower==doctest::Approx(-std::log(.975)/2));
  CHECK(ci.upper==doctest::Approx(-std::log(.025)/2));
  const std::vector<double> zero{0},negative{-1};
  CHECK(cppunivariate::exp_rate_exact_interval(mltest::as_span(zero)).status==cppunivariate::InferenceStatus::NumericalFailure);
  CHECK(cppunivariate::exp_rate_exact_interval(mltest::as_span(negative)).status==cppunivariate::InferenceStatus::InvalidObservation);

  const std::vector<double> empty;
  const cppunivariate::span data{empty.data(),empty.size()};
  CHECK(cppunivariate::exp_rate_exact_interval(data).status==cppunivariate::InferenceStatus::EmptySample);
  const std::vector<double> invalid{NAN,1,2};
  const cppunivariate::span bad{invalid.data(),invalid.size()};
  CHECK(cppunivariate::exp_rate_exact_interval(bad).status==cppunivariate::InferenceStatus::InvalidObservation);
}

TEST_CASE("exp: TASK-102.5 invalid confidence levels") {
  const std::vector<double> x{.2,.4,.6,.8};
  const auto data=mltest::as_span(x);
  for(double alpha:std::vector<double>{-1.,0.,1.,2.,NAN,INFINITY})
    CHECK(cppunivariate::exp_rate_exact_interval(data,alpha).status==cppunivariate::InferenceStatus::InvalidParameter);
}

TEST_CASE("exp: held rate avoids overflowing the unscaled sum") {
  const std::vector<double> x{1e308, 1e308};
  const auto fit = cppunivariate::exp_mle(mltest::as_span(x), 1e-308);
  CHECK(fit.status == cppunivariate::ExpStatus::Ok);
  CHECK(std::isfinite(fit.logLik));
  CHECK(fit.logLik == doctest::Approx(2 * (std::log(1e-308) - 1)).epsilon(1e-14));
}

TEST_CASE("exp: free rate avoids overflowing the sum") {
  const std::vector<double> x{1e308, 1e308};
  const auto fit = cppunivariate::exp_mle(mltest::as_span(x));
  REQUIRE(fit.status == cppunivariate::ExpStatus::Ok);
  CHECK(fit.rate / 1e-308 == doctest::Approx(1.0).epsilon(1e-14));
  CHECK(std::isfinite(fit.logLik));
}

#include "cppunivariate/distributions/exp/censored.hpp"
#include "censored_reference.hpp"
TEST_CASE("exp: right censoring independent 50-digit reference") {
  for(const auto& c:censored_reference::cases()) {
    if(std::string(c.family)!="exp")continue;
    CAPTURE(c.label); CAPTURE(c.held);
    const auto& v=c.lower;
    std::vector<double> upper=v;
    for(std::size_t i=0;i<v.size();++i)if(!c.exact[i])upper[i]=INFINITY;
    const cppunivariate::CensoredData data{mltest::as_span(v),mltest::as_span(upper)};
    const auto r=cppunivariate::exp_censored_mle(data);
    REQUIRE(r.status==cppunivariate::ExpCensoredStatus::Ok);
    CHECK(r.rate/c.coefficient==doctest::Approx(1).epsilon(1e-10));
    CHECK(r.logLik==doctest::Approx(c.logLik).epsilon(1e-12));
    if(std::string(c.label)=="0pct" || std::string(c.label)=="single") {
      const auto exact=cppunivariate::exp_mle(mltest::as_span(v));
      CHECK(r.rate/exact.rate==doctest::Approx(1).epsilon(4*std::numeric_limits<double>::epsilon()));
      CHECK(r.logLik==doctest::Approx(exact.logLik).epsilon(2e-13));
    }
  }
}
TEST_CASE("exp: right censoring classifications and refusals") {
  std::vector<double> lower{1,2},upper{INFINITY,INFINITY};
  cppunivariate::CensoredData data{mltest::as_span(lower),mltest::as_span(upper)};
  const auto c=censored_reference::Case{"","",1.,0,0,{},{}};
  CHECK(cppunivariate::exp_censored_mle(data).status==cppunivariate::ExpCensoredStatus::AllCensored);
  upper[0]=1;
  CHECK(cppunivariate::exp_censored_mle(data).status==cppunivariate::ExpCensoredStatus::Ok);
  lower[0]=-INFINITY;
  CHECK(cppunivariate::exp_censored_mle(data).status==cppunivariate::ExpCensoredStatus::NotImplemented);
  lower[0]=1;upper[0]=1.5;
  CHECK(cppunivariate::exp_censored_mle(data).status==cppunivariate::ExpCensoredStatus::NotImplemented);
  upper[0]=0;
  CHECK(cppunivariate::exp_censored_mle(data).status==cppunivariate::ExpCensoredStatus::InvalidInput);
}

TEST_CASE("censored core: validation and support-aware counts") {
  std::vector<double> l{1,2,-INFINITY,0,2},u{1,INFINITY,3,2,3};
  cppunivariate::CensoredData x{mltest::as_span(l),mltest::as_span(u)};
  auto c=cppunivariate::classify_censored(x,0);
  CHECK(c.status==cppunivariate::CensoredStatus::Ok);
  CHECK(c.exact==1);CHECK(c.right==1);CHECK(c.left==2);CHECK(c.interval==1);
  CHECK(cppunivariate::classify_censored(x,-1).interval==2);
  l[0]=NAN;CHECK(cppunivariate::classify_censored(x).status==cppunivariate::CensoredStatus::InvalidInput);
  l[0]=2;CHECK(cppunivariate::classify_censored(x).status==cppunivariate::CensoredStatus::InvalidInput);
  x.upper={u.data(),u.size()-1};
  CHECK(cppunivariate::classify_censored(x).status==cppunivariate::CensoredStatus::InvalidInput);
  x={{nullptr,0},{nullptr,0}};
  CHECK(cppunivariate::classify_censored(x).status==cppunivariate::CensoredStatus::Ok);
  CHECK(cppunivariate::exp_censored_mle(x).status==cppunivariate::ExpCensoredStatus::InvalidInput);
}
TEST_CASE("exp: censored held likelihood and boundary outcomes") {
  std::vector<double> l{0,0},u{0,INFINITY};
  cppunivariate::CensoredData x{mltest::as_span(l),mltest::as_span(u)};
  CHECK(cppunivariate::exp_censored_mle(x).status==cppunivariate::ExpCensoredStatus::Unbounded);
  CHECK(cppunivariate::exp_censored_mle(x,2).logLik==doctest::Approx(std::log(2.)));
  u[0]=INFINITY;
  CHECK(cppunivariate::exp_censored_mle(x,2).logLik==0);
  CHECK(cppunivariate::exp_censored_mle(x,0).status==cppunivariate::ExpCensoredStatus::InvalidInput);
  l={1,2};u={1,INFINITY};
  CHECK(cppunivariate::exp_censored_mle(x,2).logLik==doctest::Approx(std::log(2.)-6));
  l={1e308,1e308};u=l;
  const auto r=cppunivariate::exp_censored_mle(x);
  REQUIRE(r.status==cppunivariate::ExpCensoredStatus::Ok);
  CHECK(r.rate/1e-308==doctest::Approx(1).epsilon(mltest::kFitReferenceTol).scale(0));
}

TEST_CASE("censored outcomes are decided in the core") {
  CHECK(cppunivariate::outcome(cppunivariate::ExpCensoredResult{NAN,NAN,cppunivariate::ExpCensoredStatus::AllCensored}).reason == cppunivariate::OutcomeReason::Unattained);
  CHECK(cppunivariate::outcome(cppunivariate::ExpCensoredResult{NAN,NAN,cppunivariate::ExpCensoredStatus::Unbounded}).reason == cppunivariate::OutcomeReason::Unbounded);
  const std::vector<double> x{1e308,1e308};
  const auto r=cppunivariate::exp_mle(mltest::as_span(x),1e308);
  CHECK(r.status==cppunivariate::ExpStatus::IterationFailure);
  CHECK(cppunivariate::outcome(r).reason==cppunivariate::OutcomeReason::Evaluation);
}

TEST_CASE("right censoring at the origin has nonunique attained fits") {
  const std::vector<double> lower{0,0},upper{INFINITY,INFINITY};
  const cppunivariate::CensoredData data{mltest::as_span(lower),mltest::as_span(upper)};
  const auto r=cppunivariate::exp_censored_mle(data);
  CHECK(r.status==cppunivariate::ExpCensoredStatus::Ok);
  CHECK(r.logLik==0);
  CHECK_FALSE(cppunivariate::outcome(r).unique);
}

#include "cppunivariate/distributions/rayleigh/censored.hpp"
#include "cppunivariate/distributions/pareto/censored.hpp"
#include "cppunivariate/distributions/lomax/censored.hpp"
TEST_CASE("transformed censored cores retain origin nonidentification") {
  const std::vector<double> zero{0,0},one{1,1},upper{INFINITY,INFINITY};
  const cppunivariate::CensoredData origin{mltest::as_span(zero),mltest::as_span(upper)};
  const cppunivariate::CensoredData pareto{mltest::as_span(one),mltest::as_span(upper)};
  CHECK_FALSE(cppunivariate::outcome(cppunivariate::rayleigh_censored_mle(origin)).unique);
  CHECK_FALSE(cppunivariate::outcome(cppunivariate::pareto_censored_mle(pareto,NAN,1)).unique);
  CHECK_FALSE(cppunivariate::outcome(cppunivariate::lomax_censored_mle(origin,1)).unique);
}

#include "cppunivariate/distributions/weibull/censored.hpp"
TEST_CASE("right censoring below support is uninformative rather than invalid") {
  const double inf=std::numeric_limits<double>::infinity();
  const std::vector<double> x{-1,2,3},at_zero{0,2,3},at_one{1,2,3},upper{inf,2,inf};
  const auto data=[&](const std::vector<double>& lower) {return cppunivariate::CensoredData{mltest::as_span(lower),mltest::as_span(upper)};};
  const auto e=cppunivariate::exp_censored_mle(data(x)),e0=cppunivariate::exp_censored_mle(data(at_zero));
  REQUIRE(e.status==cppunivariate::ExpCensoredStatus::Ok);
  CHECK(e.rate==e0.rate);CHECK(e.logLik==e0.logLik);CHECK(e.rate==doctest::Approx(.2));
  const auto w=cppunivariate::weibull_censored_mle(data(x),2),w0=cppunivariate::weibull_censored_mle(data(at_zero),2);
  REQUIRE(w.status==cppunivariate::WeibullCensoredStatus::Ok);
  CHECK(w.scale==w0.scale);CHECK(w.logLik==w0.logLik);CHECK(w.scale==doctest::Approx(std::sqrt(13.)));
  const auto r=cppunivariate::rayleigh_censored_mle(data(x)),r0=cppunivariate::rayleigh_censored_mle(data(at_zero));
  REQUIRE(r.status==cppunivariate::RayleighCensoredStatus::Ok);
  CHECK(r.sigma==r0.sigma);CHECK(r.logLik==r0.logLik);CHECK(r.sigma==doctest::Approx(std::sqrt(6.5)));
  const auto l=cppunivariate::lomax_censored_mle(data(x),1),l0=cppunivariate::lomax_censored_mle(data(at_zero),1);
  REQUIRE(l.status==cppunivariate::LomaxCensoredStatus::Ok);
  CHECK(l.kappa==l0.kappa);CHECK(l.logLik==l0.logLik);CHECK(l.kappa==doctest::Approx(1/std::log(12.)));
  const auto p=cppunivariate::pareto_censored_mle(data(x),NAN,1),p0=cppunivariate::pareto_censored_mle(data(at_one),NAN,1);
  REQUIRE(p.status==cppunivariate::ParetoCensoredStatus::Ok);
  CHECK(p.shape==p0.shape);CHECK(p.logLik==p0.logLik);CHECK(p.shape==doctest::Approx(1/std::log(6.)));
  const std::vector<double> bad_upper{-1,2,inf};
  CHECK(cppunivariate::exp_censored_mle({mltest::as_span(x),mltest::as_span(bad_upper)}).status==cppunivariate::ExpCensoredStatus::InvalidInput);
}

TEST_CASE("Pareto censoring below a held endpoint distinguishes flat and unbounded likelihoods") {
  const double inf=std::numeric_limits<double>::infinity();
  const std::vector<double> low{0,.5,1},up{inf,inf,inf};
  const auto flat=cppunivariate::pareto_censored_mle({mltest::as_span(low),mltest::as_span(up)},NAN,1);
  REQUIRE(flat.status==cppunivariate::ParetoCensoredStatus::Ok);
  CHECK(flat.logLik==0);CHECK_FALSE(cppunivariate::outcome(flat).unique);
  const std::vector<double> mixed{.5,2},mixed_upper{inf,2};
  const auto finite=cppunivariate::pareto_censored_mle({mltest::as_span(mixed),mltest::as_span(mixed_upper)},NAN,1);
  REQUIRE(finite.status==cppunivariate::ParetoCensoredStatus::Ok);
  CHECK(finite.shape==doctest::Approx(1/std::log(2.)).epsilon(1e-14));
  const std::vector<double> collapse{.5,1},collapse_upper{inf,1};
  const auto unbounded=cppunivariate::pareto_censored_mle({mltest::as_span(collapse),mltest::as_span(collapse_upper)},NAN,1);
  CHECK(cppunivariate::outcome(unbounded).kind==cppunivariate::OutcomeKind::Nonexistence);
  CHECK(cppunivariate::outcome(unbounded).reason==cppunivariate::OutcomeReason::Unbounded);
  const std::vector<double> impossible{.5,2},impossible_upper{.5,inf};
  CHECK(cppunivariate::pareto_censored_mle({mltest::as_span(impossible),mltest::as_span(impossible_upper)},NAN,1).status==cppunivariate::ParetoCensoredStatus::InvalidInput);
}
