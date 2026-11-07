#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/fatigue/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

TEST_CASE("fatigue") {
  auto g = mltest::load_distribution_golden("fatigue");
  auto r = cppunivariate::fatigue_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.alpha == C_APPROX(g.estimates.at("alpha")));
  CHECK(r.beta == C_APPROX(g.estimates.at("beta")));
  // Wrapper hardcodes mu=0; golden also stores 0.
  CHECK(g.estimates.at("mu") == 0.0);
  CHECK(r.logLik == C_APPROX(g.logLik));
}

#undef C_APPROX
#undef C_APPROX_FD

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("fatigue: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("fatigue",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dfatigue(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pfatigue(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qfatigue(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rfatigue(rng, p[0], p[1], p[2]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// F=lambda x:m.erfc(-(m.sqrt(x)-1/m.sqrt(x))/m.sqrt(2))/2
TEST_CASE("fatigue: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pfatigue(1,1,1,0,true,true) == doctest::Approx(-std::log(2.0)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qfatigue(-INFINITY,1,1,0,true,true) == doctest::Approx(0).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::pfatigue(INFINITY,1,1,0,false,true) == -INFINITY);
}

// mpmath: mp.dps=100; z=findroot(lambda z:log(erfc(z/sqrt(2))/2)+10000,141)
// h=3*z/2; lower=2/(sqrt(1+h*h)+h)**2; upper=2*(sqrt(1+h*h)+h)**2
TEST_CASE("fatigue: normal transform in extreme log tails") {
  CHECK(cppunivariate::distributions::qfatigue(-10000,3,2,0,true,true) == doctest::Approx(1.1117514052021604e-5).epsilon(1e-12));
  CHECK(cppunivariate::distributions::qfatigue(-10000,3,2,0,false,true) == doctest::Approx(359792.6641948019).epsilon(1e-12));
  CHECK(cppunivariate::distributions::pfatigue(359792.6641948019,3,2,0,false,true) == doctest::Approx(-10000).epsilon(1e-12));
}


TEST_CASE("fatigue: exact held-beta alpha and held-alpha profile") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{.2,.2,1,3};
  auto r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200,nan,1);
  double ss=0;for(double v:x)ss+=v+1/v-2;
  CHECK(r.beta==1);CHECK(r.alpha==doctest::Approx(std::sqrt(ss/4)));
  for(double a:{.7,3.}) {
    r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200,a);
    CHECK(r.alpha==a);CHECK(std::isfinite(r.logLik));
    double ll=0;for(double v:x)ll+=cppunivariate::distributions::dfatigue(v,a,r.beta,0,true);
    CHECK(r.logLik==doctest::Approx(ll));
  }
  const std::vector<double> c{1,1};
  CHECK(std::isnan(cppunivariate::fatigue_mle(mltest::as_span(c),1e-10,200,nan,1).logLik));
  CHECK(std::isfinite(cppunivariate::fatigue_mle(mltest::as_span(c),1e-10,200,3,1).logLik));
}

TEST_CASE("fatigue: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("fatigue");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::fatigue_mle(x,1e-8,1).status==cppunivariate::FatigueStatus::SearchFailure);
}

TEST_CASE("fatigue core threshold transform matches former adapter") {
  const auto g=mltest::load_distribution_golden("fatigue");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=0;mask<4;++mask) {
    std::vector<double> shifted=g.data;
    for(auto &v:shifted)v-=-0.25;
    const double a=(mask&1)?0.8:nan,b=(mask&2)?1.5:nan;
    const auto old=cppunivariate::fatigue_mle(mltest::as_span(shifted),mltest::kFitTol,100,a,b);
    const auto r=cppunivariate::fatigue_mle_shifted(mltest::as_span(g.data),-0.25,mltest::kFitTol,100,a,b);
    CHECK(r.status==old.status);
    CHECK(r.alpha==old.alpha);
    CHECK(r.beta==old.beta);
    CHECK(r.logLik==old.logLik);
  }
}

TEST_CASE("fatigue certificate: reciprocal conditional maxima are a structured tie") {
  const std::vector<double> x{1};
  const auto r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200,3);
  CHECK(r.status==cppunivariate::FatigueStatus::SearchFailure);
  REQUIRE(r.stop_detail.comparison_tie);
  REQUIRE(r.stop_detail.candidates.size()==2);
  CHECK(r.stop_detail.candidates[0].coef[1]==doctest::Approx((7-3*std::sqrt(5.))/2).epsilon(1e-12));
  CHECK(r.stop_detail.candidates[1].coef[1]==doctest::Approx((7+3*std::sqrt(5.))/2).epsilon(1e-12));
  CHECK(r.stop_detail.candidates[0].loglik_lower==doctest::Approx(r.stop_detail.candidates[1].loglik_lower));
  CHECK(cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200,2).beta==1);
}

TEST_CASE("fatigue certificate: rational inclusion holds over dispersed scales") {
  const std::vector<double> x{.001,.02,.3,1,2,40};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double a:{nan,.7,3.,8.}) {
    const auto r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200,a);
    REQUIRE(r.status==cppunivariate::FatigueStatus::Ok);
    double score=0;
    for(double v:x)score+=1/(v+r.beta)-1/(2*r.beta)+v/(2*r.alpha*r.alpha*r.beta*r.beta)-1/(2*r.alpha*r.alpha*v);
    CHECK(std::abs(score*r.beta)<1e-9);
  }
}

TEST_CASE("fatigue certificate: nonconstant reciprocal data retain the two-mode tie") {
  const std::vector<double> x{.5,2};
  const auto r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,300,3);
  CHECK(r.status==cppunivariate::FatigueStatus::SearchFailure);
  CHECK(r.stop_detail.comparison_tie);
  REQUIRE(r.stop_detail.candidates.size()>=2);
  CHECK(r.stop_detail.candidates[0].coef[1]*r.stop_detail.candidates[1].coef[1]==doctest::Approx(1).epsilon(1e-10));
}

TEST_CASE("fatigue certificate: nearly coincident mean bounds stop numerically") {
  const double e=std::numeric_limits<double>::epsilon();
  const std::vector<double> x{1,1+e,1+2*e,1+3*e,1+4*e};
  const auto r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,300);
  CHECK(r.status==cppunivariate::FatigueStatus::SearchFailure);
  CHECK_FALSE(r.stop_detail.comparison_tie);
  CHECK_FALSE(r.stop_detail.budget_exhausted);
  REQUIRE(r.stop_detail.candidates.size()==1);
  CHECK(r.stop_detail.candidates[0].coef[0]>0);
}

TEST_CASE("fatigue certificate: held huge alpha avoids squaring alpha") {
  const std::vector<double> x{1e308};
  const auto r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200,1e155,1);
  REQUIRE(r.status==cppunivariate::FatigueStatus::Ok);
  CHECK(r.logLik==doctest::Approx(cppunivariate::distributions::dfatigue(x[0],1e155,1,0,true)).epsilon(1e-12));
}

TEST_CASE("fatigue certificate: rounded threshold shifts cannot assert nonexistence") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{1,2},one{1};
  CHECK(cppunivariate::fatigue_mle_shifted(mltest::as_span(x),-1e300,1e-10,200).status==cppunivariate::FatigueStatus::SearchFailure);
  CHECK(cppunivariate::fatigue_mle_shifted(mltest::as_span(one),.1,1e-10,200,nan,.9).status==cppunivariate::FatigueStatus::SearchFailure);
  CHECK(cppunivariate::fatigue_mle_shifted(mltest::as_span(one),0,1e-10,200,nan,1).status==cppunivariate::FatigueStatus::Unbounded);
}

TEST_CASE("fatigue certificate: subnormal rescaling must retain coefficient accuracy") {
  const double unit=1e-320;
  const std::vector<double> x{unit,3*unit,4*unit};
  const auto r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200);
  CHECK(r.status==cppunivariate::FatigueStatus::SearchFailure);
}
