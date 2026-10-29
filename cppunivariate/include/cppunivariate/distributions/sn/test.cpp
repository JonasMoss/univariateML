#include "test_support.hpp"
#include "cppunivariate/distributions/sn/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>

TEST_CASE("sn golden and regular scores") {
  const auto g=mltest::load_distribution_golden("sn");
  const auto fit=cppunivariate::sn_mle(mltest::as_span(g.data));
  REQUIRE(fit.status==cppunivariate::SnStatus::Ok);
  CHECK(fit.xi==doctest::Approx(g.estimates.at("xi")).epsilon(1e-6));
  CHECK(fit.omega==doctest::Approx(g.estimates.at("omega")).epsilon(1e-6));
  CHECK(fit.alpha==doctest::Approx(g.estimates.at("alpha")).epsilon(1e-6));
  CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  for(int j=0;j<3;++j) {
    std::vector<double> theta={fit.xi,fit.omega,fit.alpha};
    const double step=1e-5*(1+std::abs(theta[j]));
    auto ll=[&](double shift) {
      auto p=theta;p[j]+=shift;double total=0;
      for(double x:g.data) total+=cppunivariate::distributions::dsn(x,p[0],p[1],p[2],true);
      return total;
    };
    CHECK(std::abs((ll(step)-ll(-step))/(2*step))<1e-3);
  }
}
TEST_CASE("sn normal, tails, inverse and random moments") {
  for(double a:{-1e8,-10.0,-1.0,0.0,1.0,10.0,1e8}) {
    for(double p:{-1000.0,-100.0,-20.0,-2.0,-0.01}) for(bool lower:{true,false}) {
      const double q=cppunivariate::distributions::qsn(p,0,1,a,lower,true);
      CHECK(std::isfinite(q));
      CHECK(cppunivariate::distributions::psn(q,0,1,a,lower,true)==doctest::Approx(p).epsilon(1e-9));
    }
    CHECK(cppunivariate::distributions::psn(0,0,1,a)==doctest::Approx(a>=0 ? std::atan2(1.0,a)/M_PI : 1-std::atan2(1.0,-a)/M_PI).epsilon(2e-8));
  }
  cppunivariate::distributions::StdRng<> rng(20261029);
  double s=0,ss=0;
  for(int i=0;i<20000;++i) { const double v=cppunivariate::distributions::rsn(rng,0,1,3);s+=v;ss+=v*v; }
  CHECK(s/20000==doctest::Approx(std::sqrt(2/M_PI)*3/std::sqrt(10.0)).epsilon(0.02));
  CHECK(ss/20000==doctest::Approx(1).epsilon(0.03));
}
TEST_CASE("sn concave fixed subspaces and escapes") {
  const std::vector<double> x={0,0.1,0.2,0.4,3};
  const auto fit=cppunivariate::sn_mle(mltest::as_span(x));
  CHECK(fit.status==cppunivariate::SnStatus::RightHalfNormalBoundary);
  std::vector<double> y;for(double v:x) y.push_back(-v);
  CHECK(cppunivariate::sn_mle(mltest::as_span(y)).status==cppunivariate::SnStatus::LeftHalfNormalBoundary);
  const double nan=cppunivariate::special::kNaN;
  const auto fixed_face=cppunivariate::sn_mle(mltest::as_span(x),1e-9,200,0,2);
  CHECK(fixed_face.status==cppunivariate::SnStatus::RightHalfNormalBoundary);
  double face_ll=-std::log(2.0);
  for(double v:x) face_ll+=std::log(2.0)+cppunivariate::special::dnorm(v,0,2,true);
  CHECK(fixed_face.logLik==doctest::Approx(face_ll).epsilon(1e-10));
  for(int mask=1;mask<8;++mask) {
    const auto f=cppunivariate::sn_mle(mltest::as_span(x),1e-9,200,
      mask&1 ? 0.2 : nan,mask&2 ? 2 : nan,mask&4 ? 1 : nan);
    if(mask==2) {
      CHECK(f.status==cppunivariate::SnStatus::RightHalfNormalBoundary);
      continue;
    }
    REQUIRE(f.status==cppunivariate::SnStatus::Ok);
    if(mask&1) CHECK(f.xi==doctest::Approx(0.2));
    if(mask&2) CHECK(f.omega==doctest::Approx(2));
    if(mask&4) CHECK(f.alpha==doctest::Approx(1));
  }
}

TEST_CASE("sn rounding-level Newton steps retain accurate regular fits") {
  cppunivariate::distributions::StdRng<> rng(20361529);
  for(int sample=0;sample<=88;++sample) {
    std::vector<double> x;
    for(int i=0;i<500;++i) x.push_back(cppunivariate::distributions::rsn(rng,0,1,10));
    if(sample!=13 && sample!=88) continue;
    CAPTURE(sample);
    const auto f=cppunivariate::sn_mle(mltest::as_span(x),1e-9,200);
    REQUIRE(f.status==cppunivariate::SnStatus::Ok);
    double ll=0,gxi=0,gw=0,ga=0;
    for(double v:x) {
      ll+=cppunivariate::distributions::dsn(v,f.xi,f.omega,f.alpha,true);
      const double z=(v-f.xi)/f.omega,r=cppunivariate::sn_detail::inverse_mills(f.alpha*z);
      gxi+=(z-f.alpha*r)/f.omega;
      gw+=(-1+z*z-f.alpha*z*r)/f.omega;
      ga+=z*r;
    }
    CHECK(ll==doctest::Approx(f.logLik).epsilon(1e-11));
    CHECK(std::abs(gxi)<1e-3);
    CHECK(std::abs(gw)<1e-3);
    CHECK(std::abs(ga)<1e-3);
  }
}

#include "cppunivariate/distributions/sn/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("sn: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("sn",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dsn(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::psn(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qsn(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rsn(rng, p[0], p[1], p[2]); });
}

TEST_CASE("sn: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("sn");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::sn_mle(x,1e-8,1).status==cppunivariate::SnStatus::IterationFailure);
}

TEST_CASE("sn: P3 inner exhaustion checks the returned-point score") {
  const std::vector<double> values{-2,-.3,.1,.8,4};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto fit=cppunivariate::sn_detail::inner(values,2,nan,nan,1e-10,1);
  CHECK_FALSE(fit.converged);
  CHECK(cppunivariate::sn_detail::score_residual(values,2,nan,nan,fit.eta,fit.h)>=1e-7);
}
