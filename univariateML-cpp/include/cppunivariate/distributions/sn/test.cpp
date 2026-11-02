#include "cppunivariate/core/constants.hpp"
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
    CHECK(cppunivariate::distributions::psn(0,0,1,a)==doctest::Approx(a>=0 ? std::atan2(1.0,a)/cppunivariate::pi : 1-std::atan2(1.0,-a)/cppunivariate::pi).epsilon(2e-8));
  }
  cppunivariate::distributions::StdRng<> rng(20261029);
  double s=0,ss=0;
  for(int i=0;i<20000;++i) { const double v=cppunivariate::distributions::rsn(rng,0,1,3);s+=v;ss+=v*v; }
  CHECK(s/20000==doctest::Approx(std::sqrt(2/cppunivariate::pi)*3/std::sqrt(10.0)).epsilon(0.02));
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

// At symmetry F(0)=1/2; the local inverse is delta_p/f(0).
TEST_CASE("sn: symmetric member has exact median and signed log-half offset") {
  const double offset=2.3190468138462995584e-17*(std::sqrt(cppunivariate::pi/2));
  for(bool lower:{true,false}) for(bool log:{true,false}) {
    const double p=log?-0.6931471805599453:.5;
    const double expected=log?(lower?offset:-offset):0;
    const double got=cppunivariate::distributions::qsn(p,0,1,0,lower,log);
    if (log) CHECK(got==doctest::Approx(expected).scale(0).epsilon(1e-12));
    else CHECK(got==0);
  }
}

TEST_CASE("sn fixed shape inverse-scale concavity examples") {
  const std::vector<double> x{-1.4, -0.1, 0.3, 2.0};
  const auto ll=[&](double u,double b,double alpha) {
    double value=0;
    for(double v:x) value+=std::log(b)+cppunivariate::distributions::dsn(b*v-u,0,1,alpha,true);
    return value;
  };
  for(double alpha:{-5.0,-1.0,0.0,1.0,5.0})
    CHECK(ll(0.1,1.2,alpha)>=0.5*(ll(-0.4,0.7,alpha)+ll(0.6,1.7,alpha))-1e-12);
}

TEST_CASE("sn cached Newton evaluations retain returned-point likelihood and score") {
  const std::vector<double> x{-1.7,-0.9,-0.2,0.15,0.3,0.8,1.6,3.2};
  const double nan=cppunivariate::special::kNaN;
  for(double a:{-1000.0,-30.0,-2.0,0.0,2.0,30.0,1000.0})
    for(int mask=0;mask<4;++mask) {
      const double xi=mask&1?0.2:nan,omega=mask&2?1.3:nan;
      const auto f=cppunivariate::sn_detail::inner(x,a,xi,omega,1e-9,200);
      CAPTURE(a); CAPTURE(mask);
      REQUIRE(f.converged);
      double ll=x.size()*std::log(f.eta);
      for(double v:x)ll+=cppunivariate::sn_detail::log_density(f.eta*v-f.h,a);
      CHECK(f.ll==doctest::Approx(ll).epsilon(1e-14));
      CHECK(cppunivariate::sn_detail::score_residual(x,a,xi,omega,f.eta,f.h)<1e-6);
    }
}

TEST_CASE("sn: standardization handles overflow without producing NaN") {
  const double huge = 1e308;
  const double density = cppunivariate::distributions::dsn(huge, -huge, huge, 0, true);
  CHECK(density == doctest::Approx(-0.5 * std::log(2 * cppunivariate::pi) - 2 - std::log(huge)).epsilon(1e-14));
  const double next = std::nextafter(huge, std::numeric_limits<double>::infinity());
  CHECK(cppunivariate::distributions::dsn(next, huge, 1e-300, 0, true) ==
        -std::numeric_limits<double>::infinity());
}

TEST_CASE("sn interval and tail bounds dominate feasible likelihoods") {
  const std::vector<double> x{-1.8,-.7,-.2,.1,.4,.9,1.3};
  const double nan=cppunivariate::special::kNaN;
  const auto ll=[&](double a,double eta,double h) {
    double value=x.size()*std::log(eta);
    for(double v:x)value+=cppunivariate::sn_detail::log_density(eta*v-h,a);
    return value;
  };
  for(double a0:{-3.,0.,3.}) {
    const cppunivariate::sn_detail::Inner ref={1.2,.2,ll(a0,1.2,.2),true};
    const auto t=cppunivariate::sn_detail::shape_tangent(x,a0,ref);
    for(double omega:{nan,1.25}) {
      const double upper=std::max(cppunivariate::sn_detail::shape_upper(x,omega,-4,t),
                                  cppunivariate::sn_detail::shape_upper(x,omega,5,t));
      for(double a:{-4.,-2.,0.,1.,5.}) for(double h:{-1.,0.,1.})
        for(double eta:{.5,.8,1.5}) {
          if(std::isfinite(omega) && eta!=.8)continue;
          CHECK(ll(a,eta,h)<=upper+1e-10);
        }
      for(double sign:{-1.,1.}) {
        const double tail=cppunivariate::sn_detail::shape_tail_upper(x,omega,3,sign);
        REQUIRE(std::isfinite(tail));
        for(double a:{3.,6.,20.}) for(double h:{-1.,0.,1.})
          for(double eta:{.5,.8,1.5}) {
            if(std::isfinite(omega) && eta!=.8)continue;
            CHECK(ll(sign*a,eta,h)<=tail+1e-10);
          }
      }
    }
  }
}

TEST_CASE("sn tail bound handles ties at a fixed scale") {
  const std::vector<double> x(5,0.0);
  const double upper=cppunivariate::sn_detail::shape_tail_upper(x,2.0,64.0,1.0);
  const double face=5*(.5*std::log(2/cppunivariate::pi)-std::log(2.0));
  CHECK(upper==doctest::Approx(face).epsilon(1e-12));
}
