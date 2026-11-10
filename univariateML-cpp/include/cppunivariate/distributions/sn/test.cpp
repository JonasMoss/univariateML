#include "cppunivariate/core/constants.hpp"
#include "test_support.hpp"
#include "cppunivariate/distributions/sn/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>

TEST_CASE("sn golden and regular scores") {
  const auto g=mltest::load_distribution_golden("sn");
  const auto fit=cppunivariate::sn_mle(mltest::as_span(g.data));
  REQUIRE(fit.status==cppunivariate::SnStatus::Ok);
  CHECK(fit.xi==doctest::Approx(g.estimates.at("xi")).epsilon(mltest::kGolden_sn).scale(0));
  CHECK(fit.omega==doctest::Approx(g.estimates.at("omega")).epsilon(mltest::kGolden_sn).scale(0));
  CHECK(fit.alpha==doctest::Approx(g.estimates.at("alpha")).epsilon(mltest::kGolden_sn).scale(0));
  CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_sn).scale(0));
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

TEST_CASE("sn: face comparison cannot resolve rounded extreme objectives") {
  const double magnitude=1e20;
  CHECK(cppunivariate::compare_face(-magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(-magnitude,-2*magnitude,2e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-2*magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(-10.,-10.,2e-8)==cppunivariate::FaceComparison::FirstWins);
}

TEST_CASE("sn: audited huge held-parameter magnitude") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x{1,2};
  cppunivariate::span s{x.data(),x.size()};
  const auto r=cppunivariate::sn_mle(s,1e-9,100,0,1e-10,nan);
  CHECK((r.status==cppunivariate::SnStatus::RightHalfNormalBoundary || r.status==cppunivariate::SnStatus::IterationFailure));
}

TEST_CASE("sn: structured face payloads") {
  for(double sign:{-1.,1.}) {
    std::vector<double> x{0,.1,.2,.4,3};for(double& v:x)v*=sign;
    const auto r=cppunivariate::sn_mle(mltest::as_span(x),1e-8,500);
    REQUIRE(r.stop_detail.limit_coef.size()==2);
    CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(0));
    CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(std::sqrt(9.21/5)));
    CHECK(r.stop_detail.limit_loglik==doctest::Approx(2.5*(std::log(2/std::acos(-1.))-std::log(9.21/5)-1)));
  }
}

TEST_CASE("sn: certificate exhaustion retains original-unit witnesses") {
  const std::vector<double> x{-2,-1,0,1,2};
  const auto r=cppunivariate::sn_mle(mltest::as_span(x),1e-9,200,NAN,NAN,NAN,0);
  REQUIRE(r.budget_exhausted);REQUIRE(r.stop_detail.candidates.size()==3);
  CHECK(std::isinf(r.stop_detail.loglik_upper));
  for(const auto& c:r.stop_detail.candidates)CHECK(c.loglik_lower<=r.stop_detail.loglik_lower+1e-7);
}

TEST_CASE("sn: a negative certificate control is invalid, not exhausted") {
  const std::vector<double> x{-2,-1,0,1,2};
  const auto r=cppunivariate::sn_mle(mltest::as_span(x),1e-9,200,NAN,NAN,NAN,-1);
  CHECK(r.status==cppunivariate::SnStatus::InvalidInput);
  CHECK_FALSE(r.budget_exhausted);CHECK(r.stop_detail.candidates.empty());
}

TEST_CASE("sn: quantitative Gaussian curvature enclosures") {
  const std::vector<double> x{-2,-1,0,1,2};
  const double nan=cppunivariate::special::kNaN;
  for(int mask=0;mask<3;++mask) {
    const double xi=mask==1?.3:nan,omega=mask==2?1.7:nan;
    const auto f=cppunivariate::sn_detail::inner(x,0,xi,omega,1e-9,100);
    REQUIRE(f.converged);
    const auto cert=cppunivariate::sn_detail::acceptance(x,0,xi,omega,f.eta,f.h);
    REQUIRE(cert.accepted);
    const double eta=mask==2?1/omega:std::sqrt(5/(10+(mask==1?5*xi*xi:0)));
    const double h=mask==1?xi*eta:0;
    CHECK(std::abs(f.eta-eta)<=cert.eta_error+1e-14);
    CHECK(std::abs(f.h-h)<=cert.h_error+1e-14);
    double optimum=5*std::log(eta),actual=5*std::log(f.eta);
    for(double v:x) {
      optimum+=cppunivariate::sn_detail::log_density(eta*v-h,0);
      actual+=cppunivariate::sn_detail::log_density(f.eta*v-f.h,0);
    }
    CHECK(optimum-actual<=cert.gap+1e-13);
  }
  CHECK_FALSE(cppunivariate::sn_detail::acceptance(x,2,nan,nan,1,0).accepted);
}

TEST_CASE("sn: held-location certified root and nonunique constant shape") {
  const double nan=cppunivariate::special::kNaN;
  const std::vector<double> x{-2,-1,0,1,2};
  for(double omega:{nan,1.7}) {
    const auto f=cppunivariate::sn_mle(mltest::as_span(x),1e-9,100,0,omega);
    REQUIRE(f.status==cppunivariate::SnStatus::Ok);
    CHECK(std::abs(f.alpha)<1e-9);
    CHECK(f.omega==doctest::Approx(std::isfinite(omega)?omega:std::sqrt(2.)).epsilon(1e-12));
  }
  const std::vector<double> constant(5,3);
  const auto tie=cppunivariate::sn_mle(mltest::as_span(constant),1e-9,100,3,2);
  REQUIRE(tie.stop_detail.comparison_tie);
  REQUIRE(tie.stop_detail.candidates.size()==2);
  CHECK(tie.stop_detail.candidates[0].coef[2]!=tie.stop_detail.candidates[1].coef[2]);
  CHECK(tie.stop_detail.loglik_lower==tie.stop_detail.loglik_upper);
  const auto fixed=cppunivariate::sn_mle(mltest::as_span(constant),1e-9,100,3,2,7);
  CHECK(fixed.status==cppunivariate::SnStatus::Ok);
  CHECK(fixed.logLik==doctest::Approx(tie.stop_detail.loglik_lower));
}

TEST_CASE("sn: loose user tolerance cannot bypass certificate accuracy") {
  const std::vector<double> x{-2,-.3,.1,.8,4};
  const auto f=cppunivariate::sn_mle(mltest::as_span(x),10,100,NAN,NAN,2);
  REQUIRE(f.status==cppunivariate::SnStatus::Ok);
  const auto tight=cppunivariate::sn_mle(mltest::as_span(x),1e-12,100,NAN,NAN,2);
  REQUIRE(tight.status==cppunivariate::SnStatus::Ok);
  CHECK(f.xi==doctest::Approx(tight.xi).epsilon(1e-6));
  CHECK(f.omega==doctest::Approx(tight.omega).epsilon(1e-6));
}

TEST_CASE("sn: finite observations outside normalization range stop numerically") {
  const std::vector<double> x{-1e308,1e308};
  const auto f=cppunivariate::sn_mle(mltest::as_span(x),1e-9,100,NAN,NAN,0);
  CHECK(f.status==cppunivariate::SnStatus::IterationFailure);
  CHECK_FALSE(f.budget_exhausted);
  CHECK(f.stop_detail.candidates.empty());
}

TEST_CASE("sn: held-location residual signs survive normalization or stop") {
  const std::vector<double> x{1,2,1e308};
  // Both small values and held xi=0 round to the same standardized point.
  // Charging two endpoint log(2) factors would understate the supremum.
  const auto f=cppunivariate::sn_mle(mltest::as_span(x),1e-9,100,0);
  CHECK(f.status==cppunivariate::SnStatus::IterationFailure);
  CHECK(f.stop_detail.limit_coef.empty());
  CHECK_FALSE(f.budget_exhausted);
}

TEST_CASE("sn: returned-coordinate likelihood is checked for large held shape") {
  const std::vector<double> x{1,2,3};
  const auto f=cppunivariate::sn_mle(mltest::as_span(x),1e-9,100,1+1e-15,1.3,1e16);
  if(f.status==cppunivariate::SnStatus::Ok) {
    double ll=0;for(double v:x)ll+=cppunivariate::distributions::dsn(v,f.xi,f.omega,f.alpha,true);
    CHECK(std::abs(ll-f.logLik)<=3e-9);
  } else {
    CHECK(f.status==cppunivariate::SnStatus::IterationFailure);
    REQUIRE(f.stop_detail.candidates.size()==1);
    const auto& c=f.stop_detail.candidates[0];
    double ll=0;for(double v:x)ll+=cppunivariate::distributions::dsn(v,c.coef[0],c.coef[1],c.coef[2],true);
    CHECK(ll==c.loglik_lower);
  }
}

TEST_CASE("sn: budget witnesses retain exact original endpoints and held scale") {
  const std::vector<double> x{.1,.2,.3,.5,4};
  const auto f=cppunivariate::sn_mle(mltest::as_span(x),1e-9,200,NAN,1.3,NAN,0);
  REQUIRE(f.budget_exhausted);
  REQUIRE(f.stop_detail.candidates.size()==3);
  for(const auto& c:f.stop_detail.candidates) {
    CHECK(c.coef[1]==1.3);
    double ll=0;
    if(c.model==cppunivariate::StopModel::None) {
      for(double v:x)ll+=cppunivariate::distributions::dsn(v,c.coef[0],c.coef[1],c.coef[2],true);
    } else {
      CHECK(c.coef[0]==(c.model==cppunivariate::StopModel::Hnorm?x.front():x.back()));
      for(double v:x)ll+=std::log(2.)+cppunivariate::special::dnorm(v,c.coef[0],c.coef[1],true);
    }
    CHECK(ll==c.loglik_lower);
    CHECK(ll==c.loglik_upper);
    CHECK(f.stop_detail.loglik_lower>=ll);
  }
}

TEST_CASE("sn: held-location root exhaustion retains its actual witness likelihood") {
  const std::vector<double> x{-2,-.3,.1,.8,4};
  const auto f=cppunivariate::sn_mle(mltest::as_span(x),1e-9,1,0,1);
  CHECK(f.status==cppunivariate::SnStatus::IterationFailure);
  REQUIRE(f.stop_detail.candidates.size()==1);
  const auto& c=f.stop_detail.candidates[0];
  double ll=0;for(double v:x)ll+=cppunivariate::distributions::dsn(v,c.coef[0],c.coef[1],c.coef[2],true);
  CHECK(ll==c.loglik_lower);
  CHECK(ll==c.loglik_upper);
}

// Independent 70-digit stationary reference: frozen fit audit case 6448.
TEST_CASE("sn: coefficient accuracy on audit case 6448") {
  const std::vector<double> x{0.60490724323222, 0.4288190074283693, 1.7476712955707259, -0.11698994164072135, 1000001.7476712955};
  const auto r = cppunivariate::sn_mle(mltest::as_span(x), 1e-10, 100, 0.0712306862893739);
  REQUIRE(r.status == cppunivariate::SnStatus::Ok);
  CHECK(r.omega == doctest::Approx(447214.34522771905).epsilon(1e-10));
  CHECK(r.alpha == doctest::Approx(1126792.1138849647).epsilon(1e-10));
}

TEST_CASE("sn: subnormal location cannot satisfy the coefficient contract") {
  const double d=std::numeric_limits<double>::denorm_min();
  const std::vector<double> x{0,0,0,d};
  // With alpha=0 and omega=d held, xi*=d/4 is not representable.
  const auto r=cppunivariate::sn_mle(mltest::as_span(x),1e-10,100,NAN,d,0);
  CHECK((r.status==cppunivariate::SnStatus::AccuracyFailure||r.status==cppunivariate::SnStatus::IterationFailure));
}
