#include "test_support.hpp"
#include "cppunivariate/distributions/huber/mle.hpp"
#include "cppunivariate/distributions/huber/dpqr.hpp"
#include <vector>
#include <random>
TEST_CASE("huber: golden and joint scores") {
  auto g=mltest::load_distribution_golden("huber");
  auto r=cppunivariate::huber_mle(mltest::as_span(g.data));
  REQUIRE(r.status==cppunivariate::HuberStatus::Ok);
  CHECK(r.mu==doctest::Approx(g.estimates.at("mu")).epsilon(mltest::kGolden_huber).scale(0));
  CHECK(r.sigma==doctest::Approx(g.estimates.at("sigma")).epsilon(mltest::kGolden_huber).scale(0));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_huber).scale(0));
  double a=0,b=0;
  for (double v:g.data) { double z=(v-r.mu)/r.sigma;
    a+=cppunivariate::huber_psi(z,r.epsilon);
    b+=z*cppunivariate::huber_psi(z,r.epsilon)-1; }
  CHECK(std::abs(a)<1e-6); CHECK(std::abs(b)<1e-6);
}
TEST_CASE("huber: subsets, interval, constants, tuning and density") {
  std::vector<double> x{-10,10};
  auto r=cppunivariate::huber_mle(mltest::as_span(x),1e-12,200,1.345,NAN,1);
  CHECK(r.mu==0);
  for (double e:{.5,1.345,3.,10.}) {
    r=cppunivariate::huber_mle(mltest::as_span(x),1e-12,200,e);
    CHECK(r.status==cppunivariate::HuberStatus::Ok);
    CHECK(r.mu==doctest::Approx(0));
    CHECK(r.sigma==doctest::Approx(10*std::min(e,1.)).epsilon(1e-9));
    // Symmetric two-point likelihood is an independent analytic oracle.
    auto f=cppunivariate::huber_mle(mltest::as_span(x),1e-12,200,e,0);
    CHECK(f.sigma==doctest::Approx(r.sigma));
    auto both=cppunivariate::huber_mle(mltest::as_span(x),1e-12,200,e,2,3);
    CHECK(both.mu==2); CHECK(both.sigma==3);
    for (double p:{.001,.1,.5,.9,.999}) {
      double q=cppunivariate::distributions::qhuber(p,2,3,e,true,false);
      CHECK(cppunivariate::distributions::phuber(q,2,3,e,true,false)==doctest::Approx(p));
    }
  }
  x={1,1,1};
  CHECK(cppunivariate::huber_mle(mltest::as_span(x)).status==cppunivariate::HuberStatus::UnboundedLikelihood);
  CHECK(cppunivariate::huber_mle(mltest::as_span(x),1e-10,200,1.345,1).status==cppunivariate::HuberStatus::UnboundedLikelihood);
  CHECK(cppunivariate::huber_mle(mltest::as_span(x),1e-10,200,1.345,0).status==cppunivariate::HuberStatus::Ok);
  CHECK(cppunivariate::huber_mle(mltest::as_span(x),1e-10,200,1.345,NAN,2).mu==1);
}
TEST_CASE("huber: slow nested likelihood reference with ties") {
  const std::vector<double> x{-7,-1,-1,0,2,12};
  for (double e:{.5,1.345,3.,10.}) {
    auto objective=[&](double m,double ls) {
      double value=0;
      for (double v:x) value+=cppunivariate::distributions::dhuber(v,m,std::exp(ls),e,true);
      return value;
    };
    auto profile=[&](double ls) {
      double lo=-10,hi=15;
      for (int i=0;i<100;++i) {
        double a=lo+(hi-lo)/3,b=hi-(hi-lo)/3;
        if (objective(a,ls)<objective(b,ls)) lo=a; else hi=b;
      }
      return objective((lo+hi)/2,ls);
    };
    double lo=-5,hi=5;
    for (int i=0;i<100;++i) {
      double a=lo+(hi-lo)/3,b=hi-(hi-lo)/3;
      if (profile(a)<profile(b)) lo=a; else hi=b;
    }
    auto r=cppunivariate::huber_mle(mltest::as_span(x),1e-12,200,e);
    CHECK(r.status==cppunivariate::HuberStatus::Ok);
    CHECK(r.logLik==doctest::Approx(profile((lo+hi)/2)).epsilon(1e-10));
  }
}

// Frozen pre-Newton profile fitter, independent of the new optimization path.
namespace cppunivariate {
inline HuberResult huber_profile_reference(span x, double tol=1e-10, int maxit=200,
    double epsilon=1.345,
    double mu=std::numeric_limits<double>::quiet_NaN(),
    double sigma=std::numeric_limits<double>::quiet_NaN()) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  HuberResult r{mu,sigma,epsilon,nan,0,HuberStatus::InvalidData};
  const bool fm=!std::isnan(mu), fs=!std::isnan(sigma);
  if (x.size()==0 || !(epsilon>0) || !std::isfinite(epsilon) ||
      !(tol>0) || !std::isfinite(tol) || maxit<1 ||
      (fm && !std::isfinite(mu)) ||
      (fs && (!(sigma>0) || !std::isfinite(sigma)))) return r;
  std::vector<double> y;
  for (double v:x) { if (!std::isfinite(v)) return r; y.push_back(v); }
  std::sort(y.begin(),y.end());
  const double center=y[y.size()/2];
  double unit=std::max(std::abs(y.front()-center),std::abs(y.back()-center));
  if (fm) unit=std::max(unit,std::abs(mu-center));
  if (unit==0 && !fs) { r.status=HuberStatus::UnboundedLikelihood; return r; }
  if (unit==0) unit=sigma;
  if (!std::isfinite(unit)) return r;
  for (double& v:y) v=(v-center)/unit;
  double m=fm ? (mu-center)/unit : 0;
  double s=fs ? sigma/unit : 1;
  const double n=static_cast<double>(y.size());
  if (!fs) {
    // Median/MAD start; a zero MAD (ties) falls back to the sample range.
    std::vector<double> deviations;
    for (double v:y) deviations.push_back(std::abs(v));
    std::sort(deviations.begin(),deviations.end());
    s=deviations[y.size()/2]*1.4826;
    if (!(s>0)) s=1;
    double lo=0, hi=1/s;
    auto score = [&](double eta) {
      const double loc=fm ? m : huber_location(y,1/eta,epsilon);
      double total=0;
      for (double v:y) { const double z=(v-loc)*eta; total+=z*huber_psi(z,epsilon); }
      return total/n-1;
    };
    int brackets=0;
    while (score(hi)<0 && brackets++<1024) hi*=2;
    if (!std::isfinite(hi)) { r.status=HuberStatus::IterationFailure; return r; }
    r.status=HuberStatus::IterationFailure;
    for (int j=0;j<maxit;++j) {
      const double eta=lo/2+hi/2;
      const double g=score(eta);
      r.iterations=j+1;
      s=1/eta;
      if (std::abs(g)<=tol) { r.status=HuberStatus::Ok; break; }
      if (g>0) hi=eta; else lo=eta;
    }
  } else r.status=HuberStatus::Ok;
  if (!fm) m=huber_location(y,s,epsilon);
  r.mu=fm ? mu : center+unit*m;
  r.sigma=fs ? sigma : unit*s;
  r.logLik=-n*(std::log(r.sigma)+std::log(huber_z(epsilon)));
  for (double v:y) r.logLik-=huber_rho((v-m)/s,epsilon);
  return r;
}
}

TEST_CASE("huber: Newton agrees with frozen profile on 240 seeded samples") {
  std::mt19937 rng(192637);
  std::normal_distribution<double> normal;
  std::student_t_distribution<double> student(2);
  std::exponential_distribution<double> exponential;
  for (int rep=0;rep<4;++rep) for (int family=0;family<4;++family)
    for (int n:{2,3,10,100,1000}) for (double e:{.3,1.345,3.}) {
      std::vector<double> x;
      for (int i=0;i<n;++i) {
        double v=family==1 ? student(rng) : family==2 ?
            (exponential(rng)-exponential(rng)) : normal(rng);
        if (family==3) v=std::round(v*2)/2;
        x.push_back(v);
      }
      auto r=cppunivariate::huber_mle(mltest::as_span(x),1e-12,200,e);
      auto ref=cppunivariate::huber_profile_reference(mltest::as_span(x),1e-12,200,e);
      REQUIRE(r.status==ref.status);
      if (r.status==cppunivariate::HuberStatus::UnboundedLikelihood) continue;
      CHECK(std::abs(r.logLik-ref.logLik)<1e-9);
      for (int subset=0;subset<2;++subset) {
        double mu=subset==0 ? .25 : NAN, sigma=subset==1 ? 1.3 : NAN;
        r=cppunivariate::huber_mle(mltest::as_span(x),1e-12,200,e,mu,sigma);
        ref=cppunivariate::huber_profile_reference(mltest::as_span(x),1e-12,200,e,mu,sigma);
        REQUIRE(r.status==ref.status);
        CHECK(std::abs(r.logLik-ref.logLik)<1e-9);
      }
    }
}

TEST_CASE("huber: free shape faces, scaling and interior recovery") {
  for (const std::vector<double>& x : {std::vector<double>{-1,1},
       std::vector<double>{10,13},std::vector<double>{-3,0,3}}) {
    auto r=cppunivariate::huber_mle(mltest::as_span(x),1e-10,200,NAN);
    CHECK(r.status==cppunivariate::HuberStatus::NormalLimit);
  }
  const std::vector<double> tail{-10,-.1,0,.1,10};
  CHECK(cppunivariate::huber_mle(mltest::as_span(tail),1e-10,200,NAN).status==
      cppunivariate::HuberStatus::LaplaceLimit);
  for (double e:{.01,.001,.0001}) for (double v:{-4.,0.,.3,8.}) {
    const double h=cppunivariate::distributions::dhuber(v,0,2*e,e,true);
    CHECK(std::abs(h-(-std::log(4.)-std::abs(v)/2))<e*e);
  }
  std::vector<double> x;
  for (int i=0;i<2000;++i) x.push_back(
      cppunivariate::distributions::qhuber((i+.5)/2000,2,3,.8,true,false));
  auto r=cppunivariate::huber_mle(mltest::as_span(x),1e-10,200,NAN);
  REQUIRE(r.status==cppunivariate::HuberStatus::Ok);
  CHECK(r.epsilon==doctest::Approx(.8).epsilon(.02));
  CHECK(r.sigma==doctest::Approx(3).epsilon(.02));
}

TEST_CASE("huber: two-point profile lies below normal and compares Laplace") {
  const std::vector<double> x{-1,1};
  const double normal=-std::log(2*std::acos(-1.0))-1;
  const double laplace=-2*(1+std::log(2.0));
  CHECK(normal>laplace);
  for (double e:{.001,.01,.1,.5,1.,1.5,2.,3.}) {
    const auto r=cppunivariate::huber_mle(mltest::as_span(x),1e-12,200,e);
    REQUIRE(r.status==cppunivariate::HuberStatus::Ok);
    CHECK(r.logLik<normal);
  }
}

TEST_CASE("huber: exact prefix nominations agree with full scan above threshold") {
  std::mt19937 rng(71683);
  std::normal_distribution<double> normal;
  std::exponential_distribution<double> exponential;
  for (int family=0;family<5;++family) {
    std::vector<double> x;
    for (int i=0;i<4097;++i) {
      double v=family==0 ? normal(rng) :
          cppunivariate::distributions::qhuber((i+.5)/4097,0,1,.8,true,false);
      if (family==2) v=std::round(v*4)/4;
      if (family==3) v=(i%7==0 ? 5 : .2)*normal(rng);
      if (family==4) v=exponential(rng)-exponential(rng);
      x.push_back(v);
    }
    for (int subset=0;subset<4;++subset) {
      const double mu=subset%2 ? .2 : NAN;
      const double sigma=subset/2 ? 1.3 : NAN;
      const auto fast=cppunivariate::huber_mle(mltest::as_span(x),1e-10,200,NAN,mu,sigma);
      const auto full=cppunivariate::huber_mle(mltest::as_span(x),1e-10,200,NAN,mu,sigma,true);
      REQUIRE(fast.status==full.status);
      CHECK(fast.logLik==doctest::Approx(full.logLik).epsilon(1e-10));
      if (fast.status!=cppunivariate::HuberStatus::Ok) continue;
      CHECK(fast.mu==doctest::Approx(full.mu).epsilon(1e-8));
      CHECK(fast.sigma==doctest::Approx(full.sigma).epsilon(1e-8));
      CHECK(fast.epsilon==doctest::Approx(full.epsilon).epsilon(1e-8));
    }
  }
}

TEST_CASE("huber: prefix loss and derivatives are exact reductions") {
  std::vector<double> x;
  for (int i=0;i<4101;++i) x.push_back((i%17-8)*.25);
  std::sort(x.begin(),x.end());
  cppunivariate::HuberProfileData data(x,NAN);
  for (double a:{.1,1.,10.}) for (double b:{-.5,0.,.25})
    for (double c:{.01,.5,2.}) {
      CPPUNIV_EXTENDED_REAL loss=0;
      double gp=0,gh=0,aa=0,ab=0,bb=0;
      data.moments(a,b,c,loss,gp,gh,aa,ab,bb);
      double rl=0,rp=0,rh=0,ra=0,rb=0,rc=0;
      for (double v:data.y) {
        const double z=a*v-b, psi=cppunivariate::huber_psi(z,c);
        const double d=std::abs(z)<c ? 1 : (std::abs(z)==c ? .5 : 0);
        rl+=cppunivariate::huber_rho(z,c); rp+=v*psi; rh+=psi;
        ra+=d*v*v; rb-=d*v; rc+=d;
      }
      CHECK(static_cast<double>(loss)==doctest::Approx(rl).epsilon(1e-10));
      CHECK(gp==doctest::Approx(rp).epsilon(1e-10));
      CHECK(gh==doctest::Approx(rh).epsilon(1e-10));
      CHECK(aa==doctest::Approx(ra).epsilon(1e-10));
      CHECK(ab==doctest::Approx(rb).epsilon(1e-10));
      CHECK(bb==doctest::Approx(rc).epsilon(1e-10));
    }
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("huber: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("huber",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dhuber(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::phuber(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qhuber(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rhuber(rng, p[0], p[1], p[2]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// Z=m.sqrt(2*m.pi)*m.erf(1/m.sqrt(2))+2*m.exp(-m.mpf(1)/2); logS=lambda x:-x+m.mpf(1)/2-m.log(Z)
TEST_CASE("huber: extreme tail regressions") {
  CHECK(cppunivariate::distributions::phuber(10000,0,1,1,false,true) == doctest::Approx(-9999.5-std::log(cppunivariate::huber_z(1))).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qhuber(-10000,0,1,1,false,true) == doctest::Approx(10000.5-std::log(cppunivariate::huber_z(1))).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qhuber(-INFINITY,0,1,1,true,true) == -INFINITY);
}

// mpmath: m.mp.dps=100; p=m.exp(m.mpf(float(-math.log(2))))
// Invert the family's CDF at this p, retaining p-m.mpf('.5').
TEST_CASE("huber: rounded log half retains median offset") {
  CHECK(cppunivariate::distributions::qhuber(-std::log(2.0),0,1,1.345,true,true) == doctest::Approx(3.0851715363450619e-17).epsilon(1e-12));
  CHECK(cppunivariate::distributions::qhuber(-std::log(2.0),0,1,1.345,false,true) == doctest::Approx(-3.0851715363450619e-17).epsilon(1e-12));
}

TEST_CASE("huber: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("huber");
  const auto x=mltest::as_span(g.data);
  const std::vector<int> values(g.data.begin(),g.data.end());
  const cppunivariate::ispan ix{values.data(),values.size()};
  CHECK(cppunivariate::huber_fixed_mle(x,1e-8,1,1.345).status==cppunivariate::HuberStatus::IterationFailure);
}

TEST_CASE("huber: P3 fixed-scale fallback requires its location score") {
  const std::vector<double> values{-.1,0,1e-301,1};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto failed=cppunivariate::huber_fixed_mle(mltest::as_span(values),1e-10,1,1.345,nan,1e-300);
  CHECK(failed.status==cppunivariate::HuberStatus::IterationFailure);
  const auto fit=cppunivariate::huber_fixed_mle(mltest::as_span(values),1e-10,200,1.345,nan,1e-300);
  REQUIRE(fit.status==cppunivariate::HuberStatus::Ok);
  CHECK(std::abs(fit.mu-5e-302)<1e-310);
}

TEST_CASE("huber: face comparison cannot resolve rounded extreme objectives") {
  const double magnitude=1e20;
  CHECK(cppunivariate::compare_face(-magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(-magnitude,-2*magnitude,2e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-2*magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(-10.,-10.,2e-8)==cppunivariate::FaceComparison::FirstWins);
}

TEST_CASE("huber: audited huge held-parameter magnitude") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x{1,2};
  cppunivariate::span s{x.data(),x.size()};
  const auto r=cppunivariate::huber_mle(s,1e-10,200,nan,0,1e-10);
  CHECK((r.status==cppunivariate::HuberStatus::Ok || r.status==cppunivariate::HuberStatus::LaplaceLimit || r.status==cppunivariate::HuberStatus::IterationFailure));
  CHECK(r.status==cppunivariate::HuberStatus::IterationFailure);
}

TEST_CASE("huber: held-coordinate Gaussian gain cancels an enormous quadratic") {
  const double n=2, c=3e10, sigma=1e-10;
  // Both standardized residuals lie in the quadratic region. Thus the
  // Gaussian and Huber kernels agree exactly; only normalization differs.
  const double gain=n*(std::log(cppunivariate::huber_z(c))-.5*std::log(2*std::acos(-1.0)));
  CHECK(std::abs(gain)<1e-14);
  const double normal=-n*(std::log(sigma)+.5*std::log(2*std::acos(-1.0)))-5/(2*sigma*sigma);
  CHECK(cppunivariate::compare_face(normal,normal,1e-8*n)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(gain,0,1e-8*n,n+std::abs(gain))==cppunivariate::FaceComparison::FirstWins);
  // A supplied large absolute-term sum must not be hidden by a small gain.
  CHECK(cppunivariate::compare_face(1,0,1e-8,1e20)==cppunivariate::FaceComparison::NumericalTie);
}

TEST_CASE("huber: structured face payloads") {
  const std::vector<double> x{-2,-1,0,1,2};
  const auto r=cppunivariate::huber_mle(mltest::as_span(x),1e-10,200,NAN);
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  CHECK(r.stop_detail.limit_coef[0]==0);
  CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(std::sqrt(2.)));
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(-2.5*(std::log(4*std::acos(-1.))+1)));
}

TEST_CASE("huber: Laplace payload has the fitted public scale") {
  const std::vector<double> x{-10,-.1,0,.1,10};
  const auto r=cppunivariate::huber_mle(mltest::as_span(x),1e-10,200,NAN);
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  CHECK(r.stop_detail.limit_coef[0]==0);CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(4.04));
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(-5*(1+std::log(8.08))));
}

#include "cppunivariate/core/inference_join.hpp"
TEST_CASE("huber: analytic covariance derivatives and subset projection") {
  const std::array<double,8> theta{0,1,1};
  const std::vector<double> x{-2,-1,-.2,0,.2,1,2};
  const auto full=cppunivariate::join_family_inference({x.data(),x.size()},"huber",theta,7);
  REQUIRE(full.status==cppunivariate::InferenceStatus::Ok);
  for(unsigned mask=0;mask<(1u<<3);++mask) {
    std::size_t emitted=0;
    const auto r=cppunivariate::join_family_inference({x.data(),x.size()},"huber",theta,mask,[&](const auto&){++emitted;});
    REQUIRE(r.status==cppunivariate::InferenceStatus::Ok);
    CHECK(emitted==x.size());
    for(std::size_t i=0;i<r.dimension;++i) {
      const auto a=r.free_indices[i];CHECK(r.score[i]==doctest::Approx(full.score[a]));
      for(std::size_t j=0;j<r.dimension;++j) {
        const auto b=r.free_indices[j];
        CHECK(r.observed[i*r.dimension+j]==doctest::Approx(full.observed[a*3+b]));
        CHECK(r.opg[i*r.dimension+j]==doctest::Approx(full.opg[a*3+b]));
      }
    }
  }
  auto bad=theta;bad[1]=0;
  CHECK(cppunivariate::join_family_inference({x.data(),x.size()},"huber",bad,1).status==cppunivariate::InferenceStatus::BoundaryOrLimit);
  CHECK(cppunivariate::join_family_inference({x.data(),x.size()},"huber",theta,1u<<3).status==cppunivariate::InferenceStatus::InvalidSubset);
}

TEST_CASE("huber: supporting plane bounds nuisance competitors") {
  const std::vector<double> x{-7,-1,-1,0,2,12};
  for(bool fm:{false,true})for(bool fs:{false,true}) {
    const auto r=cppunivariate::huber_fixed_mle(mltest::as_span(x),1e-12,200,.8,
        fm?.2:NAN,fs?1.3:NAN);
    REQUIRE(r.status==cppunivariate::HuberStatus::Ok);
    const double upper=cppunivariate::huber_nuisance_upper(mltest::as_span(x),r,fm,fs);
    CHECK(upper>=r.logLik);
    CHECK(upper-r.logLik<1e-8*x.size());
    for(double m:{-4.,.2,3.})for(double s:{.3,1.3,10.}) {
      double ll=0;
      for(double v:x)ll+=cppunivariate::distributions::dhuber(v,fm?.2:m,fs?1.3:s,.8,true);
      CHECK(ll<=upper);
    }
  }
}

TEST_CASE("huber: epsilon chord bounds the refitted nuisance profile") {
  const std::vector<double> x{-7,-1,-1,0,2,12};
  const double l=.6,u=1.2,n=x.size();
  for(bool fm:{false,true})for(bool fs:{false,true}) {
    auto fit=[&](double c){return cppunivariate::huber_fixed_mle(mltest::as_span(x),1e-12,200,c,fm?.2:NAN,fs?1.3:NAN);};
    const auto left=fit(l),right=fit(u);
    const double a=cppunivariate::huber_nuisance_upper(mltest::as_span(x),left,fm,fs);
    const double b=cppunivariate::huber_nuisance_upper(mltest::as_span(x),right,fm,fs);
    for(double w:{.1,.25,.5,.9}) {
      const double c=l+(u-l)*w;
      const auto r=fit(c);
      const double chord=(1-w)*a+w*b+n*((1-w)*std::log(cppunivariate::huber_z(l))+w*std::log(cppunivariate::huber_z(u))-std::log(cppunivariate::huber_z(c)));
      CHECK(r.logLik<=chord+1e-10);
      CHECK(chord<=std::max(a,b)+n*(1+2/(l*l))*(u-l)*(u-l)/8);
    }
  }
}

TEST_CASE("huber: strict median cluster certifies the entire Laplace tail") {
  const std::vector<double> x{-10,-.1,0,.1,10};
  double m=0,b=0;
  REQUIRE(cppunivariate::huber_laplace_tail(mltest::as_span(x),NAN,.01,m,b));
  CHECK(b==doctest::Approx(4.04));
  const double face=-5*(1+std::log(2*b));
  for(double c:{.001,.005,.01}) {
    const auto r=cppunivariate::huber_fixed_mle(mltest::as_span(x),1e-12,200,c);
    REQUIRE(r.status==cppunivariate::HuberStatus::Ok);
    CHECK(r.logLik<face);
    CHECK(r.logLik==doctest::Approx(face+2*c*c-5*std::log(c*cppunivariate::huber_z(c)/2)).epsilon(1e-12));
  }
  const std::vector<double> even{-1,1};
  CHECK_FALSE(cppunivariate::huber_laplace_tail(mltest::as_span(even),NAN,.01,m,b));
}

TEST_CASE("huber: certificate preserves both faces and the finite witness") {
  const std::vector<double> x{-10,-.1,0,.1,10};
  const auto r=cppunivariate::huber_mle(mltest::as_span(x),1e-10,200,NAN);
  REQUIRE(r.status==cppunivariate::HuberStatus::LaplaceLimit);
  REQUIRE(r.stop_detail.candidates.size()>=3);
  CHECK(r.stop_detail.candidates[0].model==cppunivariate::StopModel::Norm);
  CHECK(r.stop_detail.candidates[1].model==cppunivariate::StopModel::Laplace);
  CHECK(r.stop_detail.loglik_upper>=r.stop_detail.loglik_lower);
}

TEST_CASE("huber: all-zero held-scale residuals have an analytic normal face") {
  const std::vector<double> x{2,2,2};
  const auto r=cppunivariate::huber_mle(mltest::as_span(x),1e-10,200,NAN,NAN,1);
  REQUIRE(r.status==cppunivariate::HuberStatus::NormalLimit);
  CHECK((r.stop_detail.limit_coef==std::vector<double>{2,1}));
  CHECK(r.stop_detail.loglik_lower==r.stop_detail.loglik_upper);
}

TEST_CASE("huber: numerical stop retains an evaluated finite witness") {
  const auto g=mltest::load_distribution_golden("huber");
  const auto r=cppunivariate::huber_mle(mltest::as_span(g.data),1e-10,1,1.345);
  REQUIRE(r.status==cppunivariate::HuberStatus::IterationFailure);
  REQUIRE_FALSE(r.stop_detail.candidates.empty());
  CHECK(std::isfinite(r.stop_detail.loglik_lower));
  CHECK(r.stop_detail.loglik_upper==INFINITY);
}

TEST_CASE("huber: finite stationary witness indistinguishable from the normal face stops") {
  const double gaussian=std::sqrt(2*std::acos(-1.0));
  auto gain=[&](double c) {
    const double z=cppunivariate::huber_z(c);
    const double d=2*std::exp(-c*c/2)/(c*c*z);
    return d*d/2-std::log(z/gaussian);
  };
  double lo=.7,hi=.85;
  REQUIRE(gain(lo)>0);REQUIRE(gain(hi)<0);
  for(int j=0;j<100;++j) {
    const double c=(lo+hi)/2;
    if(gain(c)>0)lo=c;else hi=c;
  }
  const double c=(lo+hi)/2;
  const double a=c+2*std::exp(-c*c/2)/(c*c*cppunivariate::huber_z(c));
  const std::vector<double> x{a,a};
  const auto r=cppunivariate::huber_mle(mltest::as_span(x),1e-10,200,NAN,0,1);
  REQUIRE(r.status==cppunivariate::HuberStatus::IterationFailure);
  REQUIRE(r.stop_detail.comparison_tie);
  CHECK(r.stop_detail.loglik_upper>=r.stop_detail.loglik_lower);
  bool interior=false;
  for(const auto& p:r.stop_detail.candidates)
    if(p.model==cppunivariate::StopModel::None && p.coef.size()==3 && p.coef[2]<1)
      interior=true;
  CHECK(interior);
}

TEST_CASE("huber: Gaussian face preserves extreme residual scales") {
  for(double scale:{1e-300,1e-200,1e200,1e300}) {
    const std::vector<double> x{-scale,scale};
    const auto r=cppunivariate::huber_mle(mltest::as_span(x),1e-10,200,NAN);
    REQUIRE(r.status==cppunivariate::HuberStatus::NormalLimit);
    CHECK(r.stop_detail.limit_coef[1]/scale==doctest::Approx(1).epsilon(1e-10));
  }
}

TEST_CASE("huber: nonrepresentable fixed-shape objective has explicit unknown bounds") {
  const std::vector<double> x{-100,100};
  const auto r=cppunivariate::huber_mle(mltest::as_span(x),1e-10,200,1.345,NAN,
      std::numeric_limits<double>::min());
  REQUIRE(r.status==cppunivariate::HuberStatus::IterationFailure);
  CHECK(r.stop_detail.loglik_upper==INFINITY);
  CHECK(std::isnan(r.stop_detail.loglik_lower));
}

TEST_CASE("huber: finite certificates retain the total shape score gate") {
  std::vector<double> x;
  for(int j=0;j<200;++j)x.push_back(cppunivariate::distributions::qhuber((j+.5)/200,2,3,.8,true,false));
  for(bool fm:{false,true})for(bool fs:{false,true})for(int maxit:{1,200}) {
    const auto r=cppunivariate::huber_mle(mltest::as_span(x),1e-10,maxit,NAN,fm?2:NAN,fs?3:NAN);
    if(maxit==200)REQUIRE(r.status==cppunivariate::HuberStatus::Ok);
    if(r.status!=cppunivariate::HuberStatus::Ok)continue;
    // Independent derivative of the settled density, not the fitter's score.
    auto ll=[&](double c){CPPUNIV_EXTENDED_REAL total=0;for(double v:x)total+=cppunivariate::distributions::dhuber(v,r.mu,r.sigma,c,true);return total;};
    const double step=1e-5;
    const double derivative=static_cast<double>((ll(r.epsilon*std::exp(step))-ll(r.epsilon*std::exp(-step)))/(2*step));
    CHECK(std::abs(derivative)<1.1e-3);
  }
}

TEST_CASE("huber: represented face coordinates are checked before certification") {
  const double tiny=std::numeric_limits<double>::denorm_min();
  for(const auto& x:std::vector<std::vector<double>>{{1e15,1e15+.125},
      {4*tiny,-2*tiny,-2*tiny,0,0,0}}) {
    const auto r=cppunivariate::huber_mle(mltest::as_span(x),1e-10,200,NAN);
    REQUIRE(r.status==cppunivariate::HuberStatus::IterationFailure);
    CHECK_FALSE(r.stop_detail.comparison_tie);
    CHECK_FALSE(r.stop_detail.budget_exhausted);
    CHECK(r.stop_detail.loglik_upper==INFINITY);
    REQUIRE_FALSE(r.stop_detail.candidates.empty());
    for(const auto& p:r.stop_detail.candidates) {
      CPPUNIV_EXTENDED_REAL ll=0;
      for(double v:x) {
        const CPPUNIV_EXTENDED_REAL z=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-p.coef[0])/p.coef[1];
        if(p.model==cppunivariate::StopModel::Norm)
          ll-=std::log(p.coef[1])+.5*std::log(2*std::acos(-1.0))+z*z/2;
        else if(p.model==cppunivariate::StopModel::Laplace)
          ll-=std::log(2.)+std::log(p.coef[1])+std::abs(z);
        else {
          const double c=p.coef[2];
          ll-=std::log(p.coef[1])+std::log(cppunivariate::huber_z(c))+
              (std::abs(z)<=c ? z*z/2 : c*(std::abs(z)-c/2));
        }
      }
      CHECK(p.loglik_lower==doctest::Approx(static_cast<double>(ll)).epsilon(1e-12));
      CHECK(p.loglik_lower==p.loglik_upper);
    }
  }
}

TEST_CASE("huber: Gaussian core logarithms survive erfc underflow") {
 CHECK(cppunivariate::distributions::qhuber(-10000,0,1000,134500000,true,true)==doctest::Approx(-141379.83987312717).epsilon(1e-9));
}
