#include "test_support.hpp"
#include "cppunivariate/distributions/huber/mle.hpp"
#include "cppunivariate/distributions/huber/dpqr.hpp"
#include <vector>
#include <random>
TEST_CASE("huber: golden and joint scores") {
  auto g=mltest::load_distribution_golden("huber");
  auto r=cppunivariate::huber_mle(mltest::as_span(g.data));
  REQUIRE(r.status==cppunivariate::HuberStatus::Ok);
  CHECK(r.mu==doctest::Approx(g.estimates.at("mu")).epsilon(1e-10));
  CHECK(r.sigma==doctest::Approx(g.estimates.at("sigma")).epsilon(1e-10));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
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
