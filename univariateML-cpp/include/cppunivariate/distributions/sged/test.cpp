#include "cppunivariate/core/constants.hpp"
#include "test_support.hpp"
#include "cppunivariate/distributions/sged/mle.hpp"
#include "cppunivariate/distributions/sged/dpqr.hpp"
#include "cppunivariate/distributions/ged/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace dist = cppunivariate::distributions;
#define D_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define D_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)
TEST_CASE("sged (skew GED)") {
  auto g = mltest::load_distribution_golden("sged");
  auto r = cppunivariate::sged_mle(mltest::as_span(g.data), mltest::kFitTol, 200);
  CHECK(r.mu == D_APPROX_FD(g.estimates.at("mean")));
  CHECK(r.sigma == D_APPROX_FD(g.estimates.at("sd")));
  CHECK(r.nu == D_APPROX_FD(g.estimates.at("nu")));
  CHECK(r.xi == D_APPROX_FD(g.estimates.at("xi")));
  CHECK(r.logLik == D_APPROX_FD(g.logLik));

  cppunivariate::SgedCost cost{mltest::as_span(g.data)};
  const double phi_hat[4] = {r.mu, std::log(r.sigma), std::log(r.nu),
                             std::log(r.xi)};
  double grad[4];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm = std::sqrt(grad[0] * grad[0] + grad[1] * grad[1] +
                                 grad[2] * grad[2] + grad[3] * grad[3]);
  if (r.nu > 1.0 && r.status == cppunivariate::SgedStatus::Ok) CHECK(gnorm < 1e-3);
}

TEST_CASE("rsged samples are unit-variance and recurse cleanly into rged") {
  cppunivariate::distributions::StdRng<> rng(29);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x =
        cppunivariate::distributions::rsged(rng, 0.0, 1.0, 1.5, 1.5);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);
}

TEST_CASE("sged constrained faces and shapes") {
  std::vector<double> uniform, peaked;
  for (int i = 0; i < 40; ++i) uniform.push_back(-1.0 + 2.0*i/39.0);
  for (int i = 0; i < 18; ++i) peaked.push_back(-0.02 + 0.04*i/17.0);
  peaked.push_back(-10.0); peaked.push_back(10.0);
  auto u = cppunivariate::sged_mle(mltest::as_span(uniform), 1e-6, 500);
  CHECK(u.status == cppunivariate::SgedStatus::UniformBoundary);
  auto l = cppunivariate::sged_mle(mltest::as_span(peaked), 1e-6, 500);
  CHECK(l.status == cppunivariate::SgedStatus::Ok);
  CHECK(l.nu == 1.0);
  CHECK(l.nu >= 1.0);
  cppunivariate::distributions::StdRng<> rng(812);
  for (double shape : {1.5, 3.0}) {
    std::vector<double> x;
    for (int i = 0; i < 500; ++i)
      x.push_back(cppunivariate::distributions::rged(rng, 0.0, 1.0, shape));
    auto r = cppunivariate::sged_mle(mltest::as_span(x), 1e-6, 500);
    CHECK(r.status == cppunivariate::SgedStatus::Ok);
    CHECK(r.nu > 1.0);
  }
}

TEST_CASE("sged translated exponential faces") {
  std::vector<double> x;
  for (int i = 0; i < 100; ++i) x.push_back(-std::log((i+0.5)/100.0));
  auto r = cppunivariate::sged_mle(mltest::as_span(x), 1e-6, 500);
  CHECK(r.status == cppunivariate::SgedStatus::RightHalfGedBoundary);
  for (double &v : x) v = -v;
  r = cppunivariate::sged_mle(mltest::as_span(x), 1e-6, 500);
  CHECK(r.status == cppunivariate::SgedStatus::LeftHalfGedBoundary);
}

TEST_CASE("sged package dpqr preserve logarithmic tails") {
  for (bool lower : {false, true}) {
    for (double lp : {-1000.0, -100.0, -2.0, -0.1}) {
      const double x = cppunivariate::distributions::qsged(lp, 0.3, 1.7, 1.4, 1.8, lower, true);
      CHECK(cppunivariate::distributions::psged(x, 0.3, 1.7, 1.4, 1.8, lower, true) == doctest::Approx(lp).epsilon(2e-12));
    }
  }
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("sged: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("sged",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dsged(x, p[0], p[1], p[2], p[3], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::psged(x, p[0], p[1], p[2], p[3], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qsged(x, p[0], p[1], p[2], p[3], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rsged(rng, p[0], p[1], p[2], p[3]); });
}

TEST_CASE("sged: classified holds preserve values and handle tied/singleton data") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{-2,-1,-.4,0,.2,.8,1,1.3,2,3};
  const auto data=mltest::as_span(x);
  for(bool mean : {false,true}) for(bool sd : {false,true}) {
    if(!mean && !sd) continue;
    const auto r=cppunivariate::sged_mle(data,1e-9,300,mean ? .2 : nan,sd ? 1.7 : nan,4.2,1.3);
    REQUIRE(r.status==cppunivariate::SgedStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    if(mean) CHECK(r.mu==.2);
    if(sd) CHECK(r.sigma==1.7);
    CHECK(r.xi==1.3);
    CHECK(r.nu==4.2);
  }
  const std::vector<double> one{.2};
  const auto location=cppunivariate::sged_mle(mltest::as_span(one),1e-9,300,nan,1.7,4.2,1.3);
  CHECK(location.status==cppunivariate::SgedStatus::Ok);
  const auto collapse=cppunivariate::sged_mle(mltest::as_span(one),1e-9,300,.2,nan,4.2,1.3);
  CHECK(collapse.status==cppunivariate::SgedStatus::Unbounded);
}

TEST_CASE("sged: P3 restrictions and closed faces") {
 const std::vector<double> x{-100,-2,-.01,0,.01,2,100};const auto data=mltest::as_span(x);
 const auto f=cppunivariate::sged_mle(data,1e-8,1000); REQUIRE(f.status==cppunivariate::SgedStatus::Ok);
 CHECK(f.nu==1);CHECK(f.restricted==16u);
 const auto h=cppunivariate::sged_mle(data,1e-8,1000,0,1,1,1);CHECK(h.restricted==0u);
}

TEST_CASE("sged: all P0b hold subsets retain golden holds") {
  const auto g=mltest::load_distribution_golden("sged");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask:std::vector<int>{1,2,4,8,3,5,9,6,10,12,7,11}) {
    const double m=mask&1 ? g.estimates.at("mean") : nan;
    const double sd=mask&2 ? g.estimates.at("sd") : nan;
    const double nu=mask&4 ? g.estimates.at("nu") : nan;
    const double xi=mask&8 ? g.estimates.at("xi") : nan;
    const auto r=cppunivariate::sged_mle(mltest::as_span(g.data),1e-6,1000,m,sd,nu,xi);
    INFO(mask);
    CHECK(r.status==cppunivariate::SgedStatus::Ok);
    if(mask&1) CHECK(r.mu==m);
    if(mask&2) CHECK(r.sigma==sd);
    if(mask&4) CHECK(r.nu==nu);
    if(mask&8) CHECK(r.xi==xi);
    CHECK(r.logLik>=g.logLik-1e-5*g.data.size());
  }
}

TEST_CASE("sged: P0b constant collapse and the held-shape exemption") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> constant{0,0,0};
  for(int mask:std::vector<int>{1,4,8,5,9,12}) {
    const auto r=cppunivariate::sged_mle(mltest::as_span(constant),1e-6,300,
      mask&1 ? 0:nan,nan,mask&4 ? 2:nan,mask&8 ? 1:nan);
    INFO(mask);
    CHECK(r.status==cppunivariate::SgedStatus::Unbounded);
  }
  const std::vector<double> x{-10,-.5,-.2,-.1,0,.1,.2,.5,10};
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-6,500,0,1,nan,1);
  CHECK(r.status==cppunivariate::SgedStatus::Ok);
  CHECK(r.nu==1);
  CHECK(r.restricted==16u);
  const auto held=cppunivariate::sged_mle(mltest::as_span(x),1e-6,500,nan,nan,.8,1);
  CHECK(held.status==cppunivariate::SgedStatus::Ok);
  CHECK(held.nu==.8);
  CHECK(held.restricted==0u);
}

TEST_CASE("sged: P0b uniform and forced half-GED faces") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> uniform;
  for(int i=0;i<20;++i) uniform.push_back((i+.5)/10.-1);
  const auto u=cppunivariate::sged_mle(mltest::as_span(uniform),1e-6,500,0,nan,nan,1);
  CHECK(u.status==cppunivariate::SgedStatus::UniformBoundary);
  CHECK(u.logLik==doctest::Approx(-20*std::log(1.9)));
  const std::vector<double> one{.5,1,1.5,2,2.5,3,10};
  for(int side:std::vector<int>{1,-1}) {
    std::vector<double> x;
    for(double a:one) x.push_back(side*a);
    const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-6,500,0,1,2);
    CHECK(r.status==(side==1 ? cppunivariate::SgedStatus::RightHalfGedBoundary : cppunivariate::SgedStatus::LeftHalfGedBoundary));
    CHECK(r.mu==0);
    CHECK(r.sigma==1);
    CHECK(r.nu==2);
    const double m=1/std::sqrt(3.14159265358979323846);
    const double t=1/std::sqrt(.5-m*m),c=-side*m*t;
    double ll=x.size()*(std::log(2/t)-.5*std::log(3.14159265358979323846));
    for(double a:x) ll-=((a-c)/t)*((a-c)/t);
    CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-9));
  }
}

TEST_CASE("sged: P0b rounded pinned-moment endpoints require numerical stops") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{-std::sqrt(3.0),std::sqrt(3.0)};
  const auto pinned=cppunivariate::sged_mle(mltest::as_span(x),1e-6,500,0,1,nan,1);
  CHECK(pinned.status==cppunivariate::SgedStatus::CertificateFailure);
  CHECK(pinned.stop_detail.limit_coef.empty());
  const auto width=cppunivariate::sged_mle(mltest::as_span(x),1e-6,500,nan,1,nan,1);
  CHECK(width.status==cppunivariate::SgedStatus::CertificateFailure);
}

TEST_CASE("sged exhausted interior cannot return analytic Laplace as a fit") {
  // Normal quantile grid: a one-step interior attempt already beats the
  // analytic nu=1 skew-Laplace face, but has not met R6's score gate.
  std::vector<double> x;
  for (int i=0;i<100;++i)
    x.push_back(cppunivariate::special::normal_quantile((i+.5)/100,true,false));
  const auto fit=cppunivariate::sged_mle(mltest::as_span(x),1e-6,1);
  CHECK(fit.status==cppunivariate::SgedStatus::BfgsFailure);
}

// At symmetry F(0)=1/2; the local inverse is delta_p/f(0).
TEST_CASE("sged: symmetric member has exact median and signed log-half offset") {
  const double offset=2.3190468138462995584e-17*(std::sqrt(cppunivariate::pi/2));
  for(bool lower:{true,false}) for(bool log:{true,false}) {
    const double p=log?-0.6931471805599453:.5;
    const double expected=log?(lower?offset:-offset):0;
    const double got=cppunivariate::distributions::qsged(p,0,1,2,1,lower,log);
    if (log) CHECK(got==doctest::Approx(expected).scale(0).epsilon(1e-12));
    else CHECK(got==0);
  }
}

TEST_CASE("sged fixed shape inverse-scale concavity examples") {
  const std::vector<double> x{-1.4, -0.1, 0.3, 2.0};
  const auto ll=[&](double u,double b,double nu,double xi) {
    double value=0;
    for(double v:x) value+=std::log(b)+cppunivariate::distributions::dsged(b*v-u,0,1,nu,xi,true);
    return value;
  };
  for(double nu:{1.0,1.2,2.0,5.0}) for(double xi:{0.4,1.0,2.5})
    CHECK(ll(0.1,1.2,nu,xi)>=0.5*(ll(-0.4,0.7,nu,xi)+ll(0.6,1.7,nu,xi))-1e-12);
  const auto f=[](double z){return cppunivariate::distributions::dsged(z,0,1,0.5,1.0,true);};
  CHECK(f(2)<0.5*(f(1)+f(3)));
}

TEST_CASE("sged: structured face payloads") {
  std::vector<double> x;for(int i=0;i<40;++i)x.push_back(-1.+2.*i/39.);
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-6,500);
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(-1));CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(1));
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(-40*std::log(2.)));
}

TEST_CASE("sged: half-GED payload retains base scale and original-unit likelihood") {
  for(double side:{-1.,1.}) {
    std::vector<double> x;for(int i=0;i<100;++i)x.push_back(-side*std::log((i+.5)/100.));
    const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-6,500);
    REQUIRE(r.stop_detail.limit_coef.size()==3);
    const double c=r.stop_detail.limit_coef[0],a=r.stop_detail.limit_coef[1],nu=r.stop_detail.limit_coef[2];
    double ll=0;for(double v:x)ll+=std::log(nu)-std::log(a)-cppunivariate::special::lgamma(1/nu)-std::pow(std::abs(v-c)/a,nu);
    CHECK(r.stop_detail.limit_loglik==doctest::Approx(ll));
    CHECK(c==doctest::Approx(side>0?*std::min_element(x.begin(),x.end()):*std::max_element(x.begin(),x.end())));
  }
}

TEST_CASE("sged: held-moment face recovery preserves the computed likelihood") {
  const std::vector<double> x{.5,1,1.5,2,2.5,3,10};
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-6,500,0,1,2);
  REQUIRE(r.stop_detail.limit_coef.size()==3);
  CHECK(r.stop_detail.limit_coef[2]==2);CHECK(r.stop_detail.limit_loglik==r.logLik);
  const double endpoint=r.stop_detail.limit_coef[0],scale=r.stop_detail.limit_coef[1];
  double direct=x.size()*(std::log(2.)-std::lgamma(.5)-std::log(scale));
  for(double v:x)direct-=std::pow((v-endpoint)/scale,2);
  REQUIRE(r.stop_detail.candidates.size()==1);
  CHECK(direct>=r.stop_detail.candidates[0].loglik_lower-1e-10);
  CHECK(direct<=r.stop_detail.candidates[0].loglik_upper+1e-10);
  const std::vector<double> u{-.9,-.6,-.3,0,.3,.6,.9};
  const auto f=cppunivariate::sged_mle(mltest::as_span(u),1e-6,500,0,NAN,NAN,1);
  REQUIRE(f.stop_detail.limit_coef.size()==2);
  CHECK(f.stop_detail.limit_coef[0]==doctest::Approx(-.9));CHECK(f.stop_detail.limit_coef[1]==doctest::Approx(.9));
}

TEST_CASE("sged: subunit half-GED shape has a certified scale profile") {
  // For nu=1/2, m=Gamma(4)/Gamma(2)=6. With all distances 1 and
  // held mean zero, the right-face scale score gives u=2+sqrt(28).
  const std::vector<double> x{1,1,1};
  const double inverse=2+std::sqrt(28.),a=1/inverse;
  for(int side:{-1,1}) {
    const double scale=side>0?a:1./6,endpoint=side>0?-6*a:1;
    double ll=3*(std::log(.5)-std::log(scale));
    for(double z:x)ll-=std::sqrt(std::abs(z-endpoint)/scale);
    cppunivariate::SgedResult r{0,1,.5,1,ll,0,side>0?cppunivariate::SgedStatus::RightHalfGedBoundary:cppunivariate::SgedStatus::LeftHalfGedBoundary};
    cppunivariate::sged_face_payload(mltest::as_span(x),0,NAN,r);
    REQUIRE(r.stop_detail.limit_coef.size()==3);
    CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(endpoint));
    CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(scale));
    CHECK(r.stop_detail.limit_coef[2]==.5);
  }
  // Mixed signs admit two score roots: the first minimum beats the support
  // endpoint. Independently bracket that root of -2/u+.5*(-1/sqrt(6-u)
  // +100/sqrt(6+100*u)) before its first turning point.
  const std::vector<double> mixed{-1,100};double lo=.01,hi=.5;
  for(int j=0;j<100;++j){const double u=(lo+hi)/2;const double g=-2/u+.5*(-1/std::sqrt(6-u)+100/std::sqrt(6+100*u));if(g>0)hi=u;else lo=u;}
  const double u=(lo+hi)/2,scale=1/u,endpoint=-6/u;
  double ll=2*(std::log(.5)-std::log(scale));for(double z:mixed)ll-=std::sqrt((z-endpoint)/scale);
  cppunivariate::SgedResult r{0,1,.5,1,ll,0,cppunivariate::SgedStatus::RightHalfGedBoundary};
  cppunivariate::sged_face_payload(mltest::as_span(mixed),0,NAN,r);
  REQUIRE(r.stop_detail.limit_coef.size()==3);
  CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(endpoint));
  CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(scale));
}

TEST_CASE("sged: held mean and subunit shape retain their reflected half face") {
  const std::vector<double> x{1,1,1};
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-6,500,0,NAN,.5);
  REQUIRE(r.status==cppunivariate::SgedStatus::LeftHalfGedBoundary);
  REQUIRE(r.stop_detail.limit_coef.size()==3);
  CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(1));
  CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(1./6));
  CHECK(r.stop_detail.limit_coef[2]==.5);
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(3*std::log(3.)).epsilon(1e-6));
}

TEST_CASE("sged: subunit profile retains a root-versus-endpoint tie") {
  // Independent analytic score calculation is retained in find-half-ged-tie.py.
  const std::vector<double> x{-1,17.742431303385693};
  cppunivariate::SgedResult r{0,1,.5,1,-8.407236173702191,0,cppunivariate::SgedStatus::RightHalfGedBoundary};
  cppunivariate::sged_face_payload(mltest::as_span(x),0,NAN,r);
  REQUIRE(r.stop_detail.comparison_tie);
  REQUIRE(r.stop_detail.candidates.size()==2);CHECK(r.stop_detail.limit_coef.empty());
  CHECK(std::isinf(r.stop_detail.loglik_upper));
  for(const auto& candidate:r.stop_detail.candidates) {
    REQUIRE(candidate.coef.size()==3);
    const double c=candidate.coef[0],a=candidate.coef[1];
    CHECK(c<=-1);CHECK(c+6*a==doctest::Approx(0).scale(1));
    double ll=2*(std::log(.5)-std::log(a));for(double z:x)ll-=std::sqrt((z-c)/a);
    CHECK(ll>=candidate.loglik_lower-1e-10);CHECK(ll<=candidate.loglik_upper+1e-10);
  }
}

TEST_CASE("sged: recovered subunit face includes the scale Jacobian") {
  for(double b:{1e-4,1e4}) {
    const std::vector<double> x{b,b,b};
    const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-6,500,0,NAN,.5);
    REQUIRE(r.stop_detail.limit_coef.size()==3);
    CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(b));
    CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(b/6));
    CHECK(r.stop_detail.limit_loglik==doctest::Approx(3*(std::log(3.)-std::log(b))).epsilon(1e-6));
  }
}

TEST_CASE("sged certified normal submodels have exact reference coefficients") {
  const std::vector<double> x{-3,-1,.2,.4,1,2,4};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  double mean=0;for(double v:x)mean+=v/x.size();
  double second=0;for(double v:x)second+=(v-mean)*(v-mean)/x.size();
  auto joint=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,nan,nan,2,1);
  REQUIRE(joint.status==cppunivariate::SgedStatus::Ok);
  CHECK(joint.certified);
  CHECK(joint.mu==doctest::Approx(mean).epsilon(2e-8));
  CHECK(joint.sigma==doctest::Approx(std::sqrt(second)).epsilon(2e-8));
  auto location=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,nan,1.7,2,1);
  CHECK(location.certified);
  CHECK(location.sigma==1.7);
  CHECK(location.mu==doctest::Approx(mean).epsilon(2e-8));
  auto scale=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,0,nan,2,1);
  double raw=0;for(double v:x)raw+=v*v/x.size();
  CHECK(scale.certified);
  CHECK(scale.mu==0);
  CHECK(scale.sigma==doctest::Approx(std::sqrt(raw)).epsilon(2e-12));
}

TEST_CASE("sged held-shape certificates expose flat and separated ties") {
  const std::vector<double> x{-1,1};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double nu:{.5,1.})for(bool held_scale:{false,true}) {
    const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,nan,held_scale?1:nan,nu,1);
    CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);
    CHECK_FALSE(r.certified);
    CHECK(r.stop_detail.comparison_tie);
    REQUIRE(r.stop_detail.candidates.size()==2);
    CHECK(r.stop_detail.candidates[0].coef[0]==-1);
    CHECK(r.stop_detail.candidates[1].coef[0]==1);
  }
  const std::vector<double> odd{-2,-1,0,1,3};
  const auto r=cppunivariate::sged_mle(mltest::as_span(odd),1e-8,200,nan,1,1,1);
  CHECK(r.status==cppunivariate::SgedStatus::Ok);
  CHECK(r.certified);CHECK(r.mu==0);
}

TEST_CASE("sged certificate budget and numerical stops retain no successful estimate") {
  const std::vector<double> x{-3,-1,.2,.4,1,2,4};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,1,nan,1,2,1);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);
  CHECK_FALSE(r.stop_detail.budget_exhausted);
  CHECK_FALSE(r.stop_detail.candidates.empty());
  r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,nan,1,.001,1);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);
  CHECK_FALSE(r.certified);
  std::vector<double> many;for(int i=0;i<140;++i)many.push_back(i);
  r=cppunivariate::sged_mle(mltest::as_span(many),1e-8,1,nan,1,.7,1.3);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);
  CHECK(r.stop_detail.budget_exhausted);
  CHECK_FALSE(r.stop_detail.candidates.empty());
}

TEST_CASE("sged certified held shapes preserve density and equivariance") {
  const std::vector<double> x{-3,-1,-.2,0,.3,.4,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double nu:{.6,1.,1.01,2.,4.,20.})for(double xi:{.3,1.,3.})for(bool hs:{false,true}) {
    INFO(nu, " ",xi," ",hs);
    const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-9,300,nan,hs?1.7:nan,nu,xi);
    REQUIRE(r.status==cppunivariate::SgedStatus::Ok);CHECK(r.certified);
    double ll=0;for(double v:x)ll+=cppunivariate::distributions::dsged(v,r.mu,r.sigma,nu,xi,true);
    CHECK(r.logLik==doctest::Approx(ll).epsilon(2e-8));
    std::vector<double> z;for(double v:x)z.push_back(7+v*.03);
    const auto s=cppunivariate::sged_mle(mltest::as_span(z),1e-9,300,nan,hs?.051:nan,nu,xi);
    REQUIRE(s.status==cppunivariate::SgedStatus::Ok);CHECK(s.certified);
    CHECK(s.mu==doctest::Approx(7+.03*r.mu).epsilon(2e-8));
    CHECK(s.sigma==doctest::Approx(.03*r.sigma).epsilon(2e-8));
    CHECK(s.logLik==doctest::Approx(r.logLik-x.size()*std::log(.03)).epsilon(2e-8));
  }
}

TEST_CASE("sged asymmetric held-shape scale certificates satisfy convex score") {
  const std::vector<double> x{-3,-1,-.2,0,.3,.4,2};
  for(double nu:{1.,1.1,2.,4.,20.})for(double xi:{.3,1.3,3.}) {
    const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,300,.1,NAN,nu,xi);
    INFO(nu," ",xi);
    REQUIRE(r.status==cppunivariate::SgedStatus::Ok);CHECK(r.certified);
    CHECK(r.mu==.1);
    const double step=1e-5;
    double left=0,right=0;
    for(double v:x) {
      left+=cppunivariate::distributions::dsged(v,.1,r.sigma*std::exp(-step),nu,xi,true);
      right+=cppunivariate::distributions::dsged(v,.1,r.sigma*std::exp(step),nu,xi,true);
    }
    CHECK(r.logLik>=left);CHECK(r.logLik>=right);
    // At nu=1 the optimum may be a cusp: its two secants need not match.
    if(nu>1)CHECK(std::abs(left-right)<1e-5);
  }
  const auto failed=cppunivariate::sged_mle(mltest::as_span(x),1e-8,1,.1,NAN,2,3);
  CHECK(failed.status==cppunivariate::SgedStatus::CertificateFailure);
  CHECK_FALSE(failed.certified);
}

TEST_CASE("sged subunit asymmetric scale certificate closes cusps and smooth extrema") {
  const std::vector<double> x{-3,-1,-.2,0,.3,.4,2};
  for(double nu:{.3,.5,.9})for(double xi:{.3,1.3,3.}) {
    INFO(nu," ",xi);
    const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,.1,NAN,nu,xi);
    REQUIRE(r.status==cppunivariate::SgedStatus::Ok);CHECK(r.certified);
    double ll=0;for(double v:x)ll+=dist::dsged(v,.1,r.sigma,nu,xi,true);
    CHECK(r.logLik==doctest::Approx(ll).epsilon(2e-8));
    // Independent broad log-scale grid: a regression check, not the proof.
    for(int j=-300;j<=300;++j) {
      const double sd=std::exp(j*.05);double candidate=0;
      for(double v:x)candidate+=dist::dsged(v,.1,sd,nu,xi,true);
      CHECK(candidate<=r.logLik+1e-6);
    }
    std::vector<double> z;for(double v:x)z.push_back(7+.03*v);
    const auto s=cppunivariate::sged_mle(mltest::as_span(z),1e-8,200,7.003,NAN,nu,xi);
    REQUIRE(s.status==cppunivariate::SgedStatus::Ok);CHECK(s.certified);
    CHECK(s.sigma==doctest::Approx(.03*r.sigma).epsilon(2e-7));
    CHECK(s.logLik==doctest::Approx(r.logLik-x.size()*std::log(.03)).epsilon(2e-7));
  }
}

TEST_CASE("sged subunit scale certificate has an analytic separated tie") {
  // nu=1/2, x=-1, held mean=0. Beyond the cusp the cost difference is
  // -log(r)+K sqrt(r-1). Its smooth minimum ties r=1 when
  // log(r)=2(1-1/r), K=2 sqrt(r-1)/r, xi^2=1+K^2/6.
  double lo=4,hi=6;
  for(int j=0;j<60;++j){double t=(lo+hi)/2;if(std::log(t)>2*(1-1/t))hi=t;else lo=t;}
  const double t=(lo+hi)/2,K=2*std::sqrt(t-1)/t,xi=std::sqrt(1+K*K/6);
  const std::vector<double> x{-1,-1,-1};
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,400,0,NAN,.5,xi);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);CHECK_FALSE(r.certified);
  CHECK(r.stop_detail.comparison_tie);REQUIRE(r.stop_detail.candidates.size()>=2);
  double small=INFINITY,large=0;
  for(const auto& c:r.stop_detail.candidates) {
    small=std::min(small,c.coef[1]);large=std::max(large,c.coef[1]);
    double ll=0;for(double v:x)ll+=dist::dsged(v,0,c.coef[1],.5,xi,true);
    CHECK(ll>=c.loglik_lower-1e-8);CHECK(ll<=c.loglik_upper+1e-8);
  }
  CHECK(large/small==doctest::Approx(t).epsilon(2e-6));
  for(double factor:{.999,1.001}) {
    const auto s=cppunivariate::sged_mle(mltest::as_span(x),1e-8,400,0,NAN,.5,xi*factor);
    REQUIRE(s.status==cppunivariate::SgedStatus::Ok);CHECK(s.certified);
    CHECK(s.sigma>0);
  }
}

TEST_CASE("sged subunit scale budget collapse and numerical stops are terminal") {
  std::vector<double> x;for(int j=0;j<160;++j)x.push_back(-1.-j*.1);
  auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,1,0,NAN,.5,1.3);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);
  CHECK(r.stop_detail.budget_exhausted);CHECK_FALSE(r.certified);
  CHECK_FALSE(r.stop_detail.candidates.empty());
  const std::vector<double> constant{0,0,0};
  r=cppunivariate::sged_mle(mltest::as_span(constant),1e-8,200,0,NAN,.5,1.3);
  CHECK(r.status==cppunivariate::SgedStatus::Unbounded);
  const std::vector<double> data{-1,1};
  r=cppunivariate::sged_mle(mltest::as_span(data),1e-8,200,0,NAN,.001,1.3);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);CHECK_FALSE(r.certified);
}

TEST_CASE("sged subunit unfundable knot partitions stop before quadratic work") {
  std::vector<double> x;for(int j=0;j<100000;++j)x.push_back(-1.-j*.1);
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,0,NAN,.5,1.3);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);
  CHECK(r.stop_detail.budget_exhausted);CHECK_FALSE(r.certified);
  CHECK(r.iterations<32);CHECK_FALSE(r.stop_detail.candidates.empty());
}

TEST_CASE("sged held two is the conditional normal certificate in public coordinates") {
  std::vector<double> x{-3,-1,-.5,0,.1,.2,.4,1,2,4};
  for(double mean:{double(NAN),.2})for(double sd:{double(NAN),1.4}) {
    const auto a=cppunivariate::sged_mle(mltest::as_span(x),1e-8,1000,mean,sd,2);
    const auto b=cppunivariate::snorm_mle(mltest::as_span(x),1e-10,1000,mean,sd,NAN);
    CHECK(a.logLik==doctest::Approx(b.logLik));
    CHECK(a.stop_detail.budget_exhausted==b.stop_detail.budget_exhausted);
    CHECK(a.stop_detail.comparison_tie==b.stop_detail.comparison_tie);
    if(b.status==cppunivariate::SnormStatus::Ok) {
      CHECK(a.certified);CHECK(a.mu==b.mu);CHECK(a.sigma==b.sigma);CHECK(a.xi==b.xi);
    }
    for(const auto& c:a.stop_detail.candidates) {
      if(c.model==cppunivariate::StopModel::None)CHECK(c.coef.size()==4);
      else {CHECK(c.coef.size()==3);CHECK(c.coef[2]==2);}
    }
  }
}
TEST_CASE("sged free moments subunit skew closes both faces and separated ties") {
  std::vector<double> x{-1,1};
  for(double nu:{.3,.5,.9,1.}) {
    const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,NAN,NAN,nu);
    CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);
    CHECK(r.stop_detail.comparison_tie);REQUIRE(r.stop_detail.candidates.size()==2);
    CHECK(std::isfinite(r.stop_detail.loglik_upper));
    CHECK(r.stop_detail.candidates[0].model==cppunivariate::StopModel::HalfGed);
    CHECK(r.stop_detail.candidates[1].model==cppunivariate::StopModel::ReflectedHalfGed);
  }
}
TEST_CASE("sged subunit free skew compares finite knots to their public density") {
  std::vector<double> x{-4,-1,0,0,0,0,0,0,.3,2};
  for(double nu:{.5,.8,1.}) {
    const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,NAN,NAN,nu);
    REQUIRE(r.status==cppunivariate::SgedStatus::Ok);CHECK(r.certified);
    double ll=0;for(double z:x)ll+=dist::dsged(z,r.mu,r.sigma,nu,r.xi,true);
    CHECK(ll==doctest::Approx(r.logLik).epsilon(1e-7));
  }
  std::vector<double> large;for(int j=0;j<140;++j)large.push_back(j);
  const auto r=cppunivariate::sged_mle(mltest::as_span(large),1e-8,1,NAN,NAN,.7);
  CHECK(r.stop_detail.budget_exhausted);CHECK_FALSE(r.certified);
}
TEST_CASE("sged subunit skew has separated finite global maxima") {
  const std::vector<double> x{-100,-1,-1,-1,-1,-1,1,1,1,1,1,100};
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,NAN,NAN,.5);
  REQUIRE(r.stop_detail.comparison_tie);REQUIRE(r.stop_detail.candidates.size()==2);
  for(const auto& c:r.stop_detail.candidates){CHECK(c.model==cppunivariate::StopModel::None);CHECK(c.coef.size()==4);}
}
TEST_CASE("sged subunit free skew coefficients match a 70 digit reference") {
  const std::vector<double> x{-4,-1,0,0,0,0,0,0,.3,2};
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,NAN,NAN,.5);
  REQUIRE(r.status==cppunivariate::SgedStatus::Ok);
  // check.py, 70 digits, independent unnormalised two-half-scale moments.
  const double a=.06917655828196826647265728464405659;
  const double b=.05211963387596999114054257892853992;
  CHECK(r.mu==doctest::Approx(6*(b-a)).epsilon(2e-12));
  CHECK(r.sigma==doctest::Approx(std::sqrt(120*(a*a-a*b+b*b)-36*(b-a)*(b-a))).epsilon(2e-12));
  CHECK(r.xi==doctest::Approx(std::sqrt(b/a)).epsilon(2e-12));
  CHECK(r.logLik==doctest::Approx(-5.83627325097221080719737442296735).epsilon(2e-12));
}
TEST_CASE("sged subunit free skew rejects an unfundable partition before quadratic work") {
  std::vector<double> x;for(int j=0;j<100000;++j)x.push_back(j*.1);
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,NAN,NAN,.7);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);
  CHECK(r.stop_detail.budget_exhausted);CHECK(r.iterations==1);
  REQUIRE(r.stop_detail.candidates.size()==1);CHECK(std::isfinite(r.stop_detail.loglik_lower));
  CHECK(r.stop_detail.loglik_upper==INFINITY);
}
TEST_CASE("sged subunit free skew stops on cancellation in the public mean") {
  std::vector<double> x{-4,-1,0,0,0,0,0,0,.3,2};
  for(auto& z:x)z=1e100*(z+.10234154643598965);
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,NAN,NAN,.5);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);CHECK_FALSE(r.certified);
}
TEST_CASE("sged subunit free skew rejects rounded subnormal face scales") {
  const std::vector<double> x{-1e-320,1e-320};
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,NAN,NAN,.5);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);CHECK_FALSE(r.certified);
}
TEST_CASE("sged normal face conversion rejects an unrepresentable half GED scale") {
  const std::vector<double> x{0,0,0};
  const auto normal=cppunivariate::snorm_mle(mltest::as_span(x),1e-8,200,1.1e308);
  REQUIRE(normal.status==cppunivariate::SnormStatus::RightHalfNormalBoundary);
  REQUIRE(normal.stop_detail.limit_coef.size()==2);
  REQUIRE(std::isfinite(normal.stop_detail.limit_coef[1]));
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,1.1e308,NAN,2);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);CHECK_FALSE(r.certified);
  CHECK(r.stop_detail.limit_coef.empty());CHECK(r.stop_detail.loglik_upper==INFINITY);
}
TEST_CASE("sged normal face conversion rejects rounded subnormal half GED scales") {
  const std::vector<double> x{0,std::ldexp(1.,-1047)};
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-8,200,NAN,NAN,2);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);CHECK_FALSE(r.certified);
  CHECK(r.stop_detail.limit_coef.empty());CHECK(r.stop_detail.loglik_upper==INFINITY);
}

TEST_CASE("sged held unit skew transports all four free-shape certificates") {
  const auto g=mltest::load_distribution_golden("ged");
  const auto x=mltest::as_span(g.data);
  for(int mask=0;mask<4;++mask) {
    const double mean=mask&1?g.estimates.at("mean"):NAN;
    const double sd=mask&2?g.estimates.at("sd"):NAN;
    const auto a=cppunivariate::sged_mle(x,1e-10,200,mean,sd,NAN,1);
    const auto b=cppunivariate::ged_mle(x,1e-10,200,mean,sd,NAN);
    REQUIRE(a.status==cppunivariate::SgedStatus::Ok);CHECK(a.certified);
    CHECK(a.mu==b.mu);CHECK(a.sigma==b.sigma);CHECK(a.nu==b.nu);
    CHECK(a.xi==1);CHECK(a.logLik==b.logLik);CHECK(a.restricted==b.restricted);
    CHECK(a.iterations==b.iterations);
    const double phi[4]={a.mu,std::log(a.sigma),std::log(a.nu),0};
    CHECK(-cppunivariate::SgedCost{x}(phi)==doctest::Approx(a.logLik).epsilon(2e-12));
  }
}
TEST_CASE("sged free-shape symmetric stops retain finite and face coordinates") {
  const std::vector<double> x{-1,1};const auto data=mltest::as_span(x);
  const auto tie=cppunivariate::sged_mle(data,1e-10,1,NAN,.1,NAN,1);
  REQUIRE(tie.status==cppunivariate::SgedStatus::CertificateFailure);
  CHECK_FALSE(tie.certified);CHECK(tie.stop_detail.comparison_tie);
  REQUIRE(tie.stop_detail.candidates.size()==2);
  for(const auto& c:tie.stop_detail.candidates) {
    CHECK(c.model==cppunivariate::StopModel::None);
    REQUIRE(c.coef.size()==4);CHECK(c.coef[1]==.1);CHECK(c.coef[2]==1);CHECK(c.coef[3]==1);
  }
  const auto face=cppunivariate::sged_mle(data,1e-10,200,NAN,NAN,NAN,1);
  REQUIRE(face.status==cppunivariate::SgedStatus::UniformBoundary);
  CHECK(face.stop_detail.limit_model==cppunivariate::StopModel::Unif);
  REQUIRE(face.stop_detail.limit_coef.size()==2);
  CHECK(face.stop_detail.limit_coef[0]==-1);CHECK(face.stop_detail.limit_coef[1]==1);
  const std::vector<double> rounded{-std::sqrt(3.),std::sqrt(3.)};
  for(double mean:{0.,double(NAN)}) {
    const auto r=cppunivariate::sged_mle(mltest::as_span(rounded),1e-10,200,mean,1,NAN,1);
    CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);CHECK_FALSE(r.certified);
    CHECK(r.stop_detail.limit_coef.empty());CHECK_FALSE(r.stop_detail.candidates.empty());
    CHECK(r.stop_detail.loglik_lower<=r.stop_detail.loglik_upper);
    for(const auto& c:r.stop_detail.candidates)if(c.model==cppunivariate::StopModel::None){REQUIRE(c.coef.size()==4);CHECK(c.coef[3]==1);}
  }
}
TEST_CASE("sged symmetric free-shape collapse restrictions and controls") {
  const std::vector<double> constant{2,2,2},odd{-3,-1,0,1,4};
  for(double mean:{2.,double(NAN)}) {
    const auto r=cppunivariate::sged_mle(mltest::as_span(constant),1e-10,200,mean,NAN,NAN,1);
    CHECK(r.status==cppunivariate::SgedStatus::Unbounded);CHECK_FALSE(r.certified);
  }
  const auto r=cppunivariate::sged_mle(mltest::as_span(odd),1e-10,1,NAN,.1,NAN,1);
  REQUIRE(r.status==cppunivariate::SgedStatus::Ok);CHECK(r.certified);
  CHECK(r.nu==1);CHECK(r.restricted==16u);
  CHECK(cppunivariate::sged_mle(mltest::as_span(odd),-1,200,NAN,NAN,NAN,1).status==cppunivariate::SgedStatus::InvalidStart);
  CHECK(cppunivariate::sged_mle(mltest::as_span(odd),1e-10,0,NAN,NAN,NAN,1).status==cppunivariate::SgedStatus::InvalidStart);
}
TEST_CASE("sged symmetric free-shape failed covers preserve the delegate budget") {
  const auto g=mltest::load_distribution_golden("ged");const auto data=mltest::as_span(g.data);
  for(int limit:{1,2,10})for(int mask=0;mask<4;++mask) {
    const double mean=mask&1?g.estimates.at("mean"):NAN,sd=mask&2?g.estimates.at("sd"):NAN;
    const auto a=cppunivariate::sged_mle(data,1e-10,limit,mean,sd,NAN,1);
    const auto b=cppunivariate::ged_mle(data,1e-10,limit,mean,sd,NAN);
    CHECK(a.certified==b.certified);CHECK(a.iterations==b.iterations);
    CHECK(a.stop_detail.budget_exhausted==b.stop_detail.budget_exhausted);
    CHECK(a.stop_detail.comparison_tie==b.stop_detail.comparison_tie);
    REQUIRE(a.stop_detail.candidates.size()==b.stop_detail.candidates.size());
    for(std::size_t j=0;j<a.stop_detail.candidates.size();++j) {
      const auto& c=a.stop_detail.candidates[j];const auto& d=b.stop_detail.candidates[j];
      CHECK(c.loglik_lower==d.loglik_lower);CHECK(c.loglik_upper==d.loglik_upper);
      if(c.model==cppunivariate::StopModel::None){REQUIRE(c.coef.size()==4);CHECK(c.coef[3]==1);}
    }
  }
}
TEST_CASE("sged free moments balanced endpoints certify the uniform for any skew") {
  const std::vector<double> x{-2,-2,3,3};
  for(double xi:{.01,.4,1.,2.,100.,double(NAN)}) {
    const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-10,200,NAN,NAN,NAN,xi);
    REQUIRE(r.status==cppunivariate::SgedStatus::UniformBoundary);CHECK(r.certified);
    CHECK(r.logLik==doctest::Approx(-4*std::log(5.)).epsilon(1e-12));
    REQUIRE(r.stop_detail.limit_coef.size()==2);
    CHECK(r.stop_detail.limit_coef[0]==-2);CHECK(r.stop_detail.limit_coef[1]==3);
    CHECK(r.stop_detail.loglik_lower<=r.logLik);CHECK(r.stop_detail.loglik_upper>=r.logLik);
  }
  const std::vector<double> huge{-1e308,1e308};
  const auto r=cppunivariate::sged_mle(mltest::as_span(huge),1e-10,1,NAN,NAN,NAN,2);
  REQUIRE(r.status==cppunivariate::SgedStatus::UniformBoundary);CHECK(r.certified);
  CHECK(std::isfinite(r.logLik));
  const std::vector<double> tiny{0,1e-320};
  const auto stopped=cppunivariate::sged_mle(mltest::as_span(tiny),1e-10,1,NAN,NAN,NAN,2);
  CHECK(stopped.status==cppunivariate::SgedStatus::CertificateFailure);CHECK_FALSE(stopped.certified);
}

TEST_CASE("sged asymmetric held skew certifies the free moment shape profile") {
  const auto g=mltest::load_distribution_golden("sged");const auto x=mltest::as_span(g.data);
  const auto r=cppunivariate::sged_mle(x,1e-10,200,NAN,NAN,NAN,1.4);
  REQUIRE(r.status==cppunivariate::SgedStatus::Ok);CHECK(r.certified);CHECK(r.xi==1.4);
  CHECK(r.nu==doctest::Approx(3.12977540545).epsilon(2e-8));
  CHECK(r.logLik==doctest::Approx(-649.7033316424115).epsilon(2e-11));
  CHECK(r.stop_detail.loglik_lower<=r.logLik);CHECK(r.stop_detail.loglik_upper>=r.logLik);
  CHECK(r.iterations<=cppunivariate::certificate_budget(200));
  const double phi[4]={r.mu,std::log(r.sigma),std::log(r.nu),std::log(r.xi)};
  CHECK(-cppunivariate::SgedCost{x}(phi)==doctest::Approx(r.logLik).epsilon(2e-12));
}
TEST_CASE("sged weighted interval scores enclose nested shape boxes and moment maps") {
  const std::vector<double> y{-1,-.55,-.1,.03,.2,.6,1};const auto data=mltest::as_span(y);
  for(double xi:{.3,1.4,3.})for(double p:{1.1,1.7,3.,8.}) {
    const cppunivariate::sged_weighted_shape::Weights w(xi);
    const auto b=cppunivariate::sged_weighted_shape::box(data,p,p+.01,w,200);
    REQUIRE(b.valid);
    for(double shape:{p,p+.005,p+.01}) {
      const auto point=cppunivariate::sged_weighted_shape::box(data,shape,shape,w,200);
      REQUIRE(point.valid);CHECK(point.mode.lo>=b.mode.lo);CHECK(point.mode.hi<=b.mode.hi);
      CHECK(point.score.lo>=b.score.lo);CHECK(point.score.hi<=b.score.hi);
      CHECK(point.curvature.lo>=b.curvature.lo);CHECK(point.curvature.hi<=b.curvature.hi);
    }
  }
}
TEST_CASE("sged weighted Laplace median faces and work limits retain witnesses") {
  std::vector<double> x{-100,-.01,-.01,-.01};for(int j=0;j<15;++j)x.push_back(.01);x.push_back(100);
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-10,200,NAN,NAN,NAN,2);
  REQUIRE(r.status==cppunivariate::SgedStatus::CertificateFailure);CHECK(r.stop_detail.comparison_tie);
  REQUIRE(r.stop_detail.candidates.size()>=2);
  CHECK(r.stop_detail.candidates[0].coef[2]==1);CHECK(r.stop_detail.candidates[1].coef[2]==1);
  CHECK(r.stop_detail.candidates[0].coef[0]!=r.stop_detail.candidates[1].coef[0]);
  const auto g=mltest::load_distribution_golden("sged");
  const auto budget=cppunivariate::sged_mle(mltest::as_span(g.data),1e-10,1,NAN,NAN,NAN,1.4);
  CHECK(budget.status==cppunivariate::SgedStatus::CertificateFailure);CHECK_FALSE(budget.certified);
  CHECK_FALSE(budget.stop_detail.candidates.empty());CHECK(budget.stop_detail.loglik_upper==INFINITY);
}
TEST_CASE("sged fixed smooth shape globally profiles both side scales and mode") {
  const auto g=mltest::load_distribution_golden("sged");
  for(double p:{1.7,4.}) {
    const auto r=cppunivariate::sged_mle(mltest::as_span(g.data),1e-10,200,NAN,NAN,p);
    REQUIRE(r.status==cppunivariate::SgedStatus::Ok);CHECK(r.certified);CHECK(r.nu==p);
    CHECK(r.stop_detail.loglik_lower<=r.logLik);CHECK(r.stop_detail.loglik_upper>=r.logLik);
    const double phi[4]={r.mu,std::log(r.sigma),std::log(r.nu),std::log(r.xi)};
    CHECK(-cppunivariate::SgedCost{mltest::as_span(g.data)}(phi)==doctest::Approx(r.logLik).epsilon(2e-12));
  }
}
TEST_CASE("sged smooth shape compares both half GED faces without assuming uniqueness") {
  const std::vector<double> x{-1,1};
  for(double p:{1.1,1.7,4.,20.}) {
    const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-10,200,NAN,NAN,p);
    REQUIRE(r.status==cppunivariate::SgedStatus::CertificateFailure);CHECK(r.stop_detail.comparison_tie);
    REQUIRE(r.stop_detail.candidates.size()>=2);CHECK(std::isfinite(r.stop_detail.loglik_upper));
    bool left=false,right=false;
    for(const auto& c:r.stop_detail.candidates){left=left||c.model==cppunivariate::StopModel::HalfGed;right=right||c.model==cppunivariate::StopModel::ReflectedHalfGed;}
    CHECK(left);CHECK(right);
  }
  std::vector<double> big;for(int j=0;j<100000;++j)big.push_back(j*.1);
  const auto r=cppunivariate::sged_mle(mltest::as_span(big),1e-10,1,NAN,NAN,4);
  CHECK(r.stop_detail.budget_exhausted);CHECK(r.iterations==1);REQUIRE(r.stop_detail.candidates.size()==1);
}
TEST_CASE("sged weighted mode rescaling closes high shape boxes without losing scores") {
  const auto g=mltest::load_distribution_golden("sged");cppunivariate::sged_profile::System system(mltest::as_span(g.data));
  const cppunivariate::sged_weighted_shape::Weights w(1.4);
  for(double p:{125.75,500.}) {
    const auto b=cppunivariate::sged_weighted_shape::box(system.data(),p,p,w,200);
    CHECK(b.valid);CHECK(b.mode.hi-b.mode.lo<2e-8);
  }
  const std::vector<double> y{-1,0,1};
  CHECK_FALSE(cppunivariate::sged_mode_skew::point(mltest::as_span(y),-.999,1000).valid);
}
TEST_CASE("sged smooth profile arithmetic failure precedes enumeration preflight") {
  std::vector<double> x;for(int j=0;j<1000;++j)x.push_back(j*.1);
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-10,1,NAN,NAN,1e308);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);CHECK_FALSE(r.certified);
  CHECK_FALSE(r.stop_detail.budget_exhausted);CHECK(r.stop_detail.loglik_upper==INFINITY);
}
TEST_CASE("sged profile coefficient enclosures include the stored representative") {
  const std::vector<double> x{-1,1};cppunivariate::sged_profile::System system(mltest::as_span(x));
  cppunivariate::sged_profile::PublicBox box;box.mean={0,0};box.logsd={0,0};box.logxi={0,0};box.valid=true;
  cppunivariate::SgedResult r{0,1,2,1,0,0,cppunivariate::SgedStatus::Ok};
  CHECK(system.compatible(box,r));r.mu=.001;CHECK_FALSE(system.compatible(box,r));
  r.mu=0;r.sigma=1.001;CHECK_FALSE(system.compatible(box,r));
  r.sigma=1;r.xi=1.001;CHECK_FALSE(system.compatible(box,r));
}

TEST_CASE("sged: residual power and tails survive intermediate range loss") {
  for (double x : {-1e-100,1e-100}) {
    CHECK(dist::dsged(x,0,1e300,.02,1,true) == doctest::Approx(-612.224579676587918).epsilon(2e-14));
    CHECK(dist::dsged(x,0,1e300,.02,1,false) == doctest::Approx(std::exp(-612.224579676587918)).epsilon(2e-12));
    CHECK(dist::dsged(x,0,1e300,.02,1.3,true) == doctest::Approx(-683.90356286107067).epsilon(2e-13));
    CHECK(dist::psged(x,0,1e300,.02,1.3,true,true) == doctest::Approx(-.0017894880664470457).epsilon(2e-10));
    const double small=dist::psged(x,0,1e300,.0005,1,x<0,true);
    CHECK(small == doctest::Approx(-41.209468768459971).epsilon(2e-12));
    CHECK(dist::qsged(small,0,1e300,.0005,1,x<0,true)/x == doctest::Approx(1).epsilon(2e-10));
  }
  // A nonzero subnormal quotient has too few significant bits as well.
  CHECK(dist::dsged(1e-23,0,1e300,.02,1,true) == doctest::Approx(-612.224611684520115).epsilon(2e-14));
  // Overflow before division is the other side of the same range loss.
  CHECK(dist::dsged(-1e308,1e308,1e308,.02,1.3,true) == doctest::Approx(-727.68316868801662).epsilon(2e-13));
  CHECK(dist::psged(-1e308,1e308,1e308,.02,1.3,true,true) == doctest::Approx(-17.775086035882197).epsilon(2e-12));
  CHECK(dist::dsged(INFINITY,0,1e300,.02,1.3,true) == -INFINITY);
}

TEST_CASE("sged: logarithmic centering survives a vanished moment") {
  // At nu=.0003 the nonzero moment is below binary64's minimum subnormal.
  // Independent 100-digit gamma-CDF and bracketed-quantile references.
  for (double x : {0.,1e-100}) {
    CHECK(dist::dsged(x,0,1e300,.0003,.7,true) == doctest::Approx(-107.064723180909907).epsilon(2e-12));
    CHECK(dist::dsged(x,0,1e300,.0003,1.3,true) == doctest::Approx(-106.798025104960350).epsilon(2e-12));
    CHECK(dist::psged(x,0,1e300,.0003,.7,true,true) == doctest::Approx(-288.220156276318788).epsilon(2e-12));
    CHECK(dist::psged(x,0,1e300,.0003,1.3,false,true) == doctest::Approx(-288.181208135391239).epsilon(2e-12));
  }
  CHECK(dist::qsged(.1,0,1e300,.0003,.7,true,false)/9.98017740400194973e-80 == doctest::Approx(1).epsilon(2e-10));
  CHECK(dist::qsged(.1,0,1e300,.0003,1.3,true,false)/-7.94579776743636934e-80 == doctest::Approx(1).epsilon(2e-10));
  // Here the centering moment is nonzero but subnormal, so log(mu) would
  // retain its already rounded relative error.
  CHECK(dist::qsged(.1,0,1e300,.00036,.7,true,false)/1.32559402858106150e-16 == doctest::Approx(1).epsilon(2e-10));
  CHECK(dist::qsged(.1,0,1e300,.00036,1.3,true,false)/-1.05538224887693826e-16 == doctest::Approx(1).epsilon(2e-10));
  for (double xi : {.7,1.3}) {
    CHECK(dist::qsged(0,0,1e300,.0003,xi,true,false) == -INFINITY);
    CHECK(dist::qsged(1,0,1e300,.0003,xi,true,false) == INFINITY);
    CHECK(std::isnan(dist::qsged(-.1,0,1e300,.0003,xi,true,false)));
  }
  // A nonfinite normalizer must retain its failure, not become the mean.
  CHECK(std::isnan(dist::qsged(.1,0,1e300,.02,1e200,true,false)));
}

TEST_CASE("sged: 80-digit moments and scales at large shape") {
 // Columns match the independent sstd-moment gamma table.
 const double cases[][9] = {
  {2.0000000000000004, 0.000000021073424255447009406680231424770341336517574117977962651925054124374662543256665, 16.982105923718660216992213101123129341574373713828549285712043818557604818257985, 0.79788456080286537602466102589960775724549274041609629206263354525707935268629559, 1.0000000000000001920282993215806661084571382353783559904241908420704807760941827, 17520097.642037474427746391716865777903730129498623401862092262391283294999018294, 0.35974083685977584838311806586420061861822165954583126401520879232128487623777636, 1.8027756377319943694311923077316248204367067468578596873326202013708082745213945, 0.000000031610136383170514110020347137155512004776361176966943977887581186561993814884997},
  {2.0000000009313226, 0.00003051757810529957225886545633475226906543212145807948877179065970774100694424626, 9.7040605281250132895931873401798618046825433138166428682991802443768731445067857, 0.79788456084511199826011949282981303394104240686406523445458151352357659030683696, 1.0000000004027125318252713169722224195046204987883040809142794952483405740663643, 12098.222514984640135719902458061396994632857310139098124615024042843384667819846, 0.35974083683686601467966427228794496614128863747029774239249099176331960004521296, 1.8027756371508142304670690557335542598376966325899034299975893393179339704466617, 0.00004577636715794935838829818450212840359814818218711923315768598956161151041636939},
  {2.01, 0.099313303945021937435357038901486096693035432393606231429959760268681626812269813, 1.6124975966674275444261381791288863764250823602470163234888078182940745731746769, 0.79833581792550095356411910619409828767006835585670881480059360933903190640472851, 1.0043064379413051906531707716273158219453133738908829679325845042092850674273179, 3.7099833991983966734345114686649211803509972449163374701310636081643751221083549, 0.35949524225374540998358493907313968249616072936835019269227544191990176508914948, 1.7966101280561479409275897487241855944494675329321869065772897658367471066302193, 0.14896995591753290615303555835222914503955314859040934714493964040302244021840472},
  {2.5, 0.53935260118837935666793572235555273276586896544304013033994663186388298848651568, -0.21192063724339982685702984638220296330411703158506060938778816620096572522068095, 0.81579499642782909572517392060650109476938348315096307810844396731643103641388868, 1.1779399192904094663134687059172944335497235857978275044086126623288001163079969, 0.63531189068105531114363379453354924865781279276363290698546909440644771943042278, 0.34840733657021741741373670284569044894609086263229890187999800131486056030953229, 1.6110469378886793519627273083799154906007188298455334959296605686607800921529714, 0.80902890178256903500190358353332909914880344816456019550991994779582448272977352},
  {3.0, 0.63661977236758134307553505349005744813783858296182579499066937623558719053690614, -0.45158270528945486472619522989488214357179467855505631739294306197874414791513136, 0.82732303605044488511530597126516486301598596469188785439545850781976862112374365, 1.2990864196143416139120366553081062247115434488435949404071019611212524958650286, 0.51644624191561298691595697780630537602983537288584187011445652748544419189551731, 0.3388971176168244772835465077402236791734039300192218938820100776609317594515113, 1.5290877500061793566147680689562383754535359264555028572847936068687965573750394, 0.95492965855137201461330258023508617220675787444273869248600406435338078580535921},
  {5.0, 0.73510519389572273268176866441729258852984864048885420373248802705470707129235973, -0.71320677717172878298459884236231756439203024656866998675362454344819834380365538, 0.84834233938739694449371059836189613400233273813183932157779123342772662072456852, 1.5284979880673955023927813280801934010776411883134512800613040252989167620330596, 0.41859812569148597820257472821124880581165407017637875290574007725778939160963793, 0.31487705274858816363236776736238678114682137898639894177601478394137931880461113, 1.4262348320988191118107825123654659127402810214004744764589078757603612958187715, 1.1026577908435840990226529966259388827947729607332813055987320405820606069385396},
  {10.0, 0.77339804192278635481342352105217863671778930684676847751849673171368182415896479, -0.83232557649384737696710260333825861825958407692654710030503338150049197181515483, 0.86054316590114856567123695349340563117612582356826655895334243384417950811334278, 1.6638640035147894498431654852663541715524572159938028830591932093139317016309076, 0.38288274017274910985058562456850778207337223500941468149954709467519620911734982, 0.29670956195283847568026543594659288109977478130135558637601004889169156846807953, 1.3799184050832498407734004620407985173442942176109245874629383795838859209427792, 1.1600970628841795322201352815782679550766839602701527162777450975705227362384472},
  {63.0, 0.79462137964502505485240430804368395152938269233200647635155243492976188278979301, -0.90677618947881934320331019448033644425668291211719502877062920359745029139380836, 0.86585393086672028667884410913784295433647593565153255442490748731067553819577332, 1.7274688412953044998478597511636678961529000651421982472361628559168600449601288, 0.36286121056566805952216189953447497688805388204363022412966012226680029012531362, 0.28891103742229792560033357781769705480085531225831165475682640766531270710201849, 1.3525154127679407569776666372643814939759269365944668087772159617904737334816566, 1.1919320694675375822786064620655259272940740384980097145273286523946428241846895},
  {64.0, 0.79467390097168634028637926661068722888748300448960077677999682423572742572329821, -0.90697027514819485451039883575574195895460659389206584934228592434942007397752258, 0.86585913146032384076432764820730788628442482706240353739099723858870818718323132, 1.7275613188711364034297368173359333986210333607363127212285914255385292909047799, 0.362811099000245814039603402564057788961651874704879798861636604618101941397312, 0.28890384113638333382172311569198477082294802241823702821372492463285728831041529, 1.3524459804396978267582284389377557342561319303823631707021502979340482098695447, 1.1920108514575295104295688999160308433312245067344011651699952363535911385849473},
  {127.0, 0.79629040337329089547944890626404575656786030233167644049543783704411682635131473, -0.91297034222380061673399376233681726467538946793160631024565135979373284293548553, 0.86598223795931764235976681410522244657188508237056293501970391577473011959316981, 1.7301218293181408731199809433430839746965318716042689307743134455420676087506982, 0.36126701277700069148434072448362839025906899605857232679784975825210632692648596, 0.28873417426975356458292833311580134425631549489384335245387925079430396899244552, 1.3503049971636422001705244770062167301261553748328472619194304801217224879820185, 1.1944356050599363432191733593960686348517904534975146607431567555661752395269721},
  {127.99999999999999, 0.79630304260259858692010484267124488832390857634696238567881565098309681692584623, -0.91301745985384315367260600520725532856190066143172804082122004322895102728938138, 0.86598290229151653441428199831080635345311911095707148074855403325174733266699133, 1.7301395603052553220502909111516758552491540400243475120138438524830262664850505, 0.36125492616400614224781098555113383163530957469893452077909775010151625885685339, 0.28873326304234055744519362519954151046512457708971070498609601006700515858156148, 1.3502882265535566517130875204296040909630229426083097702059782728982526243705798, 1.1944545639038978803801572640068673324858628645204435785182234764746452253887693},
  {128.0, 0.7963030426025985870982950433036405628152247954154530986394058899375189837015639, -0.91301745985384315433690157197061093296568137661445900837508776387886280432343749, 0.86598290229151653442361347119212478346856140894598035985653392587097908445544228, 1.7301395603052553223000033807002141589619563799685482501908619631176837188642343, 0.36125492616400614207741016073483446991173526678438599109342822307677886104213182, 0.28873326304234055743239459768246742124284894663420387116809557002183799515624517, 1.3502882265535566514766490021161131081565996088277442571704143726300173704002386, 1.1944545639038978806474425649554608442228371931231796479591088349062784755523458},
  {128.00000000000003, 0.79630304260259858745467544456843179133865432015763482448712776574541945902514582, -0.9130174598538431556654927054973216946007617980078753497704582896945542651606501, 0.86598290229151653444227641695476163427965778181452475922902734105834840269719507, 1.7301395603052553227994283197972905753843564610153279256894227046120659033241931, 0.36125492616400614173660851110223586153018140601703348247335054349146934666211905, 0.28873326304234055740679654264831925548073924949227029203389243377100927503960939, 1.3502882265535566510037719654891313020967498306412493449883399447402218075918349, 1.1944545639038978811820131668526476870079814802364522367306916486181291885377187},
  {128.99999999999997, 0.79631548299104717493467607595235995151107418286836047064231473733129659638737769, -0.91306383934091979717553213017260476180248074609864956380198987119738205971392439, 0.86598355140414153310446224293627357291002965502575598415123127791380120656334234, 1.7301569760009033983455380445076863391788668814566921633176929269299787194117987, 0.3612430294886671590950715629020277528852463507633609324113047439893139974856453, 0.28873237275049189140426598196713003390976881150536302256145298069360356484978835, 1.3502717193160250474454443850523890623781001094824871626020576191575542889783576, 1.1944732244865707624020141139285399272666112743025407059634721059969448945810665},
  {129.0, 0.79631548299104717528547193660726769326697328248826057858250097078279689553269712, -0.91306383934091979848339112386473842116935018189617159289281726791448690019099894, 0.86598355140414153312269926317810580834990444933423932270935068223251125574984253, 1.7301569760009033988361195717346423600996553333309207464764074865749670859275363, 0.36124302948866715875960447265033946555922069522262987883156884498986840444798705, 0.28873237275049189137925379006625500712474264054436832389031587688524398343732955, 1.3502717193160250469799644485119935230394132169680388129673970433951110019473246, 1.1944732244865707629282079049109015399004599237323908678737514561741953432990457},
  {129.00000000000003, 0.79631548299104717563626779726217527817096516876663950114619898787871397368857961, -0.91306383934091979979125011755687149820760749033080975153850481123572426699250573, 0.86598355140414153314093628341993803186937756880343186757988396952568494918150455, 1.7301569760009033993267010989615981329634869073181613506044654574125659799043743, 0.36124302948866715842413738239865132806586503524492548995228656432908996938611449, 0.28873237275049189135424159816537999673556439769195585771508484625165538011419724, 1.3502717193160250465144845119715981914659854006778009438173553831307749087424085, 1.1944732244865707634544016958932629172564477531499592517192984818180709605328694},
  {10000.0, 0.79786460994809431206605043399576515119345900717197081836717350639006644060993741, -0.9188635232032975417488244021426599932253240830828966003276622609049263653796794, 0.86602539666374666679959120512376807162630192763207849364381572357399843552608866, 1.7320306707852338639647462617054845326494179371345585940993154858951008538598427, 0.35975995866653023397694976314763143697791954802872516164520719034788980587624535, 0.28867514427964786020772958389790228213529482636431286635884085202520743705929751, 1.3482125739039984478573897516827394101572961984581349227528596691202191297992067, 1.1967969149221414680990756509936477267901885107579562275507602595850996609149061},
  {100000000.0, 0.79788455880815391647188927555308365686792286243758195064445320205855752060263854, -0.91893852570467264178032836140559763986108247363245007939087177697800376358202179, 0.86602540378443857553599078072580574668917516078759831514041577138810737897001639, 1.7320508055608839716811562122595894686802270468005055719882479517731825588637539, 0.35974083877162601086608428980465747379935214813013565798487466753298826334012351, 0.288675134594812979124291134695740472776355466506611722811036036766054901440388, 1.348186010658364064360975212144473279798223183308792629869163033513342522971204, 1.1968268382122308747078339133296254853018842936563729259666798030878362809039578},
  {1000000000000.0, 0.79788456080266588473969102952140588691163287764893719945156714735042685547380533, -0.91893853320392274178032873640561763848639747363578341281714839048276569059392706, 0.86602540378443864676372245847559146504637980913707518024686843746360008840248952, 1.7320508075686764942521218177014330317195627998098914682028720076359283744709143, 0.35974083685996704431923449400601138147995083352924522367450325609041184045102513, 0.28867513459481288225457535894816754557583978441891273868816090596933581319900701, 1.3481860080024781859789019448856947131174549434920888290618386129086420090827543, 1.196826841203998827109536544282108830367449316473405799177350721025640283210708},
  {1e+16, 0.79788456080286533593277809979712609987053550217875322291956207089436119492356425, -0.91893853320467266678032973640560763986139747363640841281715154028276569592726037, 0.86602540378443864676372317075292906069795542183680786141907671511761867157180694, 1.7320508075688772734475188096220929847754231722067180073289332227417383461112834, 0.359740836859775878425884459756945610707051701698370178916587591708796122206294, 0.28867513459481288225457439025098841479568907452875758143392601625649832230956233, 1.3481860080022125973954499108750022049665271285802626095148447020020733241632392, 1.1968268412042980038991671496956891498058032532681298343793431063415417923853464},
  {1e+100, 0.79788456080286535587989211986876373695171726232986931533185165934131585179860368, -0.9189385332046727417803297364056176398613974736377834128171515404827656959272604, 0.86602540378443864676372317075293618347140262690519031402790348972596650845440002, 1.7320508075688772935274463415058723669428052538103806280558069794519330169088, 0.35974083685977585930738327466805832244883702890351179337749095448011705196370266, 0.28867513459481288225457439025097872782380087563506343800930116324198883615146667, 1.3481860080022125708339354142658019486118753512291075843123952899536145308727414, 1.1968268412042980338198381798031456054275758934948039729977774890119737776979055},
  {1e+300, 0.79788456080286535587989211986876373695171726232986931533185165934131585179860368, -0.9189385332046727417803297364056176398613974736377834128171515404827656959272604, 0.86602540378443864676372317075293618347140262690519031402790348972596650845440002, 1.7320508075688772935274463415058723669428052538103806280558069794519330169088, 0.35974083685977585930738327466805832244883702890351179337749095448011705196370266, 0.28867513459481288225457439025097872782380087563506343800930116324198883615146667, 1.3481860080022125708339354142658019486118753512291075843123952899536145308727414, 1.1968268412042980338198381798031456054275758934948039729977774890119737776979055},
  {1.7976931348623157e+308, 0.79788456080286535587989211986876373695171726232986931533185165934131585179860368, -0.9189385332046727417803297364056176398613974736377834128171515404827656959272604, 0.86602540378443864676372317075293618347140262690519031402790348972596650845440002, 1.7320508075688772935274463415058723669428052538103806280558069794519330169088, 0.35974083685977585930738327466805832244883702890351179337749095448011705196370266, 0.28867513459481288225457439025097872782380087563506343800930116324198883615146667, 1.3481860080022125708339354142658019486118753512291075843123952899536145308727414, 1.1968268412042980338198381798031456054275758934948039729977774890119737776979055},
 };
 for(const auto& c:cases) {
  CAPTURE(c[0]);
  const auto ulp=[](double x){return std::nextafter(x,INFINITY)-x;};
  CHECK(std::abs(cppunivariate::dens::ged_abs_moment(c[0])-c[3])<=4*ulp(c[3]));
  CHECK(std::abs(cppunivariate::dens::ged_lambda(c[0])-c[4])<=4*ulp(c[4]));
  CHECK(std::abs(dist::dsged(0,0,1,c[0],2,false)-c[6])<=8*ulp(c[6]));
  double a=0,b=0;
  CHECK(cppunivariate::dens::sged_sigma(c[0],2,a)==dist::fs::sged_sigma(c[0],2,b));
  CHECK(a==b);
 }
}

TEST_CASE("sged: small-increment log gamma agrees with 80-digit references") {
 const double cases[][2]={
  {1e-308, -5.7721566490153280826841984525647051417999497480638003631812876085443689594064025e-309},
  {1e-16, -0.000000000000000057721566490153276629472381367107437879039993502740737372768433332244567978342429},
  {1e-08, -0.0000000057721565667686257851077259263570853071943007679733487403206826576310599835744256},
  {0.01, -0.005690307946069645638795238891835129383593648711669048931228118669342384852751618},
  {0.03125, -0.017246775001768067402891262022246231797366446505106380535078130887485701437593237},
 };
 CHECK(cppunivariate::special::lgamma1p(0)==0);
 for(const auto& c:cases) {
  CAPTURE(c[0]);
  const double ulp=std::nextafter(c[1],INFINITY)-c[1];
  CHECK(std::abs(cppunivariate::special::lgamma1p(c[0])-c[1])<=4*ulp);
 }
}

TEST_CASE("sged fixed shape and both held moments enclose free skew globally") {
  const std::vector<double> x{-3,-1,-.2,0,.3,.4,2};const auto data=mltest::as_span(x);
  for(double p:{1.,1.7}) {
    const auto r=cppunivariate::sged_mle(data,1e-10,200,.1,1.4,p);
    REQUIRE(r.status==cppunivariate::SgedStatus::Ok);CHECK(r.certified);
    CHECK(r.mu==.1);CHECK(r.sigma==1.4);CHECK(r.nu==p);
    CHECK(r.stop_detail.loglik_lower<=r.logLik);CHECK(r.stop_detail.loglik_upper>=r.logLik);
    const cppunivariate::sged_held_moment::Profile profile(data,.1,1.4,p);
    CHECK(profile.value(std::tanh(std::log(r.xi)))==doctest::Approx(r.logLik).epsilon(2e-12));
    for(double xi:{.2,.7,1.,2.,10.})CHECK(profile.value(std::tanh(std::log(xi)))<=r.logLik+1e-9);
  }
  const auto r=cppunivariate::sged_mle(data,1e-10,1,.1,1.4,1.7);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);CHECK_FALSE(r.certified);
  CHECK(r.stop_detail.budget_exhausted);CHECK_FALSE(r.stop_detail.candidates.empty());
  CHECK(r.stop_detail.loglik_upper==INFINITY);CHECK(r.iterations<=128);
}
TEST_CASE("sged constrained skew interval values and scores cover smooth samples") {
  const std::vector<double> x{-3,-1,-.2,0,.3,.4,2};const auto data=mltest::as_span(x);
  for(double p:{.5,1.,1.7,4.,10.}) {
    const cppunivariate::sged_held_moment::Profile profile(data,.1,1.4,p);REQUIRE(profile.valid);
    for(double z:{-.9,-.4,.05,.6,.95}) {
      const auto b=profile.box(z-.001,z+.001);REQUIRE(b.valid);
      for(double t:{z-.001,z,z+.001}) {
        CHECK(profile.value(t)<=b.upper);
        const double h=1e-6,score=(profile.value(t+h)-profile.value(t-h))/(2*h);
        CHECK(score>=b.score.lo-1e-5);CHECK(score<=b.score.hi+1e-5);
      }
    }
    for(const auto& b:{profile.box(-1,-.99),profile.box(.99,1)}) {
      REQUIRE(b.valid);
      for(double fraction:{.1,.5,.9})CHECK(profile.value(b.lo+fraction*(b.hi-b.lo))<=b.upper);
    }
  }
  const auto r=cppunivariate::sged_mle(data,1e-10,200,.1,1.4,1e308);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);CHECK_FALSE(r.certified);
}
TEST_CASE("sged both held moments admit two distinct finite Laplace skew maxima") {
  const std::vector<double> x{-1.1,1.1};
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-9,200,0,1,1);
  REQUIRE(r.status==cppunivariate::SgedStatus::CertificateFailure);
  CHECK(r.stop_detail.comparison_tie);CHECK_FALSE(r.certified);
  REQUIRE(r.stop_detail.candidates.size()>=2);CHECK(std::isfinite(r.stop_detail.loglik_upper));
  double smallest=INFINITY,largest=0;
  for(const auto& c:r.stop_detail.candidates) {
    REQUIRE(c.coef.size()==4);CHECK(c.coef[0]==0);CHECK(c.coef[1]==1);CHECK(c.coef[2]==1);
    smallest=std::min(smallest,c.coef[3]);largest=std::max(largest,c.coef[3]);
  }
  CHECK(smallest<.8);CHECK(largest>1.25);CHECK(smallest*largest==doctest::Approx(1).epsilon(2e-8));
}
TEST_CASE("sged both held moments certify strict support half GED endpoints") {
  for(double sign:{-1.,1.}) {
    const std::vector<double> x{sign*.2,sign*.2};
    const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-10,200,0,1,20);
    REQUIRE(r.status==(sign>0?cppunivariate::SgedStatus::LeftHalfGedBoundary:cppunivariate::SgedStatus::RightHalfGedBoundary));
    CHECK(r.certified);REQUIRE(r.stop_detail.limit_coef.size()==3);
    const auto& c=r.stop_detail.limit_coef;
    const double k=std::exp(cppunivariate::special::lgamma(.1)-cppunivariate::special::lgamma(.05));
    const double v=std::exp(cppunivariate::special::lgamma(.15)-cppunivariate::special::lgamma(.05));
    CHECK(std::abs(c[0]-sign*k*c[1])<=1e-12);
    CHECK(c[1]*std::sqrt(v-k*k)==doctest::Approx(1).epsilon(1e-12));
    double ll=2*(std::log(20.)-cppunivariate::special::lgamma(.05)-std::log(c[1]));
    for(double t:x){const double distance=-sign*(t-c[0])/c[1];CHECK(distance>0);ll-=std::pow(distance,20);}
    CHECK(ll==doctest::Approx(r.logLik).epsilon(1e-12));
    CHECK(r.stop_detail.loglik_lower<=ll);CHECK(r.stop_detail.loglik_upper>=ll);
  }
  const std::vector<double> x{0,0};
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-10,200,0,1,20);
  CHECK(r.stop_detail.comparison_tie);REQUIRE(r.stop_detail.candidates.size()==2);
  CHECK(r.stop_detail.candidates[0].model!=r.stop_detail.candidates[1].model);
}
TEST_CASE("sged held moment half GED contacts never use a closed support substitute") {
  const std::vector<double> x{-1,.2};
  const auto r=cppunivariate::sged_mle(mltest::as_span(x),1e-10,200,0,1,1);
  CHECK(r.status==cppunivariate::SgedStatus::CertificateFailure);CHECK_FALSE(r.certified);
  CHECK_FALSE(r.stop_detail.budget_exhausted);CHECK(r.stop_detail.loglik_upper==INFINITY);
}
