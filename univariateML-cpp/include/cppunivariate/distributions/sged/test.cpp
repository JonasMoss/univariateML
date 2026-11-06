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

TEST_CASE("sged: P0b uniform endpoints retain pinned-moment penalties") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{-std::sqrt(3.0),std::sqrt(3.0)};
  const auto pinned=cppunivariate::sged_mle(mltest::as_span(x),1e-6,500,0,1,nan,1);
  CHECK(pinned.status==cppunivariate::SgedStatus::UniformBoundary);
  CHECK(pinned.logLik==doctest::Approx(-2*std::log(std::sqrt(12.0))-2*std::exp(-0.5772156649015328606)));
  const auto width=cppunivariate::sged_mle(mltest::as_span(x),1e-6,500,nan,1,nan,1);
  CHECK(width.status==cppunivariate::SgedStatus::ScaleBoundary);
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
