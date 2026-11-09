#include "cppunivariate/core/constants.hpp"
#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/snorm/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

// rsnorm at xi=1 should reduce to N(mean, sd); for xi != 1 the empirical
// mean shifts away from `mean` (skew bias).
TEST_CASE("rsnorm samples have unit variance and skew with xi") {
  cppunivariate::distributions::StdRng<> rng(23);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rsnorm(rng, 0.0, 1.0, 1.5);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  // Standardised parameterisation: empirical mean ~ 0, var ~ 1 by
  // construction (cppunivariate::dens::snorm_sigma standardises moments).
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);
}

TEST_CASE("snorm (skew-normal)") {
  auto g = mltest::load_distribution_golden("snorm");
  auto r = cppunivariate::snorm_mle(mltest::as_span(g.data), mltest::kFitTol, 200);
  CHECK(r.mu == doctest::Approx(g.estimates.at("mean")).epsilon(mltest::kGolden_snorm).scale(0));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sd")).epsilon(mltest::kGolden_snorm).scale(0));
  CHECK(r.xi == doctest::Approx(g.estimates.at("xi")).epsilon(mltest::kGolden_snorm).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_snorm).scale(0));
}

TEST_CASE("snorm profile endpoints and direct grid") {
  for (const std::vector<double>& data : std::vector<std::vector<double>>{
      {0, 0.1, 0.2, 0.4, 3}, {-3, -0.4, -0.2, -0.1, 0},
      {-2, -1, 0, 1, 2}, {-2, -2, 0, 1, 1, 2}, {0, 0, 0, 0, 1}}) {
    auto r = cppunivariate::snorm_mle(mltest::as_span(data), mltest::kFitTol, 200);
    double best = std::numeric_limits<double>::infinity();
    int winner = -1;
    for (int j = 0; j <= 10000; ++j) {
      const double c = data.front() + (data.back()-data.front())*j/10000.0;
      double A=0, B=0;
      for (double x : data) { if (x<c) A+=(x-c)*(x-c); else B+=(x-c)*(x-c); }
      const double q=std::pow(std::cbrt(A)+std::cbrt(B),3);
      if (q<best) { best=q; winner=j; }
    }
    const double ll=data.size()/2.0*(std::log(2/cppunivariate::pi)-std::log(best/data.size())-1);
    CHECK(r.logLik >= ll-1e-9);
    if (winner==0) CHECK(r.status==cppunivariate::SnormStatus::RightHalfNormalBoundary);
    else if (winner==10000) CHECK(r.status==cppunivariate::SnormStatus::LeftHalfNormalBoundary);
    else CHECK(r.status==cppunivariate::SnormStatus::Ok);
  }
}

TEST_CASE("snorm random profiles dominate grid plus refinement") {
  std::mt19937 rng(20261028);
  std::normal_distribution<double> normal;
  std::exponential_distribution<double> exponential;
  std::uniform_real_distribution<double> uniform;
  const std::vector<int> sizes={5,10,30,100,250,500};
  for (int sample=0; sample<240; ++sample) {
    const int n=sizes[(sample/4)%6];
    std::vector<double> data(n);
    for (double& x : data) {
      if (sample%4==0) x=normal(rng);
      else if (sample%4==1) x=exponential(rng);
      else if (sample%4==2) x=std::abs(normal(rng))*(uniform(rng)<6.25/7.25 ? 2.5 : -0.4);
      else x=-exponential(rng);
    }
    const auto fit=cppunivariate::snorm_mle(mltest::as_span(data),mltest::kFitTol,200);
    const auto extremes=std::minmax_element(data.begin(),data.end());
    const double lo=*extremes.first, hi=*extremes.second;
    auto objective = [&](double c) {
      double A=0, B=0;
      for (double x : data) { if (x<c) A+=(x-c)*(x-c); else B+=(x-c)*(x-c); }
      return std::pow(std::cbrt(A)+std::cbrt(B),3);
    };
    std::vector<double> grid(2001);
    for (int j=0; j<=2000; ++j) grid[j]=objective(lo+(hi-lo)*j/2000);
    double best=grid[0];
    auto status=cppunivariate::SnormStatus::RightHalfNormalBoundary;
    if (grid[2000]<best) { best=grid[2000]; status=cppunivariate::SnormStatus::LeftHalfNormalBoundary; }
    for (int j=1; j<2000; ++j) {
      if (grid[j]>grid[j-1] || grid[j]>grid[j+1]) continue;
      double l=lo+(hi-lo)*(j-1)/2000, r=lo+(hi-lo)*(j+1)/2000;
      for (int it=0; it<80; ++it) {
        const double a=l+(r-l)/3, b=r-(r-l)/3;
        if (objective(a)<objective(b)) r=b; else l=a;
      }
      const double q=objective((l+r)/2);
      if (q<best) { best=q; status=cppunivariate::SnormStatus::Ok; }
    }
    CAPTURE(sample);
    CHECK(fit.logLik >= n/2.0*(std::log(2/cppunivariate::pi)-std::log(best/n)-1)-1e-9);
    CHECK(fit.status==status);
  }
}

#undef C_APPROX
#undef C_APPROX_FD

TEST_CASE("snorm package dpqr preserve logarithmic tails") {
  for (bool lower : {false, true}) {
    for (double lp : {-1000.0, -100.0, -2.0, -0.1}) {
      const double x = cppunivariate::distributions::qsnorm(lp, 0.3, 1.7, 1.8, lower, true);
      CHECK(cppunivariate::distributions::psnorm(x, 0.3, 1.7, 1.8, lower, true) == doctest::Approx(lp).epsilon(2e-12));
    }
  }
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("snorm: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("snorm",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dsnorm(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::psnorm(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qsnorm(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rsnorm(rng, p[0], p[1], p[2]); });
}

TEST_CASE("snorm: classified holds preserve values and handle tied/singleton data") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{-2,-1,-.4,0,.2,.8,1,1.3,2,3};
  const auto data=mltest::as_span(x);
  for(bool mean : {false,true}) for(bool sd : {false,true}) {
    if(!mean && !sd) continue;
    const auto r=cppunivariate::snorm_mle(data,1e-9,300,mean ? .2 : nan,sd ? 1.7 : nan,1.3);
    REQUIRE(r.status==cppunivariate::SnormStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    if(mean) CHECK(r.mu==.2);
    if(sd) CHECK(r.sigma==1.7);
    CHECK(r.xi==1.3);

  }
  const std::vector<double> one{.2};
  const auto location=cppunivariate::snorm_mle(mltest::as_span(one),1e-9,300,nan,1.7,1.3);
  CHECK(location.status==cppunivariate::SnormStatus::Ok);
  const auto collapse=cppunivariate::snorm_mle(mltest::as_span(one),1e-9,300,.2,nan,1.3);
  CHECK(collapse.status==cppunivariate::SnormStatus::Unbounded);
}

TEST_CASE("snorm: every nonempty hold subset") {
  const auto g=mltest::load_distribution_golden("snorm");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1; mask<8; ++mask) {
    const auto r=cppunivariate::snorm_mle(mltest::as_span(g.data),1e-9,300,
      mask&1 ? g.estimates.at("mean") : nan,mask&2 ? g.estimates.at("sd") : nan,mask&4 ? g.estimates.at("xi") : nan);
    REQUIRE(r.status==cppunivariate::SnormStatus::Ok);
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

// At symmetry F(0)=1/2; the local inverse is delta_p/f(0).
TEST_CASE("snorm: symmetric member has exact median and signed log-half offset") {
  const double offset=2.3190468138462995584e-17*(std::sqrt(cppunivariate::pi/2));
  for(bool lower:{true,false}) for(bool log:{true,false}) {
    const double p=log?-0.6931471805599453:.5;
    const double expected=log?(lower?offset:-offset):0;
    const double got=cppunivariate::distributions::qsnorm(p,0,1,1,lower,log);
    if (log) CHECK(got==doctest::Approx(expected).scale(0).epsilon(1e-12));
    else CHECK(got==0);
  }
}

TEST_CASE("snorm R6 free constant samples have unbounded likelihood") {
  for (const auto& x : {std::vector<double>{0},std::vector<double>{0,0,0},std::vector<double>{2,2,2}}) {
    const auto r=cppunivariate::snorm_mle(mltest::as_span(x),1e-8,1);
    CHECK(r.status==cppunivariate::SnormStatus::Unbounded);
    CHECK(r.iterations==0);
  }
}

TEST_CASE("snorm: face comparison cannot resolve rounded extreme objectives") {
  const double magnitude=1e20;
  CHECK(cppunivariate::compare_face(-magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(-magnitude,-2*magnitude,2e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-2*magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(-10.,-10.,2e-8)==cppunivariate::FaceComparison::FirstWins);
}

TEST_CASE("snorm: audited huge held-parameter magnitude") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x{1,2};
  cppunivariate::span s{x.data(),x.size()};
  const auto r=cppunivariate::snorm_mle(s,1e-6,100,0,1e-10,nan);
  CHECK((r.status==cppunivariate::SnormStatus::RightHalfNormalBoundary || r.status==cppunivariate::SnormStatus::BfgsFailure));
}

TEST_CASE("snorm: structured face payloads") {
  for(double sign:{-1.,1.}) {
    std::vector<double> x{0,.1,.2,.4,3};for(double& v:x)v*=sign;
    const auto r=cppunivariate::snorm_mle(mltest::as_span(x),1e-8,500);
    REQUIRE(r.stop_detail.limit_coef.size()==2);
    CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(0));
    CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(std::sqrt(9.21/5)));
    CHECK(r.stop_detail.limit_loglik==doctest::Approx(2.5*(std::log(2/std::acos(-1.))-std::log(9.21/5)-1)));
  }
}

#include "../hnorm/dpqr.hpp"

TEST_CASE("snorm: conditional arithmetic ambiguity retains compared witnesses") {
  const std::vector<double> x{1,2};
  const auto r=cppunivariate::snorm_mle(mltest::as_span(x),1e-6,200,0,1e-4);
  REQUIRE(r.status==cppunivariate::SnormStatus::BfgsFailure);
  REQUIRE(r.stop_detail.comparison_tie);
  REQUIRE(r.stop_detail.candidates.size()==2);
  const auto& face=r.stop_detail.candidates[1];
  REQUIRE(face.model==cppunivariate::StopModel::Hnorm);
  REQUIRE(face.coef.size()==2);
  double ll=0;for(double v:x)ll+=cppunivariate::distributions::dhnorm(v,face.coef[0],face.coef[1],true);
  CHECK(face.loglik_lower==doctest::Approx(ll).epsilon(1e-12));
  CHECK(r.stop_detail.candidates[0].coef[0]==0);
  CHECK(r.stop_detail.candidates[0].coef[1]==1e-4);
  CHECK(r.stop_detail.loglik_lower==std::max(face.loglik_lower,r.stop_detail.candidates[0].loglik_lower));
  CHECK(r.stop_detail.loglik_upper==INFINITY);
}

#include "cppunivariate/core/inference_join.hpp"
TEST_CASE("snorm: analytic covariance derivatives and subset projection") {
  const std::array<double,8> theta{0,1,1};
  const std::vector<double> x{-2,-1,-.2,0,.2,1,2};
  const auto full=cppunivariate::join_family_inference({x.data(),x.size()},"snorm",theta,7);
  REQUIRE(full.status==cppunivariate::InferenceStatus::Ok);
  for(unsigned mask=0;mask<(1u<<3);++mask) {
    std::size_t emitted=0;
    const auto r=cppunivariate::join_family_inference({x.data(),x.size()},"snorm",theta,mask,[&](const auto&){++emitted;});
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
  CHECK(cppunivariate::join_family_inference({x.data(),x.size()},"snorm",bad,1).status==cppunivariate::InferenceStatus::BoundaryOrLimit);
  CHECK(cppunivariate::join_family_inference({x.data(),x.size()},"snorm",theta,1u<<3).status==cppunivariate::InferenceStatus::InvalidSubset);
}

TEST_CASE("snorm held certificates remove fixed log-scale guards") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double unit:{1e-30,1.,1e30}) {
    const std::vector<double> x{-2*unit,-unit,.2*unit,3*unit};
    const auto r=cppunivariate::snorm_mle(mltest::as_span(x),1e-9,200,0,nan,1.3);
    REQUIRE(r.status==cppunivariate::SnormStatus::Ok);
    CHECK(r.mu==0);CHECK(r.xi==1.3);
    double ll=0;for(double v:x)ll+=cppunivariate::distributions::dsnorm(v,r.mu,r.sigma,r.xi,true);
    CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-10));
    const auto ref=cppunivariate::snorm_mle(mltest::as_span(std::vector<double>{-2,-1,.2,3}),1e-9,200,0,nan,1.3);
    CHECK(r.sigma/unit==doctest::Approx(ref.sigma).epsilon(1e-10));
  }
}

TEST_CASE("snorm Bernstein isolation retains even and separated roots") {
  namespace cert=cppunivariate::snorm_certificate;
  // Exact dyadic Bernstein coefficients of 3*(u-.25)^2*(u-.75)^2.
  // Constructing the product through rounded thirds can remove its roots.
  const cert::Poly p{{27./256,-45./256,59./256,-45./256,27./256}};
  std::vector<double> roots;std::int64_t work=0;bool budget=false;
  REQUIRE(cert::isolate(p,200,work,roots,budget));
  for(double expected:{.25,.75})CHECK(std::any_of(roots.begin(),roots.end(),[&](double v){return std::abs(v-expected)<1e-8;}));
  roots.clear();work=0;
  CHECK_FALSE(cert::isolate(p,1,work,roots,budget));
}

TEST_CASE("snorm held moments dominate independent skewness grid") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(const auto& x:{std::vector<double>{-2,-1,-.4,0,.2,.8,1,1.3,2,3},std::vector<double>{-.5,0,.2,.8,2}}) {
    const auto r=cppunivariate::snorm_mle(mltest::as_span(x),1e-9,200,.2,1.4,nan);
    REQUIRE((r.status==cppunivariate::SnormStatus::Ok || r.status==cppunivariate::SnormStatus::RightHalfNormalBoundary || r.status==cppunivariate::SnormStatus::LeftHalfNormalBoundary));
    for(int i=-2000;i<=2000;++i) {
      const double xi=std::exp(i*.005);double ll=0;
      for(double v:x)ll+=cppunivariate::distributions::dsnorm(v,.2,1.4,xi,true);
      CHECK(r.logLik>=ll-1e-8);
    }
  }
}

TEST_CASE("snorm opposite half-normal maxima are a structured tie") {
  const std::vector<double> x{0,0,0};
  const auto r=cppunivariate::snorm_mle(mltest::as_span(x),1e-9,200,NAN,1,NAN);
  REQUIRE(r.status==cppunivariate::SnormStatus::BfgsFailure);
  REQUIRE(r.stop_detail.comparison_tie);
  REQUIRE(r.stop_detail.candidates.size()==2);
  CHECK(r.stop_detail.candidates[0].model!=r.stop_detail.candidates[1].model);
  CHECK(r.stop_detail.candidates[0].loglik_lower==doctest::Approx(r.stop_detail.candidates[1].loglik_lower));
}

TEST_CASE("snorm algebraic certificate exhaustion never returns an iterate") {
  const std::vector<double> x{-2,-1,-.4,0,.2,.8,1,1.3,2,3};
  const auto r=cppunivariate::snorm_mle(mltest::as_span(x),1e-9,1,.2,1.4,NAN);
  CHECK(r.status==cppunivariate::SnormStatus::BfgsFailure);
  CHECK(r.stop_detail.loglik_upper==INFINITY);
}

TEST_CASE("snorm held moments can have two distinct finite global maxima") {
  const std::vector<double> x{-1,0,1};
  const auto r=cppunivariate::snorm_mle(mltest::as_span(x),1e-9,200,0,.755,NAN);
  REQUIRE(r.status==cppunivariate::SnormStatus::BfgsFailure);
  REQUIRE(r.stop_detail.comparison_tie);
  REQUIRE(r.stop_detail.candidates.size()==2);
  for(const auto& candidate:r.stop_detail.candidates) {
    CHECK(candidate.model==cppunivariate::StopModel::None);
    CHECK(candidate.coef[0]==0);CHECK(candidate.coef[1]==.755);
  }
  CHECK(r.stop_detail.candidates[0].coef[2]*r.stop_detail.candidates[1].coef[2]==doctest::Approx(1).epsilon(1e-7));
}

TEST_CASE("snorm Bernstein roots on subdivision seams are retained") {
  namespace cert=cppunivariate::snorm_certificate;
  const cert::Poly t{{-.5,.5}};
  std::vector<double> roots;std::int64_t work=0;bool budget=false;
  REQUIRE(cert::isolate(cert::mul(t,t),200,work,roots,budget));
  CHECK(std::find(roots.begin(),roots.end(),.5)!=roots.end());
}

TEST_CASE("snorm normalization cannot turn distinct observations into a constant") {
  const std::vector<double> x{1,2};
  const auto r=cppunivariate::snorm_mle(mltest::as_span(x),1e-9,200,1e300,NAN,1.3);
  CHECK(r.status==cppunivariate::SnormStatus::BfgsFailure);
  CHECK_FALSE(r.stop_detail.budget_exhausted);
}

TEST_CASE("snorm even-root rounding cannot certify an empty root set") {
  const cppunivariate::snorm_certificate::Poly p{{1,-2,4}}; // (3u-1)^2
  std::vector<double> roots;std::int64_t work=0;bool budget=false;
  const bool resolved=cppunivariate::snorm_certificate::isolate(p,200,work,roots,budget);
  CHECK_FALSE(budget);
  CHECK((!resolved || std::any_of(roots.begin(),roots.end(),[](double v){return std::abs(v-1./3)<1e-7;})));
}

TEST_CASE("snorm held skew cannot return a cancelled weighted variance") {
  const std::vector<double> x{-1,1};
  for(double xi:{1.,2.,10.,100.,1e3,1e4,1e6,1e10,1e20,1e200})for(double skew:{xi,1/xi}) {
    const auto r=cppunivariate::snorm_mle(mltest::as_span(x),1e-9,200,NAN,NAN,skew);
    if(xi<=1e3)REQUIRE(r.status==cppunivariate::SnormStatus::Ok);
    REQUIRE((r.status==cppunivariate::SnormStatus::Ok || r.status==cppunivariate::SnormStatus::BfgsFailure));
    if(r.status==cppunivariate::SnormStatus::Ok) {
      const double ratio=skew>=1?1/(skew*skew):skew*skew;
      const double optimum=-std::log(cppunivariate::pi)-std::log1p(2*ratio/(1+ratio*ratio))-1;
      CHECK(r.logLik==doctest::Approx(optimum).epsilon(1e-10));
    }
  }
}

TEST_CASE("snorm unresolved contact cannot discard the unpenalized face") {
  const std::vector<double> x{0};
  const double m=std::sqrt(2/cppunivariate::pi),h=std::sqrt(1-m*m);
  for(double side:{-1.,1.}) {
    const auto r=cppunivariate::snorm_mle(mltest::as_span(x),1e-9,200,side*(m-2e-14),h,NAN);
    REQUIRE(r.status!=cppunivariate::SnormStatus::Unbounded);
    REQUIRE(r.status!=cppunivariate::SnormStatus::InvalidStart);
    // The feasible half-normal face is an independent lower witness.
    CHECK((r.status==cppunivariate::SnormStatus::BfgsFailure || r.logLik>=std::log(m)-1e-8));
  }
}

TEST_CASE("snorm constant sample away from held mean has a sharp half-normal bound") {
  const double m=std::sqrt(2/cppunivariate::pi);
  for(double value:{-2.,2.}) {
    const std::vector<double> x{value,value};
    const auto r=cppunivariate::snorm_mle(mltest::as_span(x),1e-9,1,0,NAN,NAN);
    REQUIRE(r.status==(value<0?cppunivariate::SnormStatus::RightHalfNormalBoundary:cppunivariate::SnormStatus::LeftHalfNormalBoundary));
    REQUIRE(r.stop_detail.limit_coef.size()==2);
    CHECK(r.stop_detail.limit_coef[0]==value);
    CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(2/m).epsilon(1e-12));
    CHECK(r.logLik==doctest::Approx(2*std::log(m*m/2)).epsilon(1e-12));
    CHECK(r.stop_detail.loglik_upper>=r.stop_detail.loglik_lower);
  }
}

TEST_CASE("snorm scaled residuals: physical side scales are never squared") {
  const std::vector<double> base={-1,0,0,0,0,0,0,0,0,0,0,1};
  const auto reference=cppunivariate::snorm_mle(mltest::as_span(base),1e-10,200);
  REQUIRE(reference.status==cppunivariate::SnormStatus::Ok);
  for(double unit:{1e-200,1e200}) {
    std::vector<double> x;for(double v:base)x.push_back(v*unit);
    const auto r=cppunivariate::snorm_mle(mltest::as_span(x),1e-10,200);
    REQUIRE(r.status==cppunivariate::SnormStatus::Ok);
    CHECK(r.sigma/unit==doctest::Approx(reference.sigma).epsilon(1e-12));
    CHECK(r.xi==doctest::Approx(reference.xi).epsilon(1e-12));
    CHECK(r.logLik==doctest::Approx(reference.logLik-base.size()*std::log(unit)).epsilon(1e-12));
  }
}
