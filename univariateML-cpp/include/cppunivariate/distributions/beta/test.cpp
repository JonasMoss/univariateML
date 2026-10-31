#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/beta/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

TEST_CASE("beta") {
  auto g = mltest::load_distribution_golden("beta");
  auto r = cppunivariate::beta_mle(mltest::as_span(g.data), 1.490116e-08, mltest::kFitMaxit);
  CHECK(r.alpha == C_APPROX(g.estimates.at("shape1")));
  CHECK(r.beta == C_APPROX(g.estimates.at("shape2")));
  CHECK(r.logLik == C_APPROX(g.logLik));
}

#undef C_APPROX
#undef C_APPROX_FD

TEST_CASE("beta dpqr: U(0,1) special case (shape1=shape2=1)") {
  CHECK(cppunivariate::distributions::dbeta(0.3, 1.0, 1.0, false) == doctest::Approx(1.0));
  CHECK(cppunivariate::distributions::pbeta(0.4, 1.0, 1.0, true, false) == doctest::Approx(0.4));
  CHECK(cppunivariate::distributions::qbeta(0.7, 1.0, 1.0, true, false) == doctest::Approx(0.7));
  // Asymmetric round-trip.
  const double q = cppunivariate::distributions::qbeta(0.6, 2.0, 5.0, true, false);
  CHECK(cppunivariate::distributions::pbeta(q, 2.0, 5.0, true, false) == doctest::Approx(0.6));
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("beta: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("beta",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dbeta(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pbeta(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qbeta(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rbeta(rng, p[0], p[1]); });
}

TEST_CASE("beta P2 held subsets and score roots") {
  const std::vector<double> x{.2,.2,.6,.8};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for (int mask=1;mask<4;++mask) {
    const auto r=cppunivariate::beta_mle(mltest::as_span(x),1e-12,100,
      mask&1?2:nan,mask&2?3:nan);
    REQUIRE(r.status==cppunivariate::BetaStatus::Ok);
    if(mask&1) CHECK(r.alpha==2);
    else CHECK(std::abs(cppunivariate::mean_log(mltest::as_span(x))-
      cppunivariate::special::digamma(r.alpha)+cppunivariate::special::digamma(r.alpha+r.beta))<1e-11);
    if(mask&2) CHECK(r.beta==3);
    else {
      double score=0;
      for(double v:x) score+=std::log1p(-v);
      CHECK(std::abs(score/x.size()-cppunivariate::special::digamma(r.beta)+
        cppunivariate::special::digamma(r.alpha+r.beta))<1e-11);
    }
    double ll=0;
    for(double v:x) ll+=cppunivariate::distributions::dbeta(v,r.alpha,r.beta,true);
    CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-12));
  }
  CHECK(cppunivariate::beta_mle(mltest::as_span(x),1e-12,1,2).status==cppunivariate::BetaStatus::IterationFailure);
  const std::vector<double> z(5,.4);
  CHECK(cppunivariate::beta_mle(mltest::as_span(z),1e-12,100,2).status==cppunivariate::BetaStatus::Ok);
}

TEST_CASE("beta P2 held shape retains tiny log complements") {
  const std::vector<double> x(5,1e-100);
  const auto r=cppunivariate::beta_mle(mltest::as_span(x),1e-12,100,1);
  REQUIRE(r.status==cppunivariate::BetaStatus::Ok);
  CHECK(r.beta==doctest::Approx(1e100).epsilon(1e-12));
  CHECK(r.logLik==doctest::Approx(5*(100*std::log(10.0)-1)).epsilon(1e-12));
}


// Generating mpmath snippet (binary64 inputs retained exactly):
// import mpmath as mp; mp.mp.dps=100
// a,b=mp.mpf(2),mp.mpf(3)
// lp=lambda y: mp.log(mp.betainc(a,b,0,mp.exp(y),regularized=True))
// print(lp(mp.log(mp.mpf(float.fromhex('0x0.0000000000001p-1022')))))
// print(mp.exp(mp.findroot(lambda y: lp(y)+800,-400)))
TEST_CASE("beta: targeted tail and endpoint regressions") {
  CHECK(cppunivariate::distributions::pbeta(5e-324, 2.0, 3.0, true, true) == doctest::Approx(-1487.0883843735344).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qbeta(-800.0, 2.0, 3.0, true, true) == doctest::Approx(7.818647138068588e-175).epsilon(1e-10));
  CHECK(cppunivariate::distributions::dbeta(0.0, 2.0, 3.0, true) == -INFINITY);
}

// import mpmath as mp; mp.mp.dps=100
// Beta(1,b) has f(0)=b, and Beta(a,1) has f(1)=a.
TEST_CASE("beta: targeted tail unit-shape endpoint limits") {
  CHECK(cppunivariate::distributions::dbeta(0,1,3,true)==doctest::Approx(std::log(3.0)));
  CHECK(cppunivariate::distributions::dbeta(1,2,1,true)==doctest::Approx(std::log(2.0)));
}

TEST_CASE("beta: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("beta");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::beta_mle(x,1e-8,1).status==cppunivariate::BetaStatus::IterationFailure);
}

TEST_CASE("beta: symmetric median is an exact logit root") {
  CHECK(cppunivariate::distributions::qbeta(0.5,2,2,true,false)==doctest::Approx(0.5));
}

TEST_CASE("beta: TASK-132 invalid data never report success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2.0},std::vector<double>{INFINITY,2.0},std::vector<double>{-INFINITY,2.0},std::vector<double>{-1.0,2.0},std::vector<double>{0.0,2.0},std::vector<double>{1.0,0.5},std::vector<double>{2.0,0.5}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::beta_mle(x,1e-8,200).status != cppunivariate::BetaStatus::Ok);
  }
}

#include "cppunivariate/distributions/beta/score.hpp"
TEST_CASE("beta: inference every free subset and MLE score") {
  std::vector<double> x{0.08,0.2,0.4,0.6,0.8};
  const auto full=cppunivariate::beta_inference(mltest::as_span(x),1.3, 2.7);
  REQUIRE(full.status==cppunivariate::InferenceStatus::Ok);
  for(unsigned mask=0; mask<4; ++mask) {
    std::vector<double> sums(2,0);
    std::array<double,4> products{};
    std::size_t rows=0;
    const auto a=cppunivariate::beta_inference(mltest::as_span(x),1.3, 2.7,mask,
      [&](const std::vector<double>& row) {++rows; for(std::size_t j=0;j<row.size();++j) { sums[j]+=row[j]; for(std::size_t k=0;k<row.size();++k) products[j*row.size()+k]+=row[j]*row[k]; }});
    REQUIRE(a.status==cppunivariate::InferenceStatus::Ok);
    CHECK(rows==x.size());
    CHECK(a.observations==x.size());
    for(std::size_t j=0;j<a.dimension;++j) {
      CHECK(a.score[j]==doctest::Approx(sums[j]));
      CHECK(a.score[j]==doctest::Approx(full.score[a.free_indices[j]]));
      for(std::size_t k=0;k<a.dimension;++k) {
        const auto dst=j*a.dimension+k, src=a.free_indices[j]*2+a.free_indices[k];
        CHECK(a.opg[dst]==doctest::Approx(full.opg[src]));
        CHECK(a.opg[dst]==doctest::Approx(products[dst]));
        CHECK(a.observed[dst]==doctest::Approx(full.observed[src]));
        CHECK(a.fisher[dst]==doctest::Approx(full.fisher[src]));
      }
    }
    const auto f=cppunivariate::beta_mle(mltest::as_span(x),1e-12,2000,(mask&1)?NAN:1.3,(mask&2)?NAN:2.7);
    REQUIRE(f.status==cppunivariate::BetaStatus::Ok);
    const auto fitted=cppunivariate::beta_inference(mltest::as_span(x),f.alpha,f.beta,mask);
    REQUIRE(fitted.status==cppunivariate::InferenceStatus::Ok);
    for(std::size_t j=0;j<fitted.dimension;++j) CHECK(std::abs(fitted.score[j])<1e-7);
  }
  CHECK(cppunivariate::beta_inference(mltest::as_span(x),1.3, 2.7,4).status==cppunivariate::InferenceStatus::InvalidSubset);
  CHECK(cppunivariate::beta_inference(cppunivariate::span{x.data(),0},1.3, 2.7).status==cppunivariate::InferenceStatus::EmptySample);
}

TEST_CASE("beta: analytic information versus log density central differences") {
  std::vector<double> x{0.4}, p{1.3,2.7};
  const auto r=cppunivariate::beta_inference(mltest::as_span(x),p[0],p[1]);
  REQUIRE(r.status==cppunivariate::InferenceStatus::Ok);
  const auto ld=[&]() {return cppunivariate::distributions::dbeta(x[0],p[0],p[1],true);};
  const double step=1e-4, base=ld();
  for(std::size_t j=0;j<2;++j) {
    const double saved=p[j];
    p[j]=saved+step; const double plus=ld();
    p[j]=saved-step; const double minus=ld(); p[j]=saved;
    CHECK(r.score[j]==doctest::Approx((plus-minus)/(2*step)).epsilon(2e-6));
    CHECK(r.observed[j*2+j]==doctest::Approx(-(plus-2*base+minus)/(step*step)).epsilon(2e-6));
  }
  p[0]+=step; p[1]+=step; const double pp=ld();
  p[1]-=2*step; const double pm=ld();
  p[0]-=2*step; const double mm=ld();
  p[1]+=2*step; const double mp=ld();
  CHECK(r.observed[1]==doctest::Approx(-(pp-pm-mp+mm)/(4*step*step)).epsilon(2e-6));
}

TEST_CASE("beta: inference validity and total normalization") {
  std::vector<double> x{.4};
  const auto one=cppunivariate::beta_inference(mltest::as_span(x),2,3);
  x.push_back(x[0]);
  const auto two=cppunivariate::beta_inference(mltest::as_span(x),2,3);
  REQUIRE(two.status==cppunivariate::InferenceStatus::Ok);
  for(std::size_t j=0;j<two.dimension;++j) CHECK(two.score[j]==doctest::Approx(2*one.score[j]));
  for(std::size_t j=0;j<two.dimension*two.dimension;++j) {
    CHECK(two.opg[j]==doctest::Approx(2*one.opg[j]));
    CHECK(two.observed[j]==doctest::Approx(2*one.observed[j]));
    CHECK(two.fisher[j]==doctest::Approx(2*one.fisher[j]));
  }
  CHECK(cppunivariate::beta_inference(mltest::as_span(x),2,-1).status==cppunivariate::InferenceStatus::InvalidParameter);
  x[0]=1;
  CHECK(cppunivariate::beta_inference(mltest::as_span(x),2,3,0).status==cppunivariate::InferenceStatus::InvalidObservation);
}

TEST_CASE("beta: small concentrated and imbalanced inference") {
  for(double a : {1e-3,1.,1e6}) for(double b : {1e-3,1.,1e6}) {
    std::vector<double> x{a/(a+b)};
    const auto r=cppunivariate::beta_inference(mltest::as_span(x),a,b);
    CHECK(r.status==cppunivariate::InferenceStatus::Ok);
    CHECK(r.fisher[0]>0);
    CHECK(r.fisher[3]>0);
    CHECK(r.fisher_condition>=1);
  }
}

TEST_CASE("beta: cancellation-safe differences obey special-function recurrences") {
  for(double z : {19.,20.,99.,100.,1000.,1e5,1e6}) {
    CHECK(cppunivariate::inference_detail::beta_psi_increment(z,1)==doctest::Approx(1/z).epsilon(2e-12));
    CHECK(cppunivariate::inference_detail::beta_tri_difference(z,1)==doctest::Approx(1/z/z).epsilon(2e-12));
  }
}

TEST_CASE("beta mean nuisance concentration inference") {
  std::vector<double> x{.1,.2,.4,.6,.8};cppunivariate::span data{x.data(),x.size()};
  const auto r=cppunivariate::beta_mean_inference(data,.5);
  REQUIRE(r.status==cppunivariate::InferenceStatus::Ok);
  const auto ci=cppunivariate::beta_mean_lr_interval(data,3.841458820694124);
  REQUIRE(ci.status==cppunivariate::InferenceStatus::Ok);
  CHECK(ci.lower<r.estimate);CHECK(ci.upper>r.estimate);
  for(double t:{ci.lower,ci.upper}) CHECK(cppunivariate::beta_mean_inference(data,t).likelihood_ratio.statistic==doctest::Approx(3.841458820694124).epsilon(1e-7));
}
