#include <algorithm>
#include "test_support.hpp"
#include "cppunivariate/distributions/gamma/mle.hpp"
#include "cppunivariate/distributions/gamma/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>

TEST_CASE("gamma") {
  auto g = mltest::load_distribution_golden("gamma");
  auto r = cppunivariate::gamma_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kGolden_gamma).scale(0));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_gamma).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_gamma).scale(0));
}

TEST_CASE("gamma dpqr: scale parameterisation round-trip") {
  // mean = shape * scale.
  const double q = cppunivariate::distributions::qgamma(0.5, 2.0, 1.0, true, false);
  CHECK(cppunivariate::distributions::pgamma(q, 2.0, 1.0, true, false) == doctest::Approx(0.5));
  // density at mean is positive and finite.
  CHECK(cppunivariate::distributions::dgamma(2.0, 2.0, 1.0, false) > 0.0);
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("gamma: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("gamma",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dgamma(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pgamma(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qgamma(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rgamma(rng, p[0], p[1]); });
}

TEST_CASE("gamma P2 every held subset") {
  const auto g=mltest::load_distribution_golden("gamma");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("shape"),g.estimates.at("scale")};
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.scale));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dgamma(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,nan,held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dgamma(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,held[0],held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dgamma(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("gamma P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::gamma_mle(mltest::as_span(x),1e-10,1,nan,0.5).status==cppunivariate::GammaStatus::IterationFailure);
  CHECK(cppunivariate::gamma_mle(mltest::as_span(x),1e-10,100,2,nan).scale==doctest::Approx(1));
}

TEST_CASE("gamma held shape profiles scale without intermediate overflow") {
  const std::vector<double> x{1e308,1e308};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,100,1e308,nan);
  CHECK(r.shape==1e308);
  CHECK(r.scale==doctest::Approx(1));
  // The likelihood itself is outside the direct lgamma formula's range;
  // a numerical failure is returned rather than a spurious finite fit.
  CHECK(r.status==cppunivariate::GammaStatus::IterationFailure);
}


// mpmath generator: import mpmath as mp; mp.mp.dps=100
// P=lambda z:mp.gammainc(2,0,z)/mp.gamma(2)
// Q=lambda z:mp.gammainc(2,z,mp.inf)/mp.gamma(2)
// lo=mp.exp(mp.findroot(lambda y:mp.log(P(mp.exp(y)))+800,-mp.mpf(800)/2))
// hi=mp.findroot(lambda z:mp.log(Q(z))+800,800)
// [lo/2,hi/2]
TEST_CASE("gamma: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qgamma(-800.0, 2.0, 0.5, true, true) == doctest::Approx(1.3542294089587789e-174).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qgamma(-800.0, 2.0, 0.5, false, true) == doctest::Approx(403.34709175055565).epsilon(1e-11));
}

TEST_CASE("gamma: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qgamma(cppunivariate::special::kNegInf, 2.0, 0.5, true, true) == 0.0);
  CHECK(cppunivariate::distributions::qgamma(0.0, 2.0, 0.5, false, true) == 0.0);
  CHECK(cppunivariate::distributions::qgamma(cppunivariate::special::kNegInf, 2.0, 0.5, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qgamma(0.0, 2.0, 0.5, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("gamma: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dgamma(1e100, 2.0, 0.5, true) == doctest::Approx(-2e+100).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pgamma(1e100, 2.0, 0.5, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pgamma(1e100, 2.0, 0.5, false, true) == doctest::Approx(-2e+100).epsilon(1e-11));
}

TEST_CASE("gamma: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("gamma");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::gamma_mle(x,1e-8,1).status==cppunivariate::GammaStatus::IterationFailure);
}

TEST_CASE("gamma: README scale validation") {
  cppunivariate::distributions::StdRng<> rng(94);
  for (double scale : {0.0, -1.0, double(INFINITY), double(NAN)}) {
    CHECK(std::isnan(cppunivariate::distributions::dgamma(0.5, 2.0, scale, false)));
    CHECK(std::isnan(cppunivariate::distributions::pgamma(0.5, 2.0, scale, false, true)));
    CHECK(std::isnan(cppunivariate::distributions::qgamma(0.5, 2.0, scale, false, true)));
    CHECK(std::isnan(cppunivariate::distributions::rgamma(rng, 2.0, scale)));
  }
}

TEST_CASE("gamma: finite scale with overflowing reciprocal") {
  const double scale = 1e-320;
  CHECK(std::isfinite(cppunivariate::distributions::dgamma(scale, 2.0, scale, true)));
  CHECK(std::isfinite(cppunivariate::distributions::pgamma(scale, 2.0, scale, false, true)));
  CHECK(cppunivariate::distributions::qgamma(0.5, 2.0, scale, true, false) > 0.0);
}

TEST_CASE("gamma: TASK-132 invalid data never report success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2.0},std::vector<double>{INFINITY,2.0},std::vector<double>{-INFINITY,2.0},std::vector<double>{-1.0,2.0},std::vector<double>{0.0,2.0}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::gamma_mle(x,1e-8,200).status != cppunivariate::GammaStatus::Ok);
  }
}

#include "cppunivariate/distributions/gamma/score.hpp"
TEST_CASE("gamma: inference every free subset and MLE score") {
  std::vector<double> x{0.3,0.8,1.2,2.7,4.1};
  const auto full=cppunivariate::gamma_inference(mltest::as_span(x),2.3, 1.4);
  REQUIRE(full.status==cppunivariate::InferenceStatus::Ok);
  for(unsigned mask=0; mask<4; ++mask) {
    std::vector<double> sums(2,0);
    std::array<double,4> products{};
    std::size_t rows=0;
    const auto a=cppunivariate::gamma_inference(mltest::as_span(x),2.3, 1.4,mask,
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
    const auto f=cppunivariate::gamma_mle(mltest::as_span(x),1e-12,2000,(mask&1)?NAN:2.3,(mask&2)?NAN:1.4);
    REQUIRE(f.status==cppunivariate::GammaStatus::Ok);
    const auto fitted=cppunivariate::gamma_inference(mltest::as_span(x),f.shape,f.scale,mask);
    REQUIRE(fitted.status==cppunivariate::InferenceStatus::Ok);
    for(std::size_t j=0;j<fitted.dimension;++j) CHECK(std::abs(fitted.score[j])<1e-7);
  }
  CHECK(cppunivariate::gamma_inference(mltest::as_span(x),2.3, 1.4,4).status==cppunivariate::InferenceStatus::InvalidSubset);
  CHECK(cppunivariate::gamma_inference(cppunivariate::span{x.data(),0},2.3, 1.4).status==cppunivariate::InferenceStatus::EmptySample);
}

TEST_CASE("gamma: analytic information versus log density central differences") {
  std::vector<double> x{0.9}, p{2.3,1.4};
  const auto r=cppunivariate::gamma_inference(mltest::as_span(x),p[0],p[1]);
  REQUIRE(r.status==cppunivariate::InferenceStatus::Ok);
  const auto ld=[&]() {return cppunivariate::distributions::dgamma(x[0],p[0],p[1],true);};
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

TEST_CASE("gamma: inference validity and total normalization") {
  std::vector<double> x{1};
  const auto one=cppunivariate::gamma_inference(mltest::as_span(x),2,1);
  x.push_back(x[0]);
  const auto two=cppunivariate::gamma_inference(mltest::as_span(x),2,1);
  REQUIRE(two.status==cppunivariate::InferenceStatus::Ok);
  for(std::size_t j=0;j<two.dimension;++j) CHECK(two.score[j]==doctest::Approx(2*one.score[j]));
  for(std::size_t j=0;j<two.dimension*two.dimension;++j) {
    CHECK(two.opg[j]==doctest::Approx(2*one.opg[j]));
    CHECK(two.observed[j]==doctest::Approx(2*one.observed[j]));
    CHECK(two.fisher[j]==doctest::Approx(2*one.fisher[j]));
  }
  CHECK(cppunivariate::gamma_inference(mltest::as_span(x),-1,1).status==cppunivariate::InferenceStatus::InvalidParameter);
  x[0]=0;
  CHECK(cppunivariate::gamma_inference(mltest::as_span(x),2,1,0).status==cppunivariate::InferenceStatus::InvalidObservation);
}

TEST_CASE("gamma: small shapes and extreme units inference") {
  for(double a : {1e-3,1e-2,1e-1}) {
    std::vector<double> x{a};
    CHECK(cppunivariate::gamma_inference(mltest::as_span(x),a,1).status==cppunivariate::InferenceStatus::Ok);
    for(double t : {1e-200,1e200}) {
      x[0]=a*t;
      CHECK(cppunivariate::gamma_inference(mltest::as_span(x),a,t).status==cppunivariate::InferenceStatus::NumericalFailure);
      CHECK(cppunivariate::gamma_inference(mltest::as_span(x),a,t,1).status==cppunivariate::InferenceStatus::Ok);
    }
  }
}

TEST_CASE("gamma nuisance aware inference and profile intervals") {
  std::vector<double> x{.2,.5,1,2,4};cppunivariate::span data{x.data(),x.size()};
  const auto f=cppunivariate::gamma_mle(data,1e-11,2000);
  const auto r=cppunivariate::gamma_target_inference(data,cppunivariate::GammaTarget::Mean,1.5);
  REQUIRE(r.status==cppunivariate::InferenceStatus::Ok);
  // In (mean,shape), model information is orthogonal: var(mean)=mean^2/(n*shape).
  CHECK(r.uncertainty.variance==doctest::Approx(f.shape*f.scale*f.scale/x.size()));
  for(auto target:{cppunivariate::GammaTarget::Mean,cppunivariate::GammaTarget::Shape}) {
    const auto ci=cppunivariate::gamma_target_lr_interval(data,target,3.841458820694124);
    REQUIRE(ci.status==cppunivariate::InferenceStatus::Ok);
    for(double t:{ci.lower,ci.upper}) {
      const auto p=cppunivariate::gamma_target_inference(data,target,t);
      REQUIRE(p.likelihood_ratio.status==cppunivariate::InferenceStatus::Ok);
      CHECK(p.likelihood_ratio.statistic==doctest::Approx(3.841458820694124).epsilon(1e-7));
    }
  }
  const auto near=cppunivariate::gamma_target_inference(data,cppunivariate::GammaTarget::Mean,5.);
  const auto far=cppunivariate::gamma_target_inference(data,cppunivariate::GammaTarget::Mean,20.);
  REQUIRE(near.score.status==cppunivariate::InferenceStatus::Ok);
  REQUIRE(far.score.status==cppunivariate::InferenceStatus::Ok);
  CHECK(far.score.statistic<near.score.statistic);
  const auto invalid=cppunivariate::gamma_target_inference(data,cppunivariate::GammaTarget::Mean,-1);
  CHECK(invalid.status==cppunivariate::InferenceStatus::InvalidParameter);
  CHECK(invalid.score.status!=cppunivariate::InferenceStatus::Ok);
}

// Independent 1600-digit reference; density-smoke.py retains exact inputs.
TEST_CASE("gamma: compensated concentrating density") {
  CHECK(cppunivariate::distributions::dgamma(100000000000.0,100000000.0,1000.0,true)==
        doctest::Approx(-17.037034184996326).epsilon(1e-10).scale(0));
}

TEST_CASE("gamma: centered high-shape probability") {
  CHECK(cppunivariate::distributions::pgamma(1e12,1e12,1,true,true) == doctest::Approx(-0.6931469145984605).epsilon(1e-10));
}

TEST_CASE("gamma: subnormal data, scale below binary64 resolution, overflowing sums") {
  // TASK-214.3. References: dev/research/evaluations/2026-11-13-fix-nonfinite-data/references.json.
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double tiny=std::numeric_limits<double>::denorm_min();
  const double shape_ref=5.375209483690757399469522;  // (1, 2, 3) * any unit
  // (1, 2, 3) * denorm_min: the scale MLE is 0.372 denorm_min, which rounds
  // to zero. It used to return Ok with scale 0 and a NaN log likelihood.
  const std::vector<double> x{tiny,2*tiny,3*tiny};
  const auto r=cppunivariate::gamma_mle(mltest::as_span(x),mltest::kFitTol,mltest::kFitMaxit,nan,nan);
  CHECK(r.status==cppunivariate::GammaStatus::TransformFailure);
  CHECK(std::isnan(r.scale));
  CHECK(r.stop_detail.accuracy_stop==cppunivariate::AccuracyStop::Tolerance);
  CHECK(r.stop_detail.attainable_tol==doctest::Approx(1/0.3720785219754353539790749).epsilon(1e-9));
  CHECK(cppunivariate::outcome(r).kind==cppunivariate::OutcomeKind::Failure);
  CHECK(cppunivariate::outcome(r).reason==cppunivariate::OutcomeReason::Tolerance);
  CHECK(r.shape==doctest::Approx(shape_ref).epsilon(1e-12));
  CHECK(r.logLik==doctest::Approx(2229.701528936673674596429).epsilon(1e-12));
  // A subnormal mean is rounded to a multiple of denorm_min ((1, 1, 2) has
  // mean 4/3): the shape must still come from the exact relative deviations.
  const std::vector<double> y{tiny,tiny,2*tiny},w{1,1,2};
  const auto ry=cppunivariate::gamma_mle(mltest::as_span(y),1e-10,100);
  const auto rw=cppunivariate::gamma_mle(mltest::as_span(w),1e-10,100);
  REQUIRE(rw.status==cppunivariate::GammaStatus::Ok);
  CHECK(ry.shape==doctest::Approx(rw.shape).epsilon(1e-12));
  // Subnormal mean, subnormal scale still resolved to 1e-10: Ok.
  const double unit=std::ldexp(1.0,-1036);
  const std::vector<double> z{unit,2*unit,3*unit};
  const auto rz=cppunivariate::gamma_mle(mltest::as_span(z),1e-10,100);
  REQUIRE(rz.status==cppunivariate::GammaStatus::Ok);
  CHECK(rz.scale<std::numeric_limits<double>::min());
  CHECK(rz.shape==doctest::Approx(shape_ref).epsilon(1e-12));
  CHECK(rz.scale/unit==doctest::Approx(2/shape_ref).epsilon(2e-11));
  CHECK(rz.logLik==doctest::Approx(-3.6186868274701123459+3*1036*std::log(2.0)).epsilon(1e-12));
  // A sum that overflows (snapshot class 'huge'): the double-only build used
  // to stop with IterationFailure.
  const std::vector<double> h{2.1929206633396517e+307,4.316903372290887e+307,7.156392554778722e+307,
      8.806638121904167e+307,1.1769861760077501e+308,1.7000000000000001e+308};
  const auto rh=cppunivariate::gamma_mle(mltest::as_span(h),1e-10,100);
  REQUIRE(rh.status==cppunivariate::GammaStatus::Ok);
  CHECK(rh.shape==doctest::Approx(2.6999293645744443753921).epsilon(1e-12));
  CHECK(rh.scale==doctest::Approx(3.163213400119677438739919e+307).epsilon(1e-12));
  CHECK(rh.logLik==doctest::Approx(-4258.952905145047195598505).epsilon(1e-12));
}
