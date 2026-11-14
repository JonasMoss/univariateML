#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/norm/mle.hpp"
#include "cppunivariate/distributions/norm/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("norm") {
  auto g = mltest::load_distribution_golden("norm");
  auto r = cppunivariate::norm_mle(mltest::as_span(g.data));
  CHECK(r.mean == doctest::Approx(g.estimates.at("mean")).epsilon(mltest::kGolden_norm).scale(0));
  CHECK(r.sd == doctest::Approx(g.estimates.at("sd")).epsilon(mltest::kGolden_norm).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_norm).scale(0));
}

TEST_CASE("norm: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/norm/fixture-outlier.json");
  auto r = cppunivariate::norm_mle(mltest::as_span(g.data));
  CHECK(r.mean == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.sd == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
}

TEST_CASE("norm dpqr: standard-normal CDF / quantile round-trip") {
  CHECK(cppunivariate::distributions::pnorm(0.0, 0.0, 1.0, true, false) == doctest::Approx(0.5));
  CHECK(cppunivariate::distributions::qnorm(0.5, 0.0, 1.0, true, false) == doctest::Approx(0.0));
  const double q = cppunivariate::distributions::qnorm(0.7, 0.0, 1.0, true, false);
  CHECK(cppunivariate::distributions::pnorm(q, 0.0, 1.0, true, false) == doctest::Approx(0.7));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("norm: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("norm",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dnorm(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pnorm(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qnorm(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rnorm(rng, p[0], p[1]); });
}

TEST_CASE("norm: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("norm");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<4;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::norm_mle(mltest::as_span(g.data),((mask & 1) ? g.estimates.at("mean") : nan),((mask & 2) ? g.estimates.at("sd") : nan));
    if(mask & 1)CHECK(r.mean==g.estimates.at("mean"));
    if(mask & 2)CHECK(r.sd==g.estimates.at("sd"));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("norm: held scale removes constant-sample collapse") {
  const std::vector<double> x={2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::norm_mle(mltest::as_span(x),2,nan).status==cppunivariate::NormStatus::Unbounded);
  CHECK(cppunivariate::norm_mle(mltest::as_span(x),nan,1).status==cppunivariate::NormStatus::Ok);
}


// mpmath generator: import mpmath as mp; mp.mp.dps=100
// Q=lambda z:mp.erfc(z/mp.sqrt(2))/2
// z=mp.findroot(lambda z:mp.log(Q(z))+800,40); [-z,z]
TEST_CASE("norm: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qnorm(-800.0, 0.0, 1.0, true, true) == doctest::Approx(-39.88469483825668).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qnorm(-800.0, 0.0, 1.0, false, true) == doctest::Approx(39.88469483825668).epsilon(1e-11));
}

TEST_CASE("norm: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qnorm(cppunivariate::special::kNegInf, 0.0, 1.0, true, true) == cppunivariate::special::kNegInf);
  CHECK(cppunivariate::distributions::qnorm(0.0, 0.0, 1.0, false, true) == cppunivariate::special::kNegInf);
  CHECK(cppunivariate::distributions::qnorm(cppunivariate::special::kNegInf, 0.0, 1.0, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qnorm(0.0, 0.0, 1.0, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("norm: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dnorm(1e100, 0.0, 1.0, true) == doctest::Approx(-5e+199).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pnorm(1e100, 0.0, 1.0, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pnorm(1e100, 0.0, 1.0, false, true) == doctest::Approx(-5e+199).epsilon(1e-11));
}

// import mpmath as mp; mp.mp.dps=100
// -2-mp.log(mp.mpf(float(1e308)))-mp.log(2*mp.pi)/2
// mp.log(mp.erfc(mp.sqrt(2))/2)
TEST_CASE("norm: standardization and affine overflow") {
  CHECK(cppunivariate::distributions::dnorm(1e308,-1e308,1e308,true) == doctest::Approx(-712.1151471753707).epsilon(1e-12));
  CHECK(cppunivariate::distributions::pnorm(1e308,-1e308,1e308,false,true) == doctest::Approx(-3.783184333682032).epsilon(1e-12));
}

TEST_CASE("norm: TASK-132 invalid data never report success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2.0},std::vector<double>{INFINITY,2.0},std::vector<double>{-INFINITY,2.0}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::norm_mle(x).status != cppunivariate::NormStatus::Ok);
  }
}

TEST_CASE("norm: TASK-132 unresolved likelihood has numerical failure status") {
  const std::vector<double> data{1.0,2.0};
  const cppunivariate::span x{data.data(),data.size()};
  CHECK(cppunivariate::norm_mle(x,0.0,1e-300).status == cppunivariate::NormStatus::IterationFailure);
}

#include "cppunivariate/distributions/norm/score.hpp"
TEST_CASE("norm: inference every free subset and MLE score") {
  std::vector<double> x{-1,0.2,0.8,2,3};
  const auto full=cppunivariate::norm_inference(mltest::as_span(x),0.3, 1.7);
  REQUIRE(full.status==cppunivariate::InferenceStatus::Ok);
  for(unsigned mask=0; mask<4; ++mask) {
    std::vector<double> sums(2,0);
    std::array<double,4> products{};
    std::size_t rows=0;
    const auto a=cppunivariate::norm_inference(mltest::as_span(x),0.3, 1.7,mask,
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
    const auto f=cppunivariate::norm_mle(mltest::as_span(x), (mask&1)?NAN:0.3, (mask&2)?NAN:1.7);
    REQUIRE(f.status==cppunivariate::NormStatus::Ok);
    const auto fitted=cppunivariate::norm_inference(mltest::as_span(x),f.mean,f.sd,mask);
    REQUIRE(fitted.status==cppunivariate::InferenceStatus::Ok);
    for(std::size_t j=0;j<fitted.dimension;++j) CHECK(std::abs(fitted.score[j])<1e-7);
  }
  CHECK(cppunivariate::norm_inference(mltest::as_span(x),0.3, 1.7,4).status==cppunivariate::InferenceStatus::InvalidSubset);
  CHECK(cppunivariate::norm_inference(cppunivariate::span{x.data(),0},0.3, 1.7).status==cppunivariate::InferenceStatus::EmptySample);
}

TEST_CASE("norm: inference invalid and singular diagnostics") {
  std::vector<double> x{0,0};
  CHECK(cppunivariate::norm_inference(mltest::as_span(x),0,0).status==cppunivariate::InferenceStatus::InvalidParameter);
  auto r=cppunivariate::norm_inference(mltest::as_span(x),0,1);
  CHECK(r.opg_status==cppunivariate::InferenceStatus::Singular);
  CHECK(r.observed[3]<0); // valid indefinite information away from an MLE
  x[0]=INFINITY;
  CHECK(cppunivariate::norm_inference(mltest::as_span(x),0,1).status==cppunivariate::InferenceStatus::InvalidObservation);
  x={1e308,-1e308};
  CHECK(cppunivariate::norm_inference(mltest::as_span(x),0,1e-308).status==cppunivariate::InferenceStatus::NumericalFailure);
}

TEST_CASE("norm: analytic information versus log density central differences") {
  std::vector<double> x{0.9}, p{0.3,1.7};
  const auto r=cppunivariate::norm_inference(mltest::as_span(x),p[0],p[1]);
  REQUIRE(r.status==cppunivariate::InferenceStatus::Ok);
  const auto ld=[&]() {return cppunivariate::distributions::dnorm(x[0],p[0],p[1],true);};
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

TEST_CASE("norm: inference validity and total normalization") {
  std::vector<double> x{1};
  const auto one=cppunivariate::norm_inference(mltest::as_span(x),0,1);
  x.push_back(x[0]);
  const auto two=cppunivariate::norm_inference(mltest::as_span(x),0,1);
  REQUIRE(two.status==cppunivariate::InferenceStatus::Ok);
  for(std::size_t j=0;j<two.dimension;++j) CHECK(two.score[j]==doctest::Approx(2*one.score[j]));
  for(std::size_t j=0;j<two.dimension*two.dimension;++j) {
    CHECK(two.opg[j]==doctest::Approx(2*one.opg[j]));
    CHECK(two.observed[j]==doctest::Approx(2*one.observed[j]));
    CHECK(two.fisher[j]==doctest::Approx(2*one.fisher[j]));
  }
  CHECK(cppunivariate::norm_inference(mltest::as_span(x),0,0).status==cppunivariate::InferenceStatus::InvalidParameter);
  x[0]=NAN;
  CHECK(cppunivariate::norm_inference(mltest::as_span(x),0,1,0).status==cppunivariate::InferenceStatus::InvalidObservation);
}

namespace {
struct ThreeInferenceModel {
  static constexpr std::size_t count=3;
  std::array<std::string,3> names() const {return {"a","b","c"};}
  std::string conditions() const {return "synthetic quadratic";}
  bool valid() const {return true;}
  bool support(double) const {return true;}
  std::array<double,9> fisher() const {return {1,0,0,0,2,0,0,0,4};}
  void observation(double x,std::array<double,3>& s,std::array<double,9>& h) const {
    s={x,2*x,3*x}; h=fisher();
  }
};
}
TEST_CASE("norm: inference storage and eigenvalue diagnostics beyond two coordinates") {
  std::vector<double> x{1,2};
  const auto r=cppunivariate::inference_detail::accumulate(
    mltest::as_span(x),ThreeInferenceModel{},7,cppunivariate::inference_detail::Discard{});
  REQUIRE(r.status==cppunivariate::InferenceStatus::Ok);
  CHECK(r.dimension==3);
  CHECK(r.score[2]==9);
  CHECK(r.fisher[8]==8);
  CHECK(r.fisher_condition==doctest::Approx(4));
  CHECK(r.opg_status==cppunivariate::InferenceStatus::Singular);
  std::array<double,9> a{1,0,0,0,1e-10,0,0,0,2};
  double condition=0;
  CHECK(cppunivariate::inference_detail::matrix_status(a,3,true,condition)==cppunivariate::InferenceStatus::IllConditioned);
  CHECK(condition==doctest::Approx(2e10));
  a[4]=1e-16;
  CHECK(cppunivariate::inference_detail::matrix_status(a,3,true,condition)==cppunivariate::InferenceStatus::Singular);
}
TEST_CASE("norm: extreme natural units have explicit arithmetic failure") {
  for(double sd : {1e-200,1e200}) {
    std::vector<double> x{sd};
    CHECK(cppunivariate::norm_inference(mltest::as_span(x),0,sd).status==cppunivariate::InferenceStatus::NumericalFailure);
  }
}

namespace {
struct OversizedInferenceModel {static constexpr std::size_t count=9;};
}
TEST_CASE("norm: unsupported inference dimension fails before model evaluation") {
  std::vector<double> x{1};
  const auto r=cppunivariate::inference_detail::accumulate(mltest::as_span(x),
    OversizedInferenceModel{},0,cppunivariate::inference_detail::Discard{});
  CHECK(r.status==cppunivariate::InferenceStatus::DimensionExceeded);
}

TEST_CASE("norm: dense symmetric free information condition numbers") {
  double condition=0;
  std::array<double,9> dense{2,1,0,1,2,0,0,0,4};
  CHECK(cppunivariate::inference_detail::matrix_status(dense,3,true,condition)==cppunivariate::InferenceStatus::Ok);
  CHECK(condition==doctest::Approx(4));
  std::array<double,64> maximum{};
  for(std::size_t j=0;j<8;++j) for(std::size_t k=0;k<8;++k)
    maximum[j*8+k]=(j==k ? 2 : 1);
  CHECK(cppunivariate::inference_detail::matrix_status(maximum,8,true,condition)==cppunivariate::InferenceStatus::Ok);
  CHECK(condition==doctest::Approx(9));
}

TEST_CASE("inference covariance solves and nuisance contrasts") {
  std::vector<double> x{-2,-1,0,1,2};
  const auto r=cppunivariate::norm_inference({x.data(),x.size()},0,std::sqrt(2.0));
  const auto c=cppunivariate::inference_covariance(r);
  REQUIRE(c.status==cppunivariate::InferenceStatus::Ok);
  CHECK(c.matrix[0]==doctest::Approx(0.4));
  CHECK(c.matrix[3]==doctest::Approx(0.2));
  const auto s=cppunivariate::inference_covariance(r,cppunivariate::CovarianceKind::Sandwich);
  REQUIRE(s.status==cppunivariate::InferenceStatus::Ok);
  CHECK(s.matrix[0]==doctest::Approx(0.4));
  std::array<double,cppunivariate::inference_max_parameters> g{};g[0]=1;
  CHECK(cppunivariate::inference_delta(c,g).variance==doctest::Approx(.4));
  auto bad=r;bad.fisher[0]=-1;
  CHECK(cppunivariate::inference_covariance(bad).status==cppunivariate::InferenceStatus::Indefinite);
  bad=r;bad.fisher[0]=0;bad.fisher[1]=bad.fisher[2]=0;
  CHECK(cppunivariate::inference_covariance(bad).status==cppunivariate::InferenceStatus::Singular);
  bad=r;bad.fisher[0]=1e-10;
  CHECK(cppunivariate::inference_covariance(bad).status==cppunivariate::InferenceStatus::IllConditioned);
  CHECK(cppunivariate::inference_lr(-10,-11).statistic==doctest::Approx(2));
  CHECK(cppunivariate::inference_lr(-11,-10).status==cppunivariate::InferenceStatus::NumericalFailure);
}

TEST_CASE("normal mean fitted variance interval inversion") {
  std::vector<double> x{-2,-1,0,1,2};cppunivariate::span data{x.data(),x.size()};
  const double critical=3.841458820694124;
  const auto ci=cppunivariate::norm_mean_lr_interval(data,critical);
  const auto si=cppunivariate::norm_mean_score_interval(data,critical);
  const auto exact=cppunivariate::norm_mean_exact_interval(data);
  REQUIRE(ci.status==cppunivariate::InferenceStatus::Ok);
  REQUIRE(si.status==cppunivariate::InferenceStatus::Ok);
  REQUIRE(exact.status==cppunivariate::InferenceStatus::Ok);
  CHECK(cppunivariate::norm_mean_inference(data,ci.upper).likelihood_ratio.statistic==doctest::Approx(critical));
  CHECK(cppunivariate::norm_mean_inference(data,si.upper).score.statistic==doctest::Approx(critical));
  CHECK(exact.upper==doctest::Approx(2.7764451051977987*std::sqrt(.5)));
}

TEST_CASE("inference covariance eight coordinates and singular meat") {
  cppunivariate::InferenceResult r;r.dimension=8;
  for(std::size_t i=0;i<8;++i) {r.fisher[i*8+i]=i+1.;r.observed[i*8+i]=i+1.;}
  const auto c=cppunivariate::inference_covariance(r);
  REQUIRE(c.status==cppunivariate::InferenceStatus::Ok);
  for(std::size_t i=0;i<8;++i) CHECK(c.matrix[i*8+i]==doctest::Approx(1./(i+1.)));
  // Singular empirical meat does not prevent the SPD sensitivity solve.
  const auto s=cppunivariate::inference_covariance(r,cppunivariate::CovarianceKind::Sandwich);
  REQUIRE(s.status==cppunivariate::InferenceStatus::Ok);
  for(double v:s.matrix) CHECK(v==0);
  r.fisher[1]=1;
  CHECK(cppunivariate::inference_covariance(r).status==cppunivariate::InferenceStatus::NumericalFailure);
  r.dimension=9;
  CHECK(cppunivariate::inference_covariance(r).status==cppunivariate::InferenceStatus::DimensionExceeded);
}

#include "cppunivariate/distributions/norm/inference.hpp"

TEST_CASE("norm: TASK-102.5 inference analytic and invalid-input checks") {

  const std::vector<double> x{-1,0,1},knownx{-1,1};
  const auto ci=cppunivariate::norm_variance_exact_interval(mltest::as_span(x));
  REQUIRE(ci.status==cppunivariate::InferenceStatus::Ok);
  CHECK(ci.lower==doctest::Approx(-1/std::log(.025)));
  CHECK(ci.upper==doctest::Approx(-1/std::log(.975)));
  const auto known=cppunivariate::norm_variance_exact_interval(mltest::as_span(knownx),.05,0);
  CHECK(known.lower==doctest::Approx(ci.lower));CHECK(known.upper==doctest::Approx(ci.upper));
  const std::vector<double> shifted{7,10,13};
  const auto transformed=cppunivariate::norm_variance_exact_interval(mltest::as_span(shifted));
  CHECK(transformed.lower==doctest::Approx(9*ci.lower));CHECK(transformed.upper==doctest::Approx(9*ci.upper));

  const std::vector<double> empty;
  const cppunivariate::span data{empty.data(),empty.size()};
  CHECK(cppunivariate::norm_variance_exact_interval(data).status==cppunivariate::InferenceStatus::EmptySample);
  const std::vector<double> invalid{NAN,1,2};
  const cppunivariate::span bad{invalid.data(),invalid.size()};
  CHECK(cppunivariate::norm_variance_exact_interval(bad).status==cppunivariate::InferenceStatus::InvalidObservation);
}

TEST_CASE("norm: TASK-102.5 invalid confidence levels") {
  const std::vector<double> x{.2,.4,.6,.8};
  const auto data=mltest::as_span(x);
  for(double alpha:std::vector<double>{-1.,0.,1.,2.,NAN,INFINITY})
    CHECK(cppunivariate::norm_variance_exact_interval(data,alpha).status==cppunivariate::InferenceStatus::InvalidParameter);
}

#include "cppunivariate/core/inference_numeric.hpp"
#include "cppunivariate/distributions/norm/score.hpp"
TEST_CASE("numeric inference: normal derivatives and held scale") {
  const auto g=mltest::load_distribution_golden("norm");
  const double mean=g.estimates.at("mean"),sd=g.estimates.at("sd");
  std::array<cppunivariate::NumericParameter,cppunivariate::inference_max_parameters> p{};
  p[0]={"mean",mean,cppunivariate::NumericCoordinate::Real,0};
  p[1]={"sd",sd,cppunivariate::NumericCoordinate::Positive,0};
  const auto density=[](double x,const auto& v) {return cppunivariate::distributions::dnorm(x,v[0],v[1],true);};
  for(unsigned mask=1;mask<4;++mask) {
    const auto a=cppunivariate::norm_inference(mltest::as_span(g.data),mean,sd,mask);
    const auto n=cppunivariate::numeric_inference(mltest::as_span(g.data),p,2,mask,density);
    REQUIRE(n.status==cppunivariate::InferenceStatus::Ok);
    for(std::size_t i=0;i<n.dimension;++i) {
      CHECK(std::abs(n.score[i]-a.score[i])<1e-6*(1+std::abs(a.score[i])));
      for(std::size_t j=0;j<n.dimension;++j) {
        const auto k=i*n.dimension+j;
        CHECK(std::abs(n.observed[k]-a.observed[k])<2e-5*(1+std::abs(a.observed[k])));
        CHECK(std::abs(n.opg[k]-a.opg[k])<1e-6*(1+std::abs(a.opg[k])));
      }
    }
    CHECK(cppunivariate::inference_covariance(n,cppunivariate::CovarianceKind::Observed).status==cppunivariate::InferenceStatus::Ok);
    CHECK(cppunivariate::inference_covariance(n,cppunivariate::CovarianceKind::Fisher).status==cppunivariate::InferenceStatus::NumericalFailure);
  }
}

#include "cppunivariate/distributions/gamma/dpqr.hpp"
#include "cppunivariate/distributions/gamma/score.hpp"
#include "cppunivariate/distributions/beta/dpqr.hpp"
#include "cppunivariate/distributions/beta/score.hpp"
#include "cppunivariate/distributions/pois/dpqr.hpp"
#include "cppunivariate/distributions/pois/score.hpp"
TEST_CASE("numeric inference: analytic gamma beta and Poisson at nonstationary points") {
  const std::vector<double> x={0.2,0.4,0.7};
  const std::vector<double> counts={0,1,3,4};
  std::array<cppunivariate::NumericParameter,cppunivariate::inference_max_parameters> p{};
  p[0]={"shape1",2,cppunivariate::NumericCoordinate::Positive,0};
  p[1]={"shape2",3,cppunivariate::NumericCoordinate::Positive,0};
  const auto compare=[](const auto& numeric,const auto& analytic) {
    REQUIRE(numeric.status==cppunivariate::InferenceStatus::Ok);
    REQUIRE(analytic.status==cppunivariate::InferenceStatus::Ok);
    for(std::size_t i=0;i<numeric.dimension;++i) {
      CHECK(std::abs(numeric.score[i]-analytic.score[i])<2e-6*(1+std::abs(analytic.score[i])));
      for(std::size_t j=0;j<numeric.dimension;++j) {
        const auto k=i*numeric.dimension+j;
        CHECK(std::abs(numeric.observed[k]-analytic.observed[k])<2e-5*(1+std::abs(analytic.observed[k])));
        CHECK(std::abs(numeric.opg[k]-analytic.opg[k])<2e-6*(1+std::abs(analytic.opg[k])));
      }
    }
  };
  for(unsigned mask=1;mask<4;++mask) {
    const auto beta=[](double v,const auto& a) {return cppunivariate::distributions::dbeta(v,a[0],a[1],true);};
    const auto gamma=[](double v,const auto& a) {return cppunivariate::distributions::dgamma(v,a[0],a[1],true);};
    compare(cppunivariate::numeric_inference(mltest::as_span(x),p,2,mask,beta),cppunivariate::beta_inference(mltest::as_span(x),2,3,mask));
    compare(cppunivariate::numeric_inference(mltest::as_span(x),p,2,mask,gamma),cppunivariate::gamma_inference(mltest::as_span(x),2,3,mask));
  }
  const auto pois=[](double v,const auto& a) {return cppunivariate::distributions::dpois(v,a[0],true);};
  compare(cppunivariate::numeric_inference(mltest::as_span(counts),p,1,1,pois),cppunivariate::pois_inference(mltest::as_span(counts),2,1));
}

TEST_CASE("numeric inference: logit coordinate and invalid domains") {
  const std::vector<double> x={0,1,1,1};
  std::array<cppunivariate::NumericParameter,cppunivariate::inference_max_parameters> p{};
  p[0]={"p",0.4,cppunivariate::NumericCoordinate::UnitInterval,0};
  const auto density=[](double v,const auto& a) {return v ? std::log(a[0]) : std::log1p(-a[0]);};
  const auto result=cppunivariate::numeric_inference(mltest::as_span(x),p,1,1,density);
  REQUIRE(result.status==cppunivariate::InferenceStatus::Ok);
  CHECK(result.score[0]==doctest::Approx(3/0.4-1/0.6).epsilon(1e-7));
  CHECK(result.observed[0]==doctest::Approx(3/(0.4*0.4)+1/(0.6*0.6)).epsilon(1e-5));
  p[0].value=1;
  CHECK(cppunivariate::numeric_inference(mltest::as_span(x),p,1,1,density).status==cppunivariate::InferenceStatus::InvalidParameter);
  p[0]={"shifted",2,cppunivariate::NumericCoordinate::LowerBounded,2};
  CHECK(cppunivariate::numeric_inference(mltest::as_span(x),p,1,1,density).status==cppunivariate::InferenceStatus::InvalidParameter);
}

TEST_CASE("numeric inference: reject collapsed transformed stencils") {
  const std::vector<double> x={0,1};
  std::array<cppunivariate::NumericParameter,cppunivariate::inference_max_parameters> p{};
  p[0]={"p",std::nextafter(1.0,0.0),cppunivariate::NumericCoordinate::UnitInterval,0};
  const auto density=[](double v,const auto& a) {return v ? std::log(a[0]) : std::log1p(-a[0]);};
  CHECK(cppunivariate::numeric_inference(mltest::as_span(x),p,1,1,density).status==cppunivariate::InferenceStatus::NumericalFailure);
}

TEST_CASE("numeric inference: normal covariance and observation score callback") {
  const auto g=mltest::load_distribution_golden("norm");
  const double mean=g.estimates.at("mean"),sd=g.estimates.at("sd");
  std::array<cppunivariate::NumericParameter,cppunivariate::inference_max_parameters> p{};
  p[0]={"mean",mean,cppunivariate::NumericCoordinate::Real,0};
  p[1]={"sd",sd,cppunivariate::NumericCoordinate::Positive,0};
  const auto density=[](double x,const auto& a) {return cppunivariate::distributions::dnorm(x,a[0],a[1],true);};
  std::size_t nrows=0;
  std::array<double,2> sum{};
  const auto emit=[&](const auto& row) {++nrows;sum[0]+=row[0];sum[1]+=row[1];};
  const auto numeric=cppunivariate::numeric_inference(mltest::as_span(g.data),p,2,3,density,emit);
  const auto analytic=cppunivariate::norm_inference(mltest::as_span(g.data),mean,sd);
  REQUIRE(numeric.status==cppunivariate::InferenceStatus::Ok);
  CHECK(nrows==g.data.size());
  for(std::size_t i=0;i<2;++i) CHECK(sum[i]==doctest::Approx(numeric.score[i]));
  for(const auto kind:{cppunivariate::CovarianceKind::Observed,cppunivariate::CovarianceKind::Sandwich}) {
    const auto actual=cppunivariate::inference_covariance(numeric,kind);
    const auto reference=cppunivariate::inference_covariance(analytic,kind);
    REQUIRE(actual.status==cppunivariate::InferenceStatus::Ok);
    REQUIRE(reference.status==cppunivariate::InferenceStatus::Ok);
    for(std::size_t i=0;i<4;++i)
      CHECK(std::abs(actual.matrix[i]-reference.matrix[i])<1e-6*(1+std::abs(reference.matrix[i])));
  }
}

TEST_CASE("numeric inference: checked adapter refuses unavailable regimes and structural free coordinates") {
  const std::vector<double> x={0,1};
  std::array<cppunivariate::NumericParameter,cppunivariate::inference_max_parameters> p{};
  p[0]={"mean",0,cppunivariate::NumericCoordinate::Real,0};
  p[1]={"sd",1,cppunivariate::NumericCoordinate::Positive,0};
  const auto density=[](double v,const auto& a) {return cppunivariate::distributions::dnorm(v,a[0],a[1],true);};
  const cppunivariate::NumericEligibility unavailable{cppunivariate::InferenceStatus::FamilyUnavailable,0,"not classified"};
  const auto rejected=cppunivariate::numeric_inference_checked(mltest::as_span(x),p,2,1,unavailable,density);
  CHECK(rejected.status==cppunivariate::InferenceStatus::FamilyUnavailable);
  CHECK(rejected.validity_conditions=="not classified");
  const cppunivariate::NumericEligibility restricted{cppunivariate::InferenceStatus::Ok,1,"scale held"};
  CHECK(cppunivariate::numeric_inference_checked(mltest::as_span(x),p,2,3,restricted,density).status==cppunivariate::InferenceStatus::InvalidSubset);
  const auto allowed=cppunivariate::numeric_inference_checked(mltest::as_span(x),p,2,1,restricted,density);
  CHECK(allowed.status==cppunivariate::InferenceStatus::Ok);
  CHECK(allowed.validity_conditions=="scale held");
  const cppunivariate::NumericEligibility boundary{cppunivariate::InferenceStatus::BoundaryOrLimit,0,"boundary fit"};
  CHECK(cppunivariate::numeric_inference_checked(mltest::as_span(x),p,2,1,boundary,density).status==cppunivariate::InferenceStatus::BoundaryOrLimit);
}

#include "cppunivariate/core/inference_families.hpp"
TEST_CASE("family inference: analytic precedence, structural holds and unclassified status") {
  const std::vector<double> x={0,1,2,1};
  std::array<double,cppunivariate::inference_max_parameters> p{};
  p[0]=1;
  const auto actual=cppunivariate::family_inference(mltest::as_span(x),"pois",p,1);
  const auto reference=cppunivariate::pois_inference(mltest::as_span(x),1,1);
  REQUIRE(actual.status==cppunivariate::InferenceStatus::Ok);
  CHECK(actual.score[0]==reference.score[0]);
  CHECK(actual.fisher[0]==reference.fisher[0]);
  CHECK(cppunivariate::family_inference(mltest::as_span(x),"laplace",p,1).status==cppunivariate::InferenceStatus::RegimeUnavailable);
  p[0]=4;p[1]=0.4;
  CHECK(cppunivariate::family_inference(mltest::as_span(x),"binom",p,1).status==cppunivariate::InferenceStatus::InvalidSubset);
  const auto binom=cppunivariate::family_inference(mltest::as_span(x),"binom",p,2);
  REQUIRE(binom.status==cppunivariate::InferenceStatus::Ok);
  CHECK(binom.dimension==1);
  CHECK(binom.free_indices[0]==1);
  CHECK(binom.score[0]==doctest::Approx(4/0.4-12/0.6).epsilon(1e-7));
  p[1]=1;
  CHECK(cppunivariate::family_inference(mltest::as_span(x),"binom",p,2).status==cppunivariate::InferenceStatus::BoundaryOrLimit);
}

TEST_CASE("numeric inference: held infinite bounds reach the density") {
  const std::vector<double> x={0,1,2};
  std::array<cppunivariate::NumericParameter,cppunivariate::inference_max_parameters> p{};
  p[0]={"mean",0,cppunivariate::NumericCoordinate::Real,0};
  p[1]={"upper",std::numeric_limits<double>::infinity(),cppunivariate::NumericCoordinate::Real,0};
  const auto density=[](double v,const auto& a) {
    return a[1]>v ? cppunivariate::distributions::dnorm(v,a[0],1,true) : -INFINITY;
  };
  const auto held=cppunivariate::numeric_inference(mltest::as_span(x),p,2,1,density);
  REQUIRE(held.status==cppunivariate::InferenceStatus::Ok);
  CHECK(held.score[0]==doctest::Approx(3).epsilon(1e-8));
  CHECK(cppunivariate::numeric_inference(mltest::as_span(x),p,2,3,density).status==cppunivariate::InferenceStatus::InvalidParameter);
}

TEST_CASE("family inference: nonsmooth submodels") {
  const std::vector<double> x={-1,0,1};
  std::array<double,cppunivariate::inference_max_parameters> p{};
  p[0]=0;p[1]=1;
  const auto laplace=cppunivariate::family_inference(mltest::as_span(x),"laplace",p,2);
  REQUIRE(laplace.status==cppunivariate::InferenceStatus::Ok);
  CHECK(laplace.score[0]==doctest::Approx(-1).epsilon(1e-7));
  CHECK(cppunivariate::family_inference(mltest::as_span(x),"laplace",p,1).status==cppunivariate::InferenceStatus::RegimeUnavailable);
  CHECK(cppunivariate::family_inference(mltest::as_span(x),"triang",p,1).status==cppunivariate::InferenceStatus::FamilyUnavailable);

}

TEST_CASE("family inference: eligible golden data or documented status") {
#include "inference_goldens.inc"
}

TEST_CASE("family inference: symmetric skew GED normal member") {
  const std::vector<double> x={-1,0,1};
  std::array<double,cppunivariate::inference_max_parameters> p{};
  p[0]=0;p[1]=1;p[2]=2;p[3]=1;
  const auto skew=cppunivariate::family_inference(mltest::as_span(x),"sged",p,3);
  const auto normal=cppunivariate::norm_inference(mltest::as_span(x),0,1);
  REQUIRE(skew.status==cppunivariate::InferenceStatus::Ok);
  for(std::size_t i=0;i<2;++i) CHECK(std::abs(skew.score[i]-normal.score[i])<1e-6);
  for(std::size_t i=0;i<4;++i) CHECK(std::abs(skew.observed[i]-normal.observed[i])<2e-5);
  CHECK(cppunivariate::family_inference(mltest::as_span(x),"sged",p,11).status==cppunivariate::InferenceStatus::RegimeUnavailable);
}

TEST_CASE("family inference: finite-support rank and scalar submodels") {
  const std::vector<double> bernoulli={0,1,1};
  std::array<double,cppunivariate::inference_max_parameters> p{};
  p[0]=1;p[1]=2;p[2]=3;
  const auto beta_binomial=cppunivariate::family_inference(mltest::as_span(bernoulli),"bbinom",p,2);
  REQUIRE(beta_binomial.status==cppunivariate::InferenceStatus::Ok);
  CHECK(beta_binomial.score[0]==doctest::Approx(0.4).epsilon(1e-7));
  CHECK(beta_binomial.observed[0]==doctest::Approx(0.38).epsilon(1e-5));
  CHECK(cppunivariate::family_inference(mltest::as_span(bernoulli),"bbinom",p,6).status==cppunivariate::InferenceStatus::Singular);
  p[1]=.4;p[2]=.8;
  const auto cmb=cppunivariate::family_inference(mltest::as_span(bernoulli),"cmb",p,2);
  REQUIRE(cmb.status==cppunivariate::InferenceStatus::Ok);
  CHECK(cmb.score[0]==doctest::Approx(2/.4-1/.6).epsilon(1e-7));
  CHECK(cppunivariate::family_inference(mltest::as_span(bernoulli),"cmb",p,4).status==cppunivariate::InferenceStatus::Singular);

}

TEST_CASE("family inference: known boundary members retain regular submodels") {
  const std::vector<double> real={-1,0,1};
  const std::vector<double> counts={0,1,2,3};
  const std::vector<double> positive={1,2,3};
  std::array<double,cppunivariate::inference_max_parameters> p{};
  p[0]=0;p[1]=1;p[2]=0;
  const auto bhatt=cppunivariate::family_inference(mltest::as_span(real),"bhatt",p,3);
  const auto normal=cppunivariate::norm_inference(mltest::as_span(real),0,1);
  REQUIRE(bhatt.status==cppunivariate::InferenceStatus::Ok);
  for(std::size_t i=0;i<2;++i) CHECK(std::abs(bhatt.score[i]-normal.score[i])<1e-6);
  for(std::size_t i=0;i<4;++i) CHECK(std::abs(bhatt.observed[i]-normal.observed[i])<2e-5);
  CHECK(cppunivariate::family_inference(mltest::as_span(real),"bhatt",p,7).status==cppunivariate::InferenceStatus::BoundaryOrLimit);

}

TEST_CASE("family inference: unavailable triangular model explains the kink") {
  const std::vector<double> x={0.2,0.6};
  std::array<double,cppunivariate::inference_max_parameters> p{};
  p[0]=0;p[1]=1;p[2]=.5;
  const auto result=cppunivariate::family_inference(mltest::as_span(x),"triang",p,4);
  CHECK(result.status==cppunivariate::InferenceStatus::FamilyUnavailable);
  CHECK(result.validity_conditions.find("kink")!=std::string::npos);
}

TEST_CASE("family inference: further known boundary identities") {
  const std::vector<double> counts={0,1,1};
  std::array<double,cppunivariate::inference_max_parameters> p{};
  p[0]=.3;p[1]=0;
  const auto cmp=cppunivariate::family_inference(mltest::as_span(counts),"cmp",p,1);
  REQUIRE(cmp.status==cppunivariate::InferenceStatus::Ok);
  CHECK(cmp.score[0]==doctest::Approx(2/.3-3/.7).epsilon(1e-7));
  CHECK(cppunivariate::family_inference(mltest::as_span(counts),"cmp",p,3).status==cppunivariate::InferenceStatus::BoundaryOrLimit);

  const std::vector<double> positive={.2,.5,1.};p[0]=2;p[1]=0;
  const auto sgomp=cppunivariate::family_inference(mltest::as_span(positive),"sgomp",p,1);
  const auto exponential=cppunivariate::family_inference(mltest::as_span(positive),"exp",p,1);
  REQUIRE(sgomp.status==cppunivariate::InferenceStatus::Ok);
  REQUIRE(exponential.status==cppunivariate::InferenceStatus::Ok);
  CHECK(std::abs(sgomp.score[0]-exponential.score[0])<1e-7);
  CHECK(std::abs(sgomp.observed[0]-exponential.observed[0])<1e-5);
}

TEST_CASE("structured comparison candidates are diagnostic witnesses") {
  const double ll=-3*(std::log(2*std::acos(-1.))/2+.5);
  const auto d=cppunivariate::comparison_detail({{0,1},ll,ll,cppunivariate::StopModel::Norm},{{-1,1},-3*std::log(2.),-3*std::log(2.),cppunivariate::StopModel::Unif});
  CHECK(d.comparison_tie);CHECK(d.candidates.size()==2);
  CHECK(d.limit_coef.empty());CHECK(std::isnan(d.limit_loglik));
  CHECK(d.loglik_lower==std::max(ll,-3*std::log(2.)));CHECK(std::isinf(d.loglik_upper));
}

TEST_CASE("stop detail preserves the original aggregate field order") {
  const cppunivariate::StopDetail d{{},{},1,2,3,4};
  CHECK(d.limit_loglik==1);CHECK(d.loglik_lower==2);CHECK(d.loglik_upper==3);
  CHECK(d.segment_loglik_upper==4);CHECK_FALSE(d.comparison_tie);
}

TEST_CASE("stop payloads reject unrepresentable or improper face parameters") {
  CHECK(cppunivariate::limit_detail({2,0,1},0,cppunivariate::StopModel::UnitGamma).limit_coef.empty());
  CHECK(cppunivariate::limit_detail({0,0},0,cppunivariate::StopModel::Norm).limit_coef.empty());
  CHECK(cppunivariate::limit_detail({1,1},0,cppunivariate::StopModel::Unif).limit_coef.empty());
  CHECK(cppunivariate::limit_detail({1,-1},0,cppunivariate::StopModel::Power).limit_coef.empty());
  const auto invalid=cppunivariate::comparison_detail({{0,1},-1,-1,cppunivariate::StopModel::Norm},{{2,0,1},0,0,cppunivariate::StopModel::UnitGamma});
  CHECK_FALSE(invalid.comparison_tie);CHECK(invalid.candidates.empty());
}

TEST_CASE("norm scaled residuals: every held subset at extreme units") {
  for(double unit:{1e-200,1e200}) {
    const std::vector<double> x={unit,2*unit};
    for(int mask=0;mask<4;++mask) {
      const auto r=cppunivariate::norm_mle(mltest::as_span(x),mask&1?0:NAN,mask&2?unit:NAN);
      REQUIRE(r.status==cppunivariate::NormStatus::Ok);
      const double m=mask&1?0:1.5,s=mask&2?1:(mask&1?std::sqrt(2.5):.5);
      CHECK(r.mean/unit==doctest::Approx(m).epsilon(1e-14));
      CHECK(r.sd/unit==doctest::Approx(s).epsilon(1e-14));
      const double ll=-2*(std::log(unit)+std::log(s)+.5*std::log(2*cppunivariate::pi))-
          ((1-m)*(1-m)+(2-m)*(2-m))/(2*s*s);
      CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-14));
    }
  }
  const double max=std::numeric_limits<double>::max();
  const std::vector<double> wide={-max,max,max};
  const auto w=cppunivariate::norm_mle(mltest::as_span(wide));
  REQUIRE(w.status==cppunivariate::NormStatus::Ok);
  CHECK(w.mean/max==doctest::Approx(1./3).epsilon(1e-14));
  CHECK(w.sd/max==doctest::Approx(std::sqrt(8./9)).epsilon(1e-14));
  const std::vector<double> zero={0,0};
  CHECK(cppunivariate::norm_mle(mltest::as_span(zero),0,NAN).status==cppunivariate::NormStatus::Unbounded);
  CHECK(cppunivariate::norm_mle(mltest::as_span(zero),0,1e-200).status==cppunivariate::NormStatus::Ok);
  const std::vector<double> sub={0,std::numeric_limits<double>::denorm_min()};
  CHECK(cppunivariate::norm_mle(mltest::as_span(sub),0,NAN).status!=cppunivariate::NormStatus::Unbounded);
}

TEST_CASE("norm: the sd is profiled at the exact mean, not the rounded one") {
  // TASK-214.3: (1, 1, 1 + 2^-52) has mean 1 + 2^-52/3, which rounds to 1.
  // Residuals about 1 gave sd 2^-52/sqrt(3); the MLE is 2^-52 sqrt(2)/3.
  const std::vector<double> x{1,1,1+0x1p-52};
  const double sd=0x1p-52*std::sqrt(2.0)/3;
  const auto r=cppunivariate::norm_mle(mltest::as_span(x));
  REQUIRE(r.status==cppunivariate::NormStatus::Ok);
  CHECK(r.mean==1);
  CHECK(std::abs(r.sd/sd-1)<1e-14);  // relative: Approx adds an absolute 1e-14
  // logLik is the value at the returned coefficients (decision 2026-11-14):
  // the supremum 106.1302606629018611538071 less n delta^2/(2 sd^2) = 3/4,
  // delta = 2^-52/3 being the rounding of the mean.
  CHECK(r.logLik==doctest::Approx(105.3802606629018611538071).epsilon(1e-14));
  // Held sd, free mean: likewise at the returned (rounded) mean.
  const double s=1e-16;
  const auto h=cppunivariate::norm_mle(mltest::as_span(x),NAN,s);
  REQUIRE(h.status==cppunivariate::NormStatus::Ok);
  CHECK(h.mean==1);
  CHECK(h.logLik==doctest::Approx(-3*(std::log(s)+0.5*std::log(2*cppunivariate::pi))-0.5*(0x1p-52/s)*(0x1p-52/s)).epsilon(1e-14));
  // Offset ties: the same correction far from 1.
  const std::vector<double> y{1e6,1e6,std::nextafter(1e6,2e6),std::nextafter(1e6,2e6)};
  const double gap=std::nextafter(1e6,2e6)-1e6;
  const auto ry=cppunivariate::norm_mle(mltest::as_span(y));
  REQUIRE(ry.status==cppunivariate::NormStatus::Ok);
  CHECK(std::abs(ry.sd/(gap/2)-1)<1e-14);
  // Ordinary data keep the rounded-mean profile bit for bit.
  const std::vector<double> z{0.3,1.7,2.2,-0.4,5.1};
  const auto rz=cppunivariate::norm_mle(mltest::as_span(z));
  CHECK(rz.sd==cppunivariate::norm_detail::moments(mltest::as_span(z)).sd);
}
