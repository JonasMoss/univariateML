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
  CHECK(r.mean == doctest::Approx(g.estimates.at("mean")).epsilon(mltest::kTol));
  CHECK(r.sd == doctest::Approx(g.estimates.at("sd")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
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
