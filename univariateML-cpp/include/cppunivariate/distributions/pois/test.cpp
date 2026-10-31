#include "test_support.hpp"
#include "cppunivariate/distributions/pois/mle.hpp"
#include "cppunivariate/distributions/pois/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace dist = cppunivariate::distributions;
#define D_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define D_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)
namespace {
std::vector<int> as_ints(const std::vector<double> &v) {
  std::vector<int> out;
  for (double x : v) out.push_back(static_cast<int>(x));
  return out;
}
cppunivariate::ispan as_ispan(const std::vector<int> &v) { return {v.data(), v.size()}; }
}
TEST_CASE("pois") {
  auto g = mltest::load_distribution_golden("pois");
  auto v = as_ints(g.data);
  auto r = cppunivariate::pois_mle(as_ispan(v));
  CHECK(r.lambda == D_APPROX(g.estimates.at("lambda")));
  CHECK(r.logLik == D_APPROX(g.logLik));
}

TEST_CASE("pois all zeros") {
  const std::vector<int> v{0, 0, 0};
  const auto r = cppunivariate::pois_mle(as_ispan(v));
  CHECK(r.lambda == 0.0);
  CHECK(r.logLik == 0.0);
}

TEST_CASE("pois dpqr: pmf and CDF tail") {
  CHECK(dist::dpois(0.0, 1.0, false) == doctest::Approx(std::exp(-1.0)));
  CHECK(dist::ppois(0.0, 1.0, true, false) == doctest::Approx(std::exp(-1.0)));
  // qpois(p) is the smallest k with cdf >= p.
  const double k = dist::qpois(0.95, 4.0, true, false);
  CHECK(dist::ppois(k, 4.0, true, false) >= 0.95);
  CHECK(dist::ppois(k - 1.0, 4.0, true, false) < 0.95);
}

// Independent high-precision oracle; see quality/truth.py.
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("pois: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("pois",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dpois(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ppois(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qpois(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rpois(rng, p[0]); });
}

TEST_CASE("pois: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("pois");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<2;++mask) {
   const auto r=cppunivariate::pois_mle({x.data(),x.size()},(mask & 1)?g.estimates.at("lambda"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.lambda==g.estimates.at("lambda"));
 }
}

TEST_CASE("pois: P2 held boundary likelihood and domain") {
 const std::vector<int> x{0,2},same{0,0};
 const auto incompatible=cppunivariate::pois_mle({x.data(),x.size()},0);
 CHECK_FALSE(std::isfinite(incompatible.logLik));
 const auto valid=cppunivariate::pois_mle({same.data(),same.size()},0);
 CHECK(valid.lambda==0);
 CHECK(std::isfinite(valid.logLik));
 const auto invalid=cppunivariate::pois_mle({x.data(),x.size()},-1);
 CHECK_FALSE(std::isfinite(invalid.logLik));
}


TEST_CASE("pois: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // l=mpf(2.0)
  // d=lambda k:exp(-l)*l**k/factorial(k); S=lambda k:gammainc(k+1,0,l)/gamma(k+1)
  // print(log(d(mpf(1e+100)))); print(log(S(mpf(110.0))))
  // lo,hi=0,1
  // while log(S(hi))>-10000: hi*=2
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::dpois(1e+100, 2.0, true) == doctest::Approx(-2.2856536211884465e+102).epsilon(1e-10));
  CHECK(cppunivariate::distributions::ppois(110.0, 2.0, false, true) == doctest::Approx(-340.07495410418113).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qpois(-10000, 2.0, false, true) == 1733.0);
  CHECK(cppunivariate::distributions::dpois(0.5, 2.0, false) == 0);
}

TEST_CASE("pois: adjacent noninteger at zero has no mass") {
  // mp.dps=100; x=mpf(float.fromhex("0x0.0000000000001p-1022")); print(x==floor(x))
  CHECK(cppunivariate::distributions::dpois(std::nextafter(0.0,1.0),2,false)==0);
}

TEST_CASE("pois: TASK-132 invalid counts and empty input have statuses") {
  const std::vector<int> empty, negative{-1,2}, zero(4,0), one(4,1);
  CHECK(cppunivariate::pois_mle(cppunivariate::ispan{empty.data(),empty.size()}).status == cppunivariate::PoisStatus::InvalidInput);
  CHECK(cppunivariate::pois_mle(cppunivariate::ispan{negative.data(),negative.size()}).status == cppunivariate::PoisStatus::InvalidInput);
  CHECK(cppunivariate::pois_mle(cppunivariate::ispan{zero.data(),zero.size()}).status == cppunivariate::PoisStatus::Ok);
}

#include "cppunivariate/distributions/pois/score.hpp"
TEST_CASE("pois: inference every free subset and MLE score") {
  std::vector<double> x{0,1,2,4,7}; std::vector<int> ix{0,1,2,4,7};
  const auto full=cppunivariate::pois_inference(mltest::as_span(x),2.3);
  REQUIRE(full.status==cppunivariate::InferenceStatus::Ok);
  for(unsigned mask=0; mask<2; ++mask) {
    std::vector<double> sums(1,0);
    std::array<double,4> products{};
    std::size_t rows=0;
    const auto a=cppunivariate::pois_inference(mltest::as_span(x),2.3,mask,
      [&](const std::vector<double>& row) {++rows; for(std::size_t j=0;j<row.size();++j) { sums[j]+=row[j]; for(std::size_t k=0;k<row.size();++k) products[j*row.size()+k]+=row[j]*row[k]; }});
    REQUIRE(a.status==cppunivariate::InferenceStatus::Ok);
    CHECK(rows==x.size());
    CHECK(a.observations==x.size());
    for(std::size_t j=0;j<a.dimension;++j) {
      CHECK(a.score[j]==doctest::Approx(sums[j]));
      CHECK(a.score[j]==doctest::Approx(full.score[a.free_indices[j]]));
      for(std::size_t k=0;k<a.dimension;++k) {
        const auto dst=j*a.dimension+k, src=a.free_indices[j]*1+a.free_indices[k];
        CHECK(a.opg[dst]==doctest::Approx(full.opg[src]));
        CHECK(a.opg[dst]==doctest::Approx(products[dst]));
        CHECK(a.observed[dst]==doctest::Approx(full.observed[src]));
        CHECK(a.fisher[dst]==doctest::Approx(full.fisher[src]));
      }
    }
    const auto f=cppunivariate::pois_mle(cppunivariate::ispan{ix.data(),ix.size()},(mask&1)?NAN:2.3);
    REQUIRE(f.status==cppunivariate::PoisStatus::Ok);
    const auto fitted=cppunivariate::pois_inference(mltest::as_span(x),f.lambda,mask);
    REQUIRE(fitted.status==cppunivariate::InferenceStatus::Ok);
    for(std::size_t j=0;j<fitted.dimension;++j) CHECK(std::abs(fitted.score[j])<1e-7);
  }
  CHECK(cppunivariate::pois_inference(mltest::as_span(x),2.3,2).status==cppunivariate::InferenceStatus::InvalidSubset);
  CHECK(cppunivariate::pois_inference(cppunivariate::span{x.data(),0},2.3).status==cppunivariate::InferenceStatus::EmptySample);
}

TEST_CASE("pois: analytic information versus log density central differences") {
  std::vector<double> x{3.0}, p{2.3};
  const auto r=cppunivariate::pois_inference(mltest::as_span(x),p[0]);
  REQUIRE(r.status==cppunivariate::InferenceStatus::Ok);
  const auto ld=[&]() {return cppunivariate::distributions::dpois(x[0],p[0],true);};
  const double step=1e-4, base=ld();
  for(std::size_t j=0;j<1;++j) {
    const double saved=p[j];
    p[j]=saved+step; const double plus=ld();
    p[j]=saved-step; const double minus=ld(); p[j]=saved;
    CHECK(r.score[j]==doctest::Approx((plus-minus)/(2*step)).epsilon(2e-6));
    CHECK(r.observed[j*1+j]==doctest::Approx(-(plus-2*base+minus)/(step*step)).epsilon(2e-6));
  }
}

TEST_CASE("pois: inference validity and total normalization") {
  std::vector<double> x{1};
  const auto one=cppunivariate::pois_inference(mltest::as_span(x),2);
  x.push_back(x[0]);
  const auto two=cppunivariate::pois_inference(mltest::as_span(x),2);
  REQUIRE(two.status==cppunivariate::InferenceStatus::Ok);
  for(std::size_t j=0;j<two.dimension;++j) CHECK(two.score[j]==doctest::Approx(2*one.score[j]));
  for(std::size_t j=0;j<two.dimension*two.dimension;++j) {
    CHECK(two.opg[j]==doctest::Approx(2*one.opg[j]));
    CHECK(two.observed[j]==doctest::Approx(2*one.observed[j]));
    CHECK(two.fisher[j]==doctest::Approx(2*one.fisher[j]));
  }
  CHECK(cppunivariate::pois_inference(mltest::as_span(x),0).status==cppunivariate::InferenceStatus::InvalidParameter);
  x[0]=0.5;
  CHECK(cppunivariate::pois_inference(mltest::as_span(x),2,0).status==cppunivariate::InferenceStatus::InvalidObservation);
}

TEST_CASE("pois: integer input and boundary observed information") {
  std::vector<int> x{0,0};
  const auto r=cppunivariate::pois_inference(cppunivariate::ispan{x.data(),x.size()},2);
  CHECK(r.status==cppunivariate::InferenceStatus::Ok);
  CHECK(r.observed_status==cppunivariate::InferenceStatus::Singular);
  CHECK(r.fisher_status==cppunivariate::InferenceStatus::Ok);
  CHECK(r.score[0]==-2);
  CHECK(r.opg[0]==2);
}

TEST_CASE("pois: large counts retain near-zero scores") {
  for(double rate : {1e6,1e9,1e12}) {
    std::vector<double> x{rate+1};
    const auto r=cppunivariate::pois_inference(mltest::as_span(x),rate);
    REQUIRE(r.status==cppunivariate::InferenceStatus::Ok);
    CHECK(r.score[0]==doctest::Approx(1/rate).epsilon(1e-14));
  }
}
