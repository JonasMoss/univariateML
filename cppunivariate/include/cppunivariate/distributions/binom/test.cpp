#include "test_support.hpp"
#include "cppunivariate/distributions/binom/mle.hpp"
#include "cppunivariate/distributions/binom/dpqr.hpp"
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
 TEST_CASE("binom") {
  auto g = mltest::load_distribution_golden("binom");
  auto v = as_ints(g.data);
  // size_or_nan: estimate size when NaN. Wrapper passes NaN by default.
  auto r = cppunivariate::binom_mle(as_ispan(v),
                             std::numeric_limits<double>::quiet_NaN(),
                             mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.size == D_APPROX(g.estimates.at("size")));
  CHECK(r.prob == D_APPROX(g.estimates.at("prob")));
  CHECK(r.logLik == D_APPROX(g.logLik));
}

TEST_CASE("binom probability boundaries") {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  for (const double size : {nan, 5.0}) {
    const std::vector<int> v{5, 5, 5};
    const auto r = cppunivariate::binom_mle(as_ispan(v), size, mltest::kFitTol, mltest::kFitMaxit);
    REQUIRE(r.status == cppunivariate::BinomStatus::Ok);
    CHECK(r.size == 5.0);
    CHECK(r.prob == 1.0);
    CHECK(r.logLik == 0.0);
  }
  for (const double size : {nan, 0.0, 5.0}) {
    const std::vector<int> v{0, 0, 0};
    const auto r = cppunivariate::binom_mle(as_ispan(v), size, mltest::kFitTol, mltest::kFitMaxit);
    REQUIRE(r.status == cppunivariate::BinomStatus::Ok);
    CHECK(r.size == (std::isnan(size) ? 0.0 : size));
    CHECK(r.prob == 0.0);
    CHECK(r.logLik == 0.0);
  }
}

TEST_CASE("binom dpqr: trivial trial size, CDF endpoints") {
  // Bernoulli(0.5): P(X=0) = P(X=1) = 0.5.
  CHECK(dist::dbinom(0.0, 1.0, 0.5, false) == doctest::Approx(0.5));
  CHECK(dist::dbinom(1.0, 1.0, 0.5, false) == doctest::Approx(0.5));
  // CDF endpoint.
  CHECK(dist::pbinom(10.0, 10.0, 0.3, true, false) == doctest::Approx(1.0));
  // Round-trip.
  const double k = dist::qbinom(0.7, 20.0, 0.4, true, false);
  CHECK(dist::pbinom(k, 20.0, 0.4, true, false) >= 0.7);
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("binom: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("binom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dbinom(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pbinom(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qbinom(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rbinom(rng, p[0], p[1]); });
}

TEST_CASE("binom: free size brackets the integer profile maximum") {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for (auto x : {std::vector<int>{2,6,6},std::vector<int>{1,2,3,4,5},std::vector<int>{1,3,3,4}}) {
   const auto f=cppunivariate::binom_mle(as_ispan(x),nan,1e-10,100);
   REQUIRE(f.status==cppunivariate::BinomStatus::Ok);
   for(int k=*std::max_element(x.begin(),x.end()); k<=500; ++k)
     CHECK(f.logLik>=cppunivariate::binom_mle(as_ispan(x),k,1e-10,100).logLik-1e-10);
 }
 const std::vector<int> x{2,6,6};
 CHECK(cppunivariate::binom_mle(as_ispan(x),nan,1e-10,100).size==13);
}

TEST_CASE("binom: held probability profiles every integer and endpoint") {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(auto x:{std::vector<int>{1,2,2,4},std::vector<int>{0,0},std::vector<int>{3,3}}) {
  const auto f=cppunivariate::binom_mle({x.data(),x.size()},nan,1e-10,100,.4);
  REQUIRE(f.status==cppunivariate::BinomStatus::Ok);CHECK(f.prob==.4);
  for(int k=*std::max_element(x.begin(),x.end());k<100;++k) {
   const auto o=cppunivariate::binom_mle({x.data(),x.size()},k,1e-10,100,.4);
   CHECK(f.logLik>=o.logLik-1e-10);
  }
 }
 const std::vector<int> x{2,2};
 CHECK(cppunivariate::binom_mle({x.data(),x.size()},nan,1e-10,100,1).size==2);
 CHECK(cppunivariate::binom_mle({x.data(),x.size()},nan,1e-10,100,0).status==cppunivariate::BinomStatus::InvalidInput);
}
