#include "test_support.hpp"
#include "cppunivariate/distributions/nbinom/mle.hpp"
#include "cppunivariate/distributions/nbinom/dpqr.hpp"
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
 TEST_CASE("nbinom") {
  auto g = mltest::load_distribution_golden("nbinom");
  auto v = as_ints(g.data);
  auto r = cppunivariate::nbinom_mle(as_ispan(v), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.size == D_APPROX(g.estimates.at("size")));
  CHECK(r.prob == D_APPROX(g.estimates.at("prob")));
  CHECK(r.logLik == D_APPROX(g.logLik));
}

TEST_CASE("nbinom all zeros") {
  const std::vector<int> v{0, 0, 0};
  const auto r = cppunivariate::nbinom_mle(as_ispan(v), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.size == 0.0);
  CHECK(r.prob == .5);
  CHECK(r.logLik == 0.0);
  // Shared special/dpqr size-zero support belongs to TASK-78. Preserve
  // density coverage on the equally valid held-positive-size member.
  const auto held=cppunivariate::nbinom_mle(as_ispan(v),mltest::kFitTol,mltest::kFitMaxit,1);
  CHECK(cppunivariate::special::dnbinom(0.0, held.size, held.prob, true) == 0.0);
  CHECK(cppunivariate::special::dnbinom(1.0, held.size, held.prob, true) ==
        -std::numeric_limits<double>::infinity());
}

TEST_CASE("nbinom dpqr: matches dnbinom from special::") {
  CHECK(dist::dnbinom(0.0, 5.0, 0.5, false) > 0.0);
  // CDF round-trip.
  const double k = dist::qnbinom(0.6, 5.0, 0.5, true, false);
  CHECK(dist::pnbinom(k, 5.0, 0.5, true, false) >= 0.6);
  CHECK(dist::pnbinom(k - 1.0, 5.0, 0.5, true, false) < 0.6);
}

TEST_CASE("nbinom: underdispersion status") {
  const std::vector<int> x{1, 1, 1, 1};
  const auto r = cppunivariate::nbinom_mle(as_ispan(x), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::NbinomStatus::Underdispersed);
  CHECK(r.underdispersed);
}

TEST_CASE("nbinom: variance equal to the mean is the Poisson face") {
  // Population variance 2 equals the mean 2: no finite MLE.
  const std::vector<int> x{0, 1, 2, 3, 4};
  const auto r = cppunivariate::nbinom_mle(as_ispan(x), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::NbinomStatus::Underdispersed);
}

TEST_CASE("nbinom: bracketed fallback when Newton leaves the domain") {
  std::vector<int> x(9, 0);
  x.push_back(1000);
  const auto r = cppunivariate::nbinom_mle(as_ispan(x), mltest::kFitTol, mltest::kFitMaxit);
  REQUIRE(r.status == cppunivariate::NbinomStatus::Ok);
  REQUIRE(std::isfinite(r.size));
  // Profile score at the returned size is zero to rounding.
  double score = -10.0 * cppunivariate::special::digamma(r.size) -
                 10.0 * std::log1p(100.0 / r.size);
  for (int v : x) score += cppunivariate::special::digamma(r.size + v);
  CHECK(std::abs(score) < 1e-8);
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("nbinom: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("nbinom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dnbinom(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pnbinom(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qnbinom(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rnbinom(rng, p[0], p[1]); });
}

TEST_CASE("nbinom: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("nbinom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<4;++mask) {
   const auto r=cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,200,(mask & 1)?g.estimates.at("size"):nan,(mask & 2)?g.estimates.at("prob"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.size==g.estimates.at("size"));
   if(mask & 2)CHECK(r.prob==g.estimates.at("prob"));
 }
}

TEST_CASE("nbinom: P2 held-domain and non-existence statuses") {
 const std::vector<int> x{0,1,5},z{0,0};
 const double nan=std::numeric_limits<double>::quiet_NaN();
 CHECK(cppunivariate::nbinom_mle({z.data(),z.size()},1e-10,200,nan,.4).status==cppunivariate::NbinomStatus::Ok);
 CHECK(cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,200,nan,1).status==cppunivariate::NbinomStatus::InvalidInput);
 const auto r=cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,200,nan,.4);
 double score=3*std::log(.4);
 for(int v:x)for(int j=0;j<v;++j)score+=1/(r.size+j);
 CHECK(std::abs(score)<1e-10);
}

TEST_CASE("nbinom: P2 held probability near one has a stable likelihood") {
 const std::vector<int> x{0,1,5};
 const double p=1-1e-12,nan=std::numeric_limits<double>::quiet_NaN();
 const auto r=cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,200,nan,p);
 CHECK(r.prob==p);
 double ll=3*r.size*std::log(p)-cppunivariate::special::lgamma(6.0);
 ll+=6*std::log(r.size*(1-p));
 for(int j=0;j<5;++j)ll+=std::log1p(j/r.size);
 CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-13));
 CHECK(r.logLik==doctest::Approx(-6+6*std::log(2.0)-cppunivariate::special::lgamma(6.0)).epsilon(1e-10));
}

TEST_CASE("nbinom: P3 attained size zero preserves holds") {
 const std::vector<int> z{0,0,0};
 const auto r=cppunivariate::nbinom_mle({z.data(),z.size()},1e-10,200,0,.4);
 CHECK(r.status==cppunivariate::NbinomStatus::Ok);
 CHECK(r.size==0); CHECK(r.prob==.4); CHECK(r.logLik==0);
}
