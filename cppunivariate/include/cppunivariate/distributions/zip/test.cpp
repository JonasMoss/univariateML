#include "test_support.hpp"
#include "cppunivariate/distributions/zip/mle.hpp"
#include "cppunivariate/distributions/zip/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include "cppunivariate/distributions/pois/mle.hpp"
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
 TEST_CASE("zip") {
  auto g = mltest::load_distribution_golden("zip");
  auto v = as_ints(g.data);
  auto r = cppunivariate::zip_mle(as_ispan(v));
  CHECK(r.lambda == D_APPROX(g.estimates.at("lambda")));
  CHECK(r.pi == D_APPROX(g.estimates.at("pi")));
  CHECK(r.logLik == D_APPROX(g.logLik));
}

TEST_CASE("zip ordinary zero inflation") {
  const std::vector<std::vector<int>> samples{
      {0, 1, 2, 1, 1, 2, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3},
      {0, 0, 4}};
  const std::vector<double> lambdas{1.423265, 3.920695};
  const std::vector<double> pis{0.4730435, 0.6599249};
  const std::vector<double> logliks{-23.0097617, -3.52318909};
  for (std::size_t i = 0; i < samples.size(); ++i) {
    const auto r = cppunivariate::zip_mle(as_ispan(samples[i]));
    REQUIRE(r.status == cppunivariate::ZipStatus::Ok);
    CHECK(r.lambda == doctest::Approx(lambdas[i]).epsilon(1e-4));
    CHECK(r.pi == doctest::Approx(pis[i]).epsilon(1e-4));
    CHECK(r.logLik >= logliks[i] - 1e-6);
  }
}

TEST_CASE("zip boundaries and invalid input") {
  const std::vector<int> zeros{0, 0, 0};
  const auto zero = cppunivariate::zip_mle(as_ispan(zeros));
  REQUIRE(zero.status == cppunivariate::ZipStatus::Ok);
  CHECK(zero.lambda == 1.0);
  CHECK(zero.pi == 1.0);
  CHECK(zero.logLik == 0.0);
  for (const std::vector<int> &v : {std::vector<int>{1, 2, 3},
                                  std::vector<int>{0, 1, 1, 2}}) {
    const auto r = cppunivariate::zip_mle(as_ispan(v));
    const auto pois = cppunivariate::pois_mle(as_ispan(v));
    REQUIRE(r.status == cppunivariate::ZipStatus::Ok);
    CHECK(r.pi == 0.0);
    CHECK(r.lambda == pois.lambda);
    CHECK(r.logLik == doctest::Approx(pois.logLik));
  }
  for (const std::vector<int> &v : {std::vector<int>{}, std::vector<int>{-1, 0}}) {
    CHECK(cppunivariate::zip_mle(as_ispan(v)).status ==
          cppunivariate::ZipStatus::InvalidInput);
  }
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("zip: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("zip",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dzip(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pzip(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qzip(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rzip(rng, p[0], p[1]); });
}
