// lomax: golden regression, start value, existence condition, d/p/q/r.

#include "test_support.hpp"

#include "cppunivariate/distributions/lomax/dpqr.hpp"
#include "cppunivariate/distributions/lomax/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"

#include <cmath>
#include <vector>

TEST_CASE("lomax: golden") {
  auto g = mltest::load_distribution_golden("lomax");
  auto r = cppunivariate::lomax_mle(mltest::as_span(g.data), mltest::kFitTol,
                                    mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::LomaxStatus::Ok);
  CHECK(r.lambda == doctest::Approx(g.estimates.at("lambda")).epsilon(mltest::kTol));
  CHECK(r.kappa == doctest::Approx(g.estimates.at("kappa")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("lomax: an explicit start converges to the same fit") {
  auto g = mltest::load_distribution_golden("lomax");
  const auto x = mltest::as_span(g.data);
  auto a = cppunivariate::lomax_mle(x, 1e-12, 200);
  auto b = cppunivariate::lomax_mle(x, 1e-12, 200, 3.0 * a.lambda);
  CHECK(b.status == cppunivariate::LomaxStatus::Ok);
  CHECK(b.lambda == doctest::Approx(a.lambda).epsilon(1e-8));
  CHECK(b.kappa == doctest::Approx(a.kappa).epsilon(1e-8));
}

TEST_CASE("lomax: concentrated sample selects the exponential limit") {
  const std::vector<double> x = {1.0, 1.1, 0.9, 1.05, 0.95};
  auto r = cppunivariate::lomax_mle({x.data(), x.size()}, mltest::kFitTol,
                                    mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::LomaxStatus::ExponentialLimit);
}

TEST_CASE("lomax: rlomax is positive; qlomax and plomax round-trip") {
  cppunivariate::distributions::StdRng<> rng(127);
  for (int i = 0; i < 5000; ++i) {
    CHECK(cppunivariate::distributions::rlomax(rng, 2.0, 3.0) > 0.0);
  }
  const double q = cppunivariate::distributions::qlomax(0.7, 2.0, 3.0, true, false);
  const double back = cppunivariate::distributions::plomax(q, 2.0, 3.0, true, false);
  CHECK(std::abs(back - 0.7) < 1e-10);
}

TEST_CASE("lomax: bounded global profile, scaling and replication") {
  const std::vector<std::vector<double>> cases = {{1,100},{.001,4,6},{.001,1,2},{1,29},{1,1,1,12,12}};
  const std::vector<double> gaps = {.590084,1.338305,.652423,.001924,1.90927e-4};
  const std::vector<double> lambdas = {.772199,1394.08,1039.29,.236987,.0359623};
  const std::vector<double> kappas = {.405536,.161888,.196081,.878708,6.07162};
  for (std::size_t j=0;j<cases.size();++j) {
    const auto &x=cases[j];
    const auto f=cppunivariate::lomax_mle(mltest::as_span(x),1e-12,200);
    REQUIRE(f.status==cppunivariate::LomaxStatus::Ok);
    double m=0; for (double v:x) m+=v/x.size();
    CHECK(f.logLik+x.size()*(std::log(m)+1) == doctest::Approx(gaps[j]).epsilon(5e-5));
    CHECK(f.lambda == doctest::Approx(lambdas[j]).epsilon(5e-6));
    CHECK(f.kappa == doctest::Approx(kappas[j]).epsilon(5e-6));
    for (double scale : {1e-200,1e200}) {
      auto z=x; for (double &v:z) v*=scale;
      auto g=cppunivariate::lomax_mle(mltest::as_span(z),1e-12,200);
      CHECK(g.status==cppunivariate::LomaxStatus::Ok);
      CHECK(g.lambda*scale == doctest::Approx(f.lambda).epsilon(1e-6));
      CHECK(g.kappa == doctest::Approx(f.kappa).epsilon(1e-6));
    }
    std::vector<double> repeated;
    for (int k=0;k<50;++k) repeated.insert(repeated.end(),x.begin(),x.end());
    const auto g=cppunivariate::lomax_mle(mltest::as_span(repeated),1e-12,200);
    CHECK(g.lambda == doctest::Approx(f.lambda).epsilon(1e-6));
    CHECK(g.logLik/50 == doctest::Approx(f.logLik).epsilon(1e-12));
  }
  for (const auto &x : std::vector<std::vector<double>>{{1,10},{1,28},{1,1,1}})
    CHECK(cppunivariate::lomax_mle(mltest::as_span(x),1e-12,200).status==cppunivariate::LomaxStatus::ExponentialLimit);
}

TEST_CASE("lomax: compressed nominations refine against the complete sample") {
  for (const auto &x : std::vector<std::vector<double>>{{1,100},{1,29},{1,1,1,12,12}}) {
    const auto f=cppunivariate::lomax_mle(mltest::as_span(x),1e-12,200);
    std::vector<double> large;
    for (int j=0;j<3000;++j) large.insert(large.end(),x.begin(),x.end());
    const auto g=cppunivariate::lomax_mle(mltest::as_span(large),1e-12,200);
    REQUIRE(g.status==cppunivariate::LomaxStatus::Ok);
    CHECK(g.lambda==doctest::Approx(f.lambda).epsilon(1e-6));
    CHECK(g.kappa==doctest::Approx(f.kappa).epsilon(1e-6));
    CHECK(g.logLik/3000==doctest::Approx(f.logLik).epsilon(1e-10));
  }
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("lomax: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("lomax",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dlomax(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::plomax(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qlomax(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rlomax(rng, p[0], p[1]); });
}
