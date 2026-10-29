#include <algorithm>
#include "test_support.hpp"
#include "cppunivariate/distributions/gamma/mle.hpp"
#include "cppunivariate/distributions/lgamma/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>

TEST_CASE("lgamma (wrapper-only: gamma on log(x))") {
  auto g = mltest::load_distribution_golden("lgamma");
  // Replicate mllgamma_ wrapper: y = log(x); shapelog/ratelog from gamma_mle;
  // logLik recomputed with the log-gamma density.
  std::vector<double> y;
  y.reserve(g.data.size());
  for (double v : g.data) y.push_back(std::log(v));
  auto r = cppunivariate::gamma_mle(mltest::as_span(y), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shapelog")).epsilon(mltest::kTol));
  CHECK(r.rate == doctest::Approx(g.estimates.at("ratelog")).epsilon(mltest::kTol));
  // logLik for log-gamma is recomputed by the wrapper; match here.
  const double dn = static_cast<double>(g.data.size());
  double sum_y = 0.0, sum_log_y = 0.0;
  for (double v : g.data) {
    const double ly = std::log(v);
    sum_y += ly;
    sum_log_y += std::log(ly);
  }
  const double L = sum_y / dn;
  const double K = sum_log_y / dn;
  const double logLik =
      dn * (r.shape * std::log(r.rate) - std::lgamma(r.shape) +
            (r.shape - 1.0) * K - (r.rate + 1.0) * L);
  CHECK(logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}


// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("lgamma: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("lgamma",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dlgamma(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::plgamma(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qlgamma(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rlgamma(rng, p[0], p[1]); });
}

TEST_CASE("lgamma P2 every held subset") {
  const auto g=mltest::load_distribution_golden("lgamma");
  std::vector<double> x=g.data;
  for(auto &v:x)v=std::log(v);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("shapelog"),g.estimates.at("ratelog")};
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.rate));
    CHECK(std::isfinite(r.logLik));
    double jac=0;for(double v:x)jac+=v;
    CHECK(r.logLik-jac>=g.logLik-1e-6*x.size());
  }
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,nan,held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.rate==held[1]);
    CHECK(std::isfinite(r.logLik));
    double jac=0;for(double v:x)jac+=v;
    CHECK(r.logLik-jac>=g.logLik-1e-6*x.size());
  }
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,held[0],held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.rate==held[1]);
    CHECK(std::isfinite(r.logLik));
    double jac=0;for(double v:x)jac+=v;
    CHECK(r.logLik-jac>=g.logLik-1e-6*x.size());
  }
}
