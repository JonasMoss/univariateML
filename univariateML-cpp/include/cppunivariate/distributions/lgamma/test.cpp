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
  const double rate = 1.0 / r.scale;
  CHECK(r.shape == doctest::Approx(g.estimates.at("shapelog")).epsilon(mltest::kTol));
  CHECK(rate == doctest::Approx(g.estimates.at("ratelog")).epsilon(mltest::kTol));
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
      dn * (r.shape * std::log(rate) - std::lgamma(r.shape) +
            (r.shape - 1.0) * K - (rate + 1.0) * L);
  CHECK(logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}


// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("lgamma: d/p/q/r against the truth table") {
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
  // held[1] is ratelog; gamma_mle's 5th argument is a scale (1/rate).
  const std::vector<double> held{g.estimates.at("shapelog"),g.estimates.at("ratelog")};
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.scale));
    CHECK(std::isfinite(r.logLik));
    double jac=0;for(double v:x)jac+=v;
    CHECK(r.logLik-jac>=g.logLik-1e-6*x.size());
  }
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,nan,1.0/held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.scale==1.0/held[1]);
    CHECK(std::isfinite(r.logLik));
    double jac=0;for(double v:x)jac+=v;
    CHECK(r.logLik-jac>=g.logLik-1e-6*x.size());
  }
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,held[0],1.0/held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.scale==1.0/held[1]);
    CHECK(std::isfinite(r.logLik));
    double jac=0;for(double v:x)jac+=v;
    CHECK(r.logLik-jac>=g.logLik-1e-6*x.size());
  }
}


// mpmath generator: import mpmath as mp; mp.mp.dps=100
// P=lambda z:mp.gammainc(2,0,z)/mp.gamma(2)
// Q=lambda z:mp.gammainc(2,z,mp.inf)/mp.gamma(2)
// lo=mp.exp(mp.findroot(lambda y:mp.log(P(mp.exp(y)))+800,-mp.mpf(800)/2))
// hi=mp.findroot(lambda z:mp.log(Q(z))+800,800)
// [mp.exp(lo/3),mp.exp(hi/3)]
TEST_CASE("lgamma: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qlgamma(-800.0, 2.0, 3.0, true, true) == doctest::Approx(1.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qlgamma(-800.0, 2.0, 3.0, false, true) == doctest::Approx(6.038709799216898e+116).epsilon(1e-11));
}

TEST_CASE("lgamma: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qlgamma(cppunivariate::special::kNegInf, 2.0, 3.0, true, true) == 1.0);
  CHECK(cppunivariate::distributions::qlgamma(0.0, 2.0, 3.0, false, true) == 1.0);
  CHECK(cppunivariate::distributions::qlgamma(cppunivariate::special::kNegInf, 2.0, 3.0, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qlgamma(0.0, 2.0, 3.0, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("lgamma: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dlgamma(1e100, 2.0, 3.0, true) == doctest::Approx(-913.397609989046).epsilon(1e-11));
  CHECK(cppunivariate::distributions::plgamma(1e100, 2.0, 3.0, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::plgamma(1e100, 2.0, 3.0, false, true) == doctest::Approx(-684.2362663768691).epsilon(1e-11));
}

#include "cppunivariate/distributions/lgamma/mle.hpp"
TEST_CASE("lgamma core matches previous adapter for every held subset") {
  const auto g=mltest::load_distribution_golden("lgamma");
  std::vector<double> y;
  double jac=0;
  for(double v:g.data) {y.push_back(std::log(v));jac+=y.back();}
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=0;mask<4;++mask) {
    const double shape=(mask&1) ? g.estimates.at("shapelog") : nan;
    const double rate=(mask&2) ? g.estimates.at("ratelog") : nan;
    const auto old=cppunivariate::gamma_mle(mltest::as_span(y),mltest::kFitTol,100,shape,(mask&2)?1/rate:nan);
    const auto r=cppunivariate::lgamma_mle(mltest::as_span(g.data),mltest::kFitTol,100,shape,rate);
    CHECK(r.status==cppunivariate::LgammaStatus::Ok);
    CHECK(r.shapelog==old.shape);
    CHECK(r.ratelog==((mask&2)?rate:1/old.scale));
    CHECK(r.logLik==old.logLik-jac);
    CHECK(r.iterations==old.iterations);
  }
  const auto failed=cppunivariate::lgamma_mle(mltest::as_span(g.data),1e-15,1,nan,2);
  CHECK(failed.status==cppunivariate::LgammaStatus::IterationFailure);
  const std::vector<double> ties{2,2};
  CHECK(cppunivariate::lgamma_mle(mltest::as_span(ties),1e-8,100).status==cppunivariate::LgammaStatus::Degenerate);
}

TEST_CASE("lgamma: TASK-132 empty and invalid delegates preserve non-success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2},std::vector<double>{-1,2}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::lgamma_mle(x,1e-8,200).status != cppunivariate::LgammaStatus::Ok);
  }
}
