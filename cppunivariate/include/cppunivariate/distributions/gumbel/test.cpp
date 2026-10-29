#include "test_support.hpp"
#include "cppunivariate/distributions/gumbel/mle.hpp"
#include "cppunivariate/distributions/gumbel/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>


TEST_CASE("rgumbel samples; mean = mu + sigma*gamma_const, var = pi^2*sigma^2/6") {
  cppunivariate::distributions::StdRng<> rng(131);
  const int n = 20000;
  const double mu = 1.0, sigma = 2.0;
  double sum = 0.0, sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rgumbel(rng, mu, sigma);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  const double euler_mascheroni = 0.5772156649015329;
  const double expected_mean = mu + sigma * euler_mascheroni;
  const double expected_var = M_PI * M_PI * sigma * sigma / 6.0;
  CHECK(std::abs(mean - expected_mean) < 0.1);
  CHECK(std::abs(var - expected_var) < 0.5);
}

TEST_CASE("gumbel") {
  auto g = mltest::load_distribution_golden("gumbel");
  auto r = cppunivariate::fit_gumbel_newton(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.mu == doctest::Approx(g.estimates.at("mu")).epsilon(mltest::kTol));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sigma")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("gumbel: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) +
      "/gumbel/fixture-outlier.json");
  auto r = cppunivariate::fit_gumbel_newton(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.mu == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
}


// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("gumbel: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("gumbel",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dgumbel(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pgumbel(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qgumbel(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rgumbel(rng, p[0], p[1]); });
}

// from mpmath import mp, exp, log, log1p
// mp.dps=500
// -exp(7); 3-3*log(-log1p(-exp(-1000)));
// 3-3*log(10000); -1000-exp(-1000).
TEST_CASE("gumbel: high precision extreme tails") {
  CHECK(cppunivariate::distributions::pgumbel(-7,0,1,true,true) == doctest::Approx(-1096.6331584284585993).epsilon(2e-15));
  CHECK(cppunivariate::distributions::pgumbel(1e4,0,1,false,true) == -1e4);
  CHECK(cppunivariate::distributions::qgumbel(-1000,3,3,false,true) == 3003);
  CHECK(cppunivariate::distributions::qgumbel(-1e4,3,3,true,true) == doctest::Approx(-24.631021115928548208).epsilon(2e-15));
  CHECK(cppunivariate::distributions::dgumbel(1000,0,1,true) == -1000);
  CHECK(cppunivariate::distributions::pgumbel(745,0,1,false,false) == std::exp(-745.0));
  CHECK(cppunivariate::distributions::qgumbel(-1e4,3,3,false,true) == 30003);
}

TEST_CASE("gumbel: tail flags, endpoints, invalid probabilities") {
  const double lo = cppunivariate::special::kNegInf, hi = cppunivariate::special::kPosInf;
  for (bool lower : {false,true}) {
    CHECK(cppunivariate::distributions::qgumbel(0,0,1,lower,false) == (lower ? lo : hi));
    CHECK(cppunivariate::distributions::qgumbel(1,0,1,lower,false) == (lower ? hi : lo));
    CHECK(cppunivariate::distributions::qgumbel(cppunivariate::special::kNegInf,0,1,lower,true) == (lower ? lo : hi));
    CHECK(cppunivariate::distributions::qgumbel(0,0,1,lower,true) == (lower ? hi : lo));
    CHECK(std::isnan(cppunivariate::distributions::qgumbel(-.1,0,1,lower,false)));
    CHECK(std::isnan(cppunivariate::distributions::qgumbel(1.1,0,1,lower,false)));
    CHECK(std::isnan(cppunivariate::distributions::qgumbel(.1,0,1,lower,true)));
    CHECK(cppunivariate::distributions::pgumbel(lo,0,1,lower,false) == (lower ? 0 : 1));
    CHECK(cppunivariate::distributions::pgumbel(hi,0,1,lower,false) == (lower ? 1 : 0));
    for (bool log_p : {false,true}) {
      for (double lp : {-1e-20,-.01,-1.0,-20.0,-100.0}) {
        const double p = log_p ? lp : std::exp(lp);
        const double q = cppunivariate::distributions::qgumbel(p,0,1,lower,log_p);
        if (q > lo && q < hi) {
          const double back = cppunivariate::distributions::pgumbel(q,0,1,lower,true);
          // Near finite support edges, test rounding by bracketing neighbours.
          const double left = cppunivariate::distributions::pgumbel(std::nextafter(q,cppunivariate::special::kNegInf),0,1,lower,true);
          const double right = cppunivariate::distributions::pgumbel(std::nextafter(q,cppunivariate::special::kPosInf),0,1,lower,true);
          const double target = log_p ? p : std::log(p);
          CHECK((std::abs(back-target) < 1e-12 ||
                 (target >= std::min(left,right)-1e-12 && target <= std::max(left,right)+1e-12)));
        }
      }
    }
  }
}

// mpmath: mp.dps=500; -exp(-2); -2-exp(-2)-log(mp.mpf(1e308)).
TEST_CASE("gumbel: double-only affine overflow guards") {
  CHECK(cppunivariate::distributions::pgumbel(1e308,-1e308,1e308,true,true) == doctest::Approx(-0.13533528323661269189).epsilon(2e-15));
  CHECK(cppunivariate::distributions::dgumbel(1e308,-1e308,1e308,true) == doctest::Approx(-711.33154392540268338).epsilon(2e-15));
  CHECK(cppunivariate::distributions::dgumbel(1e308,-1e308,1e308,false) > 0);
  CHECK(cppunivariate::distributions::qgumbel(-std::exp(2.0),1e308,1e308,true,true) == doctest::Approx(-1e308).epsilon(2e-15));
}

TEST_CASE("gumbel: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("gumbel");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<4;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::fit_gumbel_held(mltest::as_span(g.data),1e-8,1000,((mask & 1) ? g.estimates.at("mu") : nan),((mask & 2) ? g.estimates.at("sigma") : nan));
    if(mask & 1)CHECK(r.mu==g.estimates.at("mu"));
    if(mask & 2)CHECK(r.sigma==g.estimates.at("sigma"));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("gumbel: held scale removes constant-sample collapse") {
  const std::vector<double> x={2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::fit_gumbel_held(mltest::as_span(x),1e-8,100,2,nan).status==cppunivariate::GumbelStatus::Unbounded);
  CHECK(cppunivariate::fit_gumbel_held(mltest::as_span(x),1e-8,100,nan,1).status==cppunivariate::GumbelStatus::Ok);
}
