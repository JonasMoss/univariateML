#include "test_support.hpp"
#include "cppunivariate/distributions/hnorm/mle.hpp"
#include "cppunivariate/distributions/hnorm/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>

TEST_CASE("hnorm: closed form and golden") {
  const auto g = mltest::load_distribution_golden("hnorm");
  const auto r = cppunivariate::hnorm_mle(mltest::as_span(g.data));
  CHECK(r.status == cppunivariate::HnormStatus::Ok);
  CHECK(r.mu == doctest::Approx(g.estimates.at("mu")).epsilon(1e-12));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sigma")).epsilon(1e-12));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(1e-12));
  const std::vector<double> x = {-2, -2, 1, 2};
  const auto fit = cppunivariate::hnorm_mle(mltest::as_span(x));
  CHECK(fit.mu == -2);
  CHECK(fit.sigma == doctest::Approx(2.5));
  double ll = 0;
  for (double v : x) ll += cppunivariate::distributions::dhalfnorm(v, fit.mu, fit.sigma, true);
  CHECK(fit.logLik == doctest::Approx(ll));
}
TEST_CASE("hnorm: degeneracy and nonfinite inputs") {
  for (const std::vector<double> &x : {std::vector<double>{}, {1}, {2,2}})
    CHECK(cppunivariate::hnorm_mle(mltest::as_span(x)).status == cppunivariate::HnormStatus::DegenerateData);
  const std::vector<double> bad = {0, cppunivariate::special::kPosInf};
  CHECK(cppunivariate::hnorm_mle(mltest::as_span(bad)).status == cppunivariate::HnormStatus::InvalidData);
}
TEST_CASE("hnorm: d/p/q/r") {
  for (bool lower : {false,true}) for (bool logp : {false,true})
    for (double u : {0.0, 0.001, 0.2, 0.8, 1.0}) {
      const double p = logp ? std::log(u) : u;
      const double q = cppunivariate::distributions::qhalfnorm(p,-2,3,lower,logp);
      CHECK(cppunivariate::distributions::phalfnorm(q,-2,3,lower,false) == doctest::Approx(u).epsilon(1e-12));
    }
  CHECK(cppunivariate::distributions::dhalfnorm(-3,-2,3,false) == 0);
  CHECK(cppunivariate::distributions::dhalfnorm(-2,-2,3,false) > 0);
  double integral = 0;
  for (int i=0; i<10000; ++i)
    integral += 0.003 * cppunivariate::distributions::dhalfnorm(-2+(i+0.5)*0.003,-2,3,false);
  CHECK(integral == doctest::Approx(1.0).epsilon(1e-8));
  cppunivariate::distributions::StdRng<> rng(37);
  for (int i=0;i<100;++i) CHECK(cppunivariate::distributions::rhalfnorm(rng,-2,3) >= -2);
}
TEST_CASE("hnorm: held endpoint") {
  const std::vector<double> x={0,1,2}, one={3}, bad={-1,2}, zero={0};
  CHECK(cppunivariate::hnorm_mle(mltest::as_span(x),0).sigma==doctest::Approx(std::sqrt(5.0/3)));
  CHECK(cppunivariate::hnorm_mle(mltest::as_span(one),0).sigma==doctest::Approx(3));
  CHECK(cppunivariate::hnorm_mle(mltest::as_span(bad),0).status==cppunivariate::HnormStatus::InvalidData);
  CHECK(cppunivariate::hnorm_mle(mltest::as_span(zero),0).status==cppunivariate::HnormStatus::DegenerateData);
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("hnorm: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("hnorm",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dhalfnorm(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::phalfnorm(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qhalfnorm(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rhalfnorm(rng, p[0], p[1]); });
}

TEST_CASE("hnorm: held scale removes collapse") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{2,2};
  auto r=cppunivariate::hnorm_mle(mltest::as_span(x),nan,1.7);
  CHECK(r.status==cppunivariate::HnormStatus::Ok);
  CHECK(r.mu==2);CHECK(r.sigma==1.7);
  CHECK(r.logLik==doctest::Approx(2*(.5*std::log(2/std::acos(-1.0))-std::log(1.7))));
  CHECK(cppunivariate::hnorm_mle(mltest::as_span(x),2,1).status==cppunivariate::HnormStatus::Ok);
  CHECK(cppunivariate::hnorm_mle(mltest::as_span(x),nan,-1).status==cppunivariate::HnormStatus::InvalidData);
}


// mpmath generator: import mpmath as mp; mp.mp.dps=100
// Q=lambda z: mp.erfc(z/mp.sqrt(2)); mp.mpf(float(0.003132226774118849))+mp.findroot(lambda z: mp.log(Q(z))+800,40)
TEST_CASE("hnorm: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qhalfnorm(-800.0, 0.003132226774118849, 1.0, true, true) == doctest::Approx(0.003132226774118849).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qhalfnorm(-800.0, 0.003132226774118849, 1.0, false, true) == doctest::Approx(39.90519116196223).epsilon(1e-11));
}

TEST_CASE("hnorm: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qhalfnorm(cppunivariate::special::kNegInf, 0.003132226774118849, 1.0, true, true) == 0.003132226774118849);
  CHECK(cppunivariate::distributions::qhalfnorm(0.0, 0.003132226774118849, 1.0, false, true) == 0.003132226774118849);
  CHECK(cppunivariate::distributions::qhalfnorm(cppunivariate::special::kNegInf, 0.003132226774118849, 1.0, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qhalfnorm(0.0, 0.003132226774118849, 1.0, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("hnorm: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dhalfnorm(1e100, 0.003132226774118849, 1.0, true) == doctest::Approx(-5e+199).epsilon(1e-11));
  CHECK(cppunivariate::distributions::phalfnorm(1e100, 0.003132226774118849, 1.0, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::phalfnorm(1e100, 0.003132226774118849, 1.0, false, true) == doctest::Approx(-5e+199).epsilon(1e-11));
}

// import mpmath as mp; mp.mp.dps=100
// x=mp.mpf(float(1e-300)); s=mp.mpf(float(1e300))
// mp.log(mp.erf(x/(s*mp.sqrt(2))))
// mp.exp(-800)*mp.sqrt(mp.pi/2)*s  # omitted relative correction < exp(-1600)
// mp.sqrt(2)*mp.erfinv(mp.mpf(float(1e-12)))
TEST_CASE("hnorm: small standardized distances retain their logarithms") {
  CHECK(cppunivariate::distributions::qhalfnorm(1e-12,0,1,true,false) == doctest::Approx(1.2533141373155002e-12).epsilon(1e-12));
  CHECK(cppunivariate::distributions::phalfnorm(1e-300,0,1e300,true,true) == doctest::Approx(-1381.7768471490721).epsilon(1e-12));
  CHECK(cppunivariate::distributions::qhalfnorm(-800,0,1e300,true,true) == doctest::Approx(4.5969990702501075e-48).epsilon(1e-12));
}
