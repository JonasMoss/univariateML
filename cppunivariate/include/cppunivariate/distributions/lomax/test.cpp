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

// from mpmath import mp, log, log1p, exp, expm1
// mp.dps=500; v=mp.mpf
// -2*log1p(v('1e200')); expm1(500);
// expm1(-log1p(-v('1e-300'))/2);
// expm1(-log1p(-exp(-1000))/v('1e-200'))/v('1e-200');
// -2*log1p(v('1e400')); expm1(v(745)/v('1e200'))/v('1e-200').
TEST_CASE("lomax: high precision extreme tails") {
  CHECK(cppunivariate::distributions::plomax(1e200,1,2,false,true) == doctest::Approx(-921.0340371976182736072).epsilon(2e-15));
  CHECK(std::abs(cppunivariate::distributions::qlomax(-1000,1,2,false,true)/1.4035922178528374107e217-1) < 2e-13);
  CHECK(std::abs(cppunivariate::distributions::qlomax(1e-300,1,2,true,false)/5e-301-1) < 2e-13);
  CHECK(std::abs(cppunivariate::distributions::qlomax(-1000,1e-200,1e-200,true,true)/5.0759588975494567653e-35-1) < 3e-13);
  CHECK(cppunivariate::distributions::plomax(1e200,1e200,2,false,true) == doctest::Approx(-1842.0680743952365472).epsilon(2e-15));
  CHECK(cppunivariate::distributions::plomax(1e-300,1,2,true,true) == doctest::Approx(std::log(2.0)-300*std::log(10.0)).epsilon(2e-15));
  CHECK(std::isfinite(cppunivariate::distributions::qlomax(-1600,1e100,2,false,true)));
  CHECK(cppunivariate::distributions::qlomax(-1e4,1,2,false,true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::dlomax(0,2,3,false) == doctest::Approx(6));
  CHECK(cppunivariate::distributions::qlomax(-745,1e-200,1e200,false,true) == doctest::Approx(745).epsilon(2e-15));
}

TEST_CASE("lomax: tail flags, endpoints, invalid probabilities") {
  const double lo = 0.0, hi = cppunivariate::special::kPosInf;
  for (bool lower : {false,true}) {
    CHECK(cppunivariate::distributions::qlomax(0,1,2,lower,false) == (lower ? lo : hi));
    CHECK(cppunivariate::distributions::qlomax(1,1,2,lower,false) == (lower ? hi : lo));
    CHECK(cppunivariate::distributions::qlomax(cppunivariate::special::kNegInf,1,2,lower,true) == (lower ? lo : hi));
    CHECK(cppunivariate::distributions::qlomax(0,1,2,lower,true) == (lower ? hi : lo));
    CHECK(std::isnan(cppunivariate::distributions::qlomax(-.1,1,2,lower,false)));
    CHECK(std::isnan(cppunivariate::distributions::qlomax(1.1,1,2,lower,false)));
    CHECK(std::isnan(cppunivariate::distributions::qlomax(.1,1,2,lower,true)));
    CHECK(cppunivariate::distributions::plomax(lo,1,2,lower,false) == (lower ? 0 : 1));
    CHECK(cppunivariate::distributions::plomax(hi,1,2,lower,false) == (lower ? 1 : 0));
    for (bool log_p : {false,true}) {
      for (double lp : {-1e-20,-.01,-1.0,-20.0,-100.0}) {
        const double p = log_p ? lp : std::exp(lp);
        const double q = cppunivariate::distributions::qlomax(p,1,2,lower,log_p);
        if (q > lo && q < hi) {
          const double back = cppunivariate::distributions::plomax(q,1,2,lower,true);
          // Near finite support edges, test rounding by bracketing neighbours.
          const double left = cppunivariate::distributions::plomax(std::nextafter(q,cppunivariate::special::kNegInf),1,2,lower,true);
          const double right = cppunivariate::distributions::plomax(std::nextafter(q,cppunivariate::special::kPosInf),1,2,lower,true);
          const double target = log_p ? p : std::log(p);
          CHECK((std::abs(back-target) < 1e-12 ||
                 (target >= std::min(left,right)-1e-12 && target <= std::max(left,right)+1e-12)));
        }
      }
    }
  }
}

// mpmath: mp.dps=500; v=mp.mpf
// h=v(1e200)*log1p(v(1e-200)*v(1e-200)); log(-expm1(-h)).
TEST_CASE("lomax: double-only rescued product") {
  CHECK(cppunivariate::distributions::plomax(1e-200,1e-200,1e200,true,true) == doctest::Approx(-460.51701859880913687).epsilon(2e-15));
  CHECK(cppunivariate::distributions::plomax(1e300,1e20,.1,false,true) == doctest::Approx(-73.68272297580947).epsilon(2e-15));
}
