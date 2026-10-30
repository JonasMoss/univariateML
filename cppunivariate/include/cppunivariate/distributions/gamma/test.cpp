#include <algorithm>
#include "test_support.hpp"
#include "cppunivariate/distributions/gamma/mle.hpp"
#include "cppunivariate/distributions/gamma/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>

TEST_CASE("gamma") {
  auto g = mltest::load_distribution_golden("gamma");
  auto r = cppunivariate::gamma_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kTol));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("gamma dpqr: scale parameterisation round-trip") {
  // mean = shape * scale.
  const double q = cppunivariate::distributions::qgamma(0.5, 2.0, 1.0, true, false);
  CHECK(cppunivariate::distributions::pgamma(q, 2.0, 1.0, true, false) == doctest::Approx(0.5));
  // density at mean is positive and finite.
  CHECK(cppunivariate::distributions::dgamma(2.0, 2.0, 1.0, false) > 0.0);
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("gamma: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("gamma",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dgamma(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pgamma(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qgamma(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rgamma(rng, p[0], p[1]); });
}

TEST_CASE("gamma P2 every held subset") {
  const auto g=mltest::load_distribution_golden("gamma");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("shape"),g.estimates.at("scale")};
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.scale));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dgamma(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,nan,held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dgamma(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,200,held[0],held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dgamma(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("gamma P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::gamma_mle(mltest::as_span(x),1e-10,1,nan,0.5).status==cppunivariate::GammaStatus::IterationFailure);
  CHECK(cppunivariate::gamma_mle(mltest::as_span(x),1e-10,100,2,nan).scale==doctest::Approx(1));
}

TEST_CASE("gamma held shape profiles scale without intermediate overflow") {
  const std::vector<double> x{1e308,1e308};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto r=cppunivariate::gamma_mle(mltest::as_span(x),1e-10,100,1e308,nan);
  CHECK(r.shape==1e308);
  CHECK(r.scale==doctest::Approx(1));
  // The likelihood itself is outside the direct lgamma formula's range;
  // a numerical failure is returned rather than a spurious finite fit.
  CHECK(r.status==cppunivariate::GammaStatus::IterationFailure);
}


// mpmath generator: import mpmath as mp; mp.mp.dps=100
// P=lambda z:mp.gammainc(2,0,z)/mp.gamma(2)
// Q=lambda z:mp.gammainc(2,z,mp.inf)/mp.gamma(2)
// lo=mp.exp(mp.findroot(lambda y:mp.log(P(mp.exp(y)))+800,-mp.mpf(800)/2))
// hi=mp.findroot(lambda z:mp.log(Q(z))+800,800)
// [lo/2,hi/2]
TEST_CASE("gamma: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qgamma(-800.0, 2.0, 0.5, true, true) == doctest::Approx(1.3542294089587789e-174).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qgamma(-800.0, 2.0, 0.5, false, true) == doctest::Approx(403.34709175055565).epsilon(1e-11));
}

TEST_CASE("gamma: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qgamma(cppunivariate::special::kNegInf, 2.0, 0.5, true, true) == 0.0);
  CHECK(cppunivariate::distributions::qgamma(0.0, 2.0, 0.5, false, true) == 0.0);
  CHECK(cppunivariate::distributions::qgamma(cppunivariate::special::kNegInf, 2.0, 0.5, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qgamma(0.0, 2.0, 0.5, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("gamma: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dgamma(1e100, 2.0, 0.5, true) == doctest::Approx(-2e+100).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pgamma(1e100, 2.0, 0.5, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pgamma(1e100, 2.0, 0.5, false, true) == doctest::Approx(-2e+100).epsilon(1e-11));
}

TEST_CASE("gamma: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("gamma");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::gamma_mle(x,1e-8,1).status==cppunivariate::GammaStatus::IterationFailure);
}

TEST_CASE("gamma: README scale validation") {
  cppunivariate::distributions::StdRng<> rng(94);
  for (double scale : {0.0, -1.0, double(INFINITY), double(NAN)}) {
    CHECK(std::isnan(cppunivariate::distributions::dgamma(0.5, 2.0, scale, false)));
    CHECK(std::isnan(cppunivariate::distributions::pgamma(0.5, 2.0, scale, false, true)));
    CHECK(std::isnan(cppunivariate::distributions::qgamma(0.5, 2.0, scale, false, true)));
    CHECK(std::isnan(cppunivariate::distributions::rgamma(rng, 2.0, scale)));
  }
}

TEST_CASE("gamma: finite scale with overflowing reciprocal") {
  const double scale = 1e-320;
  CHECK(std::isfinite(cppunivariate::distributions::dgamma(scale, 2.0, scale, true)));
  CHECK(std::isfinite(cppunivariate::distributions::pgamma(scale, 2.0, scale, false, true)));
  CHECK(cppunivariate::distributions::qgamma(0.5, 2.0, scale, true, false) > 0.0);
}
