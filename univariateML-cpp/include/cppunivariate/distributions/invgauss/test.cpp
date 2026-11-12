#include <algorithm>
#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/invgauss/mle.hpp"
#include "cppunivariate/distributions/invgauss/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("rinvgauss samples are positive; sample mean ~ mu") {
  cppunivariate::distributions::StdRng<> rng(151);
  const int n = 20000;
  const double mu = 2.0, shape = 5.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rinvgauss(rng, mu, shape);
    CHECK(x > 0.0);
    sum += x;
  }
  // E[X] = mu for invgauss
  CHECK(std::abs(sum / n - mu) < 0.1);
}

TEST_CASE("invgauss") {
  auto g = mltest::load_distribution_golden("invgauss");
  auto r = cppunivariate::invgauss_mle(mltest::as_span(g.data));
  CHECK(r.mu == doctest::Approx(g.estimates.at("mean")).epsilon(mltest::kGolden_invgauss).scale(0));
  CHECK(r.lambda == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kGolden_invgauss).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_invgauss).scale(0));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("invgauss: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("invgauss",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dinvgauss(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pinvgauss(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qinvgauss(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rinvgauss(rng, p[0], p[1]); });
}

TEST_CASE("invgauss P2 every held subset") {
  const auto g=mltest::load_distribution_golden("invgauss");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("mean"),g.estimates.at("shape")};
  { const auto r=cppunivariate::invgauss_mle(mltest::as_span(x),held[0],nan);
    CHECK(r.mu==held[0]);
    CHECK(std::isfinite(r.lambda));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvgauss(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.mu,r.lambda};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::invgauss_mle(mltest::as_span(x),nan,held[1]);
    CHECK(std::isfinite(r.mu));
    CHECK(r.lambda==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvgauss(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.mu,r.lambda};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::invgauss_mle(mltest::as_span(x),held[0],held[1]);
    CHECK(r.mu==held[0]);
    CHECK(r.lambda==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvgauss(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.mu,r.lambda};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("invgauss P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK_FALSE(std::isfinite(cppunivariate::invgauss_mle(mltest::as_span(x),2,nan).logLik));
  CHECK(std::isfinite(cppunivariate::invgauss_mle(mltest::as_span(x),nan,2).logLik));
}


// mpmath generator: import mpmath as mp; mp.mp.dps=100
// N=lambda z: mp.erfc(-z/mp.sqrt(2))/2; S=lambda x: N(-mp.sqrt(4/x)*(x/3-1))-mp.exp(mp.mpf(8)/3)*N(-mp.sqrt(4/x)*(x/3+1)); mp.findroot(lambda x: mp.log(S(x))+800,3500)
TEST_CASE("invgauss: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qinvgauss(-800.0, 3.0, 4.0, true, true) == doctest::Approx(0.00250809143057573).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qinvgauss(-800.0, 3.0, 4.0, false, true) == doctest::Approx(3556.5495517393183).epsilon(1e-11));
}

TEST_CASE("invgauss: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qinvgauss(cppunivariate::special::kNegInf, 3.0, 4.0, true, true) == 0.0);
  CHECK(cppunivariate::distributions::qinvgauss(0.0, 3.0, 4.0, false, true) == 0.0);
  CHECK(cppunivariate::distributions::qinvgauss(cppunivariate::special::kNegInf, 3.0, 4.0, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qinvgauss(0.0, 3.0, 4.0, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("invgauss: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dinvgauss(1e100, 3.0, 4.0, true) == doctest::Approx(-2.2222222222222225e+99).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pinvgauss(1e100, 3.0, 4.0, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pinvgauss(1e100, 3.0, 4.0, false, true) == doctest::Approx(-2.2222222222222225e+99).epsilon(1e-11));
}

// import mpmath as mp; mp.mp.dps=100
// N=lambda z:mp.erfc(-z/mp.sqrt(2))/2
// S=lambda x:N(-mp.sqrt(4/x)*(x/3-1))-mp.exp(mp.mpf(8)/3)*N(-mp.sqrt(4/x)*(x/3+1))
// mp.findroot(lambda x:mp.log(S(x))-mp.log(mp.mpf(float(1e-100))),1000)
TEST_CASE("invgauss: upper quantile stays inside its bracket") {
  CHECK(cppunivariate::distributions::qinvgauss(1e-100,3,4,false,false) == doctest::Approx(1001.2408093734702).epsilon(1e-11));
}

TEST_CASE("invgauss: TASK-132 invalid and degenerate data have statuses") {
  for (const auto& data : {std::vector<double>{}, std::vector<double>{-1,2},
      std::vector<double>{0,2}, std::vector<double>{NAN,2},
      std::vector<double>{INFINITY,2}, std::vector<double>{-INFINITY,2}}) {
    CHECK(cppunivariate::invgauss_mle(cppunivariate::span{data.data(),data.size()}).status == cppunivariate::InvGaussStatus::InvalidInput);
  }
  const std::vector<double> constant(4,2);
  CHECK(cppunivariate::invgauss_mle(cppunivariate::span{constant.data(),constant.size()}).status == cppunivariate::InvGaussStatus::Unbounded);
}

TEST_CASE("invgauss: concentrated fit logLik evaluates the rounded public point") {
  const std::vector<double> x{2.4377209513444593,2.4377209513468974,2.437720951349335,
      2.4377209513517726,2.4377209513566482};
  const auto r=cppunivariate::invgauss_mle(mltest::as_span(x));
  REQUIRE(r.status==cppunivariate::InvGaussStatus::Ok);
  double ll=0;
  for(double v:x)ll+=cppunivariate::distributions::dinvgauss(v,r.mu,r.lambda,true);
  CHECK(std::abs(r.logLik-ll)<=1e-10*std::max(1.,std::abs(ll)));
  const auto held=cppunivariate::invgauss_mle(mltest::as_span(x),2.4377209513498226,8.235308862764327e23);
  CHECK(held.logLik==doctest::Approx(123.89205062899286).epsilon(1e-12));
}

TEST_CASE("invgauss: concentrated public point retains the closed-form maximum") {
  const std::vector<double> x{2.4377209513444593,2.4377209513468974,2.437720951349335,
      2.4377209513517726,2.4377209513566482};
  const auto r=cppunivariate::invgauss_mle(mltest::as_span(x));
  REQUIRE(r.status==cppunivariate::InvGaussStatus::Ok);
  // Independent 100/160-digit evaluation at mu=mean(x), lambda=n/Q(mu).
  constexpr double maximum=123.89205063011403;
  CHECK(std::abs(r.logLik-maximum)<=1e-10*maximum);
#ifdef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
  CHECK(r.lambda==doctest::Approx(8.23530894536328e23).epsilon(1e-10));
#endif
}
