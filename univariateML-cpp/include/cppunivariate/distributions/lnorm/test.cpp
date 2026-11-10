#include <algorithm>
#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include "cppunivariate/distributions/lnorm/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("lnorm") {
  auto g = mltest::load_distribution_golden("lnorm");
  auto r = cppunivariate::lognormal_mle(mltest::as_span(g.data));
  CHECK(r.meanlog == doctest::Approx(g.estimates.at("meanlog")).epsilon(mltest::kGolden_lnorm).scale(0));
  CHECK(r.sdlog == doctest::Approx(g.estimates.at("sdlog")).epsilon(mltest::kGolden_lnorm).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_lnorm).scale(0));
}

TEST_CASE("lnorm: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/lnorm/fixture-outlier.json");
  auto r = cppunivariate::lognormal_mle(mltest::as_span(g.data));
  CHECK(r.meanlog == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.sdlog == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
}

TEST_CASE("lnorm dpqr: median = exp(meanlog)") {
  CHECK(cppunivariate::distributions::qlnorm(0.5, 0.0, 1.0, true, false) == doctest::Approx(1.0));
  const double q = cppunivariate::distributions::qlnorm(0.4, 1.0, 0.5, true, false);
  CHECK(cppunivariate::distributions::plnorm(q, 1.0, 0.5, true, false) == doctest::Approx(0.4));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("lnorm: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("lnorm",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dlnorm(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::plnorm(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qlnorm(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rlnorm(rng, p[0], p[1]); });
}

TEST_CASE("lnorm P2 every held subset") {
  const auto g=mltest::load_distribution_golden("lnorm");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("meanlog"),g.estimates.at("sdlog")};
  { const auto r=cppunivariate::lognormal_mle(mltest::as_span(x),held[0],nan);
    CHECK(r.meanlog==held[0]);
    CHECK(std::isfinite(r.sdlog));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dlnorm(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.meanlog,r.sdlog};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::lognormal_mle(mltest::as_span(x),nan,held[1]);
    CHECK(std::isfinite(r.meanlog));
    CHECK(r.sdlog==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dlnorm(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.meanlog,r.sdlog};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::lognormal_mle(mltest::as_span(x),held[0],held[1]);
    CHECK(r.meanlog==held[0]);
    CHECK(r.sdlog==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dlnorm(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.meanlog,r.sdlog};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("lnorm P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  // The held binary64 log(2) differs from the real logarithm: the
  // conditional residual and its finite MLE must not be rounded to zero.
  CHECK(std::isfinite(cppunivariate::lognormal_mle(mltest::as_span(x),std::log(2.0),nan).logLik));
  CHECK(std::isfinite(cppunivariate::lognormal_mle(mltest::as_span(x),nan,1).logLik));
}

TEST_CASE("lnorm held distant mean uses a scaled-width result") {
  const std::vector<double> x{1,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto r=cppunivariate::lognormal_mle(mltest::as_span(x),1e300,nan);
  CHECK(r.meanlog==1e300);
  CHECK(r.sdlog==doctest::Approx(1e300));
  CHECK(std::isfinite(r.logLik));
}


// mpmath generator: import mpmath as mp; mp.mp.dps=100
// Q=lambda z:mp.erfc(z/mp.sqrt(2))/2
// z=mp.findroot(lambda z:mp.log(Q(z))+800,40); [mp.exp(-z),mp.exp(z)]
TEST_CASE("lnorm: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qlnorm(-800.0, 0.0, 1.0, true, true) == doctest::Approx(4.767570450553154e-18).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qlnorm(-800.0, 0.0, 1.0, false, true) == doctest::Approx(2.0975044005568406e+17).epsilon(1e-11));
}

TEST_CASE("lnorm: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qlnorm(cppunivariate::special::kNegInf, 0.0, 1.0, true, true) == 0.0);
  CHECK(cppunivariate::distributions::qlnorm(0.0, 0.0, 1.0, false, true) == 0.0);
  CHECK(cppunivariate::distributions::qlnorm(cppunivariate::special::kNegInf, 0.0, 1.0, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qlnorm(0.0, 0.0, 1.0, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("lnorm: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dlnorm(1e100, 0.0, 1.0, true) == doctest::Approx(-26740.6680002246).epsilon(1e-11));
  CHECK(cppunivariate::distributions::plnorm(1e100, 0.0, 1.0, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::plnorm(1e100, 0.0, 1.0, false, true) == doctest::Approx(-26515.84871241671).epsilon(1e-11));
}

TEST_CASE("lnorm: TASK-132 invalid data never report success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2.0},std::vector<double>{INFINITY,2.0},std::vector<double>{-INFINITY,2.0},std::vector<double>{-1.0,2.0},std::vector<double>{0.0,2.0}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::lognormal_mle(x).status != cppunivariate::LognormalStatus::Ok);
  }
}

TEST_CASE("lnorm: TASK-132 normal delegate preserves collapse status") {
  const std::vector<double> data(4,2);
  const cppunivariate::span x{data.data(),data.size()};
  CHECK(cppunivariate::lognormal_mle(x).status == cppunivariate::LognormalStatus::Unbounded);
}

TEST_CASE("lnorm: TASK-132 unresolved likelihood has numerical failure status") {
  const std::vector<double> data{2.0,3.0};
  const cppunivariate::span x{data.data(),data.size()};
  CHECK(cppunivariate::lognormal_mle(x,0.0,1e-300).status == cppunivariate::LognormalStatus::IterationFailure);
}

TEST_CASE("lnorm scaled residuals: extreme held transformed location") {
  const std::vector<double> x={1,1};
  for(double unit:{1e-200,1e200})for(bool held:{false,true}) {
    const auto r=cppunivariate::lognormal_mle(mltest::as_span(x),unit,held?unit:NAN);
    REQUIRE(r.status==cppunivariate::LognormalStatus::Ok);
    CHECK(r.sdlog/unit==doctest::Approx(1).epsilon(1e-14));
    const double jac=0;
    CHECK(r.logLik==doctest::Approx(-2*(std::log(unit)+.5*std::log(2*cppunivariate::pi)+.5)+jac).epsilon(1e-14));
  }
}

// Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.
TEST_CASE("lnorm: raw transform audit case 4601") {
  const std::vector<double> data{1.445187510724387, 1.445187510725832, 1.4451875107272774, 1.4451875107287226, 1.445187510731613};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::lognormal_mle(x);
  REQUIRE(fit.status==cppunivariate::LognormalStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.meanlog==doctest::Approx(0.3682390783550075).epsilon(1e-10).scale(0));
  CHECK(fit.sdlog==doctest::Approx(1.7204957773060616e-12).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(126.50615506228813).epsilon(1e-10).scale(0));
}
TEST_CASE("lnorm: raw transform audit case 4604") {
  const std::vector<double> data{1.445187510724387, 1.445187510725832, 1.4451875107272774, 1.4451875107287226, 1.445187510731613};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::lognormal_mle(x);
  REQUIRE(fit.status==cppunivariate::LognormalStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.meanlog==doctest::Approx(0.3682390783550075).epsilon(1e-10).scale(0));
  CHECK(fit.sdlog==doctest::Approx(1.7204957773060616e-12).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(126.50615506228813).epsilon(1e-10).scale(0));
}
TEST_CASE("lnorm: raw transform audit case 4611") {
  const std::vector<double> data{100000001.46643509, 100000000.44635502, 100000001.90765181, 100000002.98407541, 100000001.44518751, 100000000.48315705, 100000001.20661016, 100000000.26392101, 100000004.95617464, 100000000.76818824, 100000001.8055238, 100000000.58251828};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::lognormal_mle(x);
  REQUIRE(fit.status==cppunivariate::LognormalStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.meanlog==doctest::Approx(18.42068075921553).epsilon(1e-10).scale(0));
  CHECK(fit.sdlog==doctest::Approx(1.2760092241976638e-08).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(-19.952111548312093).epsilon(1e-10).scale(0));
}
TEST_CASE("lnorm: raw transform audit case 4600") {
  const std::vector<double> data{1.445187481823527, 1.4451874962754023, 1.4451875107272774, 1.4451875251791524, 1.4451875540829027};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::lognormal_mle(x);
  REQUIRE(fit.status==cppunivariate::LognormalStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.meanlog==doctest::Approx(0.36823908035480735).epsilon(1e-10).scale(0));
  CHECK(fit.sdlog==doctest::Approx(1.7204650438422402e-08).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(80.45454250892595).epsilon(1e-10).scale(0));
}
TEST_CASE("lnorm: raw transform audit case 4603") {
  const std::vector<double> data{1.445187481823527, 1.4451874962754023, 1.4451875107272774, 1.4451875251791524, 1.4451875540829027};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::lognormal_mle(x);
  REQUIRE(fit.status==cppunivariate::LognormalStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.meanlog==doctest::Approx(0.36823908035480735).epsilon(1e-10).scale(0));
  CHECK(fit.sdlog==doctest::Approx(1.7204650438422402e-08).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(80.45454250892595).epsilon(1e-10).scale(0));
}

TEST_CASE("lnorm: public log-mean restoration retains an uncertified finite fit") {
  for(double unit:{1e-150,1e150}) {
    std::vector<double> data;
    for(double d:{-2.,-1.,0.,1.,3.})data.push_back(unit*(1+d*1e-12));
    const auto fit=cppunivariate::lognormal_mle(mltest::as_span(data));
    REQUIRE(fit.status==cppunivariate::LognormalStatus::Ok);
    CHECK_FALSE(fit.certified);
    CHECK(std::isfinite(fit.logLik));
  }
}

TEST_CASE("lnorm: shared log anchor retains its independent low component") {
  // mpmath at 100 digits on the exact binary64 inputs. These checks run
  // unchanged when extended intermediates have only binary64 width.
  const auto p=cppunivariate::transformed_detail::log_pair(2.);
  CHECK(p.hi==std::log(2.));
  CHECK(p.lo==doctest::Approx(2.3190468138462995584e-17).epsilon(1e-14).scale(0));
  const auto q=cppunivariate::transformed_detail::log_pair(1.5);
  CHECK(q.lo==doctest::Approx(-2.8811380259626426e-18).epsilon(1e-13).scale(0));
}

TEST_CASE("lnorm: exactly zero held log residual is analytic nonexistence") {
  const std::vector<double> data{1,1,1};
  const auto fit=cppunivariate::lognormal_mle(mltest::as_span(data),0,NAN);
  CHECK(fit.status==cppunivariate::LognormalStatus::Unbounded);
}

TEST_CASE("lnorm: held rounded log mean retains the exact log-anchor residual") {
  const std::vector<double> data{2,2,2};
  const auto fit=cppunivariate::lognormal_mle(mltest::as_span(data),std::log(2.),NAN);
  REQUIRE(fit.status==cppunivariate::LognormalStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.sdlog==doctest::Approx(2.3190468138462995584e-17).epsilon(1e-13).scale(0));
}
