#include <algorithm>
#include "test_support.hpp"
#include "cppunivariate/distributions/invgamma/mle.hpp"
#include "cppunivariate/distributions/invgamma/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>

TEST_CASE("invgamma") {
  auto g = mltest::load_distribution_golden("invgamma");
  auto r = cppunivariate::invgamma_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("alpha")).epsilon(mltest::kGolden_invgamma).scale(0));
  CHECK(r.scale == doctest::Approx(g.estimates.at("beta")).epsilon(mltest::kGolden_invgamma).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_invgamma).scale(0));
}


// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("invgamma: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("invgamma",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dinvgamma(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pinvgamma(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qinvgamma(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rinvgamma(rng, p[0], p[1]); });
}

TEST_CASE("invgamma P2 every held subset") {
  const auto g=mltest::load_distribution_golden("invgamma");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("alpha"),g.estimates.at("beta")};
  { const auto r=cppunivariate::invgamma_mle(mltest::as_span(x),1e-10,200,held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.scale));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvgamma(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::invgamma_mle(mltest::as_span(x),1e-10,200,nan,held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvgamma(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::invgamma_mle(mltest::as_span(x),1e-10,200,held[0],held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvgamma(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("invgamma P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::invgamma_mle(mltest::as_span(x),1e-10,1,nan,2).status==cppunivariate::GammaStatus::IterationFailure);
  CHECK(std::isfinite(cppunivariate::invgamma_mle(mltest::as_span(x),1e-10,100,2,nan).logLik));
}


// mpmath generator: import mpmath as mp; mp.mp.dps=100
// P=lambda z:mp.gammainc(3,0,z)/mp.gamma(3)
// Q=lambda z:mp.gammainc(3,z,mp.inf)/mp.gamma(3)
// lo=mp.exp(mp.findroot(lambda y:mp.log(P(mp.exp(y)))+800,-mp.mpf(800)/3))
// hi=mp.findroot(lambda z:mp.log(Q(z))+800,800)
// [4/hi,4/lo]
TEST_CASE("invgamma: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qinvgamma(-800.0, 3.0, 4.0, true, true) == doctest::Approx(0.0049218044469769165).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qinvgamma(-800.0, 3.0, 4.0, false, true) == doctest::Approx(1.4273749871939215e+116).epsilon(1e-11));
}

TEST_CASE("invgamma: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qinvgamma(cppunivariate::special::kNegInf, 3.0, 4.0, true, true) == 0.0);
  CHECK(cppunivariate::distributions::qinvgamma(0.0, 3.0, 4.0, false, true) == 0.0);
  CHECK(cppunivariate::distributions::qinvgamma(cppunivariate::special::kNegInf, 3.0, 4.0, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qinvgamma(0.0, 3.0, 4.0, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("invgamma: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dinvgamma(1e100, 3.0, 4.0, true) == doctest::Approx(-917.5683012948185).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pinvgamma(1e100, 3.0, 4.0, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pinvgamma(1e100, 3.0, 4.0, false, true) == doctest::Approx(-688.408404284082).epsilon(1e-11));
}

TEST_CASE("invgamma: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("invgamma");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::invgamma_mle(x,1e-8,1).status==cppunivariate::GammaStatus::IterationFailure);
}

TEST_CASE("invgamma: TASK-132 invalid data never report success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2.0},std::vector<double>{INFINITY,2.0},std::vector<double>{-INFINITY,2.0},std::vector<double>{-1.0,2.0},std::vector<double>{0.0,2.0}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::invgamma_mle(x,1e-8,200).status != cppunivariate::GammaStatus::Ok);
  }
}

TEST_CASE("invgamma: TASK-132 reciprocal overflow preserves numerical cause") {
  const std::vector<double> data{1,std::numeric_limits<double>::denorm_min()};
  const cppunivariate::span x{data.data(),data.size()};
  CHECK(cppunivariate::invgamma_mle(x,1e-8,200).status == cppunivariate::GammaStatus::TransformFailure);
  CHECK(cppunivariate::invgamma_mle(x,1e-8,200,2.0,3.0).status == cppunivariate::GammaStatus::TransformFailure);
}

TEST_CASE("invgamma: TASK-132 constant samples retain analytic nonexistence across ranges") {
  for(double point : {std::numeric_limits<double>::denorm_min(),1e-300,2.0,1e300}) {
    const std::vector<double> data(4,point);
    CHECK(cppunivariate::invgamma_mle(mltest::as_span(data),1e-8,200).status == cppunivariate::GammaStatus::Degenerate);
  }
}

// Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.
TEST_CASE("invgamma: raw transform audit case 3759") {
  const std::vector<double> data{1.471602327564204, 1.4716023275656755, 1.4716023275671473, 1.471602327568619, 1.4716023275715622};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invgamma_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(3.3782209017569924e+23).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(4.9713977420625715e+23).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(126.41556407783901).epsilon(1e-10).scale(0));
}
TEST_CASE("invgamma: raw transform audit case 3762") {
  const std::vector<double> data{1.471602327564204, 1.4716023275656755, 1.4716023275671473, 1.471602327568619, 1.4716023275715622};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invgamma_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(3.3782209017569924e+23).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(4.9713977420625715e+23).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(126.41556407783901).epsilon(1e-10).scale(0));
}
TEST_CASE("invgamma: raw transform audit case 10510") {
  const std::vector<double> data{0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invgamma_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(3.3783453797065964e+23).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(3.378345379707272e+23).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(128.34741532364208).epsilon(1e-10).scale(0));
}
TEST_CASE("invgamma: raw transform audit case 10509") {
  const std::vector<double> data{0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invgamma_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(3378378418017940.0).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(3378378424774695.5).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(82.29573790223182).epsilon(1e-10).scale(0));
}
TEST_CASE("invgamma: raw transform audit case 3758") {
  const std::vector<double> data{1.4716022981351007, 1.471602312851124, 1.4716023275671473, 1.4716023422831706, 1.4716023717152171};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invgamma_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(3378378416524510.5).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(4971629551103341.0).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(80.36397877160313).epsilon(1e-10).scale(0));
}
TEST_CASE("invgamma: raw transform audit case 3761") {
  const std::vector<double> data{1.4716022981351007, 1.471602312851124, 1.4716023275671473, 1.4716023422831706, 1.4716023717152171};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::invgamma_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(3378378416524510.5).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(4971629551103341.0).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(80.36397877160313).epsilon(1e-10).scale(0));
}

TEST_CASE("invgamma: held-rate shape beyond binary64 is not a capped estimate") {
  const std::vector<double> data{1e-300,2e-300};
  const auto fit=cppunivariate::invgamma_mle(mltest::as_span(data),1e-10,200,NAN,1e300);
  CHECK(fit.status==cppunivariate::GammaStatus::TransformFailure);
}

TEST_CASE("invgamma: finite public rate despite an overflowing reciprocal reference") {
  const std::vector<double> data{1e-309,1,1,1,1};
  const auto fit=cppunivariate::invgamma_mle(mltest::as_span(data),1e-10,200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(std::isfinite(fit.logLik));
  CHECK(fit.scale>0);
}

// Independent 1600-digit reference; density-smoke.py retains exact inputs.
TEST_CASE("invgamma: compensated concentrating density") {
  CHECK(cppunivariate::distributions::dinvgamma(1.0,100000000.0,100000000.0,true)==
        doctest::Approx(8.291401837938176).epsilon(1e-10).scale(0));
}
