#include "cppunivariate/core/constants.hpp"
#include <algorithm>
#include "test_support.hpp"
#include "cppunivariate/distributions/naka/mle.hpp"
#include "cppunivariate/distributions/naka/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>

TEST_CASE("rnaka samples are positive and match analytic mean") {
  cppunivariate::distributions::StdRng<> rng(11);
  const int n = 20000;
  const double shape = 2.0;
  const double scale = 1.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rnaka(rng, shape, scale);
    CHECK(x > 0.0);
    sum += x;
  }
  // E[X] = sqrt(scale/shape) * Gamma(shape+0.5)/Gamma(shape)
  const double expected = std::sqrt(scale / shape) *
                          std::tgamma(shape + 0.5) / std::tgamma(shape);
  CHECK(std::abs(sum / n - expected) < 0.05);
}

TEST_CASE("naka") {
  auto g = mltest::load_distribution_golden("naka");
  auto r = cppunivariate::naka_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kGolden_naka).scale(0));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_naka).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_naka).scale(0));
}

TEST_CASE("naka dpqr support and extreme log probabilities") {
  CHECK(cppunivariate::distributions::pnaka(-1,2,3)==0);
  CHECK(cppunivariate::distributions::pnaka(-1,2,3,false)==1);
  CHECK(cppunivariate::distributions::dnaka(-1,2,3)==0);
  CHECK(std::isinf(cppunivariate::distributions::dnaka(0,.25,2)));
  CHECK(cppunivariate::distributions::dnaka(0,.5,2)==doctest::Approx(1/std::sqrt(cppunivariate::pi)));
  CHECK(cppunivariate::distributions::dnaka(0,2,2)==0);
  CHECK(std::isnan(cppunivariate::distributions::dnaka(1,-1,2)));
  for (bool lower : {true,false}) {
    for (double lp : {-1000.0,-10000.0,-1e-20}) {
      const double x=cppunivariate::distributions::qnaka(lp,10,2,lower,true);
      CHECK(cppunivariate::distributions::pnaka(x,10,2,lower,true)==doctest::Approx(lp).epsilon(1e-10));
    }
  }
}

TEST_CASE("naka tiny shape upper tail avoids cancellation") {
  const double lx=2*std::log(1e-200)+std::log(1e-25);
  CHECK(cppunivariate::distributions::pnaka(1e-200,1e-25,1,false,true)==
    doctest::Approx(std::log(-std::expm1(1e-25*(lx+0.57721566490153286061)))).epsilon(1e-12));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("naka: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("naka",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dnaka(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pnaka(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qnaka(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rnaka(rng, p[0], p[1]); });
}

TEST_CASE("naka P2 every held subset") {
  const auto g=mltest::load_distribution_golden("naka");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("shape"),g.estimates.at("scale")};
  { const auto r=cppunivariate::naka_mle(mltest::as_span(x),1e-10,200,held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.scale));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dnaka(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::naka_mle(mltest::as_span(x),1e-10,200,nan,held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dnaka(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::naka_mle(mltest::as_span(x),1e-10,200,held[0],held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dnaka(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("naka P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::naka_mle(mltest::as_span(x),1e-10,100,nan,4).status==cppunivariate::GammaStatus::Degenerate);
  CHECK(std::isfinite(cppunivariate::naka_mle(mltest::as_span(x),1e-10,100,2,nan).logLik));
  CHECK(cppunivariate::naka_mle(mltest::as_span(x),1e-10,1,nan,3).status==cppunivariate::GammaStatus::IterationFailure);
}

TEST_CASE("naka: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("naka");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::naka_mle(x,1e-8,1).status==cppunivariate::GammaStatus::IterationFailure);
}

TEST_CASE("naka: TASK-132 invalid data never report success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2.0},std::vector<double>{INFINITY,2.0},std::vector<double>{-INFINITY,2.0},std::vector<double>{-1.0,2.0},std::vector<double>{0.0,2.0}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::naka_mle(x,1e-8,200).status != cppunivariate::GammaStatus::Ok);
  }
}

TEST_CASE("naka: TASK-132 squared-data range loss has transform status") {
  for (double unit : {1e-300,1e300}) {
    const std::vector<double> data{unit,2*unit,3*unit};
    CHECK(cppunivariate::naka_mle(mltest::as_span(data),1e-8,200).status == cppunivariate::GammaStatus::TransformFailure);
  }
}

TEST_CASE("naka: TASK-132 held shape distinguishes lost scale from valid evaluation") {
  const std::vector<double> data{1e-300,2e-300,3e-300};
  const auto x=mltest::as_span(data);
  CHECK(cppunivariate::naka_mle(x,1e-8,200,2.0,NAN).status == cppunivariate::GammaStatus::TransformFailure);
  CHECK(cppunivariate::naka_mle(x,1e-8,200,2.0,3.0).status == cppunivariate::GammaStatus::Ok);
}

TEST_CASE("naka: TASK-132 constant samples retain analytic nonexistence across ranges") {
  for(double point : {std::numeric_limits<double>::denorm_min(),1e-300,2.0,1e300}) {
    const std::vector<double> data(4,point);
    CHECK(cppunivariate::naka_mle(mltest::as_span(data),1e-8,200).status == cppunivariate::GammaStatus::Degenerate);
  }
}

// Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.
TEST_CASE("naka: raw transform audit case 5025") {
  const std::vector<double> data{1.6156758120186856, 1.6156758120203014, 1.615675812021917, 1.6156758120235326, 1.615675812026764};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::naka_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(8.445990392402279e+22).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(2.610408329553725).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(125.94868625588904).epsilon(1e-10).scale(0));
}
TEST_CASE("naka: raw transform audit case 5028") {
  const std::vector<double> data{1.6156758120186856, 1.6156758120203014, 1.615675812021917, 1.6156758120235326, 1.615675812026764};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::naka_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(8.445990392402279e+22).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(2.610408329553725).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(125.94868625588904).epsilon(1e-10).scale(0));
}
TEST_CASE("naka: raw transform audit case 5020") {
  const std::vector<double> data{1.1870206453189427, 1.8063039875699989, 0.4693270808393411};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::naka_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(1.1698574399369313).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(1.630673338911285).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(-2.359056308828766).epsilon(1e-10).scale(0));
}
TEST_CASE("naka: raw transform audit case 10593") {
  const std::vector<double> data{0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::naka_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(844594598752111.4).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(1.0000000040000003).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(82.29573788520479).epsilon(1e-10).scale(0));
}
TEST_CASE("naka: raw transform audit case 5035") {
  const std::vector<double> data{1.1870206453189427, 1.8063039875699989, 0.4693270808393411, 2.13423387168775, 1615678.355181381};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::naka_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(0.04235627451635061).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(522083309482.21204).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(-32.60680034465681).epsilon(1e-10).scale(0));
}
TEST_CASE("naka: raw transform audit case 5024") {
  const std::vector<double> data{1.6156757797084007, 1.6156757958651589, 1.615675812021917, 1.6156758281786752, 1.6156758604921915};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::naka_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(844594596402047.9).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(2.610408339994315).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(79.89697123524273).epsilon(1e-10).scale(0));
}
TEST_CASE("naka: raw transform audit case 5027") {
  const std::vector<double> data{1.6156757797084007, 1.6156757958651589, 1.615675812021917, 1.6156758281786752, 1.6156758604921915};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::naka_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(844594596402047.9).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(2.610408339994315).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(79.89697123524273).epsilon(1e-10).scale(0));
}
TEST_CASE("naka: raw transform audit case 5033") {
  const std::vector<double> data{1.1870206453189427e-150, 1.806303987569999e-150, 4.693270808393411e-151, 2.13423387168775e-150, 1.615675812021917e-150, 8.86497002301353e-151, 2.5431594641684416e-150, 1.6267796686030047e-150, 1.3177191226180992e-150, 1.4498034459162533e-150, 7.470853925818136e-151, 1.8750286103803643e-150};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::naka_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(1.6910565415381886).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(2.489126186855235e-300).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(4134.409443408661).epsilon(1e-10).scale(0));
}
TEST_CASE("naka: raw transform audit case 5034") {
  const std::vector<double> data{1.1870206453189426e+150, 1.806303987569999e+150, 4.693270808393411e+149, 2.13423387168775e+150, 1.615675812021917e+150, 8.864970023013529e+149, 2.5431594641684415e+150, 1.6267796686030047e+150, 1.3177191226180994e+150, 1.4498034459162531e+150, 7.470853925818136e+149, 1.875028610380364e+150};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::naka_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(1.6910565415381886).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(2.489126186855235e+300).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(-4154.896891369904).epsilon(1e-10).scale(0));
}
TEST_CASE("naka: raw transform audit case 5018") {
  const std::vector<double> data{1.1870206453189427, 1.8063039875699989, 0.4693270808393411, 2.13423387168775, 1.615675812021917, 0.886497002301353, 2.5431594641684416, 1.6267796686030047, 1.3177191226180993, 1.4498034459162532, 0.7470853925818136, 1.8750286103803642};
  const auto x=mltest::as_span(data);
  const auto fit=cppunivariate::naka_mle(x, 1e-10, 200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.shape==doctest::Approx(1.6910565415381886).epsilon(1e-10).scale(0));
  CHECK(fit.scale==doctest::Approx(2.489126186855235).epsilon(1e-10).scale(0));
  CHECK(fit.logLik==doctest::Approx(-10.24372398062166).epsilon(1e-10).scale(0));
}

TEST_CASE("naka: finite second moment despite an overflowing reference square") {
  const std::vector<double> data{1.5e154,1,1,1,1};
  const auto fit=cppunivariate::naka_mle(mltest::as_span(data),1e-10,200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.scale==doctest::Approx(4.5e307).epsilon(1e-13).scale(0));
}

TEST_CASE("naka: a broad normalized mean is accumulated without subtracting one") {
  std::vector<double> data(100001,1);data[0]=1e150;
  const auto fit=cppunivariate::naka_mle(mltest::as_span(data),1e-12,200);
  REQUIRE(fit.status==cppunivariate::GammaStatus::Ok);
  CHECK(fit.scale==doctest::Approx(1e300/data.size()).epsilon(1e-13).scale(0));
}

// Independent 1600-digit reference; density-smoke.py retains exact inputs.
TEST_CASE("naka: compensated concentrating density") {
  CHECK(cppunivariate::distributions::dnaka(31.622776601683793,100000000.0,1000.0,true)==
        doctest::Approx(5.530671379007053).epsilon(1e-10).scale(0));
}
