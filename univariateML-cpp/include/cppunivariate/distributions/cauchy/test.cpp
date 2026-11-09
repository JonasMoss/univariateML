#include "cppunivariate/core/constants.hpp"
#include "test_support.hpp"
#include "cppunivariate/distributions/cauchy/mle.hpp"
#include "cppunivariate/distributions/cauchy/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>


TEST_CASE("cauchy") {
  auto g = mltest::load_distribution_golden("cauchy");
  auto r = cppunivariate::fit_cauchy(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.mu == doctest::Approx(g.estimates.at("location")).epsilon(mltest::kGolden_cauchy).scale(0));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_cauchy).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_cauchy).scale(0));
}

TEST_CASE("cauchy: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) +
      "/cauchy/fixture-outlier.json");
  auto r = cppunivariate::fit_cauchy(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.mu == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.scale == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
  CHECK(r.status == cppunivariate::CauchyStatus::Ok);
}

TEST_CASE("cauchy: three observations and nonexistent MLE") {
  const std::vector<double> x{-2.2425190273413, 0.723253413022922, -2.56072856385821};
  auto r = cppunivariate::fit_cauchy(mltest::as_span(x), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::CauchyStatus::Ok);
  CHECK(r.mu == doctest::Approx(-2.369470147).epsilon(1e-4));
  CHECK(r.scale == doctest::Approx(0.272740500).epsilon(1e-4));
  CHECK(r.logLik >= -4.99686179 - 1e-6);
  for (const std::vector<double> &bad : {std::vector<double>{}, {1}, {1, 1}, {1, 1, 2}, {1, 1, 2, 3}}) {
    CHECK(cppunivariate::fit_cauchy(mltest::as_span(bad), mltest::kFitTol, mltest::kFitMaxit).status == cppunivariate::CauchyStatus::NoMLE);
  }
}

TEST_CASE("cauchy dpqr: CDF symmetric at location") {
  CHECK(cppunivariate::distributions::pcauchy(0.0, 0.0, 1.0, true, false) == doctest::Approx(0.5));
  CHECK(cppunivariate::distributions::qcauchy(0.5, 0.0, 1.0, true, false) == doctest::Approx(0.0));
  const double q = cppunivariate::distributions::qcauchy(0.75, 0.0, 1.0, true, false);
  CHECK(cppunivariate::distributions::pcauchy(q, 0.0, 1.0, true, false) == doctest::Approx(0.75));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("cauchy: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("cauchy",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dcauchy(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pcauchy(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qcauchy(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rcauchy(rng, p[0], p[1]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// logS=lambda x:m.log(m.atan(1/x)/m.pi); Q=lambda l:1/m.tan(m.pi*m.exp(l))
TEST_CASE("cauchy: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pcauchy(1e300,0,1,false,true) == doctest::Approx(-std::log(1e300)-std::log(cppunivariate::pi)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qcauchy(-800,0,1e-100,false,true) == doctest::Approx(std::exp(800-100*std::log(10.0)-std::log(cppunivariate::pi))).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::dcauchy(1e300,0,1,true) == doctest::Approx(-std::log(cppunivariate::pi)-2*std::log(1e300)).epsilon(1e-12).scale(1e-300));
}

// mpmath: m.mp.dps=100; p=m.exp(m.mpf(float(-math.log(2))))
// Invert the family's CDF at this p, retaining p-m.mpf('.5').
TEST_CASE("cauchy: rounded log half retains median offset") {
  CHECK(cppunivariate::distributions::qcauchy(-std::log(2.0),0,1,true,true) == doctest::Approx(3.642750216855176e-17).epsilon(1e-12));
  CHECK(cppunivariate::distributions::qcauchy(-std::log(2.0),0,1,false,true) == doctest::Approx(-3.642750216855176e-17).epsilon(1e-12));
}

// mpmath: m.mp.dps=100; p=m.exp(m.mpf(float(-math.log(4))))
// -1+1/m.tan(m.pi*p) = -7.2855004337103516e-17.
TEST_CASE("cauchy: quarter quantile and cancelling location") {
  CHECK(cppunivariate::distributions::qcauchy(.25,-1,1,false,false) == 0);
  CHECK(cppunivariate::distributions::qcauchy(-std::log(4.0),-1,1,false,true) == doctest::Approx(-7.2855004337103516e-17).epsilon(1e-12));
}

// mpmath: m.mp.dps=100; x=m.mpf('1e308'); s=m.mpf('1e-308')
// log(1/(m.pi*s*(1+(x/s)**2))) = -log(pi)-3*log(1e308).
TEST_CASE("cauchy: log density survives standardized overflow") {
  CHECK(cppunivariate::distributions::dcauchy(1e308,0,1e-308,true) == doctest::Approx(-std::log(cppunivariate::pi)-3*std::log(1e308)).epsilon(1e-12));
}


TEST_CASE("cauchy: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("cauchy");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<4;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::fit_cauchy(mltest::as_span(g.data),1e-8,1000,((mask & 1) ? g.estimates.at("location") : nan),((mask & 2) ? g.estimates.at("scale") : nan));
    if(mask & 1)CHECK(r.mu==g.estimates.at("location"));
    if(mask & 2)CHECK(r.scale==g.estimates.at("scale"));
    CHECK(r.status==cppunivariate::CauchyStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("cauchy: held profiles preserve extreme units") {
  const std::vector<double> x={-2,-.5,0,.7,3};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto base=cppunivariate::fit_cauchy(mltest::as_span(x),1e-8,200,0,nan);
  for(double unit:{1e-300,1e300}) {
    auto y=x;for(double& v:y)v*=unit;
    const auto fit=cppunivariate::fit_cauchy(mltest::as_span(y),1e-8,200,0,nan);
    CHECK(fit.scale/unit==doctest::Approx(base.scale).epsilon(1e-8));
    CHECK(fit.logLik+x.size()*std::log(unit)==doctest::Approx(base.logLik).epsilon(1e-8));
  }
}

TEST_CASE("cauchy: held location brackets disparate residuals") {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  for (double a : {1e-310, 1e-300, 1e-200, 1.0, 1e200}) {
    for (double b : {1.0, 1e200, 1e300}) {
      if (b < a * 1e10) continue;
      const std::vector<double> x{0, a, b};
      const auto r = cppunivariate::fit_cauchy(mltest::as_span(x), 1e-8, 100, 0, nan);
      REQUIRE(r.status == cppunivariate::CauchyStatus::Ok);
      CHECK(std::abs(std::log(r.scale)-std::log(a)) < 1e-9);
      // For b/a large, the unique root tends to a and ll tends to this expression.
      CHECK(r.logLik == doctest::Approx(-3*std::log(cppunivariate::pi)-std::log(a)-2*std::log(b)-std::log(2.0)).epsilon(1e-10));
    }
  }
  const std::vector<double> tiny{0, std::numeric_limits<double>::denorm_min(), 1};
  CHECK(cppunivariate::fit_cauchy(mltest::as_span(tiny),1e-8,100,0,nan).status == cppunivariate::CauchyStatus::IterationFailure);
  const std::vector<double> huge{-1e308,-1e308,-1e308};
  CHECK(cppunivariate::fit_cauchy(mltest::as_span(huge),1e-8,100,1e308,nan).status == cppunivariate::CauchyStatus::IterationFailure);
  const std::vector<double> x{0,1e-300,1};
  CHECK(cppunivariate::fit_cauchy(mltest::as_span(x),1e-8,1,0,nan).status == cppunivariate::CauchyStatus::IterationFailure);
  const auto held = cppunivariate::fit_cauchy(mltest::as_span(x),1e-8,100,nan,1);
  CHECK(held.status == cppunivariate::CauchyStatus::Ok);
  CHECK(held.scale == 1);
}

TEST_CASE("cauchy: held location supports overflowing finite residual differences") {
  const std::vector<double> x{-1e308,0,1e308};
  const auto r=cppunivariate::fit_cauchy(mltest::as_span(x),1e-8,100,-1e308,std::numeric_limits<double>::quiet_NaN());
  REQUIRE(r.status==cppunivariate::CauchyStatus::Ok);
  CHECK(r.scale/1e308==doctest::Approx(std::sqrt((std::sqrt(73.0)-5)/6)).epsilon(1e-10));
  CHECK(std::isfinite(r.logLik));
}

TEST_CASE("cauchy: free geometric certificate checks adversarial total scores") {
  const std::vector<std::vector<double>> samples{
      {0,0,0,0,0.01,1,2,3,4},
      {-1e6,-100,-1,0,1,100,1e6},
      {-101,-100,-99,98,99,100,101}};
  for (const auto& x : samples) {
    const auto r=cppunivariate::fit_cauchy(mltest::as_span(x),1e-8,1000);
    REQUIRE(r.status==cppunivariate::CauchyStatus::Ok);
    double a=0,b=0;
    for(double v:x) {
      const double z=(v-r.mu)/r.scale, w=1/(1+z*z);
      a+=2*z*w; b+=(z*z-1)*w;
    }
    CHECK(std::hypot(a,b)<1e-3);
  }
  const std::vector<double> x{-8,-2,0,1,3,20,100};
  // Deliberately loose step tolerance must not accept an unchecked iterate.
  const auto r=cppunivariate::fit_cauchy(mltest::as_span(x),1e6,100);
  CHECK(r.status==cppunivariate::CauchyStatus::IterationFailure);
}

TEST_CASE("cauchy: held scale certifies roots and reports global ties") {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  // The two-point score numerator is proportional to t*(a*a-s*s-t*t).
  for (double scale : {2.1,3.0,100.0}) {
    const std::vector<double> x{-2,2};
    const auto r=cppunivariate::fit_cauchy(mltest::as_span(x),1e-8,100,nan,scale);
    REQUIRE(r.status==cppunivariate::CauchyStatus::Ok);
    CHECK(std::abs(r.mu)<1e-8*scale);
    CHECK(r.logLik==doctest::Approx(-2*(std::log(cppunivariate::pi)+std::log(scale)+std::log1p(4/(scale*scale)))).epsilon(1e-12));
  }
  for (const std::vector<double>& x : {std::vector<double>{-2,2}, {-2,-2,2,2}, {-3,-2,2,3}}) {
    const auto r=cppunivariate::fit_cauchy(mltest::as_span(x),1e-8,100,nan,1);
    CHECK(r.status==cppunivariate::CauchyStatus::AmbiguousMaximum);
    CHECK(std::isnan(r.mu));
  }
  const std::vector<double> flat{-1,1};
  CHECK(cppunivariate::fit_cauchy(mltest::as_span(flat),1e-8,100,nan,1).status==cppunivariate::CauchyStatus::IterationFailure);
  const std::vector<double> x{-2,-.5,0,.7,3};
  CHECK(cppunivariate::fit_cauchy(mltest::as_span(x),1e-8,1,nan,1).status==cppunivariate::CauchyStatus::IterationFailure);
  const std::vector<double> overflow{-1e308,1e308};
  CHECK(cppunivariate::fit_cauchy(mltest::as_span(overflow),1e-8,100,nan,1e-308).status==cppunivariate::CauchyStatus::IterationFailure);
}

TEST_CASE("cauchy: held scale certificate preserves units and repeated points") {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{-2,-.5,0,.7,3};
  const auto base=cppunivariate::fit_cauchy(mltest::as_span(x),1e-8,100,nan,1);
  REQUIRE(base.status==cppunivariate::CauchyStatus::Ok);
  for(double unit : {1e-300,1e300}) {
    auto y=x;for(double& v:y)v*=unit;
    const auto r=cppunivariate::fit_cauchy(mltest::as_span(y),1e-8,100,nan,unit);
    REQUIRE(r.status==cppunivariate::CauchyStatus::Ok);
    CHECK(r.mu/unit==doctest::Approx(base.mu).epsilon(1e-8));
    CHECK(r.scale==unit);
    CHECK(r.logLik+x.size()*std::log(unit)==doctest::Approx(base.logLik).epsilon(1e-10));
  }
  for(const std::vector<double>& y : {std::vector<double>{2}, {2,2,2}}) {
    const auto r=cppunivariate::fit_cauchy(mltest::as_span(y),1e-8,100,nan,1);
    REQUIRE(r.status==cppunivariate::CauchyStatus::Ok);
    CHECK(std::abs(r.mu-2)<1e-10);
    CHECK(r.logLik==doctest::Approx(-static_cast<double>(y.size())*std::log(cppunivariate::pi)).epsilon(1e-12));
  }
}

TEST_CASE("cauchy: held scale large sample certificate") {
  const std::size_t n=100000;
  std::vector<double> x;
  // Symmetric deterministic Cauchy quantiles: the certified unique root is zero.
  for(std::size_t j=0;j<n;++j)x.push_back(std::tan(cppunivariate::pi*((j+.5)/n-.5)));
  const auto r=cppunivariate::fit_cauchy(mltest::as_span(x),1e-8,100,
      std::numeric_limits<double>::quiet_NaN(),1);
  REQUIRE(r.status==cppunivariate::CauchyStatus::Ok);
  CHECK(std::abs(r.mu)<1e-9);
  CHECK(r.scale==1);
}

TEST_CASE("cauchy: held scale chooses the winning separated mode") {
  // For t>0 the heavier left cluster has L(-t)>L(t), by reflection.
  // Both clusters have local maxima; a single local solve is insufficient.
  for(double direction : {-1.0,1.0}) {
    const std::vector<double> x{-10*direction,-10*direction,-10*direction,10*direction,10*direction};
    const auto r=cppunivariate::fit_cauchy(mltest::as_span(x),1e-8,100,
        std::numeric_limits<double>::quiet_NaN(),1);
    REQUIRE(r.status==cppunivariate::CauchyStatus::Ok);
    CHECK(r.mu*direction < -9.9);
    double score=0;for(double v:x)score+=2*(v-r.mu)/(1+(v-r.mu)*(v-r.mu));
    CHECK(std::abs(score)<1e-7);
  }
}

TEST_CASE("cauchy: structured tie candidates retain locations and likelihood bounds") {
  const std::vector<double> x{-2,2};
  const auto r=cppunivariate::fit_cauchy(mltest::as_span(x),1e-8,100,NAN,1);
  REQUIRE(r.status==cppunivariate::CauchyStatus::AmbiguousMaximum);
  REQUIRE(r.stop_detail.candidates.size()==2);
  for(const auto& candidate:r.stop_detail.candidates) {
    REQUIRE(candidate.coef.size()==2);
    CHECK(std::abs(candidate.coef[0])==doctest::Approx(std::sqrt(3.)).epsilon(1e-7));
    CHECK(candidate.coef[1]==1);
    double ll=0;for(double v:x)ll-=std::log(cppunivariate::pi)+std::log1p(std::pow(v-candidate.coef[0],2));
    CHECK(candidate.loglik_lower<=ll);
    CHECK(candidate.loglik_upper>=ll);
  }
  const std::vector<double> atom{1,1,2};
  const auto no=cppunivariate::fit_cauchy(mltest::as_span(atom),1e-8,100);
  CHECK(no.status==cppunivariate::CauchyStatus::NoMLE);
  CHECK(no.stop_detail.candidates.empty());
}
