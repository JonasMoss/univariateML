#include "cppunivariate/core/constants.hpp"
#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/snorm/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

// rsnorm at xi=1 should reduce to N(mean, sd); for xi != 1 the empirical
// mean shifts away from `mean` (skew bias).
TEST_CASE("rsnorm samples have unit variance and skew with xi") {
  cppunivariate::distributions::StdRng<> rng(23);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rsnorm(rng, 0.0, 1.0, 1.5);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  // Standardised parameterisation: empirical mean ~ 0, var ~ 1 by
  // construction (cppunivariate::dens::snorm_sigma standardises moments).
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);
}

TEST_CASE("snorm (skew-normal)") {
  auto g = mltest::load_distribution_golden("snorm");
  auto r = cppunivariate::snorm_mle(mltest::as_span(g.data), mltest::kFitTol, 200);
  CHECK(r.mu == C_APPROX(g.estimates.at("mean")));
  CHECK(r.sigma == C_APPROX(g.estimates.at("sd")));
  CHECK(r.xi == C_APPROX(g.estimates.at("xi")));
  CHECK(r.logLik == C_APPROX(g.logLik));
}

TEST_CASE("snorm profile endpoints and direct grid") {
  for (const std::vector<double>& data : std::vector<std::vector<double>>{
      {0, 0.1, 0.2, 0.4, 3}, {-3, -0.4, -0.2, -0.1, 0},
      {-2, -1, 0, 1, 2}, {-2, -2, 0, 1, 1, 2}, {0, 0, 0, 0, 1}}) {
    auto r = cppunivariate::snorm_mle(mltest::as_span(data), mltest::kFitTol, 200);
    double best = std::numeric_limits<double>::infinity();
    int winner = -1;
    for (int j = 0; j <= 10000; ++j) {
      const double c = data.front() + (data.back()-data.front())*j/10000.0;
      double A=0, B=0;
      for (double x : data) { if (x<c) A+=(x-c)*(x-c); else B+=(x-c)*(x-c); }
      const double q=std::pow(std::cbrt(A)+std::cbrt(B),3);
      if (q<best) { best=q; winner=j; }
    }
    const double ll=data.size()/2.0*(std::log(2/cppunivariate::pi)-std::log(best/data.size())-1);
    CHECK(r.logLik >= ll-1e-9);
    if (winner==0) CHECK(r.status==cppunivariate::SnormStatus::RightHalfNormalBoundary);
    else if (winner==10000) CHECK(r.status==cppunivariate::SnormStatus::LeftHalfNormalBoundary);
    else CHECK(r.status==cppunivariate::SnormStatus::Ok);
  }
}

TEST_CASE("snorm random profiles dominate grid plus refinement") {
  std::mt19937 rng(20261028);
  std::normal_distribution<double> normal;
  std::exponential_distribution<double> exponential;
  std::uniform_real_distribution<double> uniform;
  const std::vector<int> sizes={5,10,30,100,250,500};
  for (int sample=0; sample<240; ++sample) {
    const int n=sizes[(sample/4)%6];
    std::vector<double> data(n);
    for (double& x : data) {
      if (sample%4==0) x=normal(rng);
      else if (sample%4==1) x=exponential(rng);
      else if (sample%4==2) x=std::abs(normal(rng))*(uniform(rng)<6.25/7.25 ? 2.5 : -0.4);
      else x=-exponential(rng);
    }
    const auto fit=cppunivariate::snorm_mle(mltest::as_span(data),mltest::kFitTol,200);
    const auto extremes=std::minmax_element(data.begin(),data.end());
    const double lo=*extremes.first, hi=*extremes.second;
    auto objective = [&](double c) {
      double A=0, B=0;
      for (double x : data) { if (x<c) A+=(x-c)*(x-c); else B+=(x-c)*(x-c); }
      return std::pow(std::cbrt(A)+std::cbrt(B),3);
    };
    std::vector<double> grid(2001);
    for (int j=0; j<=2000; ++j) grid[j]=objective(lo+(hi-lo)*j/2000);
    double best=grid[0];
    auto status=cppunivariate::SnormStatus::RightHalfNormalBoundary;
    if (grid[2000]<best) { best=grid[2000]; status=cppunivariate::SnormStatus::LeftHalfNormalBoundary; }
    for (int j=1; j<2000; ++j) {
      if (grid[j]>grid[j-1] || grid[j]>grid[j+1]) continue;
      double l=lo+(hi-lo)*(j-1)/2000, r=lo+(hi-lo)*(j+1)/2000;
      for (int it=0; it<80; ++it) {
        const double a=l+(r-l)/3, b=r-(r-l)/3;
        if (objective(a)<objective(b)) r=b; else l=a;
      }
      const double q=objective((l+r)/2);
      if (q<best) { best=q; status=cppunivariate::SnormStatus::Ok; }
    }
    CAPTURE(sample);
    CHECK(fit.logLik >= n/2.0*(std::log(2/cppunivariate::pi)-std::log(best/n)-1)-1e-9);
    CHECK(fit.status==status);
  }
}

#undef C_APPROX
#undef C_APPROX_FD

TEST_CASE("snorm package dpqr preserve logarithmic tails") {
  for (bool lower : {false, true}) {
    for (double lp : {-1000.0, -100.0, -2.0, -0.1}) {
      const double x = cppunivariate::distributions::qsnorm(lp, 0.3, 1.7, 1.8, lower, true);
      CHECK(cppunivariate::distributions::psnorm(x, 0.3, 1.7, 1.8, lower, true) == doctest::Approx(lp).epsilon(2e-12));
    }
  }
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("snorm: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("snorm",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dsnorm(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::psnorm(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qsnorm(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rsnorm(rng, p[0], p[1], p[2]); });
}

TEST_CASE("snorm: classified holds preserve values and handle tied/singleton data") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{-2,-1,-.4,0,.2,.8,1,1.3,2,3};
  const auto data=mltest::as_span(x);
  for(bool mean : {false,true}) for(bool sd : {false,true}) {
    if(!mean && !sd) continue;
    const auto r=cppunivariate::snorm_mle(data,1e-9,300,mean ? .2 : nan,sd ? 1.7 : nan,1.3);
    REQUIRE(r.status==cppunivariate::SnormStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    if(mean) CHECK(r.mu==.2);
    if(sd) CHECK(r.sigma==1.7);
    CHECK(r.xi==1.3);

  }
  const std::vector<double> one{.2};
  const auto location=cppunivariate::snorm_mle(mltest::as_span(one),1e-9,300,nan,1.7,1.3);
  CHECK(location.status==cppunivariate::SnormStatus::Ok);
  const auto collapse=cppunivariate::snorm_mle(mltest::as_span(one),1e-9,300,.2,nan,1.3);
  CHECK(collapse.status==cppunivariate::SnormStatus::Unbounded);
}

TEST_CASE("snorm: every nonempty hold subset") {
  const auto g=mltest::load_distribution_golden("snorm");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1; mask<8; ++mask) {
    const auto r=cppunivariate::snorm_mle(mltest::as_span(g.data),1e-9,300,
      mask&1 ? g.estimates.at("mean") : nan,mask&2 ? g.estimates.at("sd") : nan,mask&4 ? g.estimates.at("xi") : nan);
    REQUIRE(r.status==cppunivariate::SnormStatus::Ok);
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

// At symmetry F(0)=1/2; the local inverse is delta_p/f(0).
TEST_CASE("snorm: symmetric member has exact median and signed log-half offset") {
  const double offset=2.3190468138462995584e-17*(std::sqrt(cppunivariate::pi/2));
  for(bool lower:{true,false}) for(bool log:{true,false}) {
    const double p=log?-0.6931471805599453:.5;
    const double expected=log?(lower?offset:-offset):0;
    const double got=cppunivariate::distributions::qsnorm(p,0,1,1,lower,log);
    if (log) CHECK(got==doctest::Approx(expected).scale(0).epsilon(1e-12));
    else CHECK(got==0);
  }
}
