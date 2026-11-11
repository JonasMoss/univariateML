#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/std/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

// rstd at (mean=0, sd=1, nu=8) is symmetric with sd 1; CDF at 0 = 0.5.
TEST_CASE("rstd samples have unit variance and a symmetric CDF") {
  cppunivariate::distributions::StdRng<> rng(19);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rstd(rng, 0.0, 1.0, 8.0);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);

  const double cdf0 =
      cppunivariate::distributions::pstd(0.0, 0.0, 1.0, 8.0, true, false);
  CHECK(std::abs(cdf0 - 0.5) < 1e-12);
}

TEST_CASE("std (Student-t with location/scale)") {
  auto g = mltest::load_distribution_golden("std");
  auto r = cppunivariate::student_t_mle(mltest::as_span(g.data), mltest::kFitTol, 200);
  CHECK(r.mu == doctest::Approx(g.estimates.at("mean")).epsilon(mltest::kGolden_std).scale(0));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sd")).epsilon(mltest::kGolden_std).scale(0));
  CHECK(r.nu == doctest::Approx(g.estimates.at("nu")).epsilon(mltest::kGolden_std).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_std).scale(0));

  cppunivariate::StudentTCost cost{mltest::as_span(g.data)};
  const double phi_hat[3] = {r.mu, std::log(r.sigma), std::log(r.nu - 2.0)};
  double grad[3];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm =
      std::sqrt(grad[0] * grad[0] + grad[1] * grad[1] + grad[2] * grad[2]);
  CHECK(gnorm < 1e-3);
}

TEST_CASE("std compares normal and df-two faces") {
  std::vector<double> uniform;
  for (int i = 0; i < 200; ++i) uniform.push_back((i - 99.5) / 100.0);
  auto normal = cppunivariate::student_t_mle(mltest::as_span(uniform), mltest::kFitTol, 200);
  CHECK(normal.status == cppunivariate::StudentTStatus::NormalBoundary);
  std::vector<double> cluster;
  for (int i = 0; i < 30; ++i) cluster.push_back(-1e-4 + 2e-4 * i / 29.0);
  for (int i = 0; i < 10; ++i) cluster.push_back(0.9999 + 2e-4 * i / 9.0);
  auto two = cppunivariate::student_t_mle(mltest::as_span(cluster), mltest::kFitTol, 200);
  CHECK(two.status == cppunivariate::StudentTStatus::DfTwoBoundary);
  CHECK(two.logLik == doctest::Approx(54.07217335).epsilon(1e-7));
}

TEST_CASE("std large nu density retains precision") {
  // Independent high precision log-density values at z=1.25.
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 1e8) ==
        doctest::Approx(-1.7001885430386572).epsilon(1e-12));
}

#undef C_APPROX
#undef C_APPROX_FD

TEST_CASE("std density across the gamma-ratio transition") {
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 10.0) ==
        doctest::Approx(-1.813567692594348).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 100.0) ==
        doctest::Approx(-1.710151642368861).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 343.0) ==
        doctest::Approx(-1.7030664401294549).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 344.0) ==
        doctest::Approx(-1.7030580425140323).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 1000.0) ==
        doctest::Approx(-1.7011732051102342).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 9999.0) ==
        doctest::Approx(-1.7002868956031254).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 10000.0) ==
        doctest::Approx(-1.7002868857656135).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 100000.0) ==
        doctest::Approx(-1.7001983673162022).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 1000000.0) ==
        doctest::Approx(-1.7001895166043817).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 10000000.0) ==
        doctest::Approx(-1.7001886315445291).epsilon(2e-11));
}

TEST_CASE("std asymptotic cutoff is accurate") {
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 127.0) ==
        doctest::Approx(-1.7080116221314585).epsilon(1e-13));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 128.0) ==
        doctest::Approx(-1.7079498782526117).epsilon(1e-13));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 129.0) ==
        doctest::Approx(-1.7078891013889226).epsilon(1e-13));
}

TEST_CASE("std: fixed nu maps ordinary t scale exactly") {
  const std::vector<double> x = {-5,-2,-1,0,0,1,2,9};
  auto r = cppunivariate::student_t_mle_fixed_nu(mltest::as_span(x),4,1e-8,5000);
  auto t = cppunivariate::lst_mle_fixed_df(mltest::as_span(x),4,1e-8,5000);
  REQUIRE(r.status == cppunivariate::StudentTStatus::Ok);
  CHECK(r.nu == 4);
  CHECK(r.mu == t.mu);
  CHECK(r.sigma == t.sigma*std::sqrt(2.0));
  CHECK(r.logLik == t.logLik);
  for (double nu : {-1.0,0.0,2.0,std::numeric_limits<double>::infinity()})
    CHECK(cppunivariate::student_t_mle_fixed_nu(mltest::as_span(x),nu,1e-8,100).status == cppunivariate::StudentTStatus::InvalidNu);
  const std::vector<double> tied = {0,0,0,1};
  CHECK(cppunivariate::student_t_mle_fixed_nu(mltest::as_span(tied),2.5,1e-8,100).status == cppunivariate::StudentTStatus::Unbounded);
  CHECK(cppunivariate::student_t_mle_fixed_nu(mltest::as_span(tied),3,1e-8,100).status == cppunivariate::StudentTStatus::ScaleBoundary);
}

TEST_CASE("std: free df tie collapse and equality") {
  for (int k : {5,6,7,8}) {
    std::vector<double> x(k,0.0); x.insert(x.end(),{1,2,3});
    const auto f=cppunivariate::student_t_mle(mltest::as_span(x),1e-8,200);
    if(k>6) CHECK(f.status==cppunivariate::StudentTStatus::Unbounded);
    if(k==6) CHECK(f.status==cppunivariate::StudentTStatus::ScaleBoundary);
    if(k==5) {
      CHECK(f.status!=cppunivariate::StudentTStatus::Unbounded);
      CHECK(f.status!=cppunivariate::StudentTStatus::ScaleBoundary);
    }
  }
}

TEST_CASE("std package dpqr preserve logarithmic tails") {
  for (bool lower : {false, true}) {
    for (double lp : {-1000.0, -100.0, -2.0, -0.1}) {
      const double x = cppunivariate::distributions::qstd(lp, 0.3, 1.7, 5.0, lower, true);
      CHECK(cppunivariate::distributions::pstd(x, 0.3, 1.7, 5.0, lower, true) == doctest::Approx(lp).epsilon(2e-12));
    }
  }
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("std: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("std",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dstd(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pstd(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qstd(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rstd(rng, p[0], p[1], p[2]); });
}

TEST_CASE("std fits are equivariant on small-scale samples") {
  const auto g = mltest::load_distribution_golden("std");
  const auto base = cppunivariate::student_t_mle(mltest::as_span(g.data), mltest::kFitTol, 200);
  for (double scale : {1e-6, 1e-2, 1e6}) {
    std::vector<double> x;
    for (double v : g.data) x.push_back(scale * (v + 3.0));
    const auto r = cppunivariate::student_t_mle(mltest::as_span(x), mltest::kFitTol, 200);
    CHECK(r.status == base.status);
    CHECK(r.mu / scale - 3.0 == doctest::Approx(base.mu).epsilon(1e-5));
    CHECK(r.sigma / scale == doctest::Approx(base.sigma).epsilon(1e-5));
    CHECK(r.nu == doctest::Approx(base.nu).epsilon(1e-5));
    CHECK(r.logLik + x.size() * std::log(scale) == doctest::Approx(base.logLik).epsilon(1e-9));
  }
}

TEST_CASE("std: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("std");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<8;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::student_t_mle(mltest::as_span(g.data),1e-8,1000,((mask & 1) ? g.estimates.at("mean") : nan),((mask & 2) ? g.estimates.at("sd") : nan),((mask & 4) ? g.estimates.at("nu") : nan));
    if(mask==1||mask==2)CHECK_FALSE(r.certified);
    if(mask & 1)CHECK(r.mu==g.estimates.at("mean"));
    if(mask & 2)CHECK(r.sigma==g.estimates.at("sd"));
    if(mask & 4)CHECK(r.nu==g.estimates.at("nu"));
    CHECK(r.status==cppunivariate::StudentTStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("std: conditional lower shape faces") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x={-20,20};for(int i=0;i<10;++i){x.push_back(-.1);x.push_back(.1);}
  const auto unresolved=cppunivariate::student_t_mle(mltest::as_span(x),1e-8,200,0,nan,nan);
  CHECK(unresolved.status==cppunivariate::StudentTStatus::DfTwoBoundary);
  CHECK_FALSE(unresolved.certified);
  CHECK(unresolved.stop_detail.candidates.size()==2);
  const std::vector<double> tied={0,0,1};
  CHECK(cppunivariate::student_t_mle(mltest::as_span(tied),1e-8,200,0,1,nan).status==cppunivariate::StudentTStatus::ScaleBoundary);
}

TEST_CASE("std: held profiles preserve extreme units") {
  const std::vector<double> x={-2,-.5,0,.7,3};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto base=cppunivariate::student_t_mle(mltest::as_span(x),1e-8,200,0,nan,1e20);
  for(double unit:{1e-300,1e300}) {
    auto y=x;for(double& v:y)v*=unit;
    const auto fit=cppunivariate::student_t_mle(mltest::as_span(y),1e-8,200,0,nan,1e20);
    CHECK(fit.sigma/unit==doctest::Approx(base.sigma).epsilon(1e-8));
    CHECK(fit.logLik+x.size()*std::log(unit)==doctest::Approx(base.logLik).epsilon(1e-8));
  }
  CHECK(base.sigma==doctest::Approx(std::sqrt(2.748)).epsilon(1e-8));
}


// Generating mpmath snippet (binary64 inputs retained exactly):
// import mpmath as mp, math; mp.mp.dps=100
// p=mp.mpf(float(-math.log(2))); n=mp.mpf(3)
// f0=mp.gamma((n+1)/2)/(mp.sqrt((n-2)*mp.pi)*mp.gamma(n/2))
// print(mp.expm1(p+mp.log(2))/(2*f0))  # omitted cubic term <1e-49
TEST_CASE("std: targeted tail and endpoint regressions") {
  CHECK(cppunivariate::distributions::qstd(0.5, 0.0, 1.0, 3.0, true, false) == doctest::Approx(0.0).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qstd(-0.6931471805599453, 0.0, 1.0, 3.0, true, true) == doctest::Approx(1.8213751084275876e-17).epsilon(1e-10));
}

// import mpmath as mp; mp.mp.dps=100
// n=mp.mpf(3); s=mp.mpf(1e-300); x=mp.mpf(1e300)
// c=(n/2-1)*mp.log(n)-mp.log(mp.beta(n/2,mp.mpf('.5')))
// print(c-n*(mp.log(x/s)+mp.log(3)/2))
// print(-mp.log(mp.beta(n/2,mp.mpf('.5')))-(n+1)/2*mp.log1p((x/s)**2)-mp.log(s))
TEST_CASE("std: targeted tail scaling without overflowing z") {
  CHECK(cppunivariate::distributions::pstd(1e300,0,1e-300,3,false,true)
        == doctest::Approx(-4146.203362383239).epsilon(1e-10));
  CHECK(cppunivariate::distributions::dstd(1e300,0,1e-300,3,true)
        == doctest::Approx(-4835.8802779927855).epsilon(1e-10));
}

TEST_CASE("std: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("std");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::student_t_mle(x,1e-8,1).status==cppunivariate::StudentTStatus::NumericalFailure);
}

TEST_CASE("std: face comparison cannot resolve rounded extreme objectives") {
  const double magnitude=1e20;
  CHECK(cppunivariate::compare_face(-magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(-magnitude,-2*magnitude,2e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-2*magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(-10.,-10.,2e-8)==cppunivariate::FaceComparison::FirstWins);
}

TEST_CASE("std: audited huge held-parameter magnitude") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x{1,2};
  cppunivariate::span s{x.data(),x.size()};
  const auto r=cppunivariate::student_t_mle(s,1e-6,200,0,1e-10,nan);
  CHECK(r.status==cppunivariate::StudentTStatus::Ok);
  CHECK(std::isfinite(r.logLik));
  CHECK(r.status!=cppunivariate::StudentTStatus::NormalBoundary);
}

TEST_CASE("std: certificate normal and critical collapse remain distinct") {
  const std::vector<double> balanced{-1,-1,1,1},critical{-1,0,0,0,0,1};
  const auto normal=cppunivariate::student_t_mle(mltest::as_span(balanced),1e-6,200);
  CHECK(normal.status==cppunivariate::StudentTStatus::NormalBoundary);
  REQUIRE(normal.stop_detail.limit_coef.size()==2);
  CHECK(normal.stop_detail.limit_coef[0]==doctest::Approx(0));
  CHECK(normal.stop_detail.limit_coef[1]==doctest::Approx(1));
  const auto collapse=cppunivariate::student_t_mle(mltest::as_span(critical),1e-6,200);
  CHECK(collapse.status==cppunivariate::StudentTStatus::ScaleBoundary);
  CHECK(collapse.logLik==doctest::Approx(-6*std::log(2.)));
  REQUIRE(collapse.stop_detail.limit_coef.size()==2);
  CHECK(collapse.stop_detail.limit_coef[0]==0);
  CHECK(collapse.stop_detail.limit_coef[1]==2);
  CHECK(collapse.stop_detail.loglik_lower<=-6*std::log(2.));
  CHECK(collapse.stop_detail.loglik_upper>=-6*std::log(2.));
}

TEST_CASE("std: certificate large-sample shallow quantile maximum") {
  std::vector<double> x;
  for(int i=0;i<4000;++i)x.push_back(cppunivariate::special::qt((i+.5)/4000,300,true,false));
  for(double floor:{1.,2.}){
    const auto r=cppunivariate::t_certificate(mltest::as_span(x),floor,4096,500);
    REQUIRE(r.status==cppunivariate::TCertificateStatus::Finite);
    CHECK(r.profiles<4096);
    CHECK(r.nu==doctest::Approx(539.46057298).epsilon(1e-7));
    CHECK(r.scale==doctest::Approx(1.00131588021).epsilon(1e-7));
  }
  const auto r=cppunivariate::student_t_mle(mltest::as_span(x),1e-7,500);
  REQUIRE(r.status==cppunivariate::StudentTStatus::Ok);
  CHECK(r.nu==doctest::Approx(539.46057298).epsilon(1e-7));
}

TEST_CASE("std: held sd and shape transport the complete location certificate") {
  const std::vector<double> x{-3,3};
  const auto tie=cppunivariate::student_t_mle(mltest::as_span(x),1e-7,200,NAN,1,3);
  REQUIRE(tie.status==cppunivariate::StudentTStatus::CertificateTie);
  CHECK(tie.stop_detail.comparison_tie);
  REQUIRE(tie.stop_detail.candidates.size()==2);
  for(const auto& c:tie.stop_detail.candidates){
    CHECK(std::abs(c.coef[0])==doctest::Approx(std::sqrt(8.)).epsilon(1e-8));
    CHECK(c.coef[1]==doctest::Approx(1));
    CHECK(c.coef[2]==3);
  }
  const std::vector<double> scale_sample{-1,1};
  const auto scale=cppunivariate::student_t_mle(mltest::as_span(scale_sample),1e-7,200,0,NAN,3);
  REQUIRE(scale.status==cppunivariate::StudentTStatus::Ok);
  CHECK(scale.sigma==doctest::Approx(std::sqrt(3.)).epsilon(1e-9));
}

TEST_CASE("std: held-shape scale transport cannot return an overflowing standard deviation") {
  const double nu=std::nextafter(2.,INFINITY);
  const std::vector<double> x{-1e301,1e301};
  for(double held_mu:std::vector<double>{NAN,0.}){
    const auto r=cppunivariate::student_t_mle(mltest::as_span(x),1e-7,500,held_mu,NAN,nu);
    REQUIRE(r.status==cppunivariate::StudentTStatus::NumericalFailure);
    CHECK(std::isnan(r.sigma));CHECK(std::isfinite(r.stop_detail.loglik_lower));
    REQUIRE_FALSE(r.stop_detail.candidates.empty());
    CHECK(r.stop_detail.candidates.front().model==cppunivariate::StopModel::Lst);
    CHECK(std::isfinite(r.stop_detail.candidates.front().coef[1]));
  }
  const std::vector<double> representable{-1e300,1e300};
  const auto r=cppunivariate::student_t_mle(mltest::as_span(representable),1e-7,500,0,NAN,nu);
  REQUIRE(r.status==cppunivariate::StudentTStatus::Ok);
  CHECK(std::isfinite(r.sigma));CHECK(r.sigma>1e307);
}

TEST_CASE("std: held-scale and represented-scale precision guards") {
  const std::vector<double> x{-2,-1,0,1,3};
  const auto r=cppunivariate::student_t_mle(mltest::as_span(x),1e-7,500,NAN,1e100,3);
  REQUIRE(r.status==cppunivariate::StudentTStatus::Ok);
  CHECK(r.mu==doctest::Approx(.2).epsilon(1e-7));
  const double tiny=std::numeric_limits<double>::denorm_min();
  const std::vector<double> subnormal{-2*tiny,2*tiny};
  const auto s=cppunivariate::student_t_mle(mltest::as_span(subnormal),1e-7,500,0,NAN,3);
  CHECK(s.status==cppunivariate::StudentTStatus::NumericalFailure);
  const double nu=std::nextafter(2.,INFINITY),sd=3e-316;
  const std::vector<double> constant{0,0};
  const auto c=cppunivariate::student_t_mle(mltest::as_span(constant),1e-7,500,NAN,sd,nu);
  REQUIRE(c.status==cppunivariate::StudentTStatus::Ok);
  CHECK(std::isfinite(c.logLik));CHECK(c.mu==0);CHECK(c.sigma==sd);
  const auto h=cppunivariate::student_t_mle(mltest::as_span(subnormal),1e-7,500,NAN,sd,nu);
  CHECK(h.status==cppunivariate::StudentTStatus::NumericalFailure);
}

TEST_CASE("std: uncertified free-shape searches still stop on solver exhaustion") {
  const auto g=mltest::load_distribution_golden("std");
  for(int mask:{1,2,3}) {
    const auto r=cppunivariate::student_t_mle(mltest::as_span(g.data),1e-8,1,
      mask&1?g.estimates.at("mean"):NAN,mask&2?g.estimates.at("sd"):NAN,NAN);
    CHECK(r.status==(mask==3?cppunivariate::StudentTStatus::NumericalFailure:cppunivariate::StudentTStatus::IterationFailure));
  }
}

TEST_CASE("std: shape-only certificate keeps actual sd and resolves the collapse face") {
  const auto g=mltest::load_distribution_golden("std");
  const auto r=cppunivariate::student_t_mle(mltest::as_span(g.data),1e-8,200,g.estimates.at("mean"),g.estimates.at("sd"),NAN);
  REQUIRE(r.status==cppunivariate::StudentTStatus::Ok);
  CHECK(r.nu==doctest::Approx(3.97765795208252294225).epsilon(1e-7));
  CHECK(r.logLik==doctest::Approx(-579.480782421475318179).epsilon(1e-12));
  const std::vector<double> x{0,0,1};
  const auto collapse=cppunivariate::student_t_mle(mltest::as_span(x),1e-8,200,0,1,NAN);
  REQUIRE(collapse.status==cppunivariate::StudentTStatus::ScaleBoundary);
  CHECK(collapse.stop_detail.loglik_lower<=-3*std::log(2.));
  CHECK(collapse.stop_detail.loglik_upper>=-3*std::log(2.));
  CHECK(collapse.stop_detail.limit_coef==std::vector<double>{0,2});
}

TEST_CASE("std: shape-only certificate is equivariant and never accepts its work cap") {
  const std::vector<double> x{-.7,-.2,.1,.4,1,4};
  const auto base=cppunivariate::student_t_mle(mltest::as_span(x),1e-8,200,0,1,NAN);
  REQUIRE(base.status==cppunivariate::StudentTStatus::Ok);
  for(double unit:{1e-100,1e100}){
    auto y=x;for(double& v:y)v*=unit;
    const auto r=cppunivariate::student_t_mle(mltest::as_span(y),1e-8,200,0,unit,NAN);
    REQUIRE(r.status==cppunivariate::StudentTStatus::Ok);
    CHECK(r.nu==doctest::Approx(base.nu).epsilon(1e-7));
    CHECK(r.logLik+y.size()*std::log(unit)==doctest::Approx(base.logLik).epsilon(1e-10));
  }
  const auto stopped=cppunivariate::student_t_mle(mltest::as_span(x),1e-8,1,0,1,NAN);
  CHECK(stopped.status!=cppunivariate::StudentTStatus::Ok);
  CHECK(std::isfinite(stopped.stop_detail.loglik_lower));
  CHECK(stopped.stop_detail.loglik_upper==INFINITY);
}

TEST_CASE("std: shape-only normal stops preserve both held normal coefficients") {
  const std::vector<double> x{-1,1};
  const auto r=cppunivariate::student_t_mle(mltest::as_span(x),1e-8,200,0,1,NAN);
  REQUIRE(r.status==cppunivariate::StudentTStatus::NormalBoundary);
  CHECK(r.stop_detail.limit_coef==std::vector<double>{0,1});
  CHECK(r.stop_detail.loglik_lower<=-1-std::log(2*cppunivariate::pi));
  CHECK(r.stop_detail.loglik_upper>=-1-std::log(2*cppunivariate::pi));
}

TEST_CASE("std: free-shape held solver stops retain admissible witnesses and unknown upper bounds") {
  const auto g=mltest::load_distribution_golden("std");
  for(int mask:{1,2,3}) {
    const auto r=cppunivariate::student_t_mle(mltest::as_span(g.data),1e-8,1,
      mask&1?g.estimates.at("mean"):NAN,mask&2?g.estimates.at("sd"):NAN,NAN);
    REQUIRE(r.status!=cppunivariate::StudentTStatus::Ok);
    CHECK(r.stop_detail.loglik_upper==INFINITY);
    CHECK(std::isfinite(r.stop_detail.loglik_lower));
    CHECK_FALSE(r.stop_detail.candidates.empty());
    for(const auto& candidate:r.stop_detail.candidates) {
      REQUIRE(candidate.coef.size()==3);
      double ll;
      if(candidate.model==cppunivariate::StopModel::Lst) {
        const double point[2]={candidate.coef[0],std::log(candidate.coef[1])};
        ll=-cppunivariate::LstCost{mltest::as_span(g.data),candidate.coef[2],true}(point);
      } else ll=cppunivariate::student_t_held_loglik(mltest::as_span(g.data),candidate.coef[0],candidate.coef[1],candidate.coef[2]);
      CHECK(ll>=candidate.loglik_lower);CHECK(ll<=candidate.loglik_upper);
    }
  }
}

TEST_CASE("std: shape-only certificate refuses unjustifiable requested coefficient precision") {
  const auto g=mltest::load_distribution_golden("std");
  const auto r=cppunivariate::student_t_mle(mltest::as_span(g.data),1e-20,200,g.estimates.at("mean"),g.estimates.at("sd"),NAN);
  CHECK(r.status!=cppunivariate::StudentTStatus::Ok);
  CHECK(r.stop_detail.loglik_upper==INFINITY);
  CHECK(std::isfinite(r.stop_detail.loglik_lower));
}

TEST_CASE("std: balanced separated atoms have proved distinct held-sd global maximizers") {
  for(const std::vector<double>& x:{std::vector<double>{-100,100},std::vector<double>{-100,-100,100,100}}){
    const auto r=cppunivariate::student_t_mle(mltest::as_span(x),1e-8,200,NAN,1,NAN);
    REQUIRE(r.status==cppunivariate::StudentTStatus::NonUniqueHeldSd);
    REQUIRE(r.stop_detail.candidates.size()==2);
    CHECK(r.stop_detail.comparison_tie);
    CHECK(r.stop_detail.loglik_upper==INFINITY);
    for(const auto& point:r.stop_detail.candidates){
      const double ll=cppunivariate::student_t_held_loglik(mltest::as_span(x),point.coef[0],point.coef[1],point.coef[2]);
      CHECK(ll>=point.loglik_lower);CHECK(ll<=point.loglik_upper);
    }
  }
  const std::vector<double> close{-1,1},unbalanced{-100,100,100};
  CHECK(cppunivariate::t_symmetry_detail::held_sd(mltest::as_span(close),1).outcome==0);
  CHECK(cppunivariate::t_symmetry_detail::held_sd(mltest::as_span(unbalanced),1).outcome==0);
}

TEST_CASE("std: default budget distinguishes scalar certificates and joint numerical fits") {
  const auto g=mltest::load_distribution_golden("std");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<=3;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::student_t_mle(mltest::as_span(g.data),1.220703e-4,200,
      mask&1?g.estimates.at("mean"):nan,
      mask&2?g.estimates.at("sd"):nan,nan);
    REQUIRE(r.status==cppunivariate::StudentTStatus::Ok);
    CHECK(r.certified==(mask==3));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("std: held-sd likelihood bounds stay narrow at the normal face") {
  const std::vector<double> x={-1,2};const double sd=std::sqrt(2.34);
  cppunivariate::StopDetail point;
  cppunivariate::t_shape_detail::retain_point(mltest::as_span(x),.2,sd,1e8,true,point);
  const auto normal=cppunivariate::t_shape_detail::normal_point(mltest::as_span(x),.2,sd);
  REQUIRE(point.candidates.size()==1);
  CHECK(point.candidates[0].loglik_upper-point.loglik_lower<1e-9);
  CHECK(cppunivariate::t_shape_detail::dominated(point,normal));
}

TEST_CASE("std: signed normal profile covers every finite shape at actual holds") {
  std::vector<double> x;
  for(int j=0;j<100;++j)x.push_back(cppunivariate::special::qnorm((j+.5)/100.,0,1,true,false));
  for(int mask:{1,2}){
    const double mu=mask==1?0:NAN,scale=mask==2?1:NAN;
    const auto proof=cppunivariate::t_signed_detail::solve(mltest::as_span(x),mu,scale,true,200);
    REQUIRE(proof.proved);CHECK(proof.coefficient_upper<0);CHECK(proof.cutoff>proof.lower_shape);
    CHECK(proof.evaluations<=cppunivariate::certificate_budget(200));
    const auto result=cppunivariate::student_t_mle(mltest::as_span(x),1e-8,200,mu,scale,NAN);
    CHECK(result.status==cppunivariate::StudentTStatus::NormalBoundary);CHECK(result.certified);
    CHECK(std::isfinite(result.stop_detail.loglik_upper));
  }
}

TEST_CASE("std: signed face zero and uncertain coefficients make no claim") {
  std::vector<cppunivariate::t_cert_detail::I> q{0,0,0,0,3,3};
  const auto zero=cppunivariate::t_signed_detail::coefficient(q,true);
  CHECK(zero.lo<=0);CHECK(zero.hi>=0);
  q={1,4};
  const auto value=cppunivariate::t_signed_detail::coefficient(q,true);
  CHECK(value.lo<=-1.75);CHECK(value.hi>=-1.75);
}

TEST_CASE("std: negative normal coefficient does not select a beaten face") {
  std::vector<double> x(26,0);for(int j=0;j<7;++j){x.push_back(-1);x.push_back(1);}
  const double sd=std::sqrt(.35);
  const auto proof=cppunivariate::t_signed_detail::solve(mltest::as_span(x),NAN,sd,true,200);
  CHECK_FALSE(proof.proved);CHECK(proof.coefficient_upper<0);
  const auto normal=cppunivariate::t_shape_detail::normal_point(mltest::as_span(x),0,sd);
  REQUIRE_FALSE(proof.detail.candidates.empty());
  CHECK(proof.detail.loglik_lower>normal.candidates[0].loglik_upper);
  const auto fit=cppunivariate::student_t_mle(mltest::as_span(x),1e-8,200,NAN,sd,NAN);
  CHECK(fit.status==cppunivariate::StudentTStatus::Ok);CHECK_FALSE(fit.certified);
}

TEST_CASE("std: zero normal profile coefficient preserves uncertified legacy search") {
  const double a=std::sqrt(9+4*std::sqrt(6.));
  const std::vector<double> x{-a,a,-1,1,-1,1,-1,1};
  const auto proof=cppunivariate::t_signed_detail::solve(mltest::as_span(x),0,NAN,true,200);
  CHECK_FALSE(proof.proved);CHECK(proof.coefficient_lower<=0);CHECK(proof.coefficient_upper>=0);
  const auto fit=cppunivariate::student_t_mle(mltest::as_span(x),1e-8,200,0,NAN,NAN);
  CHECK(fit.status==cppunivariate::StudentTStatus::Ok);CHECK_FALSE(fit.certified);
}
