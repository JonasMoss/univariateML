#include "cppunivariate/core/constants.hpp"
#include "test_support.hpp"
#include "cppunivariate/distributions/sstd/mle.hpp"
#include "cppunivariate/distributions/sstd/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include "cppunivariate/distributions/snorm/mle.hpp"
#include "cppunivariate/special/special.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace dist = cppunivariate::distributions;
#define D_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define D_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)
TEST_CASE("sstd (skew Student-t)") {
  auto g = mltest::load_distribution_golden("sstd");
  auto r = cppunivariate::sstd_mle(mltest::as_span(g.data), mltest::kFitTol, 200);
  CHECK(r.mu == doctest::Approx(g.estimates.at("mean")).epsilon(mltest::kGolden_sstd).scale(0));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sd")).epsilon(mltest::kGolden_sstd).scale(0));
  CHECK(r.nu == doctest::Approx(g.estimates.at("nu")).epsilon(mltest::kGolden_sstd).scale(0));
  CHECK(r.xi == doctest::Approx(g.estimates.at("xi")).epsilon(mltest::kGolden_sstd).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_sstd).scale(0));

  cppunivariate::SstdCost cost{mltest::as_span(g.data)};
  const double phi_hat[4] = {r.mu, std::log(r.sigma), std::log(r.nu - 2.0),
                             std::log(r.xi)};
  double grad[4];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm = std::sqrt(grad[0] * grad[0] + grad[1] * grad[1] +
                                 grad[2] * grad[2] + grad[3] * grad[3]);
  CHECK(gnorm < 1e-3);
}

TEST_CASE("rsstd samples are unit-variance and recurse cleanly into rstd") {
  cppunivariate::distributions::StdRng<> rng(31);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x =
        cppunivariate::distributions::rsstd(rng, 0.0, 1.0, 8.0, 1.5);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.15);
  CHECK(std::abs(var - 1.0) < 0.15);
}

namespace {
// Deterministic samples: quantiles at (i + 1/2) / n.
template <class Q> std::vector<double> quantile_sample(int n, Q q) {
  std::vector<double> x;
  for (int i = 0; i < n; ++i) x.push_back(q((i + 0.5) / n));
  return x;
}
cppunivariate::SstdResult fit(const std::vector<double> &x) {
  return cppunivariate::sstd_mle(mltest::as_span(x), mltest::kFitTol, 200);
}
// Translated half-t log-likelihood at endpoint e, scale s, df nu, via Boost.
double half_t_loglik(const std::vector<double> &x, double e, double s, double nu) {
  double ll = 0.0;
  for (double v : x) {
    ll += std::log(2.0 / s) + cppunivariate::special::dt(std::abs(v - e) / s, nu, true);
  }
  return ll;
}
}  // namespace

TEST_CASE("sstd: normal data escape to the skew-normal face") {
  const auto x = quantile_sample(200, [](double p) {
    return cppunivariate::special::qnorm(p, 0.0, 1.0, true, false);
  });
  const auto r = fit(x);
  CHECK(r.status == cppunivariate::SstdStatus::SnormBoundary);
  CHECK(std::isinf(r.nu));
  const auto sn = cppunivariate::snorm_mle(mltest::as_span(x), mltest::kFitTol, 200);
  CHECK(r.logLik == D_APPROX(sn.logLik));
  CHECK(r.xi == D_APPROX(sn.xi));
}

TEST_CASE("sstd: half-normal data escape to the half-normal corner") {
  const auto x = quantile_sample(200, [](double p) {
    return cppunivariate::special::qnorm(0.5 + 0.5 * p, 0.0, 1.0, true, false);
  });
  const auto r = fit(x);
  CHECK(r.status == cppunivariate::SstdStatus::RightHalfNormalBoundary);
  CHECK(r.mu == *std::min_element(x.begin(), x.end()));
  CHECK(std::isinf(r.xi));
}

TEST_CASE("sstd: exponential data escape to a translated half-t") {
  const auto x = quantile_sample(200, [](double p) { return -std::log1p(-p); });
  const auto r = fit(x);
  CHECK(r.status == cppunivariate::SstdStatus::RightHalfTBoundary);
  CHECK(r.mu == *std::min_element(x.begin(), x.end()));
  CHECK(std::isinf(r.xi));
  CHECK(r.nu > 2.0);
  CHECK(r.nu < 10.0);
  // Independent log-likelihood; the profile is maximal at (sigma, nu).
  CHECK(r.logLik == D_APPROX(half_t_loglik(x, r.mu, r.sigma, r.nu)));
  for (double fs : {0.99, 1.01}) {
    for (double fn : {0.95, 1.0, 1.05}) {
      if (fs == 1.0 && fn == 1.0) continue;
      CHECK(half_t_loglik(x, r.mu, fs * r.sigma, fn * r.nu) < r.logLik);
    }
  }
  CHECK(half_t_loglik(x, r.mu, r.sigma, 1e6 * r.nu) < r.logLik);

  // Reflected data: the left face with the same df and log-likelihood.
  std::vector<double> y;
  for (double v : x) y.push_back(-v);
  const auto l = fit(y);
  CHECK(l.status == cppunivariate::SstdStatus::LeftHalfTBoundary);
  CHECK(l.xi == 0.0);
  CHECK(l.logLik == D_APPROX(r.logLik));
  CHECK(l.nu == doctest::Approx(r.nu).epsilon(1e-5));
}

TEST_CASE("sstd: skewed Cauchy data escape to the df-2 face") {
  const auto x = quantile_sample(200, [](double p) {
    const double c = std::tan(3.14159265358979323846 * (p - 0.5));
    return c > 0.0 ? 2.0 * c : c;
  });
  const auto r = fit(x);
  CHECK(r.status == cppunivariate::SstdStatus::DfTwoBoundary);
  CHECK(r.nu == 2.0);
  CHECK(std::isinf(r.sigma));
  CHECK(r.xi > 1.0);
}

TEST_CASE("sstd: large-df interior fits do not overflow") {
  // tgamma in sstd_sigma used to overflow above nu = 343.
  double mu_u = 0.0;
  const double sigma_u = cppunivariate::dens::sstd_sigma(1e4, 1.5, mu_u);
  CHECK(std::isfinite(sigma_u));
  CHECK(std::isfinite(mu_u));
  const double m1 = std::sqrt(2.0 / 3.14159265358979323846);  // normal limit
  CHECK(mu_u == doctest::Approx(m1 * (1.5 - 1.0 / 1.5)).epsilon(1e-3));
}

TEST_CASE("sstd: free df tie collapse and equality") {
  for (int k : {5,6,7,8}) {
    std::vector<double> x(k,0.0); x.insert(x.end(),{-1,1,2});
    const auto f=cppunivariate::sstd_mle(mltest::as_span(x),1e-8,200);
    if(k>6) CHECK(f.status==cppunivariate::SstdStatus::Unbounded);
    if(k==6) CHECK(f.status==cppunivariate::SstdStatus::ScaleBoundary);
    if(k==5) {
      CHECK(f.status!=cppunivariate::SstdStatus::Unbounded);
      CHECK(f.status!=cppunivariate::SstdStatus::ScaleBoundary);
    }
  }
}

TEST_CASE("sstd: half t scale root strict inequality and finite equality supremum") {
  double logscale=0;
  const std::vector<double> strict={0,0,0,0,0,0,0,1,2,3};
  CHECK(cppunivariate::sstd_detail::half_t_profile(strict,.5,logscale)==INFINITY);
  const std::vector<double> equality={0,0,0,0,0,0,1,2,3};
  const double ll=cppunivariate::sstd_detail::half_t_profile(equality,.5,logscale);
  CHECK(std::isfinite(ll));
  CHECK(logscale==-INFINITY);
  // At nu=2 the scale powers cancel, leaving the analytic limit.
  CHECK(ll==doctest::Approx(-3*std::log(6.0)).epsilon(1e-12));
  const std::vector<double> regular={0,0,0,0,0,1,2,3};
  CHECK(std::isfinite(cppunivariate::sstd_detail::half_t_profile(regular,.5,logscale)));
  CHECK(std::isfinite(logscale));
}

TEST_CASE("sstd package dpqr preserve logarithmic tails") {
  for (bool lower : {false, true}) {
    for (double lp : {-1000.0, -100.0, -2.0, -0.1}) {
      const double x = cppunivariate::distributions::qsstd(lp, 0.3, 1.7, 5.0, 1.8, lower, true);
      CHECK(cppunivariate::distributions::psstd(x, 0.3, 1.7, 5.0, 1.8, lower, true) == doctest::Approx(lp).epsilon(2e-12));
    }
  }
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("sstd: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("sstd",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dsstd(x, p[0], p[1], p[2], p[3], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::psstd(x, p[0], p[1], p[2], p[3], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qsstd(x, p[0], p[1], p[2], p[3], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rsstd(rng, p[0], p[1], p[2], p[3]); });
}

TEST_CASE("sstd fits are equivariant on small-scale samples") {
  const auto g = mltest::load_distribution_golden("sstd");
  const auto base = cppunivariate::sstd_mle(mltest::as_span(g.data), mltest::kFitTol, 200);
  for (double scale : {1e-6, 1e-2, 1e6}) {
    std::vector<double> x;
    for (double v : g.data) x.push_back(scale * (v + 3.0));
    const auto r = cppunivariate::sstd_mle(mltest::as_span(x), mltest::kFitTol, 200);
    CHECK(r.status == base.status);
    CHECK(r.mu / scale - 3.0 == doctest::Approx(base.mu).epsilon(1e-5));
    CHECK(r.sigma / scale == doctest::Approx(base.sigma).epsilon(1e-5));
    CHECK(r.nu == doctest::Approx(base.nu).epsilon(1e-5));
    CHECK(r.logLik + x.size() * std::log(scale) == doctest::Approx(base.logLik).epsilon(1e-9));
    CHECK(r.xi == doctest::Approx(base.xi).epsilon(1e-5));
  }
}

TEST_CASE("sstd: classified holds preserve values and handle tied/singleton data") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{-2,-1,-.4,0,.2,.8,1,1.3,2,3};
  const auto data=mltest::as_span(x);
  for(bool mean : {false,true}) for(bool sd : {false,true}) {
    if(!mean && !sd) continue;
    const auto r=cppunivariate::sstd_mle(data,1e-9,300,mean ? .2 : nan,sd ? 1.7 : nan,4.2,1.3);
    REQUIRE(r.status==cppunivariate::SstdStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    if(mean) CHECK(r.mu==.2);
    if(sd) CHECK(r.sigma==1.7);
    CHECK(r.xi==1.3);
    CHECK(r.nu==4.2);
  }
  const std::vector<double> one{.2};
  const auto location=cppunivariate::sstd_mle(mltest::as_span(one),1e-9,300,nan,1.7,4.2,1.3);
  CHECK(location.status==cppunivariate::SstdStatus::Ok);
  const auto collapse=cppunivariate::sstd_mle(mltest::as_span(one),1e-9,300,.2,nan,4.2,1.3);
  CHECK(collapse.status==cppunivariate::SstdStatus::Unbounded);
}

TEST_CASE("sstd: all P0b hold subsets retain golden holds") {
  const auto g=mltest::load_distribution_golden("sstd");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask:std::vector<int>{1,2,4,8,3,5,9,6,10,12,7,11}) {
    const double m=mask&1 ? g.estimates.at("mean") : nan;
    const double sd=mask&2 ? g.estimates.at("sd") : nan;
    const double nu=mask&4 ? g.estimates.at("nu") : nan;
    const double xi=mask&8 ? g.estimates.at("xi") : nan;
    const auto r=cppunivariate::sstd_mle(mltest::as_span(g.data),1e-6,1000,m,sd,nu,xi);
    INFO(mask);
    CHECK(r.status==cppunivariate::SstdStatus::Ok);
    if(mask&1) CHECK(r.mu==m);
    if(mask&2) CHECK(r.sigma==sd);
    if(mask&4) CHECK(r.nu==nu);
    if(mask&8) CHECK(r.xi==xi);
    CHECK(r.logLik>=g.logLik-1e-5*g.data.size());
  }
}

TEST_CASE("sstd: P0b tie collapse includes the held-sd df-two path") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> heavy{0,0,0,0,0,0,0,1,2};
  for(int mask:std::vector<int>{1,2,4,8,3,5,9,10,12,11}) {
    const auto r=cppunivariate::sstd_mle(mltest::as_span(heavy),1e-6,300,
      mask&1 ? 0:nan,mask&2 ? 1:nan,mask&4 ? 3:nan,mask&8 ? 1:nan);
    INFO(mask);
    CHECK(r.status==cppunivariate::SstdStatus::Unbounded);
  }
  const std::vector<double> equality{0,0,0,0,1,2};
  CHECK(cppunivariate::sstd_mle(mltest::as_span(equality),1e-6,300,nan,1).status==
        cppunivariate::SstdStatus::ScaleBoundary);
  // Holding both sd and nu removes scale collapse despite the heavy tie.
  const auto bounded=cppunivariate::sstd_mle(mltest::as_span(heavy),1e-6,500,nan,1,3);
  CHECK(bounded.status!=cppunivariate::SstdStatus::Unbounded);
  CHECK(std::isfinite(bounded.logLik));
}

TEST_CASE("sstd: P0b normal, df-two and forced half-t faces") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> uniform;
  for(int i=0;i<20;++i) uniform.push_back((i+.5)/10.-1);
  const auto normal=cppunivariate::sstd_mle(mltest::as_span(uniform),1e-6,500,nan,nan,nan,1);
  CHECK(normal.status==cppunivariate::SstdStatus::SnormBoundary);
  const std::vector<double> heavy{-10,-.5,-.2,-.1,0,.1,.2,.5,10};
  const auto two=cppunivariate::sstd_mle(mltest::as_span(heavy),1e-6,500,0,nan,nan,1);
  CHECK(two.status==cppunivariate::SstdStatus::DfTwoBoundary);
  CHECK(two.mu==0); // the held MEAN remains exact on an escape result, too.
  double left=-20,right=20;
  for(int i=0;i<100;++i) {
    const double t=std::exp((left+right)/2);
    double score=-static_cast<double>(heavy.size());
    for(double a:heavy) score+=3*a*a/(2*t*t+a*a);
    if(score>0) left=std::log(t); else right=std::log(t);
  }
  const double scale=std::exp((left+right)/2);
  double ll=0;
  for(double a:heavy) ll+=cppunivariate::special::dt(a/scale,2,true)-std::log(scale);
  CHECK(two.logLik==doctest::Approx(ll).epsilon(1e-9));
  std::vector<double> one{.5,1,1.5,2,2.5,3,10};
  for(int side:std::vector<int>{1,-1}) {
    std::vector<double> x;
    for(double a:one) x.push_back(side*a);
    const auto r=cppunivariate::sstd_mle(mltest::as_span(x),1e-6,500,0,1,5);
    CHECK(r.status==(side==1 ? cppunivariate::SstdStatus::RightHalfTBoundary : cppunivariate::SstdStatus::LeftHalfTBoundary));
    CHECK(r.mu==0);
    CHECK(r.sigma==1);
    CHECK(r.nu==5);
    const double m=std::sqrt(5.0)/(.75*3.14159265358979323846);
    const double t=1/std::sqrt(5.0/3-m*m),c=-side*m*t;
    double ll=0;
    for(double a:x) ll+=std::log(2/t)+cppunivariate::special::dt((a-c)/t,5,true);
    CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-9));
  }
}

// At symmetry F(0)=1/2; the local inverse is delta_p/f(0).
TEST_CASE("sstd: symmetric member has exact median and signed log-half offset") {
  const double offset=2.3190468138462995584e-17*(3*cppunivariate::pi*std::sqrt(3.0)/16);
  for(bool lower:{true,false}) for(bool log:{true,false}) {
    const double p=log?-0.6931471805599453:.5;
    const double expected=log?(lower?offset:-offset):0;
    const double got=cppunivariate::distributions::qsstd(p,0,1,5,1,lower,log);
    if (log) CHECK(got==doctest::Approx(expected).scale(0).epsilon(1e-12));
    else CHECK(got==0);
  }
}

TEST_CASE("sstd core preserves previous adapter unconditional failure score gate") {
  const auto g=mltest::load_distribution_golden("sstd");
  const auto x=mltest::as_span(g.data);
  const auto old=cppunivariate::sstd_mle_impl(x,mltest::kFitTol,1,false);
  auto status=old.status;
  if(status==cppunivariate::SstdStatus::BfgsFailure) {
    cppunivariate::SstdCost cost{x};
    const double phi[4]={old.mu,std::log(old.sigma),std::log(old.nu-2),std::log(old.xi)};
    double score[4];cppunivariate::finite_diff_gradient(cost,phi,score);
    double norm=0;for(double v:score)norm+=v*v;
    if(std::sqrt(norm)<1e-3)status=cppunivariate::SstdStatus::Ok;
  }
  const auto r=cppunivariate::sstd_mle(x,mltest::kFitTol,1);
  CHECK(r.status==status);
  CHECK((r.logLik==old.logLik || (std::isnan(r.logLik) && std::isnan(old.logLik))));
}

#include "cppunivariate/core/inference_join.hpp"
TEST_CASE("sstd: analytic covariance derivatives and subset projection") {
  const std::array<double,8> theta{0,1,5,1};
  const std::vector<double> x{-2,-1,-.2,0,.2,1,2};
  const auto full=cppunivariate::join_family_inference({x.data(),x.size()},"sstd",theta,15);
  REQUIRE(full.status==cppunivariate::InferenceStatus::Ok);
  for(unsigned mask=0;mask<(1u<<4);++mask) {
    std::size_t emitted=0;
    const auto r=cppunivariate::join_family_inference({x.data(),x.size()},"sstd",theta,mask,[&](const auto&){++emitted;});
    REQUIRE(r.status==cppunivariate::InferenceStatus::Ok);
    CHECK(emitted==x.size());
    for(std::size_t i=0;i<r.dimension;++i) {
      const auto a=r.free_indices[i];CHECK(r.score[i]==doctest::Approx(full.score[a]));
      for(std::size_t j=0;j<r.dimension;++j) {
        const auto b=r.free_indices[j];
        CHECK(r.observed[i*r.dimension+j]==doctest::Approx(full.observed[a*4+b]));
        CHECK(r.opg[i*r.dimension+j]==doctest::Approx(full.opg[a*4+b]));
      }
    }
  }
  auto bad=theta;bad[1]=0;
  CHECK(cppunivariate::join_family_inference({x.data(),x.size()},"sstd",bad,1).status==cppunivariate::InferenceStatus::BoundaryOrLimit);
  CHECK(cppunivariate::join_family_inference({x.data(),x.size()},"sstd",theta,1u<<4).status==cppunivariate::InferenceStatus::InvalidSubset);
}

TEST_CASE("sstd: held-shape certificates enumerate competing location modes") {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 const std::vector<double> x{-2,2};
 const auto r=cppunivariate::sstd_mle(mltest::as_span(x),1e-6,200,nan,1,3,1);
 REQUIRE(r.status==cppunivariate::SstdStatus::CertificateFailure);
 REQUIRE(r.stop_detail.comparison_tie);
 REQUIRE(r.stop_detail.candidates.size()>=2);
 CHECK(r.stop_detail.candidates.front().coef[0]==doctest::Approx(-std::sqrt(3.)).epsilon(1e-8));
 CHECK(r.stop_detail.candidates.back().coef[0]==doctest::Approx(std::sqrt(3.)).epsilon(1e-8));
 const std::vector<double> y{-1,.2,.4,1,2};
 const auto f=cppunivariate::sstd_mle(mltest::as_span(y),1e-6,200,nan,1,5,2);
 REQUIRE(f.status==cppunivariate::SstdStatus::Ok);CHECK(f.certified);
 CHECK(f.sigma==1);CHECK(f.nu==5);CHECK(f.xi==2);
}

TEST_CASE("sstd: nonunit-skew scale certificate retains both maximum branches") {
 const std::vector<double> x{-35.321072334324725,11.459074428000962,-.015351125824354262,-.2513099360462538,-297.3459307209368,.5881721220894813};
 const auto r=cppunivariate::sstd_mle(mltest::as_span(x),1e-6,200,0,NAN,3,10);
 REQUIRE(r.status==cppunivariate::SstdStatus::Ok);CHECK(r.certified);
 CHECK(std::log(r.sigma)==doctest::Approx(5.894765235143508).epsilon(1e-8));
 CHECK(r.logLik==doctest::Approx(-38.8073025560888).epsilon(1e-12));
 // Removing the old fixed log(sd) window must preserve very small units.
 std::vector<double> tiny;for(double v:x)tiny.push_back(v*1e-30);
 const auto t=cppunivariate::sstd_mle(mltest::as_span(tiny),1e-6,200,0,NAN,3,10);
 REQUIRE(t.status==cppunivariate::SstdStatus::Ok);CHECK(t.certified);
 CHECK(t.sigma/1e-30==doctest::Approx(r.sigma).epsilon(1e-8));
 const auto b=cppunivariate::sstd_mle(mltest::as_span(x),1e-6,1,0,NAN,3,10);
 REQUIRE(b.status==cppunivariate::SstdStatus::CertificateFailure);
 CHECK_FALSE(b.certified);CHECK_FALSE(b.stop_detail.candidates.empty());
 const std::vector<double> tied{-2,2};
 const auto budget=cppunivariate::sstd_mle(mltest::as_span(tied),1e-6,1,NAN,1,3,1);
 CHECK(budget.stop_detail.budget_exhausted);CHECK_FALSE(budget.stop_detail.candidates.empty());
}

TEST_CASE("sstd: symmetric equality is a monotone unattained scale supremum") {
 const std::vector<double> x{0,0,0,1};
 const auto r=cppunivariate::sstd_mle(mltest::as_span(x),1e-6,200,0,NAN,3,1);
 CHECK(r.status==cppunivariate::SstdStatus::ScaleBoundary);CHECK(r.certified);
 CHECK(std::isfinite(r.stop_detail.loglik_lower));CHECK(std::isfinite(r.stop_detail.loglik_upper));
 const auto unresolved=cppunivariate::sstd_mle(mltest::as_span(x),1e-6,200,0,NAN,3,2);
 CHECK(unresolved.status==cppunivariate::SstdStatus::CertificateFailure);
 CHECK_FALSE(unresolved.certified);
}

TEST_CASE("sstd: rational score ranges include extrema and seam crossings") {
 for(double xi:{.1,1.,10.})for(double shift:{-6.,0.,6.})for(bool scale:{false,true})
 for(const auto& ab:std::vector<std::pair<double,double>>{{-100,-.2},{-2,3},{0,20},{-.1,.1}}) {
  const auto r=cppunivariate::sstd_cert_detail::term(ab.first,ab.second,3,xi,shift,scale);
  for(int j=0;j<=40;++j){const double z=ab.first+(ab.second-ab.first)*j/40;
   const double c=z>0?xi:1/xi;
   const double v=scale?z*(z-shift)/(c*c+z*z):z/(c*c+z*z);
   CHECK(v>=r.lo-1e-12*(1+std::abs(v)));CHECK(v<=r.hi+1e-12*(1+std::abs(v)));
  }
 }
}

TEST_CASE("sstd: lossy sample normalization stops the scalar certificate") {
 const std::vector<double> x{1e-300,1,1e300};
 const auto r=cppunivariate::sstd_mle(mltest::as_span(x),1e-6,200,0,NAN,3,2);
 CHECK(r.status==cppunivariate::SstdStatus::CertificateFailure);CHECK_FALSE(r.certified);
}

TEST_CASE("sstd: held unit skew delegates the complete certified std model") {
 const auto g=mltest::load_distribution_golden("std");
 const auto parent=cppunivariate::student_t_mle_certified(mltest::as_span(g.data),1e-6,200);
 const auto r=cppunivariate::sstd_mle(mltest::as_span(g.data),1e-6,200,NAN,NAN,NAN,1);
 REQUIRE(parent.status==cppunivariate::StudentTStatus::Ok);
 REQUIRE(r.status==cppunivariate::SstdStatus::Ok);CHECK(r.certified);
 CHECK(r.mu==parent.mu);CHECK(r.sigma==parent.sigma);CHECK(r.nu==parent.nu);CHECK(r.logLik==parent.logLik);CHECK(r.xi==1);
 const auto b=cppunivariate::sstd_mle(mltest::as_span(g.data),1e-6,1,NAN,NAN,NAN,1);
 REQUIRE(b.status==cppunivariate::SstdStatus::CertificateFailure);
 const auto limited_parent=cppunivariate::student_t_mle_certified(mltest::as_span(g.data),1e-6,1);
 CHECK(b.stop_detail.budget_exhausted==(limited_parent.status==cppunivariate::StudentTStatus::CertificateBudget));
 CHECK(b.stop_detail.comparison_tie==(limited_parent.status==cppunivariate::StudentTStatus::CertificateTie));
 CHECK(b.stop_detail.candidates.size()==limited_parent.stop_detail.candidates.size());
 CHECK_FALSE(b.stop_detail.candidates.empty());
 const std::vector<double> normal{-2,-1,0,1,2};
 const auto f=cppunivariate::sstd_mle(mltest::as_span(normal),1e-6,200,NAN,NAN,NAN,1);
 REQUIRE(f.status==cppunivariate::SstdStatus::SnormBoundary);CHECK(f.certified);
 CHECK(f.stop_detail.limit_model==cppunivariate::StopModel::Norm);
}

TEST_CASE("sstd: score ranges avoid overflowing finite-extremum products") {
 const double c=1e155,shift=8e153;
 const auto positive=cppunivariate::sstd_cert_detail::kernel(0,shift,c,shift,true);
 const auto negative=cppunivariate::sstd_cert_detail::kernel(-1e157,0,c,shift,true);
 const auto value=[&](double z){const double t=z/c;return t*(t-shift/c)/(1+t*t);};
 CHECK(positive.lo<=value(4e153));CHECK(positive.lo<-.0015);CHECK(positive.hi>=0);
 CHECK(negative.hi>=value(-2.5e156));CHECK(negative.hi>1.0015);CHECK(negative.lo<=0);
 const auto unresolved=cppunivariate::sstd_cert_detail::kernel(0,1.7e308,1.7e308,0,true);
 CHECK(std::isnan(unresolved.lo));CHECK(std::isnan(unresolved.hi));
}

TEST_CASE("sstd: near-two location peaks require a likelihood remainder check") {
 const double nu=std::nextafter(2.,INFINITY);
 const std::vector<double> x{-2,2,2.5};
 const auto r=cppunivariate::sstd_mle(mltest::as_span(x),1e-6,200,NAN,1,nu,1);
 REQUIRE(r.status==cppunivariate::SstdStatus::Ok);CHECK(r.certified);
 CHECK(r.mu==doctest::Approx(2.).epsilon(1e-10));
 double reference=0;for(double v:x)reference+=cppunivariate::distributions::dsstd(v,2,1,nu,1,true);
 CHECK(r.logLik>=reference-1e-10);
}

TEST_CASE("sstd: compact skew score and likelihood enclose seam crossings") {
 const std::vector<double> x{-2,-1,-.4,0,.2,.3,.5,1,2,5};
 for(double nu:{2.1,3.,5.,20.}){
  double shift=0;cppunivariate::dens::sstd_sigma(nu,2,shift);const double m=shift/1.5;
  for(const auto& cell:std::vector<std::pair<double,double>>{{0,.01},{.02,.2},{.2,.7},{.98,1},{.49,.51}}){
   const auto b=cppunivariate::sstd_skew_cert::bounds(mltest::as_span(x),1,nu,m,cell.first,cell.second);
   for(int j=1;j<20;++j){const double q=cell.first+(cell.second-cell.first)*j/20;
    const auto p=cppunivariate::sstd_skew_cert::bounds(mltest::as_span(x),1,nu,m,q,q);
    CHECK(p.value.lo>=b.value.lo-1e-10);CHECK(p.value.hi<=b.value.hi+1e-10);
    CHECK(p.score.lo>=b.score.lo-1e-10);CHECK(p.score.hi<=b.score.hi+1e-10);
   }
  }
 }
}

TEST_CASE("sstd: xi-only certificate has finite, reflected and budget outcomes") {
 const std::vector<double> x{-2,-1,-.4,0,.2,.3,.5,1,2,5};
 const auto a=cppunivariate::sstd_mle(mltest::as_span(x),1e-6,200,0,1,5);
 REQUIRE(a.status==cppunivariate::SstdStatus::Ok);CHECK(a.certified);
 CHECK(a.mu==0);CHECK(a.sigma==1);CHECK(a.nu==5);
 std::vector<double> y;for(double v:x)y.push_back(-v*1e-20);
 const auto b=cppunivariate::sstd_mle(mltest::as_span(y),1e-6,200,0,1e-20,5);
 REQUIRE(b.status==cppunivariate::SstdStatus::Ok);CHECK(b.certified);
 CHECK(b.xi*a.xi==doctest::Approx(1).epsilon(1e-9));
 CHECK(b.logLik==doctest::Approx(a.logLik-x.size()*std::log(1e-20)).epsilon(1e-12));
 const auto low=cppunivariate::sstd_mle(mltest::as_span(x),1e-6,1,0,1,5);
 REQUIRE(low.status==cppunivariate::SstdStatus::CertificateFailure);
 CHECK(low.stop_detail.budget_exhausted);CHECK_FALSE(low.stop_detail.candidates.empty());
 CHECK(std::isinf(low.stop_detail.loglik_upper));
}

#include "cppunivariate/distributions/ht/dpqr.hpp"
TEST_CASE("sstd: xi-only constrained faces carry actual held-moment witnesses") {
 const std::vector<double> x{.5,1,1.5,2,2.5,3,10};
 const auto r=cppunivariate::sstd_mle(mltest::as_span(x),1e-6,200,0,1,5);
 REQUIRE(r.status==cppunivariate::SstdStatus::RightHalfTBoundary);CHECK(r.certified);
 REQUIRE(r.stop_detail.limit_model==cppunivariate::StopModel::Ht);
 REQUIRE(r.stop_detail.limit_coef.size()==3);
 const auto& c=r.stop_detail.limit_coef;
 double ll=0;for(double v:x)ll+=dist::dht(v,c[0],c[1],c[2],true);
 CHECK(ll==doctest::Approx(r.logLik).epsilon(1e-12));
 const std::vector<double> tied{-1,1};
 const auto t=cppunivariate::sstd_mle(mltest::as_span(tied),1e-6,200,0,1,5);
 REQUIRE(t.stop_detail.comparison_tie);REQUIRE(t.stop_detail.candidates.size()==2);
 CHECK(t.stop_detail.candidates[0].model!=t.stop_detail.candidates[1].model);
 double shift=0;cppunivariate::dens::sstd_sigma(5,2,shift);const double m=shift/1.5;
 const std::vector<double> contact{-m/std::sqrt(1-m*m),1};
 const auto u=cppunivariate::sstd_mle(mltest::as_span(contact),1e-6,200,0,1,5);
 CHECK(u.status==cppunivariate::SstdStatus::CertificateFailure);CHECK(u.certificate_reason==3);
 CHECK_FALSE(u.stop_detail.candidates.empty());CHECK_FALSE(u.certified);
}

TEST_CASE("sstd: compact skew bounds keep overflow unresolved") {
 const double v=cppunivariate::sstd_skew_cert::kernel_log(std::log(1e300),5);
 CHECK(std::isfinite(v));CHECK(v==doctest::Approx(2*std::log(1e300)-std::log(3.)).epsilon(1e-14));
 const auto r=cppunivariate::sstd_skew_cert::mul({0,1},{-INFINITY,INFINITY});
 CHECK(r.lo==-INFINITY);CHECK(r.hi==INFINITY);
 const std::vector<double> x{1e-300,1,1e300};
 const auto fit=cppunivariate::sstd_mle(mltest::as_span(x),1e-6,1,0,1,5);
 CHECK(fit.status==cppunivariate::SstdStatus::CertificateFailure);CHECK_FALSE(fit.certified);
}

TEST_CASE("sstd: xi-only certification accepts stable large held moments") {
 const std::vector<double> x{-2,-1,0,1,3};
 for(double nu:{128.,1e6,1e12,1e17}){
  const auto r=cppunivariate::sstd_mle(mltest::as_span(x),1e-6,200,0,1,nu);
  REQUIRE(r.status==cppunivariate::SstdStatus::Ok);CHECK(r.certified);
  CHECK(r.certificate_reason==0);
  double ll=0;for(double v:x)ll+=dist::dsstd(v,0,1,nu,r.xi,true);
  CHECK(ll==doctest::Approx(r.logLik).epsilon(1e-12));
 }
}

TEST_CASE("sstd: 80-digit absolute moments and normalizers across binary64 shapes") {
 // Independently generated with mpmath gamma, at >=100 working digits.
 // Columns: nu, E|Z_t|, log f_t(0), E|Z_GED|, GED lambda,
 //          skew-t f(0), skew-GED f(0), skew-t sigma, skew-t shift (xi=2).
 const double cases[][9] = {
  {2.0000000000000004, 0.000000021073424255447009406680231424770341336517574117977962651925054124374662543256665, 16.982105923718660216992213101123129341574373713828549285712043818557604818257985, 0.79788456080286537602466102589960775724549274041609629206263354525707935268629559, 1.0000000000000001920282993215806661084571382353783559904241908420704807760941827, 17520097.642037474427746391716865777903730129498623401862092262391283294999018294, 0.35974083685977584838311806586420061861822165954583126401520879232128487623777636, 1.8027756377319943694311923077316248204367067468578596873326202013708082745213945, 0.000000031610136383170514110020347137155512004776361176966943977887581186561993814884997},
  {2.0000000009313226, 0.00003051757810529957225886545633475226906543212145807948877179065970774100694424626, 9.7040605281250132895931873401798618046825433138166428682991802443768731445067857, 0.79788456084511199826011949282981303394104240686406523445458151352357659030683696, 1.0000000004027125318252713169722224195046204987883040809142794952483405740663643, 12098.222514984640135719902458061396994632857310139098124615024042843384667819846, 0.35974083683686601467966427228794496614128863747029774239249099176331960004521296, 1.8027756371508142304670690557335542598376966325899034299975893393179339704466617, 0.00004577636715794935838829818450212840359814818218711923315768598956161151041636939},
  {2.01, 0.099313303945021937435357038901486096693035432393606231429959760268681626812269813, 1.6124975966674275444261381791288863764250823602470163234888078182940745731746769, 0.79833581792550095356411910619409828767006835585670881480059360933903190640472851, 1.0043064379413051906531707716273158219453133738908829679325845042092850674273179, 3.7099833991983966734345114686649211803509972449163374701310636081643751221083549, 0.35949524225374540998358493907313968249616072936835019269227544191990176508914948, 1.7966101280561479409275897487241855944494675329321869065772897658367471066302193, 0.14896995591753290615303555835222914503955314859040934714493964040302244021840472},
  {2.5, 0.53935260118837935666793572235555273276586896544304013033994663186388298848651568, -0.21192063724339982685702984638220296330411703158506060938778816620096572522068095, 0.81579499642782909572517392060650109476938348315096307810844396731643103641388868, 1.1779399192904094663134687059172944335497235857978275044086126623288001163079969, 0.63531189068105531114363379453354924865781279276363290698546909440644771943042278, 0.34840733657021741741373670284569044894609086263229890187999800131486056030953229, 1.6110469378886793519627273083799154906007188298455334959296605686607800921529714, 0.80902890178256903500190358353332909914880344816456019550991994779582448272977352},
  {3.0, 0.63661977236758134307553505349005744813783858296182579499066937623558719053690614, -0.45158270528945486472619522989488214357179467855505631739294306197874414791513136, 0.82732303605044488511530597126516486301598596469188785439545850781976862112374365, 1.2990864196143416139120366553081062247115434488435949404071019611212524958650286, 0.51644624191561298691595697780630537602983537288584187011445652748544419189551731, 0.3388971176168244772835465077402236791734039300192218938820100776609317594515113, 1.5290877500061793566147680689562383754535359264555028572847936068687965573750394, 0.95492965855137201461330258023508617220675787444273869248600406435338078580535921},
  {5.0, 0.73510519389572273268176866441729258852984864048885420373248802705470707129235973, -0.71320677717172878298459884236231756439203024656866998675362454344819834380365538, 0.84834233938739694449371059836189613400233273813183932157779123342772662072456852, 1.5284979880673955023927813280801934010776411883134512800613040252989167620330596, 0.41859812569148597820257472821124880581165407017637875290574007725778939160963793, 0.31487705274858816363236776736238678114682137898639894177601478394137931880461113, 1.4262348320988191118107825123654659127402810214004744764589078757603612958187715, 1.1026577908435840990226529966259388827947729607332813055987320405820606069385396},
  {10.0, 0.77339804192278635481342352105217863671778930684676847751849673171368182415896479, -0.83232557649384737696710260333825861825958407692654710030503338150049197181515483, 0.86054316590114856567123695349340563117612582356826655895334243384417950811334278, 1.6638640035147894498431654852663541715524572159938028830591932093139317016309076, 0.38288274017274910985058562456850778207337223500941468149954709467519620911734982, 0.29670956195283847568026543594659288109977478130135558637601004889169156846807953, 1.3799184050832498407734004620407985173442942176109245874629383795838859209427792, 1.1600970628841795322201352815782679550766839602701527162777450975705227362384472},
  {63.0, 0.79462137964502505485240430804368395152938269233200647635155243492976188278979301, -0.90677618947881934320331019448033644425668291211719502877062920359745029139380836, 0.86585393086672028667884410913784295433647593565153255442490748731067553819577332, 1.7274688412953044998478597511636678961529000651421982472361628559168600449601288, 0.36286121056566805952216189953447497688805388204363022412966012226680029012531362, 0.28891103742229792560033357781769705480085531225831165475682640766531270710201849, 1.3525154127679407569776666372643814939759269365944668087772159617904737334816566, 1.1919320694675375822786064620655259272940740384980097145273286523946428241846895},
  {64.0, 0.79467390097168634028637926661068722888748300448960077677999682423572742572329821, -0.90697027514819485451039883575574195895460659389206584934228592434942007397752258, 0.86585913146032384076432764820730788628442482706240353739099723858870818718323132, 1.7275613188711364034297368173359333986210333607363127212285914255385292909047799, 0.362811099000245814039603402564057788961651874704879798861636604618101941397312, 0.28890384113638333382172311569198477082294802241823702821372492463285728831041529, 1.3524459804396978267582284389377557342561319303823631707021502979340482098695447, 1.1920108514575295104295688999160308433312245067344011651699952363535911385849473},
  {127.0, 0.79629040337329089547944890626404575656786030233167644049543783704411682635131473, -0.91297034222380061673399376233681726467538946793160631024565135979373284293548553, 0.86598223795931764235976681410522244657188508237056293501970391577473011959316981, 1.7301218293181408731199809433430839746965318716042689307743134455420676087506982, 0.36126701277700069148434072448362839025906899605857232679784975825210632692648596, 0.28873417426975356458292833311580134425631549489384335245387925079430396899244552, 1.3503049971636422001705244770062167301261553748328472619194304801217224879820185, 1.1944356050599363432191733593960686348517904534975146607431567555661752395269721},
  {127.99999999999999, 0.79630304260259858692010484267124488832390857634696238567881565098309681692584623, -0.91301745985384315367260600520725532856190066143172804082122004322895102728938138, 0.86598290229151653441428199831080635345311911095707148074855403325174733266699133, 1.7301395603052553220502909111516758552491540400243475120138438524830262664850505, 0.36125492616400614224781098555113383163530957469893452077909775010151625885685339, 0.28873326304234055744519362519954151046512457708971070498609601006700515858156148, 1.3502882265535566517130875204296040909630229426083097702059782728982526243705798, 1.1944545639038978803801572640068673324858628645204435785182234764746452253887693},
  {128.0, 0.7963030426025985870982950433036405628152247954154530986394058899375189837015639, -0.91301745985384315433690157197061093296568137661445900837508776387886280432343749, 0.86598290229151653442361347119212478346856140894598035985653392587097908445544228, 1.7301395603052553223000033807002141589619563799685482501908619631176837188642343, 0.36125492616400614207741016073483446991173526678438599109342822307677886104213182, 0.28873326304234055743239459768246742124284894663420387116809557002183799515624517, 1.3502882265535566514766490021161131081565996088277442571704143726300173704002386, 1.1944545639038978806474425649554608442228371931231796479591088349062784755523458},
  {128.00000000000003, 0.79630304260259858745467544456843179133865432015763482448712776574541945902514582, -0.9130174598538431556654927054973216946007617980078753497704582896945542651606501, 0.86598290229151653444227641695476163427965778181452475922902734105834840269719507, 1.7301395603052553227994283197972905753843564610153279256894227046120659033241931, 0.36125492616400614173660851110223586153018140601703348247335054349146934666211905, 0.28873326304234055740679654264831925548073924949227029203389243377100927503960939, 1.3502882265535566510037719654891313020967498306412493449883399447402218075918349, 1.1944545639038978811820131668526476870079814802364522367306916486181291885377187},
  {128.99999999999997, 0.79631548299104717493467607595235995151107418286836047064231473733129659638737769, -0.91306383934091979717553213017260476180248074609864956380198987119738205971392439, 0.86598355140414153310446224293627357291002965502575598415123127791380120656334234, 1.7301569760009033983455380445076863391788668814566921633176929269299787194117987, 0.3612430294886671590950715629020277528852463507633609324113047439893139974856453, 0.28873237275049189140426598196713003390976881150536302256145298069360356484978835, 1.3502717193160250474454443850523890623781001094824871626020576191575542889783576, 1.1944732244865707624020141139285399272666112743025407059634721059969448945810665},
  {129.0, 0.79631548299104717528547193660726769326697328248826057858250097078279689553269712, -0.91306383934091979848339112386473842116935018189617159289281726791448690019099894, 0.86598355140414153312269926317810580834990444933423932270935068223251125574984253, 1.7301569760009033988361195717346423600996553333309207464764074865749670859275363, 0.36124302948866715875960447265033946555922069522262987883156884498986840444798705, 0.28873237275049189137925379006625500712474264054436832389031587688524398343732955, 1.3502717193160250469799644485119935230394132169680388129673970433951110019473246, 1.1944732244865707629282079049109015399004599237323908678737514561741953432990457},
  {129.00000000000003, 0.79631548299104717563626779726217527817096516876663950114619898787871397368857961, -0.91306383934091979979125011755687149820760749033080975153850481123572426699250573, 0.86598355140414153314093628341993803186937756880343186757988396952568494918150455, 1.7301569760009033993267010989615981329634869073181613506044654574125659799043743, 0.36124302948866715842413738239865132806586503524492548995228656432908996938611449, 0.28873237275049189135424159816537999673556439769195585771508484625165538011419724, 1.3502717193160250465144845119715981914659854006778009438173553831307749087424085, 1.1944732244865707634544016958932629172564477531499592517192984818180709605328694},
  {10000.0, 0.79786460994809431206605043399576515119345900717197081836717350639006644060993741, -0.9188635232032975417488244021426599932253240830828966003276622609049263653796794, 0.86602539666374666679959120512376807162630192763207849364381572357399843552608866, 1.7320306707852338639647462617054845326494179371345585940993154858951008538598427, 0.35975995866653023397694976314763143697791954802872516164520719034788980587624535, 0.28867514427964786020772958389790228213529482636431286635884085202520743705929751, 1.3482125739039984478573897516827394101572961984581349227528596691202191297992067, 1.1967969149221414680990756509936477267901885107579562275507602595850996609149061},
  {100000000.0, 0.79788455880815391647188927555308365686792286243758195064445320205855752060263854, -0.91893852570467264178032836140559763986108247363245007939087177697800376358202179, 0.86602540378443857553599078072580574668917516078759831514041577138810737897001639, 1.7320508055608839716811562122595894686802270468005055719882479517731825588637539, 0.35974083877162601086608428980465747379935214813013565798487466753298826334012351, 0.288675134594812979124291134695740472776355466506611722811036036766054901440388, 1.348186010658364064360975212144473279798223183308792629869163033513342522971204, 1.1968268382122308747078339133296254853018842936563729259666798030878362809039578},
  {1000000000000.0, 0.79788456080266588473969102952140588691163287764893719945156714735042685547380533, -0.91893853320392274178032873640561763848639747363578341281714839048276569059392706, 0.86602540378443864676372245847559146504637980913707518024686843746360008840248952, 1.7320508075686764942521218177014330317195627998098914682028720076359283744709143, 0.35974083685996704431923449400601138147995083352924522367450325609041184045102513, 0.28867513459481288225457535894816754557583978441891273868816090596933581319900701, 1.3481860080024781859789019448856947131174549434920888290618386129086420090827543, 1.196826841203998827109536544282108830367449316473405799177350721025640283210708},
  {1e+16, 0.79788456080286533593277809979712609987053550217875322291956207089436119492356425, -0.91893853320467266678032973640560763986139747363640841281715154028276569592726037, 0.86602540378443864676372317075292906069795542183680786141907671511761867157180694, 1.7320508075688772734475188096220929847754231722067180073289332227417383461112834, 0.359740836859775878425884459756945610707051701698370178916587591708796122206294, 0.28867513459481288225457439025098841479568907452875758143392601625649832230956233, 1.3481860080022125973954499108750022049665271285802626095148447020020733241632392, 1.1968268412042980038991671496956891498058032532681298343793431063415417923853464},
  {1e+100, 0.79788456080286535587989211986876373695171726232986931533185165934131585179860368, -0.9189385332046727417803297364056176398613974736377834128171515404827656959272604, 0.86602540378443864676372317075293618347140262690519031402790348972596650845440002, 1.7320508075688772935274463415058723669428052538103806280558069794519330169088, 0.35974083685977585930738327466805832244883702890351179337749095448011705196370266, 0.28867513459481288225457439025097872782380087563506343800930116324198883615146667, 1.3481860080022125708339354142658019486118753512291075843123952899536145308727414, 1.1968268412042980338198381798031456054275758934948039729977774890119737776979055},
  {1e+300, 0.79788456080286535587989211986876373695171726232986931533185165934131585179860368, -0.9189385332046727417803297364056176398613974736377834128171515404827656959272604, 0.86602540378443864676372317075293618347140262690519031402790348972596650845440002, 1.7320508075688772935274463415058723669428052538103806280558069794519330169088, 0.35974083685977585930738327466805832244883702890351179337749095448011705196370266, 0.28867513459481288225457439025097872782380087563506343800930116324198883615146667, 1.3481860080022125708339354142658019486118753512291075843123952899536145308727414, 1.1968268412042980338198381798031456054275758934948039729977774890119737776979055},
  {1.7976931348623157e+308, 0.79788456080286535587989211986876373695171726232986931533185165934131585179860368, -0.9189385332046727417803297364056176398613974736377834128171515404827656959272604, 0.86602540378443864676372317075293618347140262690519031402790348972596650845440002, 1.7320508075688772935274463415058723669428052538103806280558069794519330169088, 0.35974083685977585930738327466805832244883702890351179337749095448011705196370266, 0.28867513459481288225457439025097872782380087563506343800930116324198883615146667, 1.3481860080022125708339354142658019486118753512291075843123952899536145308727414, 1.1968268412042980338198381798031456054275758934948039729977774890119737776979055},
 };
 for(const auto& c:cases) {
  CAPTURE(c[0]);
  const auto ulp=[](double x){return std::nextafter(x,INFINITY)-x;};
  CHECK(std::abs(cppunivariate::dens::std_abs_moment(c[0])-c[1])<=4*ulp(c[1]));
  CHECK(std::abs(cppunivariate::dens::log_dstd_unit_const(c[0])-c[2])<=8*ulp(std::abs(c[2])));
  double shift=0;
  const double sigma=cppunivariate::dens::sstd_sigma(c[0],2,shift);
  CHECK(std::abs(sigma-c[7])<=5*ulp(c[7]));
  CHECK(std::abs(shift-c[8])<=5*ulp(c[8]));
  double shift2=0;
  CHECK(dist::fs::sstd_sigma(c[0],2,shift2)==sigma);CHECK(shift2==shift);
  CHECK(std::abs(dist::dsstd(0,0,1,c[0],2,false)-c[5])<=32*ulp(c[5]));
  if(c[0]>=128) {
   const double lb=-c[2]-.5*std::log(c[0]-2);
   CHECK(std::abs(cppunivariate::special::lbeta(c[0]/2,.5)-lb)<1e-13);
   CHECK(std::abs(cppunivariate::special::lbeta(.5,c[0]/2)-lb)<1e-13);
  }
 }
}

TEST_CASE("sstd: large-shape inference jets agree with independent 80-digit derivatives") {
 // Value, four natural-coordinate scores, then row-major log-density Hessian.
 const double cases[][22]={
 {128, -1.0181714032083151300150149298027433177451894221657715770425044165161001255892557, 0.41164953227302154518757861515998566544597749167915447680814701884886495878713943, -1.0, -0.000033192328133727266931078331167239418152028536140793850636551137991244244263344703, -0.045099522758409347125481504342443483787650012973495025101502501734761593145129407, -0.46272788032892676077042345977403174061059236606014091581282270780983294018194485, -0.41164953227302154518757861515998566544597749167915447680814701884886495878713943, -0.000065382628281326349869398658764274150950221633839076477641565026465090063654871697, 0.085513030995305694293634596862209331654824219035676687263458883129477041415439444, -0.41164953227302154518757861515998566544597749167915447680814701884886495878713943, 1.0, 0.0, 0.0, -0.000065382628281326349869398658764274150950221633839076477641565026465090063654871697, 0.0, 0.00000052463710020777267175984466097792680960619097195431486312328222837296681620463404, 0.0000046087007479487614047279849003893083001514649410738181613327032881700227143984017, 0.085513030995305694293634596862209331654824219035676687263458883129477041415439444, 0.0, 0.0000046087007479487614047279849003893083001514649410738181613327032881700227143984017, 0.086975593658030475846217058542552107920723775473354207068212631423907902608810809},
 {129, -1.0182043352747576199744628573649513359695392397630444632044100622295129271878251, 0.41158466292421041069312069873841398987622304772770589910757856638319736445962122, -1.0, -0.000032673845482030184774088776012607859014747885607346711521859568604469424558274586, -0.045094949898390570517131026067611754614752635444694087843912776856371766682491646, -0.4626626219655654637883005340119269096255018454413850405145681083250712950039579, -0.41158466292421041069312069873841398987622304772770589910757856638319736445962122, -0.000064360098805344489758812189005296992685930560637270603346201772126695272219679968, 0.085501013801635171952085200685525200215325247227903871969179020756435310945281469, -0.41158466292421041069312069873841398987622304772770589910757856638319736445962122, 1.0, 0.0, 0.0, -0.000064360098805344489758812189005296992685930560637270603346201772126695272219679968, 0.0, 0.00000051239246261612360810653861379055517268080562496523223924525063570177264048395898, 0.0000045372980025889286090125378080679200381200126550708984415057039063437732747452135, 0.085501013801635171952085200685525200215325247227903871969179020756435310945281469, 0.0, 0.0000045372980025889286090125378080679200381200126550708984415057039063437732747452135, 0.086965147231190323418958307412942162285943901152266971134753043113648317249959533},
 {10000, -1.0223182514114800165840142876632411365350108461778134689632902760506044016375562, 0.40349075061763447510562299292223810579547312596957992230335059244740930944659564, -1.0, -0.0000000053160760254526887149897865308497129325981737668850106441023335961613929552345565, -0.044519032317051461396073858554734301536553629119691641442262535126204477977163144, -0.45450680202952903198686944146908567071006595839096217003337327818583359420787606, -0.40349075061763447510562299292223810579547312596957992230335059244740930944659564, -0.00000001044671733748949756172664224564947988496585189204693254814993518230774865226141, 0.08399959303184829872795531693426626880277735001883159554489451220624438471286737, -0.40349075061763447510562299292223810579547312596957992230335059244740930944659564, 1.0, 0.0, 0.0, -0.00000001044671733748949756172664224564947988496585189204693254814993518230774865226141, 0.0, 0.000000000001063370775099522873581944281028045192107785905702503992449675509630272345152402, 0.00000000075021161358432781874294656875415133163862267684424798020937107888025274880039318, 0.08399959303184829872795531693426626880277735001883159554489451220624438471286737, 0.0, 0.00000000075021161358432781874294656875415133163862267684424798020937107888025274880039318, 0.085653512694085616486753705648197501992601670612620531779018957783527876255767555},
 {100000000, -1.0223713990799101329804161589794263501998700840308207767270534609403680908206349, 0.4033863107716211181933630482880796697581338490374501546423332886213172819750821, -1.0, -0.000000000000000053145208398822146085591105733351254100239269065434054292615696267700108429364397, -0.044511531271655723415882367917783622419836957378829164987834471166748969292100343, -0.45440138858415487474114461876190719317973779649552796835630299165793148469719693, -0.4033863107716211181933630482880796697581338490374501546423332886213172819750821, -0.00000000000000010443341151272529736117759995362527562965309888414041709977125165501346344007906, 0.083980193119254024371837775469063417006325470983780798803512905314511906429502353, -0.4033863107716211181933630482880796697581338490374501546423332886213172819750821, 1.0, 0.0, 0.0, -0.00000000000000010443341151272529736117759995362527562965309888414041709977125165501346344007906, 0.0, 0.000000000000000000000001062904183526264879012726614773637198097524523448668560525186829497934574646718, 0.0000000000000000075014749784467760123174851934557850559525472211812340070080803312510026802516412, 0.083980193119254024371837775469063417006325470983780798803512905314511906429502353, 0.0, 0.0000000000000000075014749784467760123174851934557850559525472211812340070080803312510026802516412, 0.085636481446472902438161489035236777001352481917452980060952150328510900902063495},
 {1000000000000, -1.0223714043938994430450829999083255893815239364656504636266014810451125834157327, 0.40338630032932446978635618798222367312695151826618811984454610746584109191141107, -1.0, -0.00000000000000000000000053145206843995484454730360662433261779596524198713516240987352095253192483870198, -0.044511530521583243527000035228488731315378541911226671346324655834013061581720229, -0.45440137804428958644324348805564573882350600304497340249651475577061751507785073, -0.40338630032932446978635618798222367312695151826618811984454610746584109191141107, -0.0000000000000000000000010443340813737987837714551160016034329370500089442777501513940763589000231638492, 0.083980191179542368565760868969596993564425658460881392297539082254333845658185203, -0.40338630032932446978635618798222367312695151826618811984454610746584109191141107, 1.0, 0.0, 0.0, -0.0000000000000000000000010443340813737987837714551160016034329370500089442777501513940763589000231638492, 0.0, 1.0629041368814646712185708144216923154449245528977516151106689619816556585338596e-36, 7.5014749143201469709920752124726739356082964725516084309665582697022891615899641e-26, 0.083980191179542368565760868969596993564425658460881392297539082254333845658185203, 0.0, 7.5014749143201469709920752124726739356082964725516084309665582697022891615899641e-26, 0.085636479743487477602356640367094514445744174216433599169648553703323134215656521},
 {10000000000000000, -1.022371404394430841968315333421816895138810299168882272887905339233057231623698, 0.40338630032828024013839221408223079147407906447509072865807016101534951883585081, -1.0, -5.3145206843840001792155598977693967576913754932616358460810872247292514097510452e-33, -0.044511530521508236279331780139034514964237838737524687212070831123359825633877285, -0.45440137804323559992920385545950549141273875577106111876539062093355112155417435, -0.40338630032828024013839221408223079147407906447509072865807016101534951883585081, -1.0443340813704234384365317898970145199752041100077189562911185744543134847724163e-32, 0.083980191179348397402975834153475510050041784362873784527763416317325270989368785, -0.40338630032828024013839221408223079147407906447509072865807016101534951883585081, 1.0, 0.0, 0.0, -1.0443340813704234384365317898970145199752041100077189562911185744543134847724163e-32, 0.0, 1.0629041368768001913413243751963317843930151708909320503341125908931803893015684e-48, 7.5014749143137343079790885042931523263088482841107884306999541166164799928002689e-34, 0.083980191179348397402975834153475510050041784362873784527763416317325270989368785, 0.0, 7.5014749143137343079790885042931523263088482841107884306999541166164799928002689e-34, 0.085636479743317179061266384517457514444443475386252590086733173776077517702943766},
 };
 for(const auto& c:cases) {
  CAPTURE(c[0]);
  const std::array<double,cppunivariate::inference_max_parameters> p{0,1,c[0],2};
  const auto a=cppunivariate::inference_join_detail::density(0,"sstd",p);
  CHECK(std::abs(a.v-c[1])<2e-15);
  for(int i=0;i<4;++i)CHECK(std::abs(a.g[i]-c[2+i])<2e-12*std::abs(c[2+i]));
  for(int i=0;i<16;++i)CHECK(std::abs(a.h[i]-c[6+i])<=2e-11*std::abs(c[6+i]));
 }
 for(double nu:{1e100,std::numeric_limits<double>::max()}) {
  const std::array<double,cppunivariate::inference_max_parameters> p{0,1,nu,2};
  const auto a=cppunivariate::inference_join_detail::density(0,"sstd",p);
  CHECK(std::abs(a.v-std::log(.35974083685977598))<2e-15);
  for(double v:a.g)CHECK(std::isfinite(v));
  for(double v:a.h)CHECK(std::isfinite(v));
 }
}

TEST_CASE("sstd all-free chain Hessian agrees with the existing symbolic Hessian") {
  const std::vector<double> x{-2,-.4,.2,1.3,5};
  for(double shape:{2.7,6.,20.}) {
    const std::array<double,4> p{{-.3,std::log(1.2),std::log(shape-2),std::log(1.7)}};
    const auto f=cppunivariate::local_accuracy::skew_objective(x,cppunivariate::local_accuracy::variables(p),true);
    const cppunivariate::SstdCost cost{mltest::as_span(x)};
    double h[16];cost.hessian(p.data(),h);
    CHECK(f.v==doctest::Approx(cost(p.data())).epsilon(2e-13));
    for(int j=0;j<16;++j)CHECK(f.h[j]==doctest::Approx(h[j]).epsilon(2e-11));
  }
}

TEST_CASE("sstd ordinary weak-shape fit agrees with an independent 70-digit root") {
 const std::vector<double> x{2.4291773262964673,-.73163185976732792,-.93682922797550661,-.26048472949381396,-.95674691470653528,-.78459441323041201,.35700820366317593,-1.6367807511092567,-1.3898330381499311,-.41998156853316515,.40347324334581075,.66523827780574563,.8324664072029595,1.0189818340718511,1.1290436557557941,.054518682776008152,-.24178505202140846,-.33310311771134749,-.41896350134081606,-.66604654971825816};
 const auto r=cppunivariate::sstd_mle({x.data(),x.size()},1e-10,200);
 REQUIRE(r.status==cppunivariate::SstdStatus::Ok);
 CHECK(r.mu==doctest::Approx(-.07871618235652491).epsilon(1e-10).scale(0));
 CHECK(r.sigma==doctest::Approx(.9390815087264246).epsilon(1e-10).scale(0));
 CHECK(r.nu==doctest::Approx(84.08872795888163).epsilon(1e-10).scale(0));
 CHECK(r.xi==doctest::Approx(1.5751160992666097).epsilon(1e-10).scale(0));
}

TEST_CASE("sstd large-sample compensated derivatives preserve replicated coefficients") {
 std::vector<double> base;
 for(int j=0;j<1000;++j)base.push_back(cppunivariate::special::qt((j+.5)/1000.,4.,true,false)/std::sqrt(2.));
 const auto b=cppunivariate::sstd_mle({base.data(),base.size()},1e-10,200);
 REQUIRE(b.status==cppunivariate::SstdStatus::Ok);
 std::vector<double> x;for(int j=0;j<100;++j)x.insert(x.end(),base.begin(),base.end());
 const auto r=cppunivariate::sstd_mle({x.data(),x.size()},1e-10,200);
 REQUIRE(r.status==cppunivariate::SstdStatus::Ok);
 CHECK(std::abs(r.mu)<=1e-10*r.sigma);
 CHECK(r.sigma==doctest::Approx(.99656856660305998).epsilon(1e-10).scale(0));
 CHECK(r.nu==doctest::Approx(4.046365436983925).epsilon(1e-10).scale(0));
 CHECK(r.xi==doctest::Approx(1.).epsilon(1e-10).scale(0));
 CHECK(r.sigma==doctest::Approx(b.sigma).epsilon(1e-10).scale(0));
 CHECK(r.nu==doctest::Approx(b.nu).epsilon(1e-10).scale(0));
 CHECK(r.xi==doctest::Approx(b.xi).epsilon(1e-10).scale(0));
 CHECK(r.logLik/100==doctest::Approx(b.logLik).epsilon(1e-10));
}

TEST_CASE("sstd: logarithmic thin-side radius exceeds binary64 range") {
 CHECK(cppunivariate::distributions::dsstd(1e300,0,1,5,1e-8,true)==doctest::Approx(-4360.482172927685).epsilon(1e-10));
 CHECK(cppunivariate::distributions::psstd(1e300,0,1,5,1e-8,false,true)==doctest::Approx(-3671.316082941905).epsilon(1e-10));
}

TEST_CASE("sstd: compensated standardized moment at nu 2.5492086449176115") {
 CHECK(cppunivariate::distributions::dsstd(-0.6644244801433566,0.012734271748561354,1.0184483076867987,2.5492086449176115,77179.93142228863,true)==doctest::Approx(-0.18927952912370902).epsilon(1e-10));
}
