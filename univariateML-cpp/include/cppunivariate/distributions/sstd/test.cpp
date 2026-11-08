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
  CHECK(r.mu == D_APPROX_FD(g.estimates.at("mean")));
  CHECK(r.sigma == D_APPROX_FD(g.estimates.at("sd")));
  CHECK(r.nu == D_APPROX_FD(g.estimates.at("nu")));
  CHECK(r.xi == D_APPROX_FD(g.estimates.at("xi")));
  CHECK(r.logLik == D_APPROX_FD(g.logLik));

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

TEST_CASE("sstd: xi-only certification does not endorse cancelling skew moments") {
 const std::vector<double> x{-2,-1,0,1,3};
 for(double nu:{128.,1e6,1e12,1e17}){
  const auto r=cppunivariate::sstd_mle(mltest::as_span(x),1e-6,200,0,1,nu);
  CHECK(r.status==cppunivariate::SstdStatus::CertificateFailure);CHECK_FALSE(r.certified);
  CHECK(r.certificate_reason==4);CHECK_FALSE(r.stop_detail.candidates.empty());
  CHECK(std::isinf(r.stop_detail.loglik_upper));
 }
}
