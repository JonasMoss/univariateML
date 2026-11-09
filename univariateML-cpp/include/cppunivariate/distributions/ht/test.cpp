#include "test_support.hpp"
#include "cppunivariate/distributions/ht/mle.hpp"
#include "cppunivariate/distributions/ht/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
TEST_CASE("ht: golden and scores (mu held at 0)") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto g=mltest::load_distribution_golden("ht");
  const auto r=cppunivariate::ht_mle(mltest::as_span(g.data),0.0,nan,nan);
  CHECK(r.status==cppunivariate::HtStatus::Ok);
  CHECK(r.mu==0.0);
  CHECK(r.nu==doctest::Approx(g.estimates.at("nu")).epsilon(mltest::kGolden_ht).scale(0));
  CHECK(r.sigma==doctest::Approx(g.estimates.at("sigma")).epsilon(mltest::kGolden_ht).scale(0));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_ht).scale(0));
  double ls=std::log(r.sigma), l1=ls,l2=ls;
  CHECK(std::abs((cppunivariate::ht_detail::profile(mltest::as_span(g.data),r.nu*std::exp(1e-5),l1,true)-cppunivariate::ht_detail::profile(mltest::as_span(g.data),r.nu*std::exp(-1e-5),l2,true))/2e-5)<1e-3);
}
TEST_CASE("ht: fixed subsets, collapse and normal limit (mu held at 0)") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x={0,1,2}, eq={0,1}, z={0,0}, positive={1,1,1};
  CHECK(cppunivariate::ht_mle(mltest::as_span(x),0.0,nan,nan).status==cppunivariate::HtStatus::Unbounded);
  CHECK(cppunivariate::ht_mle(mltest::as_span(x),0.0,1.0,nan).status==cppunivariate::HtStatus::Ok);
  CHECK(cppunivariate::ht_mle(mltest::as_span(eq),0.0,1.0,nan).status==cppunivariate::HtStatus::ScaleBoundary);
  CHECK(cppunivariate::ht_mle(mltest::as_span(eq),0.0,.5,nan).status==cppunivariate::HtStatus::Unbounded);
  CHECK(cppunivariate::ht_mle(mltest::as_span(z),0.0,1.0,2.0).status==cppunivariate::HtStatus::Ok);
  CHECK(cppunivariate::ht_mle(mltest::as_span(z),0.0,nan,2.0).status==cppunivariate::HtStatus::NormalLimit);
  CHECK(cppunivariate::ht_mle(mltest::as_span(positive),0.0,nan,nan).status==cppunivariate::HtStatus::NormalLimit);
  CHECK(cppunivariate::ht_mle(mltest::as_span(x),0.0,-1.0,nan).status==cppunivariate::HtStatus::InvalidParameter);
  CHECK(cppunivariate::ht_mle(mltest::as_span(positive),0.0,1e-300,nan).status==cppunivariate::HtStatus::SearchFailure);
  const std::vector<double> bad={-1,2};
  CHECK(cppunivariate::ht_mle(mltest::as_span(bad),0.0,nan,nan).status==cppunivariate::HtStatus::InvalidData);
}
TEST_CASE("ht: d/p/q/r and extreme tails") {
  CHECK(cppunivariate::distributions::dht(-1,0,3,1,false)==0);
  CHECK(std::isfinite(cppunivariate::distributions::dht(1e200,0,3,1e-200,true)));
  for(double nu:{.1,1.,3.,100.}) for(bool lower:{false,true}) for(bool logs:{false,true}) for(double p:{0.,.01,.5,.99,1.}) {
    const double q=cppunivariate::distributions::qht(logs?std::log(p):p,0,nu,2,lower,logs);
    CHECK(cppunivariate::distributions::pht(q,0,nu,2,lower,false)==doctest::Approx(p).epsilon(1e-10));
  }
  const double q=cppunivariate::distributions::qht(-100,0,3,1,false,true);
  CHECK(cppunivariate::distributions::pht(q,0,3,1,false,true)==doctest::Approx(-100).epsilon(1e-10));
  cppunivariate::distributions::StdRng<> rng(9);
  for(int i=0;i<20;++i) CHECK(cppunivariate::distributions::rht(rng,0,3,1)>=0);
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("ht: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("ht",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dht(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pht(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qht(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rht(rng, p[0], p[1], p[2]); });
}

// Generating mpmath snippet (binary64 inputs retained exactly):
// import mpmath as mp; mp.mp.dps=100
// print(mp.mpf(0))  # inverse upper tail 1, equivalently log upper tail 0
TEST_CASE("ht: targeted tail and endpoint regressions") {
  CHECK(cppunivariate::distributions::qht(1.0,0, 3.0, 1.0, false, false) == doctest::Approx(0.0).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qht(0.0,0, 3.0, 1.0, false, true) == doctest::Approx(0.0).epsilon(1e-10));
}


TEST_CASE("ht: P3 restrictions and closed faces (mu held at 0)") {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 const std::vector<double> x{.01,.1,1,10,100};
 const auto f=cppunivariate::ht_mle(mltest::as_span(x),0.0,nan,nan);
 REQUIRE(f.status==cppunivariate::HtStatus::Ok);CHECK(f.nu<1);CHECK(f.restricted==0u);
}

// P4 (C5): free translated threshold. Values independently checked against
// extraDistr::dht multistart profiles; see notes/evaluations/2026-10-29-p4-report.md.
TEST_CASE("ht: free threshold is the sample minimum") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x={5,7,9,20,2};
  const auto r=cppunivariate::ht_mle(mltest::as_span(x),nan,nan,nan);
  CHECK(r.mu==2.0);
  CHECK(r.status==cppunivariate::HtStatus::Ok);
  auto ll=[&](double nu,double s){double a=0;for(double v:x)a+=cppunivariate::distributions::dht(v,r.mu,nu,s,true);return a;};
  CHECK(ll(r.nu,r.sigma)==doctest::Approx(r.logLik).epsilon(1e-6));
  // Held mu above an observation is invalid; at or below the minimum is fine.
  CHECK(cppunivariate::ht_mle(mltest::as_span(x),2.5,nan,nan).status==cppunivariate::HtStatus::InvalidData);
  CHECK(cppunivariate::ht_mle(mltest::as_span(x),2.0,nan,nan).status!=cppunivariate::HtStatus::InvalidData);
  CHECK(cppunivariate::ht_mle(mltest::as_span(x),0.0,nan,nan).status!=cppunivariate::HtStatus::InvalidData);
}

TEST_CASE("ht: free threshold, nu held, reduces to the shifted zero-threshold model") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x={2,2,3,5,12}; // k=2 ties at the minimum, n=5.
  const auto r=cppunivariate::ht_mle(mltest::as_span(x),nan,3.0,nan);
  std::vector<double> y{x}; for(double &v:y) v-=2.0;
  const auto z=cppunivariate::ht_mle_zero(mltest::as_span(y),3.0,nan);
  CHECK(r.mu==2.0);
  CHECK(r.nu==3.0);
  CHECK(r.sigma==doctest::Approx(z.sigma));
  CHECK(r.logLik==doctest::Approx(z.logLik));
  CHECK(r.status==cppunivariate::HtStatus::Ok);
}

TEST_CASE("ht: free threshold restricts free nu with held sigma") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x={4,4,4,4,9}; // k=4 of 5 at the minimum: no sigma collapse risk when sigma is held.
  const auto r=cppunivariate::ht_mle(mltest::as_span(x),nan,nan,2.0);
  CHECK(r.status==cppunivariate::HtStatus::Ok);
  CHECK(r.mu==4.0);
  CHECK(r.sigma==2.0);
  CHECK(std::isfinite(r.nu));
  CHECK(r.nu>=1.0);
}

TEST_CASE("ht: free threshold, nu and sigma both free: collapse tie at 2k vs n") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  { // 2k < n: nu* = k/(n-k) < 1, so the nu >= 1 restriction suffices for existence.
    const std::vector<double> x={5,5,7,9,20};
    const auto r=cppunivariate::ht_mle(mltest::as_span(x),nan,nan,nan);
    CHECK(r.status==cppunivariate::HtStatus::Ok);
    CHECK(r.mu==5.0);
    CHECK(r.nu>=1.0);
    auto ll=[&](double nu,double s){double a=0;for(double v:x)a+=cppunivariate::distributions::dht(v,r.mu,nu,s,true);return a;};
    CHECK(ll(r.nu,r.sigma)==doctest::Approx(r.logLik).epsilon(1e-6));
    // Scale score vanishes; at nu=1 the shape score satisfies the one-sided KKT condition.
    {const double h=1e-5*r.sigma;CHECK(std::abs((ll(r.nu,r.sigma+h)-ll(r.nu,r.sigma-h))/(2*h))<1e-3);}
    {const double h=1e-5*std::max(1.0,r.nu);CHECK((ll(r.nu+h,r.sigma)-ll(r.nu,r.sigma))/h<1e-3);}
  }
  { // 2k == n exactly: the finite nu=1, sigma->0 limit is the (unattained) global supremum here.
    const std::vector<double> x={3,3,8,20};
    CHECK(cppunivariate::ht_mle(mltest::as_span(x),nan,nan,nan).status==cppunivariate::HtStatus::ScaleBoundary);
  }
  { // 2k > n: unbounded even under the restriction (its own break-even nu* exceeds 1).
    const std::vector<double> x={3,3,3,8};
    CHECK(cppunivariate::ht_mle(mltest::as_span(x),nan,nan,nan).status==cppunivariate::HtStatus::Unbounded);
  }
}

TEST_CASE("ht: free threshold escapes when the half-normal face wins") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  // A deterministic light-tailed sample avoids RNG-dependent kurtosis.
  std::vector<double> x;
  for (int i=0;i<300;++i) x.push_back(2.0+(i+0.5)/300.0);
  const auto r=cppunivariate::ht_mle(mltest::as_span(x),nan,nan,nan);
  CHECK(r.status==cppunivariate::HtStatus::NormalLimit);
  CHECK(std::isinf(r.nu));
}

TEST_CASE("ht: translated restriction bit follows mu, nu, sigma coefficient order") {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{5,5,7,9,20};
  const auto f = cppunivariate::ht_mle(mltest::as_span(x),nan,nan,nan);
  REQUIRE(f.status == cppunivariate::HtStatus::Ok);
  CHECK(f.nu == 1);
  CHECK(f.restricted == 4u);
  const auto held = cppunivariate::ht_mle(mltest::as_span(x),nan,0.9,nan);
  CHECK(held.status == cppunivariate::HtStatus::Ok);
  CHECK(held.nu == .9);
  CHECK(held.restricted == 0u);
}

TEST_CASE("ht: README threshold shifts all four operations") {
  const double mu = 3.0;
  CHECK(cppunivariate::distributions::dht(2.0, mu, 3.0, 2.0, false) == 0.0);
  CHECK(cppunivariate::distributions::qht(0.0, mu, 3.0, 2.0, true, false) == mu);
  for (bool lower : {true, false}) for (bool logs : {true, false}) {
    CHECK(cppunivariate::distributions::dht(4.0, mu, 3.0, 2.0, logs) == doctest::Approx(cppunivariate::distributions::dht(1.0, 0.0, 3.0, 2.0, logs)));
    CHECK(cppunivariate::distributions::pht(4.0, mu, 3.0, 2.0, lower, logs) == doctest::Approx(cppunivariate::distributions::pht(1.0, 0.0, 3.0, 2.0, lower, logs)));
    CHECK(cppunivariate::distributions::qht((logs ? std::log(0.25) : 0.25), mu, 3.0, 2.0, lower, logs) == doctest::Approx(mu + cppunivariate::distributions::qht((logs ? std::log(0.25) : 0.25), 0.0, 3.0, 2.0, lower, logs)));
  }
  cppunivariate::distributions::StdRng<> a(94), b(94);
  for (int i = 0; i < 32; ++i)
    CHECK(cppunivariate::distributions::rht(a, mu, 3.0, 2.0) == mu + cppunivariate::distributions::rht(b, 0.0, 3.0, 2.0));
  CHECK(std::isnan(cppunivariate::distributions::dht(4.0, INFINITY, 3.0, 2.0, true)));
}

TEST_CASE("ht: outer certificate handles multiple modes and a spent budget") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{.12773,12.5979,3.91782,2.58658,.138212};
  const auto initial=cppunivariate::ht_detail::ht_mle_zero_legacy(mltest::as_span(x));
  REQUIRE(initial.status==cppunivariate::HtStatus::Ok);
  const auto exhausted=cppunivariate::ht_detail::certified_search(mltest::as_span(x),nan,false,initial,0);
  CHECK(exhausted.status==cppunivariate::HtStatus::IterationFailure);
  const auto fit=cppunivariate::ht_mle(mltest::as_span(x),0,nan,nan);
  REQUIRE(fit.status==cppunivariate::HtStatus::Ok);
  CHECK(fit.nu==doctest::Approx(.41508).epsilon(1e-3));
  CHECK(fit.logLik>=initial.logLik-1e-8*x.size());
}

#include <random>
TEST_CASE("ht: 600 seeded profiles agree with the original search") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::mt19937_64 rng(1162026);
  for(int i=0;i<600;++i) {
    INFO(i);
    const int group=i/200;
    std::student_t_distribution<double> td(group==0?.4:group==1?80.:3.);
    std::vector<double> x(24);
    for(double& v:x) v=std::abs(td(rng));
    if(group==2) {x[0]=0; x[1]=0;}
    const auto input=mltest::as_span(x);
    const auto old=group==2 ? cppunivariate::ht_detail::free_threshold_search(input,nan)
                            : cppunivariate::ht_detail::ht_mle_zero_legacy(input);
    const auto fit=cppunivariate::ht_mle(input,group==2?nan:0.,nan,nan);
    CHECK(fit.status!=cppunivariate::HtStatus::IterationFailure);
    CHECK(fit.status!=cppunivariate::HtStatus::SearchFailure);
    if(old.status==cppunivariate::HtStatus::Ok && fit.status==cppunivariate::HtStatus::Ok)
      CHECK(fit.logLik>=old.logLik-1e-8*x.size());
    else if(old.status==cppunivariate::HtStatus::NormalLimit && fit.status==cppunivariate::HtStatus::Ok)
      CHECK(fit.logLik>old.logLik);
    else if(old.status==cppunivariate::HtStatus::Ok && fit.status==cppunivariate::HtStatus::NormalLimit)
      CHECK(fit.logLik>=old.logLik-1e-8*x.size());
    else CHECK(fit.status==old.status);
  }
}

TEST_CASE("ht: cached and log-domain scale boxes enclose roots") {
  for (const std::vector<double>& logs : {std::vector<double>{-2.,-1.,0.}, std::vector<double>{-300.,-1.,0.}}) {
    std::vector<double> squares;
    for(double d:logs) if(2*d >= -460) squares.push_back(std::exp(2*d));
    if(squares.size()!=logs.size()) squares.clear();
    for(bool cached : {false,true}) {
      const std::vector<double> empty;
      for(const auto ab : {std::pair<double,double>{.4,80.}, std::pair<double,double>{1e200,1e202}}) {
        double lo=0,hi=0;
        REQUIRE(cppunivariate::ht_detail::scale_box(logs,cached?squares:empty,0.,3.,ab.first,ab.second,lo,hi));
        for(double nu : {ab.first,10*ab.first,ab.second}) {
          double sl=-3,sh=-3;
          for(double d:logs) {
            sl+=(nu+1)*cppunivariate::ht_detail::logistic(2*(d-lo)-std::log(nu));
            sh+=(nu+1)*cppunivariate::ht_detail::logistic(2*(d-hi)-std::log(nu));
          }
          CHECK(sl>=-1e-12);
          CHECK(sh<=1e-12);
        }
      }
    }
  }
}

TEST_CASE("ht: structured face payloads") {
  const std::vector<double> x{1,1.5,2,2.5,3};
  const auto r=cppunivariate::ht_mle(mltest::as_span(x),0,NAN,NAN);
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  CHECK(r.stop_detail.limit_coef[0]==0);
  CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(std::sqrt(4.5)));
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(5*(.5*std::log(2/std::acos(-1.))-.5*std::log(4.5)-.5)));
  const auto b=cppunivariate::ht_detail::certified_search(mltest::as_span(x),NAN,false,r,0);
  REQUIRE(b.budget_exhausted);CHECK(b.stop_detail.candidates.size()>=1);
  CHECK(std::isinf(b.stop_detail.loglik_upper));
  CHECK(b.stop_detail.loglik_lower>=b.stop_detail.candidates[0].loglik_lower);
}
