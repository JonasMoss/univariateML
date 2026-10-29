#include "test_support.hpp"
#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <limits>
#include <cmath>

namespace {
std::vector<int> bnb_sample(unsigned seed, int n, double r=1, double a=2, double b=5) {
  cppunivariate::distributions::StdRng<> rng(seed);
  std::vector<int> x;
  for (int j=0;j<n;++j)
    x.push_back(static_cast<int>(cppunivariate::distributions::rbnbinom(rng,r,a,b)));
  return x;
}
}
TEST_CASE("bnbinom: golden, exact masses, score gate") {
  const auto golden=mltest::load_distribution_golden("bnbinom");
  std::vector<int> x;
  for (double v:golden.data) x.push_back(static_cast<int>(v));
  const auto fit=cppunivariate::bnbinom_mle({x.data(),x.size()});
  REQUIRE(fit.status==cppunivariate::BnbinomStatus::Ok);
  CHECK(fit.size<=fit.beta);
  // Independent nlminb + lgamma/lbeta oracle, seed 13086; replay with
  // notes/evaluations/2026-10-29-bnbinom-oracle.R.
  CHECK(fit.logLik==doctest::Approx(-1275.6474863695578).epsilon(1e-10));
  CHECK(fit.logLik==doctest::Approx(golden.logLik).epsilon(1e-10));
  CHECK(fit.size==doctest::Approx(golden.estimates.at("size")).epsilon(2e-6));
  CHECK(fit.alpha==doctest::Approx(golden.estimates.at("alpha")).epsilon(2e-6));
  CHECK(fit.beta==doctest::Approx(golden.estimates.at("beta")).epsilon(2e-6));
  cppunivariate::bnbinom_detail::Data d;
  d.n=x.size();
  for (int v:x) { d.x.push_back(v); d.count.push_back(1); }
  const std::vector<double> p{fit.size,fit.alpha,fit.beta};
  CHECK(cppunivariate::bnbinom_detail::likelihood(d,p)==doctest::Approx(fit.logLik).epsilon(1e-12));
  cppunivariate::bnbinom_detail::Cost c{d,{0,0,0},{0,1,2}};
  const auto g=c.gradient({std::log(fit.size),std::log(fit.alpha),std::log(fit.beta)});
  double norm=0;
  for (int j=0;j<3;++j) norm+=std::pow(g[j]*d.n/p[j],2);
  CHECK(std::sqrt(norm)<1e-3);
}
TEST_CASE("bnbinom: independent gamma formula and exact mirror") {
  for (double r:{.2,1.,5.}) for (double a:{.4,2.,10.}) for (double b:{.7,1.,6.})
    for (int x:{0,1,4,50}) {
      const double expected=cppunivariate::special::lgamma(r+x)-cppunivariate::special::lgamma(r)-
        cppunivariate::special::lgamma(x+1.)+cppunivariate::special::lbeta(a+r,b+x)-
        cppunivariate::special::lbeta(a,b);
      CHECK(cppunivariate::bnbinom_detail::logpmf(x,r,a,b)==doctest::Approx(expected).epsilon(1e-11));
      CHECK(cppunivariate::bnbinom_detail::logpmf(x,r,a,b)==
        doctest::Approx(cppunivariate::bnbinom_detail::logpmf(x,b,a,r)).epsilon(1e-13));
    }
  for (int x:{0,1,10}) {
    CHECK(cppunivariate::bnbinom_detail::logpmf(x,2,4e14,1e14)==
      doctest::Approx(cppunivariate::special::dnbinom(x,2,.8,true)).epsilon(1e-11));
    CHECK(cppunivariate::bnbinom_detail::logpmf(x,1,2,1)==
      doctest::Approx(std::log(2.)+cppunivariate::special::lbeta(x+1.,3)).epsilon(1e-12));
  }
}
TEST_CASE("bnbinom: every held subset, scores and mirror fits") {
  const auto x=bnb_sample(7,1000);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> p{1,2,5};
  for (int mask=0;mask<8;++mask) {
    std::vector<double> held(3,nan);
    for (int j=0;j<3;++j) if (mask&(1<<j)) held[j]=p[j];
    const auto fit=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,held[0],held[1],held[2]);
    REQUIRE(fit.status==cppunivariate::BnbinomStatus::Ok);
    const std::vector<double> estimate{fit.size,fit.alpha,fit.beta};
    for (int j=0;j<3;++j) if (mask&(1<<j)) CHECK(estimate[j]==p[j]);
    cppunivariate::bnbinom_detail::Data d;
    d.n=x.size();
    for (int v:x) { d.x.push_back(v); d.count.push_back(1); }
    for (int j=0;j<3;++j) if (!(mask&(1<<j))) {
      auto lo=estimate, hi=estimate;
      lo[j]*=std::exp(-1e-5); hi[j]*=std::exp(1e-5);
      const double score=(cppunivariate::bnbinom_detail::likelihood(d,hi)-
        cppunivariate::bnbinom_detail::likelihood(d,lo))/(2e-5*estimate[j]);
      CHECK(std::abs(score)<1e-3);
    }
  }
  const auto r=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,1,nan,nan);
  const auto b=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,nan,nan,1);
  CHECK(r.logLik==doctest::Approx(b.logLik).epsilon(1e-10));
  CHECK(r.alpha==doctest::Approx(b.alpha).epsilon(1e-5));
  CHECK(r.beta==doctest::Approx(b.size).epsilon(1e-5));
}
TEST_CASE("bnbinom: explicit faces and input validation") {
  const std::vector<int> constant(20,2), zeros(10,0), bad{-1,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::bnbinom_mle({constant.data(),constant.size()}).status==cppunivariate::BnbinomStatus::PoissonLimit);
  CHECK(cppunivariate::bnbinom_mle({constant.data(),constant.size()},1e-8,600,1).status==cppunivariate::BnbinomStatus::NegativeBinomialLimit);
  CHECK(cppunivariate::bnbinom_mle({constant.data(),constant.size()},1e-8,600,nan,nan,1).status==cppunivariate::BnbinomStatus::NegativeBinomialLimit);
  CHECK(cppunivariate::bnbinom_mle({zeros.data(),zeros.size()}).status==cppunivariate::BnbinomStatus::PointMass);
  CHECK(cppunivariate::bnbinom_mle({zeros.data(),zeros.size()},1e-8,600,1,2,5).status==cppunivariate::BnbinomStatus::Ok);
  CHECK(cppunivariate::bnbinom_mle({bad.data(),bad.size()}).status==cppunivariate::BnbinomStatus::InvalidInput);
  CHECK(cppunivariate::bnbinom_mle({}).status==cppunivariate::BnbinomStatus::InvalidInput);
  CHECK(cppunivariate::bnbinom_mle({constant.data(),constant.size()},1e-8,0).status==cppunivariate::BnbinomStatus::InvalidInput);
}
TEST_CASE("bnbinom: CDF and quantile beyond the former count budget") {
  for (double u:{.1,.5,.9,.99}) {
    const double q=cppunivariate::distributions::qbnbinom(u,1,2,5,true,false);
    CHECK(cppunivariate::distributions::pbnbinom(q-1,1,2,5,true,false)<u);
    CHECK(cppunivariate::distributions::pbnbinom(q,1,2,5,true,false)>=u);
  }
  CHECK(std::isfinite(cppunivariate::distributions::qbnbinom(.99,1,.01,1,true,false)));
  CHECK(std::isfinite(cppunivariate::distributions::pbnbinom(1e12,1,2,5,true,true)));
}

TEST_CASE("bnbinom: overdispersion can select NB; diagonal finite maxima occur") {
  std::vector<int> two(20,0); two.insert(two.end(),20,5);
  CHECK(cppunivariate::bnbinom_mle({two.data(),two.size()}).status==
    cppunivariate::BnbinomStatus::NegativeBinomialLimit);
  const auto x=bnb_sample(1,500,1,2,1);
  const auto fit=cppunivariate::bnbinom_mle({x.data(),x.size()});
  REQUIRE(fit.status==cppunivariate::BnbinomStatus::Ok);
  CHECK(fit.size==doctest::Approx(fit.beta).epsilon(1e-5));
}

#include "cppunivariate/distributions/bnbinom/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("bnbinom: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("bnbinom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dbnbinom(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pbnbinom(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qbnbinom(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rbnbinom(rng, p[0], p[1], p[2]); });
}

// mpmath (100 digits): S(x)=30/((x+6)*(x+7)) and
// p(x)=60/((x+5)*(x+6)*(x+7)) for size=1, alpha=2, beta=5.
// mp.log(60/((x+5)*(x+6)*(x+7))); mp.log(30/((x+6)*(x+7)))
TEST_CASE("bnbinom: power law across double counts and log quantiles") {
  CHECK(cppunivariate::distributions::dbnbinom(1e100,1,2,5,true)==doctest::Approx(-686.6811833359916045).epsilon(2e-15));
  CHECK(cppunivariate::distributions::pbnbinom(1e100,1,2,5,false,true)==doctest::Approx(-457.1158212171469814).epsilon(2e-15));
  CHECK(cppunivariate::distributions::qbnbinom(1e-300,1,2,5,false,false)==doctest::Approx(5.477225575051661e150).epsilon(2e-13));
  for(double lp:{-800.,-10000.}) {
    const double q=cppunivariate::distributions::qbnbinom(lp,1,20,5,false,true);
    REQUIRE(std::isfinite(q));
    CHECK(cppunivariate::distributions::pbnbinom(q,1,20,5,false,true)==doctest::Approx(lp).epsilon(2e-14));
  }
  CHECK(std::isfinite(cppunivariate::distributions::dbnbinom(std::numeric_limits<double>::max(),1,2,5,true)));
  CHECK(std::isfinite(cppunivariate::distributions::pbnbinom(std::numeric_limits<double>::max(),1,2,5,false,true)));
}

TEST_CASE("bnbinom: StdRng Poisson beyond integer range") {
  for(double mean:{3e9,1e12,1e18,1e30}) {
    cppunivariate::distributions::StdRng<> rng(7813);
    double sum=0,squares=0;
    const int n=20000;
    for(int j=0;j<n;++j) {
      const double x=rng.rpois(mean);
      REQUIRE(std::isfinite(x));
      REQUIRE(x>=0);
      const double z=(x-mean)/std::sqrt(mean);
      sum+=z;squares+=z*z;
    }
    CHECK(std::abs(sum/n)<.04);
    CHECK(std::abs(squares/n-1)<.06);
  }
}
