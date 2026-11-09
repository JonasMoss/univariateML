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
  CHECK(mltest::truth_lattice_quantile(cppunivariate::distributions::qbnbinom(.99,1,.01,1,true,false),.99,true,
      [](double x) {return cppunivariate::distributions::pbnbinom(x,1,.01,1,true,false);}));
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
  CHECK(mltest::truth_lattice_quantile(cppunivariate::distributions::qbnbinom(1e-300,1,2,5,false,false),1e-300,false,
      [](double x) {return cppunivariate::distributions::pbnbinom(x,1,2,5,false,false);}));
  CHECK(cppunivariate::distributions::pbnbinom(5.477225575051661e150,1,2,5,false,true)==doctest::Approx(std::log(1e-300)).epsilon(2e-13));
  for(double lp:{-800.,-10000.}) {
    const double q=cppunivariate::distributions::qbnbinom(lp,1,20,5,false,true);
    CHECK(mltest::truth_lattice_quantile(q,lp,false,
        [](double x) {return cppunivariate::distributions::pbnbinom(x,1,20,5,false,true);}));
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

TEST_CASE("bnbinom: P3 unfinished optimizer is a failure") {
  const std::vector<int> values{0,1,2,3,6,12,24};
  const cppunivariate::ispan ix{values.data(),values.size()};
  CHECK(cppunivariate::bnbinom_mle(ix,1e-8,1,NAN,2,3).status==cppunivariate::BnbinomStatus::IterationFailure);
}

// Uniform beta mixing with size one telescopes: mass=1/((k+1)(k+2)),
// S(k)=1/(k+2). Inputs below are binary64, not exact decimal probabilities.
TEST_CASE("bnbinom: unit member quantiles allow adjacent answers at jumps") {
  const auto adjacent=[](double x,double lo) {return x==lo || x==lo+1;};
  CHECK(cppunivariate::distributions::qbnbinom(.83,1,1,1,true,false)==4);
  CHECK(cppunivariate::distributions::qbnbinom(.17,1,1,1,false,false)==4);

  CHECK(adjacent(cppunivariate::distributions::qbnbinom(-0.6931471805599453,1,1,1,true,true),0));
  CHECK(adjacent(cppunivariate::distributions::qbnbinom(-4.605170185988092,1,1,1,false,true),98));
  CHECK(adjacent(cppunivariate::distributions::qbnbinom(1e-6,1,1,1,false,false),999998));
  CHECK(adjacent(cppunivariate::distributions::qbnbinom(.5,1,1,1,true,false),0));
  CHECK(adjacent(cppunivariate::distributions::qbnbinom(.25,1,1,1,false,false),2));
  CHECK(cppunivariate::distributions::qbnbinom(-800,1,1,1,false,true)==INFINITY);
  CHECK(mltest::truth_lattice_quantile(cppunivariate::distributions::qbnbinom(1e-300,1,1,1,false,false),1e-300,false,
      [](double x) {return cppunivariate::distributions::pbnbinom(x,1,1,1,false,false);}));
  for(double k:{0.,1.,98.,999998.,1e100}) {
    CHECK(cppunivariate::distributions::pbnbinom(k,1,1,1,false,true)==doctest::Approx(-std::log(k+2)).epsilon(1e-13));
    CHECK(cppunivariate::distributions::dbnbinom(k,1,1,1,true)==doctest::Approx(-std::log(k+1)-std::log(k+2)).epsilon(1e-13));
  }
}

TEST_CASE("bnbinom: structured stop payloads") {
  const std::vector<int> x{2,2,2};
  const auto r=cppunivariate::bnbinom_mle({x.data(),x.size()});
  REQUIRE(r.stop_detail.limit_model==cppunivariate::StopModel::Pois);
  CHECK(r.stop_detail.limit_coef[0]==2);
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(3*(2*std::log(2.)-2-std::lgamma(3.))));
  const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,1);
  REQUIRE(f.stop_detail.limit_model==cppunivariate::StopModel::Nbinom);
  REQUIRE(f.stop_detail.limit_coef.size()==2);
  CHECK(f.stop_detail.limit_coef[0]==1);
  CHECK(f.stop_detail.limit_coef[1]==doctest::Approx(1./3));
}

TEST_CASE("bnbinom: scalar global certificates and exact roots") {
  const std::vector<int> x(12,1);
  const double root=std::sqrt(2.);
  for(int j=0;j<3;++j) {
    std::vector<double> held{1,1,1};held[j]=NAN;
    const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,held[0],held[1],held[2]);
    REQUIRE(f.status==cppunivariate::BnbinomStatus::Ok);
    const std::vector<double> p{f.size,f.alpha,f.beta};
    CHECK(p[j]==doctest::Approx(root).epsilon(1e-8));
    CHECK(f.logLik==doctest::Approx(12*std::log(root/((root+1)*(root+2)))).epsilon(1e-12));
    for(int k=0;k<3;++k)if(k!=j)CHECK(p[k]==1);
    // A deliberately insufficient certificate budget is terminal, with a
    // witness rather than an unchecked coefficient vector.
    const auto stop=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,1,held[0],held[1],held[2]);
    CHECK(stop.status==cppunivariate::BnbinomStatus::IterationFailure);
    CHECK(stop.stop_detail.budget_exhausted);
    REQUIRE(stop.stop_detail.candidates.size()==1);
    CHECK(std::isnan(stop.size));
    CHECK(stop.stop_detail.loglik_upper==INFINITY);
    const auto &c=stop.stop_detail.candidates[0];
    CHECK(c.coef.size()==3);
    CHECK(c.loglik_lower<=f.logLik);
  }
}

TEST_CASE("bnbinom: scalar all-zero limits for each free coordinate") {
  const std::vector<int> x(8,0);
  for(int j=0;j<3;++j) {
    std::vector<double> p{.37,1.8,4.6};p[j]=NAN;
    const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,p[0],p[1],p[2]);
    CHECK(f.status==cppunivariate::BnbinomStatus::PointMass);
    CHECK(f.stop_detail.limit_model==cppunivariate::StopModel::Pois);
    CHECK(f.stop_detail.limit_coef[0]==0);
  }
}

TEST_CASE("bnbinom: noninteger scalar roots against independent 80-digit oracle") {
  // oracle.py/oracle.json in dev/research/evaluations/2026-11-06-bnbinom-cert-data.
  const std::vector<int> x{0,0,1,2,3,6,12,24};
  const std::vector<double> roots{1.2350155733754016305,.7885962837901386579,22.077541746455293487};
  const std::vector<double> ll{-22.705386239276122233,-23.775023031191501427,-23.021170044851784493};
  for(int j=0;j<3;++j) {
    std::vector<double> p{.37,1.8,4.6};p[j]=NAN;
    const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,p[0],p[1],p[2]);
    REQUIRE(f.status==cppunivariate::BnbinomStatus::Ok);
    const std::vector<double> estimate{f.size,f.alpha,f.beta};
    CHECK(estimate[j]==doctest::Approx(roots[j]).epsilon(1e-8));
    CHECK(f.logLik==doctest::Approx(ll[j]).epsilon(1e-12));
    // The certificate root must beat fixed multiplicative perturbations.
    auto lo=estimate,hi=estimate;lo[j]*=.9;hi[j]*=1.1;
    cppunivariate::bnbinom_detail::Data d;
    d.n=x.size();for(int v:x){d.x.push_back(v);d.count.push_back(1);}
    CHECK(cppunivariate::bnbinom_detail::likelihood(d,lo)<f.logLik);
    CHECK(cppunivariate::bnbinom_detail::likelihood(d,hi)<f.logLik);
  }
}

TEST_CASE("bnbinom: scalar cancellation is a terminal numerical stop") {
  const std::vector<int> x(8,1);
  const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-20,600,1,NAN,1);
  CHECK(f.status==cppunivariate::BnbinomStatus::NumericalFailure);
  CHECK_FALSE(f.stop_detail.budget_exhausted);
  CHECK(std::isnan(f.alpha));
  CHECK_FALSE(f.stop_detail.candidates.empty());
}

TEST_CASE("bnbinom: scalar scaled exact roots and floating-point failure") {
  const std::vector<int> x(5,1);
  for(double beta:{1e-8,.01,10.,100.}) {
    const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,1,NAN,beta);
    REQUIRE(f.status==cppunivariate::BnbinomStatus::Ok);
    CHECK(f.alpha==doctest::Approx(std::sqrt(beta*(beta+1))).epsilon(1e-8));
  }
  // At this scale the digamma cancellation margin cannot isolate the shape
  // to 1e-8 relative accuracy. The exact root still exists.
  const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,1,NAN,1e5);
  CHECK(f.status==cppunivariate::BnbinomStatus::NumericalFailure);
  CHECK(std::isnan(f.alpha));
}

TEST_CASE("bnbinom: alpha-held global off-diagonal certificate retains both labels and seam") {
  const auto golden=mltest::load_distribution_golden("bnbinom");
  std::vector<int> x;for(double v:golden.data)x.push_back(static_cast<int>(v));
  const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,NAN,2,NAN);
  REQUIRE(f.status==cppunivariate::BnbinomStatus::Ok);
  CHECK(f.size==doctest::Approx(1.15097395202066).epsilon(1e-8));
  CHECK(f.beta==doctest::Approx(4.50374529147196).epsilon(1e-8));
  CHECK(f.logLik==doctest::Approx(-1275.720827702446).epsilon(1e-12));
  CHECK(f.size<f.beta);
  bool direct=false,mirror=false,diagonal=false;
  for(const auto &c:f.stop_detail.candidates) {
    REQUIRE(c.coef.size()==3);
    direct=direct || (std::abs(c.coef[0]-f.size)<1e-7 && std::abs(c.coef[2]-f.beta)<1e-7);
    mirror=mirror || (std::abs(c.coef[2]-f.size)<1e-7 && std::abs(c.coef[0]-f.beta)<1e-7);
    if(c.coef[0]==c.coef[2]) {
      diagonal=true;CHECK(c.loglik_upper<f.logLik-.1);
    }
  }
  CHECK(direct);CHECK(mirror);CHECK(diagonal);
  CHECK(f.stop_detail.loglik_lower<=f.logLik);
  CHECK(f.stop_detail.loglik_upper>=f.logLik);
  CHECK_FALSE(f.stop_detail.budget_exhausted);
}

TEST_CASE("bnbinom: alpha-held diagonal is certified without imposing symmetry") {
  const std::vector<int>x{0,0,1,2,3,6,12,24};
  const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,NAN,.5,NAN);
  REQUIRE(f.status==cppunivariate::BnbinomStatus::Ok);
  CHECK(f.size==doctest::Approx(1.1088158545389952).epsilon(1e-8));
  CHECK(f.beta==doctest::Approx(f.size).epsilon(1e-10));
  CHECK(f.logLik==doctest::Approx(-23.848045048100712).epsilon(1e-12));
  // The population information is singular on this seam; the observed
  // likelihood Hessian can still be negative definite.
  cppunivariate::bnbinom_detail::Data d;
  d.n=x.size();for(int v:x){d.x.push_back(v);d.count.push_back(1);}
  const auto h=cppunivariate::bnbinom_detail::alpha_hessian(d,.5,f.size,f.beta);
  CHECK(h.rr<0);CHECK(h.rr*h.bb>h.rb*h.rb);
}

TEST_CASE("bnbinom: alpha-held work exhaustion and cancellation never return an estimate") {
  const std::vector<int>x{0,0,1,2,3,6,12,24};
  const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,1,NAN,2,NAN);
  CHECK(f.status==cppunivariate::BnbinomStatus::IterationFailure);
  CHECK(f.stop_detail.budget_exhausted);
  CHECK(std::isnan(f.size));CHECK(std::isnan(f.beta));
  CHECK_FALSE(f.stop_detail.candidates.empty());
  CHECK(f.stop_detail.loglik_upper>=f.stop_detail.loglik_lower);
  const auto zero=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,0,NAN,2,NAN);
  CHECK(zero.status==cppunivariate::BnbinomStatus::InvalidInput);
}

TEST_CASE("bnbinom: alpha-held box Hessian encloses interior derivatives") {
  const std::vector<int>x{0,1,3,12,24};
  cppunivariate::bnbinom_detail::Data d;
  d.n=x.size();for(int v:x){d.x.push_back(v);d.count.push_back(1);}
  for(double a:{.1,1.,10.})for(double r:{.2,1.,8.})for(double b:{.3,2.,20.}) {
    const cppunivariate::bnbinom_detail::AlphaBox box{.99*r,1.01*r,.99*b,1.01*b};
    const auto h=cppunivariate::bnbinom_detail::alpha_hessian_bounds(d,a,box);
    for(double u:{.99,1.,1.01})for(double v:{.99,1.,1.01}) {
      const auto actual=cppunivariate::bnbinom_detail::alpha_hessian(d,a,u*r,v*b);
      CHECK(h[0].rr<=actual.rr);CHECK(actual.rr<=h[1].rr);
      CHECK(h[0].rb<=actual.rb);CHECK(actual.rb<=h[1].rb);
      CHECK(h[0].bb<=actual.bb);CHECK(actual.bb<=h[1].bb);
    }
  }
}

TEST_CASE("bnbinom: alpha-held unrepresentable certificate range stops numerically") {
  const std::vector<int>x{0,0,1,2,3,6,12,24};
  const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,NAN,1e308,NAN);
  CHECK(f.status==cppunivariate::BnbinomStatus::NumericalFailure);
  CHECK_FALSE(f.stop_detail.budget_exhausted);
  CHECK(std::isnan(f.size));CHECK(std::isnan(f.beta));
  for(const auto &candidate:f.stop_detail.candidates) {
    CHECK(std::isfinite(candidate.loglik_lower));CHECK(std::isfinite(candidate.loglik_upper));
  }
}

TEST_CASE("bnbinom: alpha-free pair certificates transport the held value under swapping") {
  const auto golden=mltest::load_distribution_golden("bnbinom");
  std::vector<int> x;for(double v:golden.data)x.push_back(static_cast<int>(v));
  for(double held:{1.,5.,1.0672234787479615,5.3647338351087397}) {
    const auto a=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,held,NAN,NAN);
    const auto b=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,NAN,NAN,held);
    REQUIRE(a.status==cppunivariate::BnbinomStatus::Ok);
    REQUIRE(b.status==cppunivariate::BnbinomStatus::Ok);
    CHECK(a.size==held);CHECK(b.beta==held);
    CHECK(a.alpha==b.alpha);CHECK(a.beta==b.size);CHECK(a.logLik==b.logLik);
    CHECK(a.stop_detail.loglik_lower<=a.logLik);CHECK(a.stop_detail.loglik_upper>=a.logLik);
    CHECK(a.stop_detail.limit_model==cppunivariate::StopModel::Nbinom);
    CHECK(a.stop_detail.limit_coef[0]==held);
    CHECK(a.stop_detail.limit_loglik<a.stop_detail.loglik_lower);
    CHECK_FALSE(a.stop_detail.budget_exhausted);
    CHECK_FALSE(a.stop_detail.candidates.empty());
    bool root=false;
    for(const auto &candidate:a.stop_detail.candidates)if(candidate.model==cppunivariate::StopModel::None && candidate.loglik_lower<=a.logLik && candidate.loglik_upper>=a.logLik)root=true;
    CHECK(root);
  }
}

TEST_CASE("bnbinom: alpha-free pair budgets and unproved faces are terminal") {
  const std::vector<int> x{0,0,1,2,3,6,12,24};
  for(int mirror=0;mirror<2;++mirror) {
    const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,1,mirror?NAN:1.,NAN,mirror?1.:NAN);
    CHECK(f.status==cppunivariate::BnbinomStatus::IterationFailure);
    CHECK(f.stop_detail.budget_exhausted);
    CHECK(std::isnan(f.size));CHECK(std::isnan(f.alpha));CHECK(std::isnan(f.beta));
    CHECK_FALSE(f.stop_detail.candidates.empty());
    CHECK(f.stop_detail.loglik_upper>=f.stop_detail.loglik_lower);
  }
  // A numerical face comparison alone does not prove this sample's supremum.
  const std::vector<int> under{0,0,5,5};
  const auto unresolved=cppunivariate::bnbinom_mle({under.data(),under.size()},1e-8,600,1,NAN,NAN);
  CHECK(unresolved.status==cppunivariate::BnbinomStatus::NumericalFailure);
  CHECK(std::isnan(unresolved.alpha));CHECK(std::isnan(unresolved.beta));
  CHECK(unresolved.stop_detail.limit_model==cppunivariate::StopModel::Nbinom);
}

TEST_CASE("bnbinom: exact mixture face proofs for constant and binary pair samples") {
  for(const std::vector<int> &x:{std::vector<int>{2,2,2},std::vector<int>{0,0,1,1,1}}) {
    for(int mirror=0;mirror<2;++mirror) {
      const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,mirror?NAN:.7,NAN,mirror?.7:NAN);
      CHECK(f.status==cppunivariate::BnbinomStatus::NegativeBinomialLimit);
      CHECK(f.stop_detail.limit_model==cppunivariate::StopModel::Nbinom);
      CHECK(f.stop_detail.limit_coef[0]==.7);
      CHECK(std::isnan(f.alpha));
    }
  }
}

TEST_CASE("bnbinom: alpha-free pair mass and Hessian intervals enclose interior values") {
  cppunivariate::bnbinom_detail::Data d{{0,1,4,15},{2,1,2,1},6};
  for(double r:{.2,1.,5.})for(double a:{.3,2.,20.})for(double b:{.2,3.,10.}) {
    const cppunivariate::bnbinom_detail::AlphaBox q{a*.99,a*1.01,b*.99,b*1.01};
    const auto bound=cppunivariate::bnbinom_detail::pair_bound(d,r,q);
    const auto h=cppunivariate::bnbinom_detail::pair_hessian_bounds(d,r,q);
    for(double u:{.99,1.,1.01})for(double v:{.99,1.,1.01}) {
      const auto point=cppunivariate::bnbinom_detail::pair_hessian(d,r,a*u,b*v);
      CHECK(h[0].rr<=point.rr);CHECK(point.rr<=h[1].rr);
      CHECK(h[0].rb<=point.rb);CHECK(point.rb<=h[1].rb);
      CHECK(h[0].bb<=point.bb);CHECK(point.bb<=h[1].bb);
      CHECK(cppunivariate::bnbinom_detail::likelihood(d,{r,a*u,b*v})<=bound.value+bound.margin);
    }
  }
}

TEST_CASE("bnbinom: supporting polynomials prove broader NB and Poisson faces") {
  const std::vector<int> x{0,1,2,3,4};
  for(int mirror=0;mirror<2;++mirror) {
    const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,mirror?NAN:1.,NAN,mirror?1.:NAN);
    CHECK(f.status==cppunivariate::BnbinomStatus::NegativeBinomialLimit);
    CHECK(f.stop_detail.limit_coef[0]==1);
  }
  const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()});
  CHECK(f.status==cppunivariate::BnbinomStatus::PoissonLimit);
}

TEST_CASE("bnbinom: all-free numerical candidates retain both labels and a diagonal witness") {
  const auto golden=mltest::load_distribution_golden("bnbinom");
  std::vector<int> x;for(double v:golden.data)x.push_back(static_cast<int>(v));
  const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()});
  REQUIRE(f.status==cppunivariate::BnbinomStatus::Ok);
  bool mirror=false,diagonal=false;
  for(const auto &c:f.stop_detail.candidates) {
    REQUIRE(c.coef.size()==3);
    mirror=mirror || (c.coef[0]==f.beta && c.coef[2]==f.size);
    diagonal=diagonal || (c.coef[0]==c.coef[2]);
  }
  CHECK(mirror);CHECK(diagonal);
  CHECK(f.stop_detail.loglik_upper==0);
}

TEST_CASE("bnbinom: unrepresentable held-size NB faces preserve numerical stop classification") {
  const std::vector<int> x{0,1,2,4};
  for(int mirror=0;mirror<2;++mirror) {
    const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,mirror?NAN:1e308,NAN,mirror?1e308:NAN);
    CHECK(f.status==cppunivariate::BnbinomStatus::NumericalFailure);
    CHECK_FALSE(f.stop_detail.budget_exhausted);
    CHECK(std::isnan(f.size));CHECK(std::isnan(f.alpha));CHECK(std::isnan(f.beta));
    CHECK(f.stop_detail.loglik_upper==0);
  }
}

#include "cppunivariate/distributions/bnbinom/dpqr.hpp"
TEST_CASE("bnbinom: cancelled gamma increments cannot stall a quantile") {
  CHECK(std::isnan(cppunivariate::distributions::qbnbinom(.5,1,1e308,1,true,false)));
}

TEST_CASE("bnbinom: Bernstein face proof handles multiple derivative sign changes") {
  // mean=13/14. After removing the known root y=1, the Poisson
  // derivative quotient is (101-29*y+13*y*y)/14. Its negative
  // discriminant proves positivity independently of the subdivision code.
  cppunivariate::bnbinom_detail::Data d{{0,1,3},{3,10,1},14};
  CHECK_FALSE(cppunivariate::bnbinom_detail::single_crossing_mixture_face(d,0,true));
  CHECK_FALSE(cppunivariate::bnbinom_detail::single_crossing_mixture_face(d,1,false));
  for(bool poisson:{false,true}) {
    const auto proof=cppunivariate::bnbinom_detail::polynomial_mixture_face(d,1,poisson,600);
    CHECK(proof.proved);CHECK_FALSE(proof.budget_exhausted);
    CHECK(proof.evaluations>4);CHECK(proof.evaluations<=cppunivariate::certificate_budget(600));
  }
  const std::vector<int> x{0,0,0,1,1,1,1,1,1,1,1,1,1,3};
  const auto all=cppunivariate::bnbinom_mle({x.data(),x.size()});
  CHECK(all.status==cppunivariate::BnbinomStatus::PoissonLimit);
  CHECK(all.stop_detail.limit_model==cppunivariate::StopModel::Pois);
  REQUIRE(all.stop_detail.limit_coef.size()==1);
  CHECK(all.stop_detail.limit_coef[0]==doctest::Approx(13./14));
  CHECK(std::isnan(all.size));CHECK(std::isnan(all.alpha));CHECK(std::isnan(all.beta));
  for(int mirror=0;mirror<2;++mirror) {
    const auto pair=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,mirror?NAN:1,NAN,mirror?1:NAN);
    CHECK(pair.status==cppunivariate::BnbinomStatus::NegativeBinomialLimit);
    CHECK(pair.stop_detail.limit_model==cppunivariate::StopModel::Nbinom);
    REQUIRE(pair.stop_detail.limit_coef.size()==2);
    CHECK(pair.stop_detail.limit_coef[0]==1);
    CHECK(pair.stop_detail.limit_coef[1]==doctest::Approx(14./27));
  }
}

TEST_CASE("bnbinom: unsuccessful Bernstein proof never implies a winning face") {
  cppunivariate::bnbinom_detail::Data over{{0,5},{2,2},4};
  for(bool poisson:{false,true}) {
    const auto proof=cppunivariate::bnbinom_detail::polynomial_mixture_face(over,1,poisson,600);
    CHECK_FALSE(proof.proved);CHECK_FALSE(proof.budget_exhausted);
  }
  // Bound dense storage before allocation; raising iterlim raises this cap.
  cppunivariate::bnbinom_detail::Data large{{0,129},{1,1},2};
  const auto proof=cppunivariate::bnbinom_detail::polynomial_mixture_face(large,1,true,1);
  CHECK_FALSE(proof.proved);CHECK(proof.budget_exhausted);
  CHECK(proof.evaluations==0);
  // An exact zero quotient endpoint is unresolved, not a strict certificate.
  cppunivariate::bnbinom_detail::Data zero{{0,1,2},{1,1,1},3};
  CHECK_FALSE(cppunivariate::bnbinom_detail::polynomial_mixture_face(zero,1,true,600).proved);
}

TEST_CASE("bnbinom: all-free proposals pass a parameter-uniform global inner certificate") {
  const auto golden=mltest::load_distribution_golden("bnbinom");
  std::vector<int> x;for(double v:golden.data)x.push_back(static_cast<int>(v));
  const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()});
  REQUIRE(f.status==cppunivariate::BnbinomStatus::Ok);
  REQUIRE(std::isfinite(f.profile_alpha[0]));
  CHECK(f.profile_alpha[0]<f.alpha);CHECK(f.alpha<f.profile_alpha[1]);
  for(double a:{f.profile_alpha[0],f.alpha,f.profile_alpha[1]}) {
    const auto inner=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,NAN,a,NAN);
    REQUIRE(inner.status==cppunivariate::BnbinomStatus::Ok);
    CHECK(f.profile_mode[0]<=inner.size);CHECK(inner.size<=f.profile_mode[1]);
    CHECK(f.profile_mode[2]<=inner.beta);CHECK(inner.beta<=f.profile_mode[3]);
  }
  cppunivariate::bnbinom_detail::Data d;d.n=x.size();
  std::map<int,double> counts;for(int v:x)counts[v]++;
  for(const auto &kv:counts){d.x.push_back(kv.first);d.count.push_back(kv.second);}
  const auto wide=cppunivariate::bnbinom_detail::alpha_interval_profile(d,{f.size,f.alpha,f.beta},.01,100,1e-8,600);
  CHECK_FALSE(wide.proved);CHECK_FALSE(wide.budget_exhausted);
  const auto budget=cppunivariate::bnbinom_detail::alpha_interval_profile(d,{f.size,f.alpha,f.beta},f.profile_alpha[0],f.profile_alpha[1],1e-8,1);
  CHECK_FALSE(budget.proved);CHECK(budget.budget_exhausted);
}

TEST_CASE("bnbinom: alpha-slab mass and Hessian bounds contain every sampled interior") {
  cppunivariate::bnbinom_detail::Data d{{0,1,4,15},{2,1,2,1},6};
  for(double a:{.3,2.,20.})for(double r:{.2,1.,5.})for(double b:{.2,3.,10.}) {
    const cppunivariate::bnbinom_detail::AlphaBox q{r*.99,r*1.01,b*.99,b*1.01};
    const auto bound=cppunivariate::bnbinom_detail::alpha_interval_bound(d,a*.99,a*1.01,q);
    const auto h=cppunivariate::bnbinom_detail::alpha_interval_hessian(d,a*.99,a*1.01,q);
    for(double u:{.99,1.,1.01})for(double v:{.99,1.,1.01})for(double w:{.99,1.,1.01}) {
      const auto point=cppunivariate::bnbinom_detail::alpha_hessian(d,a*u,r*v,b*w);
      CHECK(h[0].rr<=point.rr);CHECK(point.rr<=h[1].rr);
      CHECK(h[0].rb<=point.rb);CHECK(point.rb<=h[1].rb);
      CHECK(h[0].bb<=point.bb);CHECK(point.bb<=h[1].bb);
      CHECK(cppunivariate::bnbinom_detail::likelihood(d,{r*v,a*u,b*w})<=bound.value+bound.margin);
    }
  }
}

TEST_CASE("bnbinom: uniform inner gate rejects a proposal on another conditional branch") {
  cppunivariate::bnbinom_detail::Data d{{0,1,2,3,6,12,24},{2,1,1,1,1,1,1},8};
  // The independently checked global inner maximum at alpha=.5 is near
  // size=beta=1.1088. A proof about that root cannot validate (100,.5,100).
  const auto proof=cppunivariate::bnbinom_detail::alpha_interval_profile(d,{100,.5,100},.4999995,.5000005,1e-8,600);
  CHECK_FALSE(proof.proved);CHECK_FALSE(proof.budget_exhausted);
}

TEST_CASE("bnbinom: complete all-free cover encloses the independent 80-digit maximizer") {
  const auto golden=mltest::load_distribution_golden("bnbinom");
  std::vector<int> x;for(double v:golden.data)x.push_back(static_cast<int>(v));
  const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()});
  REQUIRE(f.status==cppunivariate::BnbinomStatus::Ok);
  const std::array<double,3> truth{{1.0672234392758119184,2.1419792202550637215,5.3647342962537853096}};
  const std::array<double,3> estimate{{f.size,f.alpha,f.beta}};
  for(int j=0;j<3;++j) {
    CHECK(std::abs(estimate[j]/truth[j]-1)<1e-8);
    CHECK(estimate[j]*(1-2.5e-9)<=truth[j]);
    CHECK(truth[j]<=estimate[j]*(1+2.5e-9));
  }
  CHECK(std::isfinite(f.global_interval[0]));CHECK(std::isfinite(f.global_interval[1]));
  CHECK(f.global_interval[0]<=-1275.6474863695586167);
  CHECK(f.global_interval[1]>=-1275.6474863695586167);
}

TEST_CASE("bnbinom: joint Hessian intervals agree with independently validated pair derivatives") {
  cppunivariate::bnbinom_detail::Data d{{0,1,4,15},{2,1,2,1},6};
  for(double r:{.2,1.,5.})for(double a:{.3,2.,20.})for(double b:{.2,3.,10.}) {
    const std::array<double,3> lo{{r*.99,a*.99,b*.99}},hi{{r*1.01,a*1.01,b*1.01}};
    const auto bounds=cppunivariate::bnbinom_detail::joint_hessian_bounds(d,lo,hi);
    for(double u:{.99,1.,1.01})for(double v:{.99,1.,1.01})for(double w:{.99,1.,1.01}) {
      const auto rb=cppunivariate::bnbinom_detail::alpha_hessian(d,a*v,r*u,b*w);
      const auto ab=cppunivariate::bnbinom_detail::pair_hessian(d,r*u,a*v,b*w);
      const auto ar=cppunivariate::bnbinom_detail::pair_hessian(d,b*w,a*v,r*u);
      const std::array<std::array<double,3>,3> h{{{{rb.rr,ar.rb,rb.rb}},{{ar.rb,ab.rr,ab.rb}},{{rb.rb,ab.rb,rb.bb}}}};
      for(int i=0;i<3;++i)for(int j=0;j<3;++j) {
        CHECK(bounds[0].v[i][j]<=h[i][j]);CHECK(h[i][j]<=bounds[1].v[i][j]);
      }
    }
  }
}

TEST_CASE("bnbinom: joint Newton reduction preserves a known stationary root") {
  const auto golden=mltest::load_distribution_golden("bnbinom");
  cppunivariate::bnbinom_detail::Data d;d.n=golden.data.size();std::map<int,double> counts;
  for(double v:golden.data)counts[static_cast<int>(v)]++;
  for(const auto &kv:counts){d.x.push_back(kv.first);d.count.push_back(kv.second);}
  const std::array<double,3> p{{1.0672234392758119184,2.1419792202550637215,5.3647342962537853096}};
  std::array<double,3> lo{},hi{};for(int j=0;j<3;++j){lo[j]=p[j]*.99999;hi[j]=p[j]*1.00002;}
  CHECK_FALSE(cppunivariate::bnbinom_detail::joint_reduce(d,lo,hi));
  for(int j=0;j<3;++j){CHECK(lo[j]<=p[j]);CHECK(p[j]<=hi[j]);}
  const auto budget=cppunivariate::bnbinom_detail::joint_cover(d,{p[0],p[1],p[2]},-1303.7427190771177,-3336.7458743346015,1e-8,1);
  CHECK_FALSE(budget.proved);CHECK(budget.budget_exhausted);
  for(double v:budget.coef)CHECK(std::isnan(v));
}

TEST_CASE("bnbinom: unresolved all-free faces retain legacy limits with face witnesses") {
  const std::vector<int> x{0,0,5,5};
  const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()});
  CHECK(f.status==cppunivariate::BnbinomStatus::NegativeBinomialLimit);
  CHECK_FALSE(f.stop_detail.budget_exhausted);
  CHECK(std::isnan(f.size));CHECK(std::isnan(f.alpha));CHECK(std::isnan(f.beta));
  CHECK(f.stop_detail.limit_model==cppunivariate::StopModel::Nbinom);
  CHECK_FALSE(f.stop_detail.candidates.empty());
  CHECK(f.stop_detail.loglik_lower<=f.stop_detail.loglik_upper);
}

TEST_CASE("bnbinom: NB face height includes delegated size-root uncertainty") {
  const auto golden=mltest::load_distribution_golden("bnbinom");
  std::vector<int> x;std::map<int,double> counts;
  for(double v:golden.data){x.push_back(static_cast<int>(v));counts[static_cast<int>(v)]++;}
  cppunivariate::bnbinom_detail::Data d;d.n=x.size();
  for(const auto &kv:counts){d.x.push_back(kv.first);d.count.push_back(kv.second);}
  const auto nb=cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,600);
  REQUIRE(nb.status==cppunivariate::NbinomStatus::Ok);
  const auto bound=cppunivariate::bnbinom_detail::joint_nb_profile_bound(d,nb.size);
  CHECK(std::isfinite(bound.value+bound.margin));
  CHECK(bound.value-bound.margin<=-1303.7427190771177);
  CHECK(bound.value+bound.margin>=-1303.7427190771177);
}

TEST_CASE("bnbinom: delegated NB precision failures retain correctly typed witnesses") {
  std::vector<int> x;
  for(const auto &p:std::array<std::array<int,2>,7>{{{{969,3182}},{{970,1816}},{{1001,1}},{{1003,1}},{{1004,1}},{{1032,1785}},{{1033,3215}}}})
    x.insert(x.end(),p[1],p[0]);
  long long sum=0,squares=0;
  for(int v:x){sum+=v;squares+=static_cast<long long>(v)*v;}
  const long long n=static_cast<long long>(x.size());
  REQUIRE(n*squares-sum*sum-n*sum==1);
  const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()});
  CHECK(f.status==cppunivariate::BnbinomStatus::NumericalFailure);
  CHECK(std::isnan(f.size));CHECK(std::isnan(f.alpha));CHECK(std::isnan(f.beta));
  REQUIRE_FALSE(f.stop_detail.candidates.empty());
  for(const auto &c:f.stop_detail.candidates) {
    CHECK(c.model==cppunivariate::StopModel::Nbinom);
    REQUIRE(c.coef.size()==2);
    CHECK(c.coef[0]>0);CHECK(c.coef[1]>0);CHECK(c.coef[1]<1);
  }
}

TEST_CASE("bnbinom: equal frequencies through four have a compound Poisson certificate") {
  const cppunivariate::bnbinom_detail::Data d{{0,1,2,3,4},{7,7,7,7,7},35};
  CHECK(cppunivariate::bnbinom_detail::compound_poisson_uniform_four_face(d));
  CHECK_FALSE(cppunivariate::bnbinom_detail::mixture_face(d,0,true,600));
  auto unequal=d;unequal.count[0]+=1;unequal.n+=1;
  CHECK_FALSE(cppunivariate::bnbinom_detail::compound_poisson_uniform_four_face(unequal));
  auto shifted=d;for(auto &v:shifted.x)++v;
  CHECK_FALSE(cppunivariate::bnbinom_detail::compound_poisson_uniform_four_face(shifted));
  std::vector<int> x;for(int j=0;j<5;++j)for(int k=0;k<7;++k)x.push_back(j);
  const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()});
  CHECK(f.status==cppunivariate::BnbinomStatus::PoissonLimit);
  REQUIRE(f.stop_detail.limit_coef.size()==1);
  CHECK(f.stop_detail.limit_coef[0]==2);
}

TEST_CASE("bnbinom: all-free decisions retain explicit certification flags") {
  for(int repeats:{2,20}) {
    std::vector<int> x(repeats,0);x.insert(x.end(),repeats,5);
    const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()});
    CHECK(f.status==cppunivariate::BnbinomStatus::NegativeBinomialLimit);
    CHECK(f.certified);CHECK_FALSE(f.stop_detail.budget_exhausted);
    CHECK(f.stop_detail.limit_model==cppunivariate::StopModel::Nbinom);
  }
  for(const std::vector<int> &x:{std::vector<int>{0,1,2,3,4},std::vector<int>{0,0,0,1,1,1,1,1,1,1,1,1,1,3}}) {
    const auto f=cppunivariate::bnbinom_mle({x.data(),x.size()});
    CHECK(f.status==cppunivariate::BnbinomStatus::PoissonLimit);CHECK(f.certified);
  }
}

TEST_CASE("bnbinom: insufficient certificate work preserves a successful finite legacy search") {
  const auto golden=mltest::load_distribution_golden("bnbinom");
  std::vector<int> x;for(double v:golden.data)x.push_back(static_cast<int>(v));
  const auto legacy=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,200);
  REQUIRE(legacy.status==cppunivariate::BnbinomStatus::Ok);
  CHECK_FALSE(legacy.certified);CHECK_FALSE(legacy.stop_detail.budget_exhausted);
  CHECK(std::isnan(legacy.global_interval[0]));
  CHECK(legacy.size==doctest::Approx(1.0672234787479615).epsilon(2e-6));
  CHECK(legacy.alpha==doctest::Approx(2.1419791546447171).epsilon(2e-6));
  CHECK(legacy.beta==doctest::Approx(5.3647338351087397).epsilon(2e-6));
  const auto verified=cppunivariate::bnbinom_mle({x.data(),x.size()});
  REQUIRE(verified.status==cppunivariate::BnbinomStatus::Ok);
  CHECK(verified.certified);CHECK(std::isfinite(verified.global_interval[0]));
  CHECK(verified.logLik>=legacy.logLik-1e-8);
}

TEST_CASE("bnbinom: an unfinished joint cover retains its proved finite face advantage") {
  const auto golden=mltest::load_distribution_golden("bnbinom");
  cppunivariate::bnbinom_detail::Data d;d.n=golden.data.size();std::map<int,double> counts;
  for(double v:golden.data)counts[static_cast<int>(v)]++;
  for(const auto &kv:counts){d.x.push_back(kv.first);d.count.push_back(kv.second);}
  const auto proof=cppunivariate::bnbinom_detail::joint_cover(d,
    {1.0672234787479615,2.1419791546447171,5.3647338351087397},
    -1303.7427190771177,-3336.7458743346015,1e-8,1);
  CHECK_FALSE(proof.proved);CHECK(proof.budget_exhausted);
  for(double v:proof.coef)CHECK(std::isnan(v));
  CHECK(proof.loglik_lower>-1303.7427190771177);
  CHECK(proof.loglik_lower<-1275.6474863695586167);
  const auto &p=proof.witness_coef;
  for(double v:p)CHECK(std::isfinite(v));
  CHECK(cppunivariate::bnbinom_detail::likelihood(d,{p[0],p[1],p[2]})>=proof.loglik_lower);
}

TEST_CASE("bnbinom: signed concentration bounds enclose the NB likelihood difference") {
  const cppunivariate::bnbinom_detail::Data d{{0,1,4,15},{2,1,2,1},6};
  for(double r:{.2,1.,5.})for(double p:{.1,.5,.9}) {
    const auto bound=cppunivariate::bnbinom_detail::signed_nb_bound(d,r*.99,r*1.01,p*.99,p*1.01);
    for(double u:{.99,1.,1.01})for(double v:{.99,1.,1.01})for(double s:{10.,100.,1000.}) {
      const double rr=r*u,pp=p*v;
      double nb=0;
      for(std::size_t i=0;i<d.x.size();++i)nb+=d.count[i]*cppunivariate::special::dnbinom(d.x[i],rr,pp,true);
      const double bnb=cppunivariate::bnbinom_detail::likelihood(d,{rr,s*pp,s*(1-pp)});
      CHECK(bnb-nb<=bound.coefficient/s+bound.remainder/(s*s)+1e-8);
    }
  }
}

TEST_CASE("bnbinom: signed mixed Poisson bound holds away from and near the face") {
  const cppunivariate::bnbinom_detail::Data d{{0,1,4,15},{2,1,2,1},6};
  for(double r:{.2,2.,20.,200.})for(double b:{.3,3.,30.,300.})for(double lambda:{.1,2.,10.}) {
    double po=0,B=0;
    for(std::size_t i=0;i<d.x.size();++i) {
      const double x=d.x[i];
      po+=d.count[i]*(-lambda+x*std::log(lambda)-cppunivariate::special::lgamma(x+1));
      B+=d.count[i]*((x-lambda)*(x-lambda)-x)/2;
    }
    const double h=1/r+1/b,R=cppunivariate::bnbinom_detail::signed_poisson_remainder(d,lambda,h);
    const double ll=cppunivariate::bnbinom_detail::likelihood(d,{r,r*b/lambda,b});
    CHECK(ll<=po+B*h+R*h*h+1e-8);
  }
}

TEST_CASE("bnbinom: signed global cover certifies NB and Poisson beyond mixture supports") {
  const cppunivariate::bnbinom_detail::Data not_supported{{0,2},{1,3},4};
  CHECK_FALSE(cppunivariate::bnbinom_detail::mixture_face(not_supported,0,true));
  for(const std::vector<int> &x:{std::vector<int>{0,0,5,5},std::vector<int>{0,2,2,2}}) {
    const auto fit=cppunivariate::bnbinom_mle({x.data(),x.size()});
    CHECK(fit.certified);
    CHECK(fit.status==(x.back()==5?cppunivariate::BnbinomStatus::NegativeBinomialLimit:cppunivariate::BnbinomStatus::PoissonLimit));
    CHECK_FALSE(fit.stop_detail.budget_exhausted);
    CHECK(fit.stop_detail.loglik_lower<=fit.stop_detail.loglik_upper);
  }
  const std::vector<int> x{0,0,5,5};
  const auto fallback=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,100);
  CHECK(fallback.status==cppunivariate::BnbinomStatus::NegativeBinomialLimit);
  CHECK_FALSE(fallback.certified);CHECK_FALSE(fallback.stop_detail.budget_exhausted);
  const cppunivariate::bnbinom_detail::Data equal_dispersion{{0,2},{1,1},2};
  const auto unresolved=cppunivariate::bnbinom_detail::signed_face_cover(equal_dispersion,NAN,-2.,-2.,600);
  CHECK_FALSE(unresolved.proved);CHECK_FALSE(unresolved.budget_exhausted);
}
