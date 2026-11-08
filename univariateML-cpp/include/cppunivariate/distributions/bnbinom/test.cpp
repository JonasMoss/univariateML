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
