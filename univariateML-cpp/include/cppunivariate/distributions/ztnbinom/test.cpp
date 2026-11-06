#include "test_support.hpp"
#include "cppunivariate/distributions/ztnbinom/mle.hpp"
#include "cppunivariate/distributions/ztnbinom/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <cmath>
TEST_CASE("ztnbinom: golden, score and distribution") {
 const auto g=mltest::load_distribution_golden("ztnbinom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const auto fit=cppunivariate::ztnbinom_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZtnbinomStatus::Ok);
 CHECK(fit.size==doctest::Approx(g.estimates.at("size")).epsilon(1e-10));
 CHECK(fit.prob==doctest::Approx(g.estimates.at("prob")).epsilon(1e-10));
 CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
 std::vector<double> p{fit.size,fit.prob};
 auto ll=[&](const std::vector<double> &par){double l=0;for(int v:x)l+=cppunivariate::distributions::dztnbinom(v,par[0],par[1],true);return l;};
 CHECK(ll(p)==doctest::Approx(fit.logLik).epsilon(1e-10));
 double norm2=0;
 {auto a=p,b=p;const double h=1e-5;a[0]+=h;b[0]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 {auto a=p,b=p;const double h=1e-5;a[1]+=h;b[1]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 CHECK(std::sqrt(norm2)<1e-3);
 const auto fixed=cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,2.5);
 CHECK(fixed.status==cppunivariate::ZtnbinomStatus::Ok);
 CHECK(fixed.size==2.5);
 CHECK(cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,-1).status==cppunivariate::ZtnbinomStatus::InvalidSize);
 CHECK(cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,std::numeric_limits<double>::infinity()).status==cppunivariate::ZtnbinomStatus::InvalidSize);
}
TEST_CASE("ztnbinom: attained boundaries and invalid input") {
 std::vector<int> ones{1,1,1},zeros{0,0,0},negative{-1,1};
 CHECK(cppunivariate::ztnbinom_mle({}).status==cppunivariate::ZtnbinomStatus::InvalidInput);
 CHECK(cppunivariate::ztnbinom_mle({negative.data(),negative.size()}).status==cppunivariate::ZtnbinomStatus::InvalidInput);
 const auto one=cppunivariate::ztnbinom_mle({ones.data(),ones.size()});
 CHECK(one.logLik==0);
 CHECK(one.status==cppunivariate::ZtnbinomStatus::Ok);
 CHECK(cppunivariate::ztnbinom_mle({zeros.data(),zeros.size()}).status==cppunivariate::ZtnbinomStatus::InvalidInput);
}
TEST_CASE("ztnbinom: scalar tails and quantile endpoints") {
 const auto g=mltest::load_distribution_golden("ztnbinom");
 CHECK(std::isfinite(cppunivariate::distributions::dztnbinom(10000,g.estimates.at("size"),g.estimates.at("prob"),true)));
 CHECK(cppunivariate::distributions::qztnbinom(0,g.estimates.at("size"),g.estimates.at("prob"))==1);
 CHECK(cppunivariate::distributions::qztnbinom(1,g.estimates.at("size"),g.estimates.at("prob"))==std::numeric_limits<double>::infinity());
}

TEST_CASE("ztnbinom: infinite-size escape and fixed-size removal") {
 std::vector<int> x{1,2,2,3,3,3};
 CHECK(cppunivariate::ztnbinom_mle({x.data(),x.size()}).status==cppunivariate::ZtnbinomStatus::PoissonLimit);
 CHECK(cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,20).status==cppunivariate::ZtnbinomStatus::Ok);
}

TEST_CASE("ztnbinom: attained logarithmic face") {
 std::vector<int> x{1,1,1,1,1,2,3,10,30};
 const auto fit=cppunivariate::ztnbinom_mle({x.data(),x.size()});
 CHECK(fit.status==cppunivariate::ZtnbinomStatus::Ok);
 CHECK(fit.size==0);
 const auto fixed=cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,0);
 CHECK(fixed.size==0);
 CHECK(fixed.logLik==doctest::Approx(fit.logLik));
}

TEST_CASE("ztnbinom: quantile inversion and sampling") {
 const auto g=mltest::load_distribution_golden("ztnbinom");
 const double q=cppunivariate::distributions::qztnbinom(.37,g.estimates.at("size"),g.estimates.at("prob"));
 CHECK(cppunivariate::distributions::pztnbinom(q,g.estimates.at("size"),g.estimates.at("prob"))>=.37);
 CHECK(cppunivariate::distributions::pztnbinom(q-1,g.estimates.at("size"),g.estimates.at("prob"))<.37);
 CHECK(cppunivariate::distributions::pztnbinom(q,g.estimates.at("size"),g.estimates.at("prob"),true,false)+cppunivariate::distributions::pztnbinom(q,g.estimates.at("size"),g.estimates.at("prob"),false,false)==doctest::Approx(1));
 cppunivariate::distributions::StdRng<> rng(19);
 for(int j=0;j<20;++j) {const double v=cppunivariate::distributions::rztnbinom(rng,g.estimates.at("size"),g.estimates.at("prob"));CHECK(v>=1);CHECK(v==std::floor(v));}
}

// Independent high-precision oracle; see quality/truth.py.
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("ztnbinom: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("ztnbinom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dztnbinom(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pztnbinom(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qztnbinom(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rztnbinom(rng, p[0], p[1]); });
}

TEST_CASE("ztnbinom: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("ztnbinom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<4;++mask) {
   const auto r=cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,(mask & 1)?g.estimates.at("size"):nan,(mask & 2)?g.estimates.at("prob"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.size==g.estimates.at("size"));
   if(mask & 2)CHECK(r.prob==g.estimates.at("prob"));
 }
}

TEST_CASE("ztnbinom: P2 held-domain and non-existence statuses") {
 const std::vector<int> x{1,2,5},one{1,1};
 const double nan=std::numeric_limits<double>::quiet_NaN();
 CHECK(cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,nan,1).status==cppunivariate::ZtnbinomStatus::InvalidInput);
 const auto r=cppunivariate::ztnbinom_mle({one.data(),one.size()},1e-10,200,nan,1);
 CHECK(r.prob==1);CHECK(r.logLik==0);
 CHECK(cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,1,nan,.4).status==cppunivariate::ZtnbinomStatus::IterationFailure);
}


TEST_CASE("ztnbinom: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // s=mpf(2.0); p=mpf(0.4)
  // d=lambda k:gamma(k+s)/(gamma(s)*gamma(k+1))*p**s*(1-p)**k/(1-p**s); S=lambda k:betainc(k+1,s,0,1-p,regularized=True)/(1-p**s)
  // print(log(d(mpf(1e+100)))); print(log(S(mpf(135.0))))
  // lo,hi=0,1
  // while log(S(hi))>-10000: hi*=2
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::dztnbinom(1e+100, 2.0, 0.4, true) == doctest::Approx(-5.108256237659907e+99).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pztnbinom(135.0, 2.0, 0.4, false, true) == doctest::Approx(-65.28335185127672).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qztnbinom(-10000, 2.0, 0.4, false, true) == 19594.0);
  CHECK(cppunivariate::distributions::dztnbinom(0.5, 2.0, 0.4, false) == 0);
}

TEST_CASE("ztnbinom: tiny conditional lower tail") {
  // from mpmath import *; mp.dps=100; print(log(20)+20*log(mpf(0.4))+log(1-mpf(0.4))-log(1-mpf(0.4)**20))
  CHECK(cppunivariate::distributions::pztnbinom(1,20.0,0.4,true,true)==doctest::Approx(-15.840907976699983).epsilon(1e-10));
}

// Size-one NB conditioned positive is geometric: S(k)=(1-prob)^k.
TEST_CASE("ztnbinom: geometric member allows adjacent answers at jumps") {
  const auto adjacent=[](double x,double lo) {return x==lo || x==lo+1;};
  CHECK(cppunivariate::distributions::qztnbinom(.83,1,.5,true,false)==3);
  CHECK(cppunivariate::distributions::qztnbinom(.17,1,.5,false,false)==3);
  CHECK(adjacent(cppunivariate::distributions::qztnbinom(-1.3862943611198906,1,.5,false,true),2));
  CHECK(adjacent(cppunivariate::distributions::qztnbinom(.25,1,.5,false,false),2));
  CHECK(adjacent(cppunivariate::distributions::qztnbinom(.5,1,.5,true,false),1));
  CHECK(cppunivariate::distributions::pztnbinom(2,1,.5,false,false)==doctest::Approx(.25).epsilon(1e-14));
}

TEST_CASE("ztnbinom: global certificate preserves the historical proposer") {
 const auto g=mltest::load_distribution_golden("ztnbinom");
 std::vector<int>x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const auto old=cppunivariate::ztnbinom_candidate_mle({x.data(),x.size()});
 const auto fit=cppunivariate::ztnbinom_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZtnbinomStatus::Ok);
 CHECK(fit.size==old.size);CHECK(fit.prob==old.prob);CHECK(fit.logLik==old.logLik);
 CHECK(fit.iterations>0);
}

TEST_CASE("ztnbinom: certified limit and exhausted budget carry details") {
 const std::vector<int>x{1,2,2,3,3,3},finite{1,2,5},large{100001};
 const auto fit=cppunivariate::ztnbinom_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZtnbinomStatus::PoissonLimit);
 REQUIRE(fit.stop_detail.limit_coef.size()==1);
 const auto face=cppunivariate::ztpois_mle({x.data(),x.size()});
 CHECK(fit.stop_detail.limit_coef[0]==face.lambda);
 CHECK(fit.stop_detail.limit_loglik==face.logLik);
 CHECK(fit.stop_detail.loglik_lower<=face.logLik);
 CHECK(fit.stop_detail.loglik_upper>=face.logLik);
 const auto fail=cppunivariate::ztnbinom_mle({finite.data(),finite.size()},1e-10,1);
 CHECK(fail.status==cppunivariate::ZtnbinomStatus::IterationFailure);
 CHECK(fail.budget_exhausted);CHECK(std::isnan(fail.size));CHECK(std::isnan(fail.prob));
 CHECK(fail.stop_detail.loglik_upper==0);
 CHECK(cppunivariate::ztnbinom_mle({large.data(),large.size()}).status==cppunivariate::ZtnbinomStatus::IterationFailure);
}

TEST_CASE("ztnbinom: held probability has a unique signed size solution") {
 const std::vector<int>x{1,2,5};
 const auto fit=cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,NAN,.4);
 REQUIRE(fit.status==cppunivariate::ZtnbinomStatus::Ok);
 auto score=[&](long double r) {
   long double out=0,t=-std::log(.4L);
   for(int v:x) {for(int j=0;j<v;++j)out+=1/(r+j);out-=t/(-std::expm1(-r*t));}
   return out;
 };
 CHECK(score(fit.size*(1-3e-6))>0);CHECK(score(fit.size*(1+3e-6))<0);
 const auto endpoint=cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,NAN,.001);
 REQUIRE(endpoint.status==cppunivariate::ZtnbinomStatus::Ok);CHECK(endpoint.size==0);
 CHECK(endpoint.prob==.001);
}

TEST_CASE("ztnbinom: near Poisson finite maximum is never labelled an escape") {
 // mean=2, K=1.5937 > mean*lambda/2=1.59362426004... .
 // The proved tail coefficient is negative, hence some finite likelihood
 // exceeds the Poisson face. A resolution/budget stop remains permissible.
 std::vector<int>x(3958,1);x.insert(x.end(),4063,2);x.insert(x.end(),1979,4);
 const auto fit=cppunivariate::ztnbinom_mle({x.data(),x.size()});
 CHECK(fit.status!=cppunivariate::ZtnbinomStatus::PoissonLimit);
 CHECK((fit.status==cppunivariate::ZtnbinomStatus::Ok || fit.status==cppunivariate::ZtnbinomStatus::IterationFailure));
}

TEST_CASE("ztnbinom: invalid domains precede the certificate workload guard") {
 const std::vector<int>x{1,100001},bad{-1,100001};
 CHECK(cppunivariate::ztnbinom_mle({x.data(),x.size()},0).status==cppunivariate::ZtnbinomStatus::InvalidInput);
 CHECK(cppunivariate::ztnbinom_mle({bad.data(),bad.size()}).status==cppunivariate::ZtnbinomStatus::InvalidInput);
 CHECK(cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,-1).status==cppunivariate::ZtnbinomStatus::InvalidSize);
 CHECK(cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,NAN,1).status==cppunivariate::ZtnbinomStatus::InvalidInput);
}

TEST_CASE("ztnbinom: unresolved large-count Poisson-tail comparison stops") {
 // Mean and population variance are both 313^2. The truncated Poisson
 // rate is smaller than the mean by an exponentially tiny amount, so its
 // tail coefficient is negative but unresolvable in binary64.
 const std::vector<int>x{97656,98282};
 const auto fit=cppunivariate::ztnbinom_mle({x.data(),x.size()});
 CHECK(fit.status==cppunivariate::ZtnbinomStatus::IterationFailure);
 CHECK(fit.certificate_failure==cppunivariate::ZtnbinomCertificateFailure::Arithmetic);
 CHECK(std::isnan(fit.size));CHECK(std::isnan(fit.prob));
}
