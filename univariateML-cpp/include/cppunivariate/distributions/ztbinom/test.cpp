#include "test_support.hpp"
#include "cppunivariate/distributions/ztbinom/mle.hpp"
#include "cppunivariate/distributions/ztbinom/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <cmath>
TEST_CASE("ztbinom: golden, score and distribution") {
 const auto g=mltest::load_distribution_golden("ztbinom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const auto fit=cppunivariate::ztbinom_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZtbinomStatus::Ok);
 CHECK(fit.size==doctest::Approx(g.estimates.at("size")).epsilon(mltest::kGolden_ztbinom).scale(0));
 CHECK(fit.prob==doctest::Approx(g.estimates.at("prob")).epsilon(mltest::kGolden_ztbinom).scale(0));
 CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_ztbinom).scale(0));
 std::vector<double> p{fit.size,fit.prob};
 auto ll=[&](const std::vector<double> &par){double l=0;for(int v:x)l+=cppunivariate::distributions::dztbinom(v,par[0],par[1],true);return l;};
 CHECK(ll(p)==doctest::Approx(fit.logLik).epsilon(1e-10));
 double norm2=0;
 {auto a=p,b=p;const double h=1e-5;a[1]+=h;b[1]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 CHECK(std::sqrt(norm2)<1e-3);
 const auto fixed=cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,200,15);
 CHECK(fixed.status==cppunivariate::ZtbinomStatus::Ok);
 CHECK(fixed.size==15);
 CHECK(cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,200,-1).status==cppunivariate::ZtbinomStatus::InvalidSize);
 CHECK(cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,200,std::numeric_limits<double>::infinity()).status==cppunivariate::ZtbinomStatus::InvalidSize);
 CHECK(cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,200,0.5).status==cppunivariate::ZtbinomStatus::InvalidSize);
 CHECK(cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,200,0).status==cppunivariate::ZtbinomStatus::InvalidSize);
}
TEST_CASE("ztbinom: attained boundaries and invalid input") {
 std::vector<int> ones{1,1,1},zeros{0,0,0},negative{-1,1};
 CHECK(cppunivariate::ztbinom_mle({}).status==cppunivariate::ZtbinomStatus::InvalidInput);
 CHECK(cppunivariate::ztbinom_mle({negative.data(),negative.size()}).status==cppunivariate::ZtbinomStatus::InvalidInput);
 const auto one=cppunivariate::ztbinom_mle({ones.data(),ones.size()});
 CHECK(one.logLik==0);
 CHECK(one.status==cppunivariate::ZtbinomStatus::Ok);
 CHECK(cppunivariate::ztbinom_mle({zeros.data(),zeros.size()}).status==cppunivariate::ZtbinomStatus::InvalidInput);
}
TEST_CASE("ztbinom: scalar tails and quantile endpoints") {
 const auto g=mltest::load_distribution_golden("ztbinom");
 CHECK(std::isfinite(cppunivariate::distributions::dztbinom(4,g.estimates.at("size"),g.estimates.at("prob"),true)));
 CHECK(cppunivariate::distributions::qztbinom(0,g.estimates.at("size"),g.estimates.at("prob"))==1);
 CHECK(cppunivariate::distributions::qztbinom(1,g.estimates.at("size"),g.estimates.at("prob"))==g.estimates.at("size"));
}

TEST_CASE("ztbinom: infinite-size escape and fixed-size removal") {
 std::vector<int> x{1,1,1,2,3,7,12};
 CHECK(cppunivariate::ztbinom_mle({x.data(),x.size()}).status==cppunivariate::ZtbinomStatus::PoissonLimit);
 CHECK(cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,200,20).status==cppunivariate::ZtbinomStatus::Ok);
}

TEST_CASE("ztbinom: quantile inversion and sampling") {
 const auto g=mltest::load_distribution_golden("ztbinom");
 const double q=cppunivariate::distributions::qztbinom(.37,g.estimates.at("size"),g.estimates.at("prob"));
 CHECK(cppunivariate::distributions::pztbinom(q,g.estimates.at("size"),g.estimates.at("prob"))>=.37);
 CHECK(cppunivariate::distributions::pztbinom(q-1,g.estimates.at("size"),g.estimates.at("prob"))<.37);
 CHECK(cppunivariate::distributions::pztbinom(q,g.estimates.at("size"),g.estimates.at("prob"),true,false)+cppunivariate::distributions::pztbinom(q,g.estimates.at("size"),g.estimates.at("prob"),false,false)==doctest::Approx(1));
 cppunivariate::distributions::StdRng<> rng(19);
 for(int j=0;j<20;++j) {const double v=cppunivariate::distributions::rztbinom(rng,g.estimates.at("size"),g.estimates.at("prob"));CHECK(v>=1);CHECK(v==std::floor(v));}
}

// Independent high-precision oracle; see quality/truth.py.
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("ztbinom: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("ztbinom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dztbinom(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pztbinom(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qztbinom(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rztbinom(rng, p[0], p[1]); });
}

TEST_CASE("ztbinom: held probability with free and held size") {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 const std::vector<int> x{1,2,2,3,4};
 const auto f=cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,200,nan,.4);
 REQUIRE(f.status==cppunivariate::ZtbinomStatus::Ok);
 CHECK(f.prob==.4);
 for(int k=4;k<100;++k) {
  const auto o=cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,200,k,.4);
  CHECK(f.logLik>=o.logLik-1e-9);
 }
 CHECK(cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,200,8,-.1).status==cppunivariate::ZtbinomStatus::InvalidInput);
}


TEST_CASE("ztbinom: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // n=mpf(8.0); p=mpf(0.4)
  // d=lambda k:binomial(n,k)*p**k*(1-p)**(n-k)/(1-(1-p)**n); S=lambda k:fsum(d(j) for j in range(int(k)+1,int(n)+1))
  // print(log(d(mpf(4.0)))); print(log(S(mpf(4.0))))
  // lo,hi=0,8
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::dztbinom(4.0, 8.0, 0.4, true) == doctest::Approx(-1.4430313653876057).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pztbinom(4.0, 8.0, 0.4, false, true) == doctest::Approx(-1.7336572138833999).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qztbinom(-10000, 8.0, 0.4, false, true) == 8.0);
  CHECK(cppunivariate::distributions::dztbinom(0.5, 8.0, 0.4, false) == 0);
}

TEST_CASE("ztbinom: tiny conditional lower tail") {
  // from mpmath import *; mp.dps=100; print(log(8)+log(mpf(0.94))+7*log(1-mpf(0.94))-log(1-(1-mpf(0.94))**8))
  CHECK(cppunivariate::distributions::pztbinom(1,8.0,0.94,true,true)==doctest::Approx(-17.67630887919054).epsilon(1e-10));
}

TEST_CASE("ztbinom: unit probability is a point mass at size") {
  // from mpmath import *; mp.dps=100
  // n=5; p=mpf(1); print(fsum(binomial(n,k)*p**k*(1-p)**(n-k) for k in range(1,5)))
  CHECK(cppunivariate::distributions::pztbinom(4,5,1,true,false)==0);
  CHECK(cppunivariate::distributions::pztbinom(4,5,1,false,true)==0);
  CHECK(cppunivariate::distributions::qztbinom(0.3,5,1,true,false)==5);
}

TEST_CASE("ztbinom: P3 unfinished optimizer is a failure") {
  const std::vector<int> values{1,2,3,3,5};
  const cppunivariate::ispan ix{values.data(),values.size()};
  CHECK(cppunivariate::ztbinom_mle(ix,1e-8,1).status==cppunivariate::ZtbinomStatus::IterationFailure);
}

// TASK-85/87: the free size+prob search is an exponential grid widened
// to grow with the iteration budget rather than a hardcoded exponent
// (see mle.hpp); this adds the n=20/100, ties and near-Poisson cases
// the task calls out, cross-checked against dztbinom (a separate code
// path from the fitter) and a coarse brute-force grid. test.R adds the
// fully independent reference-density multistart oracle.
TEST_CASE("ztbinom: free size oracle at n=20, n=100, ties, and near-Poisson") {
  cppunivariate::distributions::StdRng<> rng(85087);
  auto sample=[&](int n,double size,double prob) {
    std::vector<int> x;
    for(int i=0;i<n;++i) x.push_back(static_cast<int>(cppunivariate::distributions::rztbinom(rng,size,prob)));
    return x;
  };
  std::vector<std::vector<int>> samples{
    sample(20,8,.4), sample(100,8,.4),
    {2,2,2,2,3,4,5,5},
    sample(100,500,.02),  // near-Poisson: large size, small prob
  };
  for (auto &x : samples) {
    const auto f=cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,1000);
    REQUIRE((f.status==cppunivariate::ZtbinomStatus::Ok || f.status==cppunivariate::ZtbinomStatus::PoissonLimit));
    double ref=0; for(int v:x) ref+=cppunivariate::distributions::dztbinom(v,f.size,f.prob,true);
    CHECK(f.logLik==doctest::Approx(ref).epsilon(1e-8));
    if (f.status==cppunivariate::ZtbinomStatus::Ok) {
      const int mx=*std::max_element(x.begin(),x.end());
      for (double factor : {0.25,0.5,2.0,4.0}) {
        const double alt_size=std::max<double>(mx,std::floor(f.size*factor));
        const auto alt=cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,1000,alt_size);
        REQUIRE(alt.status==cppunivariate::ZtbinomStatus::Ok);
        CHECK(f.logLik+1e-6>=alt.logLik);
      }
    }
  }
}

TEST_CASE("ztbinom attained point sampler ignores zero uniform") {
  // At success probability one the (possibly truncated) law is delta_size.
  struct ZeroRng { double runif(double,double) { return 0; } } rng;
  CHECK(cppunivariate::distributions::rztbinom(rng,5,1)==5);
}

#include "../ztpois/dpqr.hpp"

TEST_CASE("ztbinom: structured stop payloads") {
  const std::vector<int> x{1,1,1,2,3,7,12};
  const auto r=cppunivariate::ztbinom_mle({x.data(),x.size()});
  REQUIRE(r.stop_detail.limit_model==cppunivariate::StopModel::Ztpois);
  REQUIRE(r.stop_detail.limit_coef.size()==1);
  double ll=0;for(int v:x)ll+=cppunivariate::distributions::dztpois(v,r.stop_detail.limit_coef[0],true);
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(ll));
}

TEST_CASE("ztbinom: retained-support certificate bounds the entire tail") {
  const std::vector<int> x{1,2,2,3,3,3,4,4,5,5};
  const cppunivariate::ispan sample{x.data(),x.size()};
  const auto f=cppunivariate::ztbinom_mle(sample);
  REQUIRE(f.status==cppunivariate::ZtbinomStatus::Ok);
  cppunivariate::binomial_certificate_detail::Work work{0,0,200};
  const double end=cppunivariate::binomial_certificate_detail::cutoff(sample,1,INFINITY,false,NAN,f.logLik,work);
  REQUIRE(std::isfinite(end));
  for(double N : {end,end*2,end*100}) {
    const auto g=cppunivariate::ztbinom_mle(sample,1e-10,200,N);
    CHECK(g.logLik<f.logLik);
  }
  for(int N=5;N<end;++N)CHECK(cppunivariate::ztbinom_mle(sample,1e-10,200,N).logLik<=f.logLik+1e-9);
}
TEST_CASE("ztbinom: signed certificate proves its Poisson witness wins") {
  const std::vector<int>x{1,1,1,2,3,7,12};
  const auto f=cppunivariate::ztbinom_mle({x.data(),x.size()});
  CHECK_FALSE(f.stop_detail.comparison_tie);
  CHECK(std::isfinite(f.stop_detail.loglik_upper));
  CHECK(f.stop_detail.candidates.size()>=2);
  // Size one leaves probability unidentified: distinct finite maximizers.
  CHECK(cppunivariate::ztbinom_mle({x.data(),1},1e-10,200,1,.2).logLik==0);
  CHECK(cppunivariate::ztbinom_mle({x.data(),1},1e-10,200,1,.8).logLik==0);
}

TEST_CASE("ztbinom: convex correction covers finite size blocks") {
  const std::vector<int>x{1,2,2,3,4};
  const cppunivariate::ispan sample{x.data(),x.size()};
  for(double A : {4.,8.,32.}) {
    cppunivariate::binomial_certificate_detail::Work work{0,0,200};
    const double B=2*A;
    const double upper=cppunivariate::ztbinom_mle(sample,1e-10,200,A).logLik+
      cppunivariate::binomial_certificate_detail::block_error(sample,A,B,work);
    for(double N=A;N<=B;++N)CHECK(cppunivariate::ztbinom_mle(sample,1e-10,200,N).logLik<=upper+1e-10);
  }
}

TEST_CASE("ztbinom: an unresolved tiny probability never becomes a successful fit") {
  const std::vector<int>x{1,2,3};
  for(const auto& controls : {std::pair<double,int>{1e12,40},{1e100,200}}) {
    const auto f=cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,controls.second,controls.first);
    CHECK(f.status==cppunivariate::ZtbinomStatus::IterationFailure);
    CHECK_FALSE(f.stop_detail.budget_exhausted);
    CHECK(f.stop_detail.loglik_upper==INFINITY);
    REQUIRE(f.stop_detail.candidates.size()==1);
  }
}

TEST_CASE("ztbinom: signed infinite tail and complete prefix certify a strict face") {
  const std::vector<int>x{1,1,1,2,2,8};
  const cppunivariate::ispan sample{x.data(),x.size()};
  const auto face=cppunivariate::ztpois_mle(sample);
  cppunivariate::binomial_certificate_detail::Work work{0,0,200};
  const double end=cppunivariate::binomial_certificate_detail::signed_cutoff(sample,1,INFINITY,face.lambda,work);
  REQUIRE(std::isfinite(end));
  for(int N=8;N<end;++N)CHECK(cppunivariate::ztbinom_mle(sample,1e-10,200,N).logLik<face.logLik);
  for(double N : {end,end*2,end*8}) {
    const auto f=cppunivariate::ztbinom_mle(sample,1e-10,1000,N);
    REQUIRE(f.status==cppunivariate::ZtbinomStatus::Ok);
    CHECK(f.logLik<face.logLik);
  }
  const auto f=cppunivariate::ztbinom_mle(sample);
  REQUIRE(f.status==cppunivariate::ZtbinomStatus::PoissonLimit);
  CHECK_FALSE(f.stop_detail.comparison_tie);
  CHECK(f.stop_detail.loglik_lower==doctest::Approx(face.logLik));
  CHECK(f.stop_detail.loglik_upper>=face.logLik);
  CHECK(std::isfinite(f.stop_detail.loglik_upper));
}

TEST_CASE("ztbinom: signed bound refuses nonpositive gaps and inaccurate face means") {
  const std::vector<int>x{1,2,2,3,3,3,4,4,5,5};
  const cppunivariate::ispan sample{x.data(),x.size()};
  const auto face=cppunivariate::ztpois_mle(sample);
  cppunivariate::binomial_certificate_detail::Work work{0,0,200};
  CHECK_FALSE(std::isfinite(cppunivariate::binomial_certificate_detail::signed_cutoff(sample,1,INFINITY,face.lambda,work)));
  CHECK_FALSE(work.numerical);
  CHECK_FALSE(std::isfinite(cppunivariate::binomial_certificate_detail::signed_cutoff(sample,1,INFINITY,face.lambda*1.01,work)));
  CHECK(work.numerical);
}

TEST_CASE("ztbinom: face prefix keeps hidden improvements ambiguity and budgets distinct") {
  const std::vector<int>x{1,3};const cppunivariate::ispan sample{x.data(),x.size()};
  cppunivariate::binomial_certificate_detail::Work work{0,0,200};
  CHECK(cppunivariate::binomial_certificate_detail::cover_face(sample,3,5,-10.995,work,[](double N){return N==4?-10.99:-11.;})==cppunivariate::binomial_certificate_detail::FaceCoverage::Finite);
  CHECK(cppunivariate::binomial_certificate_detail::cover_face(sample,2,3,-10,work,[](double){return -10.;})==cppunivariate::binomial_certificate_detail::FaceCoverage::Unresolved);
  cppunivariate::binomial_certificate_detail::Work exhausted{0,0,0};
  CHECK(cppunivariate::binomial_certificate_detail::cover_face(sample,2,3,-10,exhausted,[](double){return -11.;})==cppunivariate::binomial_certificate_detail::FaceCoverage::Failure);
  CHECK_FALSE(exhausted.numerical);
}

TEST_CASE("ztbinom: signed moment enumeration and representation refusals retain their reason") {
  const std::vector<int>x{0,0,2};
  cppunivariate::binomial_certificate_detail::Work work{0,0,0};bool budget=false;
  CHECK_FALSE(std::isfinite(cppunivariate::binomial_certificate_detail::signed_cutoff({x.data(),x.size()},0,2,1,work,budget)));
  CHECK(budget);CHECK_FALSE(work.numerical);
  cppunivariate::binomial_certificate_detail::Work huge{0,0,200};
  CHECK_FALSE(std::isfinite(cppunivariate::binomial_certificate_detail::signed_cutoff({x.data(),x.size()},0,1e100,1,huge,budget)));
  CHECK_FALSE(budget);CHECK(huge.numerical);
}

#include "cppunivariate/distributions/ztbinom/dpqr.hpp"
TEST_CASE("ztbinom: quantile search guards and representability") {
  CHECK(std::isnan(cppunivariate::distributions::qztbinom(.5,1e308,.9)));
}

TEST_CASE("ztbinom: extended positive-Poisson moment recheck retains strict signs") {
  const std::vector<int> over{1,1,1,1,1,12},under{1,2,2,3,3,3,4,4,5,5};
  cppunivariate::binomial_certificate_detail::Work a{0,0,200},b{0,0,200};
  CHECK(cppunivariate::binomial_certificate_detail::positive_poisson_gap({over.data(),over.size()},a)>0);
  CHECK(cppunivariate::binomial_certificate_detail::positive_poisson_gap({under.data(),under.size()},b)<0);
}

TEST_CASE("ztbinom: held-probability prefix returns an uncertified candidate") {
  const std::vector<int> x{1,2,3};
  const auto f=cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,1,NAN,1e-9);
  REQUIRE(f.status==cppunivariate::ZtbinomStatus::Ok);
  CHECK_FALSE(f.certified);CHECK(std::isfinite(f.logLik));CHECK(f.prob==1e-9);
  CHECK(f.stop_detail.budget_exhausted);
}

TEST_CASE("ztbinom: two observed positive atoms attain the saturated upper bound") {
  const std::vector<int>x{1,1,2,2,2};
  const auto f=cppunivariate::ztbinom_mle({x.data(),x.size()});
  REQUIRE(f.status==cppunivariate::ZtbinomStatus::Ok);CHECK(f.certified);CHECK(f.size==2);
  CHECK(f.logLik==doctest::Approx(2*std::log(.4)+3*std::log(.6)).epsilon(1e-12));
}

TEST_CASE("ztbinom: a tiny held probability does not hide the attained size-one atom") {
  const std::vector<int>x{1,1,1};
  const auto f=cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,1,NAN,1e-9);
  REQUIRE(f.status==cppunivariate::ZtbinomStatus::Ok);CHECK(f.certified);
  CHECK(f.size==1);CHECK(f.prob==1e-9);CHECK(f.logLik==0);
}

TEST_CASE("ztbinom: sub-machine probability targets retain the finite fit uncertified") {
  const std::vector<int> x{1,2};
  const auto f=cppunivariate::ztbinom_mle({x.data(),x.size()},1e-18);
  REQUIRE(f.status==cppunivariate::ZtbinomStatus::IterationFailure);
  CHECK(f.accuracy_failure);
  CHECK_FALSE(f.certified);CHECK(std::isfinite(f.logLik));
}


TEST_CASE("ztbinom: uncertified coefficients meet the local accuracy contract") {
  {
    const std::vector<int> x{1000000,1000001,1000003,1000008,1000011};
    const auto f=cppunivariate::ztbinom_mle({x.data(),x.size()});
    REQUIRE(f.status==cppunivariate::ZtbinomStatus::Ok);
    CHECK_FALSE(f.certified);
    CHECK(f.size==1000023.0);
    CHECK(std::abs(f.prob-(0.9999816004231903))<=1e-10*1);
  }
}

TEST_CASE("ztbinom: an exactly represented probability meets a sub-machine target") {
  const std::vector<int>x{1,1,2};
  const auto f=cppunivariate::ztbinom_mle({x.data(),x.size()},1e-18);
  REQUIRE(f.status==cppunivariate::ZtbinomStatus::Ok);
  CHECK(f.prob==.5);CHECK_FALSE(f.accuracy_failure);
}

TEST_CASE("ztbinom: exact conditional means beyond size two meet sub-machine accuracy") {
  const std::vector<int>x{1,1,1,2,2,2,3};
  const auto f=cppunivariate::ztbinom_mle({x.data(),x.size()},1e-18,200,3);
  REQUIRE(f.status==cppunivariate::ZtbinomStatus::Ok);CHECK(f.prob==.5);
}
