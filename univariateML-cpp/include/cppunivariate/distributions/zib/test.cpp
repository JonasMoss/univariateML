#include "test_support.hpp"
#include "cppunivariate/distributions/zib/mle.hpp"
#include "cppunivariate/distributions/zib/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <cmath>
TEST_CASE("zib: golden, score and distribution") {
 const auto g=mltest::load_distribution_golden("zib");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const auto fit=cppunivariate::zib_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZibStatus::Ok);
 CHECK(fit.size==doctest::Approx(g.estimates.at("size")).epsilon(mltest::kGolden_zib).scale(0));
 CHECK(fit.prob==doctest::Approx(g.estimates.at("prob")).epsilon(mltest::kGolden_zib).scale(0));
 CHECK(fit.pi==doctest::Approx(g.estimates.at("pi")).epsilon(mltest::kGolden_zib).scale(0));
 CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_zib).scale(0));
 std::vector<double> p{fit.size,fit.prob,fit.pi};
 auto ll=[&](const std::vector<double> &par){double l=0;for(int v:x)l+=cppunivariate::distributions::dzib(v,par[0],par[1],par[2],true);return l;};
 CHECK(ll(p)==doctest::Approx(fit.logLik).epsilon(1e-10));
 double norm2=0;
 {auto a=p,b=p;const double h=1e-5;a[1]+=h;b[1]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 {auto a=p,b=p;const double h=1e-5;a[2]+=h;b[2]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 CHECK(std::sqrt(norm2)<1e-3);
 const auto fixed=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,15);
 CHECK(fixed.status==cppunivariate::ZibStatus::Ok);
 CHECK(fixed.size==15);
 CHECK(cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,-1).status==cppunivariate::ZibStatus::InvalidSize);
 CHECK(cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,std::numeric_limits<double>::infinity()).status==cppunivariate::ZibStatus::InvalidSize);
 CHECK(cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,0.5).status==cppunivariate::ZibStatus::InvalidSize);
 CHECK(cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,0).status==cppunivariate::ZibStatus::InvalidSize);
}
TEST_CASE("zib: attained boundaries and invalid input") {
 std::vector<int> ones{1,1,1},zeros{0,0,0},negative{-1,1};
 CHECK(cppunivariate::zib_mle({}).status==cppunivariate::ZibStatus::InvalidInput);
 CHECK(cppunivariate::zib_mle({negative.data(),negative.size()}).status==cppunivariate::ZibStatus::InvalidInput);
 const auto one=cppunivariate::zib_mle({ones.data(),ones.size()});
 CHECK(one.logLik==0);
 CHECK(one.status==cppunivariate::ZibStatus::Ok);
 const auto zero=cppunivariate::zib_mle({zeros.data(),zeros.size()});
 CHECK(zero.logLik==0);
 CHECK(zero.status==cppunivariate::ZibStatus::Ok);
}
TEST_CASE("zib: scalar tails and quantile endpoints") {
 const auto g=mltest::load_distribution_golden("zib");
 CHECK(std::isfinite(cppunivariate::distributions::dzib(4,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"),true)));
 CHECK(cppunivariate::distributions::qzib(0,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"))==0);
 CHECK(cppunivariate::distributions::qzib(1,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"))==g.estimates.at("size"));
}

TEST_CASE("zib: infinite-size escape and fixed-size removal") {
 std::vector<int> x{0,0,0,0,0,0,0,0,1,1,1,2,3,7,12};
 CHECK(cppunivariate::zib_mle({x.data(),x.size()}).status==cppunivariate::ZibStatus::ZipLimit);
 CHECK(cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,20).status==cppunivariate::ZibStatus::Ok);
}

TEST_CASE("zib: quantile inversion and sampling") {
 const auto g=mltest::load_distribution_golden("zib");
 const double q=cppunivariate::distributions::qzib(.37,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"));
 CHECK(cppunivariate::distributions::pzib(q,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"))>=.37);
 CHECK(cppunivariate::distributions::pzib(q-1,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"))<.37);
 CHECK(cppunivariate::distributions::pzib(q,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"),true,false)+cppunivariate::distributions::pzib(q,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"),false,false)==doctest::Approx(1));
 cppunivariate::distributions::StdRng<> rng(19);
 for(int j=0;j<20;++j) {const double v=cppunivariate::distributions::rzib(rng,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"));CHECK(v>=0);CHECK(v==std::floor(v));}
}

TEST_CASE("zib retains tiny positive probabilities") {
 CHECK(std::isfinite(cppunivariate::distributions::dzib(1,10,1e-20,.5,true)));
 CHECK(cppunivariate::distributions::pzib(0,10,1e-20,.5,false,true)==doctest::Approx(std::log(5e-20)));
 CHECK(cppunivariate::distributions::qzib(-50,10,1e-20,.5,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(1,10,.4,1)==10);
 CHECK(cppunivariate::distributions::pzib(4,5,1,.3)==doctest::Approx(.3));
}

TEST_CASE("zib fixed zero size is an attained member") {
 std::vector<int> x{0,0};
 const auto fit=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,0);
 CHECK(fit.status==cppunivariate::ZibStatus::Ok);
 CHECK(fit.size==0);
 CHECK(fit.logLik==0);
 CHECK(cppunivariate::distributions::dzib(0,0,.5,.3)==1);
 CHECK(cppunivariate::distributions::qzib(1,0,.5,.3)==0);
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("zib: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("zib",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dzib(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pzib(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qzib(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rzib(rng, p[0], p[1], p[2]); });
}

TEST_CASE("zib: held probability with free and held size") {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 const std::vector<int> x{1,2,2,3,4};
 const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,nan,.4,.3);
 REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
 CHECK(f.prob==.4);
 for(int k=4;k<100;++k) {
  const auto o=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,k,.4,.3);
  CHECK(f.logLik>=o.logLik-1e-9);
 }
 CHECK(cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,8,-.1,.3).status==cppunivariate::ZibStatus::InvalidInput);
}

TEST_CASE("zib: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("zib");
  const std::vector<int> values(g.data.begin(),g.data.end());
  const cppunivariate::ispan ix{values.data(),values.size()};
  CHECK(cppunivariate::zib_mle(ix,1e-8,1).status==cppunivariate::ZibStatus::IterationFailure);
}

// TASK-85/87: same widened-grid change as ztbinom's free-size search
// (see mle.hpp); covers the n=20/100, ties and near-Poisson/ZIP cases
// the task calls out, cross-checked against dzib (a separate code path
// from the fitter) and a coarse brute-force grid. test.R adds the
// fully independent reference-density multistart oracle.
TEST_CASE("zib: free size oracle at n=20, n=100, ties, and near-Poisson") {
  cppunivariate::distributions::StdRng<> rng(85087);
  auto sample=[&](int n,double size,double prob,double pi) {
    std::vector<int> x;
    for(int i=0;i<n;++i) x.push_back(static_cast<int>(cppunivariate::distributions::rzib(rng,size,prob,pi)));
    return x;
  };
  std::vector<std::vector<int>> samples{
    sample(20,8,.4,.3), sample(100,8,.4,.3),
    {0,0,2,2,2,2,3,4,5,5},
    sample(100,500,.02,.2),  // near-Poisson/ZIP: large size, small prob
  };
  for (auto &x : samples) {
    const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,1000);
    REQUIRE((f.status==cppunivariate::ZibStatus::Ok || f.status==cppunivariate::ZibStatus::ZipLimit));
    double ref=0; for(int v:x) ref+=cppunivariate::distributions::dzib(v,f.size,f.prob,f.pi,true);
    CHECK(f.logLik==doctest::Approx(ref).epsilon(1e-8));
    if (f.status==cppunivariate::ZibStatus::Ok) {
      const int mx=*std::max_element(x.begin(),x.end());
      for (double factor : {0.25,0.5,2.0,4.0}) {
        const double alt_size=std::max<double>(mx,std::floor(f.size*factor));
        const auto alt=cppunivariate::zib_mle({x.data(),x.size()},1e-10,1000,alt_size);
        REQUIRE(alt.status==cppunivariate::ZibStatus::Ok);
        CHECK(f.logLik+1e-6>=alt.logLik);
      }
    }
  }
}

TEST_CASE("zib: tiny zero mass remains finite without inflation") {
  const double p=1-1e-9, expected=8*std::log1p(-p);
  CHECK(cppunivariate::distributions::dzib(0,8,p,0,true)==doctest::Approx(expected));
  CHECK(cppunivariate::distributions::dzib(0,8,1e-20,.5,true)==doctest::Approx(-4e-20).epsilon(1e-12));
  CHECK(cppunivariate::distributions::pzib(0,8,p,0,true,true)==doctest::Approx(expected));
  CHECK(cppunivariate::distributions::pzib(0,8,1e-20,.5,false,true)==doctest::Approx(std::log(4e-20)));
  CHECK(cppunivariate::distributions::pzib(-1,8,.4,1)==0);
  CHECK(cppunivariate::distributions::pzib(0,8,0,.3)==1);
  CHECK(cppunivariate::distributions::pzib(0,8,1,.3)==doctest::Approx(.3));
}

TEST_CASE("zib attained point sampler ignores zero uniform") {
  // At success probability one the (possibly truncated) law is delta_size.
  struct ZeroRng { double runif(double,double) { return 0; } } rng;
  CHECK(cppunivariate::distributions::rzib(rng,5,1,0)==5);
}

TEST_CASE("zib all-zero free-size held subsets attain the exact maximum") {
  const std::vector<int> x{0,0,0};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double p : {nan,0.,.5,1.}) for(double pi : {nan,0.,.2,1.}) {
    const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,nan,p,pi);
    REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
    CHECK(f.logLik==0);
    CHECK(f.iterations==0);
    CHECK(f.size==((std::isnan(p)&&std::isnan(pi))?1:0));
    CHECK(f.prob==(std::isnan(p)?0:p));
    CHECK(f.pi==(std::isnan(pi)?0:pi));
    CHECK(cppunivariate::distributions::dzib(0,f.size,f.prob,f.pi,true)==0);
    for(double size : {0.,1.,2.,10.}) {
      const double q=std::isnan(p)?0:p, mix=std::isnan(pi)?0:pi;
      CHECK(cppunivariate::distributions::dzib(0,size,q,mix)<=1);
    }
  }
}

#include "../zip/dpqr.hpp"

TEST_CASE("zib: structured stop payloads") {
  const std::vector<int> x{0,0,0,0,0,0,0,0,1,1,1,2,3,7,12};
  const auto r=cppunivariate::zib_mle({x.data(),x.size()});
  REQUIRE(r.stop_detail.limit_model==cppunivariate::StopModel::Zip);
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  double ll=0;for(int v:x)ll+=cppunivariate::distributions::dzip(v,r.stop_detail.limit_coef[0],r.stop_detail.limit_coef[1],true);
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(ll));
}

TEST_CASE("zib: constrained retained-support finite winner and J1 face witness") {
  const std::vector<int>x{0,0,0,0,0,1,2,2,3,3,3,4,4,5,5};
  for(double pi : {double(NAN),.2}) {
    const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,NAN,NAN,pi);
    REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
    for(int N=5;N<100;++N) {
      const auto g=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,N,NAN,pi);
      CHECK(g.logLik<=f.logLik+1e-8);
    }
  }
  const std::vector<int>tail{0,0,0,0,0,0,0,0,1,1,1,2,3,7,12};
  const auto f=cppunivariate::zib_mle({tail.data(),tail.size()},1e-10,200,NAN,NAN,.2);
  CHECK_FALSE(f.stop_detail.comparison_tie);
  CHECK(std::isfinite(f.stop_detail.loglik_upper));
}

TEST_CASE("zib: distinct coefficients can describe the same saturated maximum") {
  const std::vector<int>x{0,1,1};
  const auto a=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,1,2./3,0);
  const auto b=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,1,1,1./3);
  CHECK(a.logLik==doctest::Approx(b.logLik));
  CHECK(a.status==cppunivariate::ZibStatus::Ok);
  CHECK(b.status==cppunivariate::ZibStatus::Ok);
}

TEST_CASE("zib: block correction preserves held inflation constraints") {
  const std::vector<int>x{0,0,1,2,2,3,4};
  const cppunivariate::ispan sample{x.data(),x.size()};
  for(double pi : {double(NAN),0.,.3,.8}) {
    cppunivariate::binomial_certificate_detail::Work work{0,0,200};
    const double upper=cppunivariate::zib_mle(sample,1e-10,200,4,NAN,pi).logLik+
      cppunivariate::binomial_certificate_detail::block_error(sample,4,40,work);
    for(int N=4;N<=40;++N)CHECK(cppunivariate::zib_mle(sample,1e-10,200,N,NAN,pi).logLik<=upper+1e-9);
  }
}

TEST_CASE("zib: held inflation needs relative probability root resolution") {
  const std::vector<int>x{0,1,2,3,4};
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,1,1e12,NAN,.2);
  CHECK(f.status==cppunivariate::ZibStatus::IterationFailure);
  CHECK_FALSE(f.stop_detail.budget_exhausted);
  CHECK_FALSE(f.stop_detail.comparison_tie);
  CHECK(f.stop_detail.loglik_upper==INFINITY);
  REQUIRE(f.stop_detail.candidates.size()==1);
  CHECK(std::isfinite(f.stop_detail.candidates[0].loglik_lower));
}

TEST_CASE("zib: validate the positive mean root before selecting the inflation branch") {
  const std::vector<int>x{0,0,0,0,1,2,3,4};
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,1e100);
  CHECK(f.status==cppunivariate::ZibStatus::IterationFailure);
  CHECK_FALSE(f.stop_detail.budget_exhausted);
  CHECK(f.stop_detail.loglik_upper==INFINITY);
  REQUIRE(f.stop_detail.candidates.size()==1);
}

TEST_CASE("zib: held inflation remains stable at a huge held size") {
  const std::vector<int>x{0,1,2,3,4};
  const cppunivariate::ispan sample{x.data(),x.size()};
  const auto f=cppunivariate::zib_mle(sample,1e-10,200,1e100,NAN,.2);
  const auto face=cppunivariate::zip_mle(sample,NAN,.2);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
  REQUIRE(face.status==cppunivariate::ZipStatus::Ok);
  CHECK(f.prob*1e100==doctest::Approx(face.lambda).epsilon(1e-10));
  CHECK(f.logLik==doctest::Approx(face.logLik).epsilon(1e-10));
}

TEST_CASE("zib: no-zero data use the exact ordinary-binomial reduction") {
  const std::vector<int>x{1,2,3};
  for(double pi : {double(NAN),.2}) {
    const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,1,1e100,NAN,pi);
    REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
    CHECK(f.prob*1e100==doctest::Approx(2).epsilon(1e-12));
    CHECK(f.pi==(std::isnan(pi)?0:pi));
  }
}

TEST_CASE("zib: admissible relaxed positive face and ordinary binomial reductions") {
  const std::vector<int>x{0,0,0,0,0,0,0,0,1,1,1,2,3,7,12};
  const auto f=cppunivariate::zib_mle({x.data(),x.size()});
  REQUIRE(f.status==cppunivariate::ZibStatus::ZipLimit);
  CHECK_FALSE(f.stop_detail.comparison_tie);
  CHECK(std::isfinite(f.stop_detail.loglik_upper));
  CHECK(f.stop_detail.limit_coef[1]>0);
  const auto zero=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,NAN,NAN,0);
  REQUIRE(zero.status==cppunivariate::ZibStatus::ZipLimit);
  CHECK_FALSE(zero.stop_detail.comparison_tie);
  CHECK(zero.stop_detail.limit_coef[1]==0);
  CHECK(std::isfinite(zero.stop_detail.loglik_upper));
  const std::vector<int>positive{1,1,1,2,3,7,12};
  const auto a=cppunivariate::zib_mle({positive.data(),positive.size()},1e-10,200,NAN,NAN,.2);
  const auto b=cppunivariate::zib_mle({positive.data(),positive.size()});
  REQUIRE(a.status==cppunivariate::ZibStatus::ZipLimit);
  REQUIRE(b.status==cppunivariate::ZibStatus::ZipLimit);
  CHECK(a.stop_detail.limit_coef[1]==.2);
  CHECK(b.stop_detail.limit_coef[1]==0);
  CHECK(a.stop_detail.limit_loglik==doctest::Approx(b.stop_detail.limit_loglik+positive.size()*std::log(.8)));
  CHECK_FALSE(a.stop_detail.comparison_tie);
  const auto held=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,NAN,NAN,.2);
  CHECK_FALSE(held.stop_detail.comparison_tie);
  CHECK(std::isfinite(held.stop_detail.loglik_upper));
}

TEST_CASE("zib: inflation-uniform signed cutoff and held ZIP faces") {
  const std::vector<int>x{0,0,0,0,0,0,0,0,1,1,1,2,3,7,12};
  const cppunivariate::ispan sample{x.data(),x.size()};
  cppunivariate::binomial_certificate_detail::Work work{0,0,200};bool budget=false;
  const double end=cppunivariate::zib_fit_detail::signed_zip_cutoff(sample,work,budget);
  REQUIRE(std::isfinite(end));CHECK_FALSE(budget);
  for(double pi : {.01,.2,.8}) {
    const auto f=cppunivariate::zib_mle(sample,1e-10,200,NAN,NAN,pi);
    REQUIRE(f.status==cppunivariate::ZibStatus::ZipLimit);
    CHECK_FALSE(f.stop_detail.comparison_tie);
    CHECK(std::isfinite(f.stop_detail.loglik_upper));
    CHECK(f.stop_detail.limit_coef[1]==pi);
    const auto face=cppunivariate::zip_mle(sample,NAN,pi);
    REQUIRE(face.status==cppunivariate::ZipStatus::Ok);
    CHECK(f.stop_detail.limit_loglik==doctest::Approx(face.logLik).epsilon(1e-10));
    for(double N : {end,end*2,end*8}) {
      const auto finite=cppunivariate::zib_mle(sample,1e-10,200,N,NAN,pi);
      REQUIRE(finite.status==cppunivariate::ZibStatus::Ok);
      CHECK(finite.logLik<face.logLik);
    }
  }
}

TEST_CASE("zib: localization resolves a held-inflation face outside the uniform moment gap") {
  const std::vector<int>x{0,0,0,0,0,1,1,2,2,4};
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,NAN,NAN,.2);
  CHECK_FALSE(f.stop_detail.comparison_tie);
  CHECK(std::isfinite(f.stop_detail.loglik_upper));
  CHECK(f.stop_detail.limit_coef[1]==.2);
}

TEST_CASE("zib: localized held-face tails and near-zero coefficients are distinguished") {
  const std::vector<int>x{0,0,0,0,0,1,1,2,2,4};
  const cppunivariate::ispan sample{x.data(),x.size()};
  const auto face=cppunivariate::zip_mle(sample,NAN,.2);
  REQUIRE(face.status==cppunivariate::ZipStatus::Ok);
  cppunivariate::binomial_certificate_detail::Work work{0,0,200};bool budget=false;
  const double end=cppunivariate::zib_fit_detail::localized_zip_cutoff(sample,.2,face.logLik,work,budget);
  REQUIRE(std::isfinite(end));CHECK_FALSE(budget);
  for(double N : {end,end*2,end*8}) {
    const auto finite=cppunivariate::zib_mle(sample,1e-10,200,N,NAN,.2);
    REQUIRE(finite.status==cppunivariate::ZibStatus::Ok);
    CHECK(finite.logLik<face.logLik);
  }
  const double q=std::exp(-1.6),pi=.75*q/(.25+.75*q);
  const auto near=cppunivariate::zib_mle(sample,1e-10,200,NAN,NAN,pi);
  CHECK(near.stop_detail.comparison_tie);
  CHECK(near.stop_detail.loglik_upper==INFINITY);
}

TEST_CASE("zib: globally winning tied ZIP faces retain both rate candidates") {
  std::vector<int>x(198,0);x.push_back(1);x.push_back(19);
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,NAN,NAN,.7254971052291191);
  REQUIRE(f.status==cppunivariate::ZibStatus::FaceTie);
  CHECK_FALSE(f.stop_detail.comparison_tie);
  CHECK(std::isfinite(f.stop_detail.loglik_upper));
  int faces=0;for(const auto& c:f.stop_detail.candidates)if(c.model==cppunivariate::StopModel::Zip)++faces;
  CHECK(faces==2);
  CHECK(f.stop_detail.candidates.size()>=3);
}

TEST_CASE("zib: a strict finite winner is retained above tied ZIP rates") {
  std::vector<int>x(99,0);x.push_back(10);
  const double pi=.7254971052291191;
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,NAN,NAN,pi);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
  CHECK(f.logLik>=99*std::log(pi)+std::log1p(-pi)-1e-10);
}

TEST_CASE("zib: noninflated faces use the binomial escape including equality") {
  for(const std::vector<int> x : {std::vector<int>{0,1,1,1,2,4},std::vector<int>{0,1,1,1,2,3,4,4}}) {
    const auto f=cppunivariate::zib_mle({x.data(),x.size()});
    REQUIRE(f.status==cppunivariate::ZibStatus::ZipLimit);
    CHECK_FALSE(f.stop_detail.comparison_tie);
    CHECK(std::isfinite(f.stop_detail.loglik_upper));
    CHECK(f.stop_detail.limit_coef[1]==0);
    const auto face=cppunivariate::zip_mle({x.data(),x.size()});
    REQUIRE(face.status==cppunivariate::ZipStatus::Ok);
    CHECK(f.stop_detail.limit_loglik==doctest::Approx(face.logLik).epsilon(1e-10));
  }
}

TEST_CASE("zib: subnormal held inflation retains log masses and the probability root") {
  const double pi=std::numeric_limits<double>::denorm_min();
  const std::vector<int> x{0,500};
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,1075,.5,pi);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
  double ll=0;for(int v:x)ll+=cppunivariate::distributions::dzib(v,1075,.5,pi,true);
  CHECK(std::abs(f.logLik-ll)<1e-9);
  std::vector<int> y(1000,535);y.push_back(0);
  const auto g=cppunivariate::zib_mle({y.data(),y.size()},1e-10,200,1075,NAN,pi);
  REQUIRE(g.status==cppunivariate::ZibStatus::Ok);
  // 70-digit root; the exact turning discriminant is -1461373763040000/1849.
  CHECK(g.prob==doctest::Approx(.4971795798757613128165584136).epsilon(1e-10));
  CHECK(std::abs(g.logLik+4455.659181549239638934)<1e-7);
  const std::vector<int> zero{0};
  const auto tiny=cppunivariate::zib_mle({zero.data(),zero.size()},1e-10,200,1,1e-300,.5);
  REQUIRE(tiny.status==cppunivariate::ZibStatus::Ok);
  CHECK(tiny.logLik<0);
  CHECK(tiny.logLik/(-5e-301)==doctest::Approx(1).epsilon(1e-10));
}

TEST_CASE("zib: an unrepresentable finite log likelihood is a numerical stop") {
  const std::vector<int> x{0};
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,1e308,.9,0);
  CHECK(f.status==cppunivariate::ZibStatus::IterationFailure);
  CHECK(f.stop_detail.loglik_upper==INFINITY);
  CHECK_FALSE(f.stop_detail.budget_exhausted);
}

TEST_CASE("zib: a rounded base zero mass does not hide the unique all-zero inflation") {
  const std::vector<int> x{0};
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,1,1e-300);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
  CHECK(f.pi==1);
  CHECK(f.logLik==0);
}

#include "cppunivariate/distributions/zib/dpqr.hpp"
TEST_CASE("zib: quantile search guards and representability") {
  CHECK(std::isnan(cppunivariate::distributions::qzib(.5,1e308,.9,0)));
  CHECK(cppunivariate::distributions::qzib(.5,1e308,1,0)==1e308);
  CHECK(cppunivariate::distributions::qzib(.5,1e308,.9,1)==0);
}

TEST_CASE("zib: log half stays on the exact side of a two-point jump") {
  // The binary64 value of -log(2) is above the exact logarithm, so its
  // exponential is strictly greater than 1/2. All positive mass is at 10.
  CHECK(cppunivariate::distributions::qzib(.5,10,1,.5)==0);
  CHECK(cppunivariate::distributions::qzib(-0.69314718055994530942,10,1,.5,true,true)==10);
  CHECK(cppunivariate::distributions::qzib(-0.69314718055994530942,10,1,.5,false,true)==0);
}

TEST_CASE("zib: adjacent near-critical binary64 inflations retain unresolved comparisons") {
  const std::vector<int>x{0,0,0,0,0,1,1,2,2,4};
  for(double pi : {.3772146069463776,.37721460694637765}) {
    const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,NAN,NAN,pi);
    CHECK(f.status==cppunivariate::ZibStatus::Ok);
    CHECK_FALSE(f.certified);
    CHECK(f.stop_detail.limit_model==cppunivariate::StopModel::Zip);
    CHECK(f.stop_detail.limit_coef[1]==pi);
    CHECK(f.stop_detail.comparison_tie);
    CHECK(f.stop_detail.loglik_upper==INFINITY);
    CHECK_FALSE(f.stop_detail.budget_exhausted);
    CHECK(f.stop_detail.candidates.size()>=2);
  }
}

TEST_CASE("zib: large held evaluator and quantile remain consistent") {
  const std::vector<int>x{0,1,2,3,4};
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,1e100,NAN,.2);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
  double ll=0;for(int v:x)ll+=cppunivariate::distributions::dzib(v,f.size,f.prob,f.pi,true);
  CHECK(ll==doctest::Approx(f.logLik).epsilon(1e-10));
  for(double p : {.01,.5,.99}) {
    const double q=cppunivariate::distributions::qzib(p,100,.02,.2);
    CHECK(cppunivariate::distributions::pzib(q,100,.02,.2)>=p);
    if(q>0)CHECK(cppunivariate::distributions::pzib(q-1,100,.02,.2)<p);
  }
}

TEST_CASE("zib: held-probability uncertified prefix preserves the hold") {
  const std::vector<int> x{0,1,2,3};
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,1,NAN,1e-9);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
  CHECK_FALSE(f.certified);CHECK(std::isfinite(f.logLik));CHECK(f.prob==1e-9);
}

TEST_CASE("zib: an exact zero turning discriminant admits an exact integer recheck") {
  std::vector<int>x(17,0);x.insert(x.end(),2,1);x.insert(x.end(),15,2);
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,2,NAN,.2);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);CHECK(f.certified);
  CHECK(f.size==2);CHECK(f.pi==.2);CHECK(std::isfinite(f.logLik));
  const double logq=2*std::log1p(-f.prob),w=.8*std::exp(logq)/(.2+.8*std::exp(logq));
  CHECK(std::abs(32-(17+17*w)*2*f.prob)<1e-7);
  std::vector<int> big;big.reserve(x.size()*2000);
  for(int j=0;j<2000;++j)big.insert(big.end(),x.begin(),x.end());
  const auto unresolved=cppunivariate::zib_mle({big.data(),big.size()},1e-10,200,2,NAN,.2);
  REQUIRE(unresolved.status==cppunivariate::ZibStatus::Ok);CHECK_FALSE(unresolved.certified);

}

TEST_CASE("zib: large held counts consume their falling-factorial work") {
  const std::vector<int>x{1000000,1000001,1000003,1000008,1000011};
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,NAN,.4);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);CHECK_FALSE(f.certified);
  CHECK(f.stop_detail.budget_exhausted);CHECK(f.prob==.4);CHECK(std::isfinite(f.logLik));
}

TEST_CASE("zib: an exact turning comparison does not certify a flat probability root") {
  std::vector<int>x(17,0);x.insert(x.end(),2,1);x.insert(x.end(),15,2);
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,2,NAN,54./343);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);CHECK_FALSE(f.certified);
  CHECK(std::isfinite(f.logLik));CHECK(f.prob>0);CHECK(f.prob<1);
}

TEST_CASE("zib: a closed binomial mean does not certify sub-machine coefficient accuracy") {
  const std::vector<int> x{1,1,2};
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-18,200,2,NAN,0);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
  CHECK_FALSE(f.certified);CHECK(std::isfinite(f.logLik));CHECK(f.pi==0);
  const std::vector<int> binary{0,1,1};
  const auto g=cppunivariate::zib_mle({binary.data(),binary.size()},1e-18,200,1,1,NAN);
  REQUIRE(g.status==cppunivariate::ZibStatus::Ok);
  CHECK_FALSE(g.certified);CHECK(g.prob==1);CHECK(g.pi==1./3);
}
