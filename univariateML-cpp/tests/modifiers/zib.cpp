#include "test_support.hpp"
#include "cppunivariate/distributions/binom/zib_mle.hpp"
#include "cppunivariate/distributions/binom/zib_dpqr.hpp"
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

#include "../../include/cppunivariate/distributions/pois/zip_dpqr.hpp"

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

#include "cppunivariate/distributions/binom/zib_dpqr.hpp"
TEST_CASE("zib: quantile search guards and representability") {
  CHECK(std::isnan(cppunivariate::distributions::qzib(.5,1e308,.9,0)));
  CHECK(cppunivariate::distributions::qzib(.5,1e308,1,0)==1e308);
  CHECK(cppunivariate::distributions::qzib(.5,1e308,.9,1)==0);
}

TEST_CASE("zib: log half uses the band at a two-point jump") {
  // The binary64 value of -log(2) is above the exact logarithm, so its
  // exponential is strictly greater than 1/2. The public band covers zero.
  CHECK(cppunivariate::distributions::qzib(.5,10,1,.5)==0);
  CHECK(cppunivariate::distributions::qzib(-0.69314718055994530942,10,1,.5,true,true)==0);
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
  REQUIRE(f.status==cppunivariate::ZibStatus::IterationFailure);
  CHECK(f.accuracy_failure);
  CHECK_FALSE(f.certified);CHECK(std::isfinite(f.logLik));CHECK(f.pi==0);
  const std::vector<int> binary{0,1,1};
  const auto g=cppunivariate::zib_mle({binary.data(),binary.size()},1e-18,200,1,1,NAN);
  REQUIRE(g.status==cppunivariate::ZibStatus::IterationFailure);
  CHECK(g.accuracy_failure);
  CHECK_FALSE(g.certified);CHECK(g.prob==1);CHECK(g.pi==1./3);
}


TEST_CASE("zib: compensated score resolves a nearly triple probability root") {
  const std::vector<int> x{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2};
    const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,2,NAN,54./343);
    REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
    CHECK_FALSE(f.certified);
    CHECK(f.size==2);
    CHECK(std::abs(f.prob-(0.8235304158971448))<=1e-10*1);
    CHECK(f.pi==0.15743440233236153);
}

TEST_CASE("zib: an exactly represented inflation ratio meets a sub-machine target") {
  const std::vector<int>x{0,1};
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-18,200,1,1);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
  CHECK(f.pi==.5);CHECK_FALSE(f.accuracy_failure);
}

TEST_CASE("zib: an exactly flat probability maximum has a closed rational root") {
  std::vector<int>x(20,0);x.insert(x.end(),2,1);x.insert(x.end(),13,2);
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,2,NAN,7./32);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
  CHECK(std::abs(f.prob-.8)<=1e-10);
}

TEST_CASE("zib: no-zero held inflation preserves an exact binomial mean") {
  const std::vector<int>x{1,2,3};
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-18,200,4,NAN,.2);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
  CHECK(f.prob==.5);CHECK(f.pi==.2);
}

TEST_CASE("zib: strict positive-probability inflation is checked after transformation") {
  std::vector<int> x(91,0);x.insert(x.end(),27,1);x.insert(x.end(),9,2);x.push_back(3);
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-18,200,3);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);CHECK(f.prob==.25);CHECK(f.pi==.5);
  std::vector<int> y(7,0);y.insert(y.end(),3,1);y.insert(y.end(),3,2);y.push_back(3);
  const auto g=cppunivariate::zib_mle({y.data(),y.size()},1e-18,200,3,.5);
  CHECK(g.status==cppunivariate::ZibStatus::IterationFailure);CHECK(g.accuracy_failure);
}

TEST_CASE("zib: a strict binomial-boundary fit keeps the exact active branch") {
  const std::vector<int>x{0,1,1,1,2,2,2,3};
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-18,200,3);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);CHECK(f.prob==.5);CHECK(f.pi==0);
}

TEST_CASE("zib: strict Bernoulli saturation uses the nonidentified maximum curve") {
  const std::vector<int>x{0,0,1};
  const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-18,200,1);
  REQUIRE(f.status==cppunivariate::ZibStatus::Ok);
  CHECK(f.prob>=1./3);CHECK(f.pi>=0);
  CHECK(std::abs(std::fma(-3*f.prob,f.pi,std::fma(3.,f.prob,-1.)))<=1e-18);
  const std::vector<int>y{0,0,0,1};
  const auto g=cppunivariate::zib_mle({y.data(),y.size()},1e-18,200,1,NAN,.5);
  REQUIRE(g.status==cppunivariate::ZibStatus::Ok);CHECK(g.prob==.5);
}

TEST_CASE("zib: exact-input density from likelihood seam replay") {
 CHECK(cppunivariate::distributions::dzib(1000003,1000023.0,0.9999816004231903,0.0,true)==doctest::Approx(-2.4885944485828846).epsilon(1e-10));
}

TEST_CASE("zib: certified large-count binomial delegate likelihood") {
  const std::vector<int> x{1000000,1000001,1000003,1000008,1000011};
  const auto fit=cppunivariate::zib_mle({x.data(),x.size()});
  REQUIRE(fit.status==cppunivariate::ZibStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.size==1000023);
  CHECK(fit.pi==0);
  // 80-digit log-gamma reference at the returned binary64 probability.
  CHECK(std::abs(fit.logLik-(-14.363696715670824))<1e-10);
}

TEST_CASE("zib accuracy stop distinguishes an unrepresentable rational maximizer") {
  const std::vector<int> x{0,1,1};
  const auto r=cppunivariate::zib_mle({x.data(),x.size()},1e-20,200,1,NAN,0);
  CHECK(r.status!=cppunivariate::ZibStatus::Ok);
  CHECK(r.stop_detail.accuracy_stop==cppunivariate::AccuracyStop::Tolerance);
  CHECK(r.stop_detail.attainable_tol==doctest::Approx(1.0/27021597764222976.).epsilon(1e-14));
  REQUIRE(!r.stop_detail.candidates.empty());
  CHECK(r.prob==2.0/3.0);
  const auto exact=cppunivariate::zib_mle({x.data(),x.size()},1e-20,200,1,.5,NAN);
  // An explicitly held probability does not itself constrain tolerance.
  CHECK(exact.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::Tolerance);
}

#include "roundtrips.hpp"
TEST_CASE("zib: modifier four-mode round trips") {
 mltest::modifier_roundtrips("zib",
 [](double x,const std::vector<double>& p,bool l,bool g){return cppunivariate::distributions::pzib(x,p[0],p[1],p[2],l,g);},
 [](double x,const std::vector<double>& p,bool l,bool g){return cppunivariate::distributions::qzib(x,p[0],p[1],p[2],l,g);});
}

TEST_CASE("zib: public fuzz does not change the sampling inverse") {
  struct FixedRng {double runif(double,double) {return .25+std::numeric_limits<double>::epsilon();}} rng;
  CHECK(cppunivariate::distributions::qzib(rng.runif(0,1),2,.5,0)==0);
  CHECK(cppunivariate::distributions::rzib(rng,2,.5,0)==1);
}

TEST_CASE("zib: exact rational modifier ties") {
 CHECK(cppunivariate::distributions::qzib(0.671875,2,0.25,0.25,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.39768296766610944,2,0.25,0.25,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.328125,2,0.25,0.25,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-1.114360645636249,2,0.25,0.25,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.953125,2,0.25,0.25,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.048009219186360606,2,0.25,0.25,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.046875,2,0.25,0.25,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-3.0602707946915624,2,0.25,0.25,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.78125,2,0.25,0.5,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.24686007793152578,2,0.25,0.5,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.21875,2,0.25,0.5,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-1.5198257537444133,2,0.25,0.5,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.96875,2,0.25,0.5,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.0317486983145803,2,0.25,0.5,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.03125,2,0.25,0.5,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-3.4657359027997265,2,0.25,0.5,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.4375,2,0.5,0.25,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.8266785731844679,2,0.5,0.25,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.5625,2,0.5,0.25,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.5753641449035618,2,0.5,0.25,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.8125,2,0.5,0.25,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.2076393647782445,2,0.5,0.25,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.1875,2,0.5,0.25,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-1.6739764335716716,2,0.5,0.25,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.625,2,0.5,0.5,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.4700036292457356,2,0.5,0.5,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.375,2,0.5,0.5,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.9808292530117262,2,0.5,0.5,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.875,2,0.5,0.5,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.13353139262452263,2,0.5,0.5,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.125,2,0.5,0.5,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-2.0794415416798357,2,0.5,0.5,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.296875,2,0.75,0.25,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-1.2144441041932315,2,0.75,0.25,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.703125,2,0.75,0.25,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.3522205935893521,2,0.75,0.25,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.578125,2,0.75,0.25,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.5479651707154474,2,0.75,0.25,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.421875,2,0.75,0.25,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.8630462173553428,2,0.75,0.25,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.53125,2,0.75,0.5,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.6325225587435105,2,0.75,0.5,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.46875,2,0.75,0.5,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.7576857016975165,2,0.75,0.5,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.71875,2,0.75,0.5,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.33024168687057687,2,0.75,0.5,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.28125,2,0.75,0.5,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-1.2685113254635072,2,0.75,0.5,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.56640625,3,0.25,0.25,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.5684437020589881,3,0.25,0.25,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.43359375,3,0.25,0.25,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.8356472431672284,3,0.25,0.25,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.8828125,3,0.25,0.25,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.1246424452072766,3,0.25,0.25,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.1171875,3,0.25,0.25,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-2.1439800628174073,3,0.25,0.25,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.98828125,3,0.25,0.25,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.01178795575204224,3,0.25,0.25,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.01171875,3,0.25,0.25,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-4.446565155811453,3,0.25,0.25,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.7109375,3,0.25,0.5,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.34117075740276714,3,0.25,0.5,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.2890625,3,0.25,0.5,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-1.2411123512753928,3,0.25,0.5,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.921875,3,0.25,0.5,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.0813456394539524,3,0.25,0.5,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.078125,3,0.25,0.5,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-2.5494451709255714,3,0.25,0.5,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.9921875,3,0.25,0.5,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.007843177461025893,3,0.25,0.5,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.0078125,3,0.25,0.5,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-4.852030263919617,3,0.25,0.5,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.34375,3,0.5,0.25,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-1.067840630001356,3,0.5,0.25,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.65625,3,0.5,0.25,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.42121346507630353,3,0.5,0.25,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.625,3,0.5,0.25,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.4700036292457356,3,0.5,0.25,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.375,3,0.5,0.25,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.9808292530117262,3,0.5,0.25,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.90625,3,0.5,0.25,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.09844007281325252,3,0.5,0.25,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.09375,3,0.5,0.25,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-2.367123614131617,3,0.5,0.25,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.5625,3,0.5,0.5,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.5753641449035618,3,0.5,0.5,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.4375,3,0.5,0.5,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.8266785731844679,3,0.5,0.5,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.75,3,0.5,0.5,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.2876820724517809,3,0.5,0.5,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.25,3,0.5,0.5,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-1.3862943611198906,3,0.5,0.5,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.9375,3,0.5,0.5,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.06453852113757118,3,0.5,0.5,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.0625,3,0.5,0.5,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-2.772588722239781,3,0.5,0.5,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.26171875,3,0.75,0.25,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-1.3404848250885963,3,0.75,0.25,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.73828125,3,0.75,0.25,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.3034304294199201,3,0.75,0.25,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.3671875,3,0.75,0.25,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-1.0018826622095587,3,0.75,0.25,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.6328125,3,0.75,0.25,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.4575811092471784,3,0.75,0.25,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.68359375,3,0.75,0.25,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.38039147055604844,3,0.75,0.25,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.31640625,3,0.75,0.25,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-1.1507282898071236,3,0.75,0.25,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.5078125,3,0.75,0.5,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.6776429940239801,3,0.75,0.5,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.4921875,3,0.75,0.5,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.7088955375280844,3,0.75,0.5,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.578125,3,0.75,0.5,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.5479651707154474,3,0.75,0.5,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.421875,3,0.75,0.5,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.8630462173553428,3,0.75,0.5,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.7890625,3,0.75,0.5,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.2369097470783577,3,0.75,0.5,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.2109375,3,0.75,0.5,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-1.556193397915288,3,0.75,0.5,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.3250846862792969,8,0.25,0.25,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-1.1236695574286235,8,0.25,0.25,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.6749153137207031,8,0.25,0.25,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.3931680571350301,8,0.25,0.25,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.5253105163574219,8,0.25,0.25,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.6437657315054574,8,0.25,0.25,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.4746894836425781,8,0.25,0.25,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.7450944073619021,8,0.25,0.25,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.7589073181152344,8,0.25,0.25,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.2758756195627165,8,0.25,0.25,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.24109268188476562,8,0.25,0.25,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-1.4225738472844356,8,0.25,0.25,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.9146385192871094,8,0.25,0.25,true,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.08922635265213245,8,0.25,0.25,true,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.08536148071289062,8,0.25,0.25,false,false)==3);
 CHECK(cppunivariate::distributions::qzib(-2.4608603254557426,8,0.25,0.25,false,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.9795265197753906,8,0.25,0.25,true,false)==4);
 CHECK(cppunivariate::distributions::qzib(-0.02068596715508303,8,0.25,0.25,true,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.020473480224609375,8,0.25,0.25,false,false)==4);
 CHECK(cppunivariate::distributions::qzib(-3.8886248779021866,8,0.25,0.25,false,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.9968299865722656,8,0.25,0.25,true,false)==5);
 CHECK(cppunivariate::distributions::qzib(-0.003175048564082702,8,0.25,0.25,true,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.003170013427734375,8,0.25,0.25,false,false)==5);
 CHECK(cppunivariate::distributions::qzib(-5.754019455223568,8,0.25,0.25,false,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.9997138977050781,8,0.25,0.25,true,false)==6);
 CHECK(cppunivariate::distributions::qzib(-0.00028614322999138584,8,0.25,0.25,true,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.000286102294921875,8,0.25,0.25,false,false)==6);
 CHECK(cppunivariate::distributions::qzib(-8.159161136542705,8,0.25,0.25,false,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.9999885559082031,8,0.25,0.25,true,false)==7);
 CHECK(cppunivariate::distributions::qzib(-1.1444157280993133e-05,8,0.25,0.25,true,true)==7);
 CHECK(cppunivariate::distributions::qzib(1.1444091796875e-05,8,0.25,0.25,false,false)==7);
 CHECK(cppunivariate::distributions::qzib(-11.378036961410906,8,0.25,0.25,false,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.5500564575195312,8,0.25,0.5,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.5977343559882635,8,0.25,0.5,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.44994354248046875,8,0.25,0.5,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.7986331652431945,8,0.25,0.5,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.6835403442382812,8,0.25,0.5,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.3804695986079652,8,0.25,0.5,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.31645965576171875,8,0.25,0.5,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-1.1505595154700665,8,0.25,0.5,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.8392715454101562,8,0.25,0.5,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.175220971232089,8,0.25,0.5,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.16072845458984375,8,0.25,0.5,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-1.8280389553926,8,0.25,0.5,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.9430923461914062,8,0.25,0.5,true,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.05859107305063265,8,0.25,0.5,true,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.05690765380859375,8,0.25,0.5,false,false)==3);
 CHECK(cppunivariate::distributions::qzib(-2.8663254335639072,8,0.25,0.5,false,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.9863510131835938,8,0.25,0.5,true,false)==4);
 CHECK(cppunivariate::distributions::qzib(-0.013742990587845592,8,0.25,0.5,true,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.01364898681640625,8,0.25,0.5,false,false)==4);
 CHECK(cppunivariate::distributions::qzib(-4.294089986010351,8,0.25,0.5,false,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.9978866577148438,8,0.25,0.5,true,false)==5);
 CHECK(cppunivariate::distributions::qzib(-0.002115578544172685,8,0.25,0.5,true,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.00211334228515625,8,0.25,0.5,false,false)==5);
 CHECK(cppunivariate::distributions::qzib(-6.159484563331731,8,0.25,0.5,false,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.9998092651367188,8,0.25,0.5,true,false)==6);
 CHECK(cppunivariate::distributions::qzib(-0.000190753055488581,8,0.25,0.5,true,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.00019073486328125,8,0.25,0.5,false,false)==6);
 CHECK(cppunivariate::distributions::qzib(-8.564626244650869,8,0.25,0.5,false,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.9999923706054688,8,0.25,0.5,true,false)==7);
 CHECK(cppunivariate::distributions::qzib(-7.629423635228488e-06,8,0.25,0.5,true,true)==7);
 CHECK(cppunivariate::distributions::qzib(7.62939453125e-06,8,0.25,0.5,false,false)==7);
 CHECK(cppunivariate::distributions::qzib(-11.78350206951907,8,0.25,0.5,false,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.2529296875,8,0.5,0.25,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-1.3746437438999153,8,0.5,0.25,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.7470703125,8,0.5,0.25,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.29159597177291724,8,0.5,0.25,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.2763671875,8,0.5,0.25,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-1.2860249079562154,8,0.5,0.25,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.7236328125,8,0.5,0.25,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.3234711803033662,8,0.5,0.25,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.3583984375,8,0.5,0.25,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-1.0261099575448829,8,0.5,0.25,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.6416015625,8,0.5,0.25,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.44378778711484257,8,0.5,0.25,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.5224609375,8,0.5,0.25,true,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.6492050587034466,8,0.5,0.25,true,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.4775390625,8,0.5,0.25,false,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.7391093161245811,8,0.5,0.25,false,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.7275390625,8,0.5,0.25,true,false)==4);
 CHECK(cppunivariate::distributions::qzib(-0.31808758721989355,8,0.5,0.25,true,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.2724609375,8,0.5,0.25,false,false)==4);
 CHECK(cppunivariate::distributions::qzib(-1.3002600237780875,8,0.5,0.25,false,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.8916015625,8,0.5,0.25,true,false)==5);
 CHECK(cppunivariate::distributions::qzib(-0.11473592500448462,8,0.5,0.25,true,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.1083984375,8,0.5,0.25,false,false)==5);
 CHECK(cppunivariate::distributions::qzib(-2.221941604287119,8,0.5,0.25,false,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.9736328125,8,0.5,0.25,true,false)==6);
 CHECK(cppunivariate::distributions::qzib(-0.026721035637614764,8,0.5,0.25,true,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.0263671875,8,0.5,0.25,false,false)==6);
 CHECK(cppunivariate::distributions::qzib(-3.635634939595124,8,0.5,0.25,false,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.9970703125,8,0.5,0.25,true,false)==7);
 CHECK(cppunivariate::distributions::qzib(-0.0029339874347875383,8,0.5,0.25,true,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.0029296875,8,0.5,0.25,false,false)==7);
 CHECK(cppunivariate::distributions::qzib(-5.832859516931343,8,0.5,0.25,false,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.501953125,8,0.5,0.5,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.6892485401442879,8,0.5,0.5,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.498046875,8,0.5,0.5,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.6970610798810817,8,0.5,0.5,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.517578125,8,0.5,0.5,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.6585947990532856,8,0.5,0.5,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.482421875,8,0.5,0.5,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.7289362884115306,8,0.5,0.5,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.572265625,8,0.5,0.5,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.5581520160224405,8,0.5,0.5,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.427734375,8,0.5,0.5,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.849252895223007,8,0.5,0.5,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.681640625,8,0.5,0.5,true,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.3832527028370806,8,0.5,0.5,true,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.318359375,8,0.5,0.5,false,false)==3);
 CHECK(cppunivariate::distributions::qzib(-1.1445744242327454,8,0.5,0.5,false,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.818359375,8,0.5,0.5,true,false)==4);
 CHECK(cppunivariate::distributions::qzib(-0.20045370511737004,8,0.5,0.5,true,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.181640625,8,0.5,0.5,false,false)==4);
 CHECK(cppunivariate::distributions::qzib(-1.7057251318862519,8,0.5,0.5,false,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.927734375,8,0.5,0.5,true,false)==5);
 CHECK(cppunivariate::distributions::qzib(-0.07500982100486657,8,0.5,0.5,true,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.072265625,8,0.5,0.5,false,false)==5);
 CHECK(cppunivariate::distributions::qzib(-2.6274067123952833,8,0.5,0.5,false,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.982421875,8,0.5,0.5,true,false)==6);
 CHECK(cppunivariate::distributions::qzib(-0.01773445493976858,8,0.5,0.5,true,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.017578125,8,0.5,0.5,false,false)==6);
 CHECK(cppunivariate::distributions::qzib(-4.041100047703289,8,0.5,0.5,false,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.998046875,8,0.5,0.5,true,false)==7);
 CHECK(cppunivariate::distributions::qzib(-0.0019550348358033506,8,0.5,0.5,true,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.001953125,8,0.5,0.5,false,false)==7);
 CHECK(cppunivariate::distributions::qzib(-6.238324625039508,8,0.5,0.5,false,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.2500114440917969,8,0.75,0.25,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-1.386248585800409,8,0.75,0.25,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.7499885559082031,8,0.75,0.25,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.28769733135725994,8,0.75,0.25,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.2502861022949219,8,0.75,0.25,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-1.3851506062772165,8,0.75,0.25,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.7497138977050781,8,0.75,0.25,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.28806361495642857,8,0.75,0.25,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.2531700134277344,8,0.75,0.25,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-1.373694026105743,8,0.75,0.25,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.7468299865722656,8,0.75,0.25,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.2919177147030938,8,0.75,0.25,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.2704734802246094,8,0.75,0.25,true,false)==3);
 CHECK(cppunivariate::distributions::qzib(-1.307581225336905,8,0.75,0.25,true,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.7295265197753906,8,0.75,0.25,false,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.3153595583210639,8,0.75,0.25,false,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.3353614807128906,8,0.75,0.25,true,false)==4);
 CHECK(cppunivariate::distributions::qzib(-1.0925462820055756,8,0.75,0.25,true,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.6646385192871094,8,0.75,0.25,false,false)==4);
 CHECK(cppunivariate::distributions::qzib(-0.40851196613884677,8,0.75,0.25,false,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.4910926818847656,8,0.75,0.25,true,false)==5);
 CHECK(cppunivariate::distributions::qzib(-0.7111224075247212,8,0.75,0.25,true,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.5089073181152344,8,0.75,0.25,false,false)==5);
 CHECK(cppunivariate::distributions::qzib(-0.6754893652289364,8,0.75,0.25,false,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.7246894836425781,8,0.75,0.25,true,false)==6);
 CHECK(cppunivariate::distributions::qzib(-0.32201201429745985,8,0.75,0.25,true,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.2753105163574219,8,0.75,0.25,false,false)==6);
 CHECK(cppunivariate::distributions::qzib(-1.2898556679357676,8,0.75,0.25,false,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.9249153137207031,8,0.75,0.25,true,false)==7);
 CHECK(cppunivariate::distributions::qzib(-0.07805309839529426,8,0.75,0.25,true,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.07508468627929688,8,0.75,0.25,false,false)==7);
 CHECK(cppunivariate::distributions::qzib(-2.5891386520660284,8,0.75,0.25,false,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.5000076293945312,8,0.75,0.5,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.693131921887297,8,0.75,0.5,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.49999237060546875,8,0.75,0.5,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.6931624394654243,8,0.75,0.5,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.5001907348632812,8,0.75,0.5,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.6927657835744605,8,0.75,0.5,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.49980926513671875,8,0.75,0.5,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.693528723064593,8,0.75,0.5,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.5021133422851562,8,0.75,0.5,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.6889294033306679,8,0.75,0.5,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.49788665771484375,8,0.75,0.5,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.6973828228112582,8,0.75,0.5,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.5136489868164062,8,0.75,0.5,true,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.6662151518392706,8,0.75,0.5,true,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.48635101318359375,8,0.75,0.5,false,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.7208246664292283,8,0.75,0.5,false,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.5569076538085938,8,0.75,0.5,true,false)==4);
 CHECK(cppunivariate::distributions::qzib(-0.5853558448849899,8,0.75,0.5,true,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.44309234619140625,8,0.75,0.5,false,false)==4);
 CHECK(cppunivariate::distributions::qzib(-0.8139770742470112,8,0.75,0.5,false,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.6607284545898438,8,0.75,0.5,true,false)==5);
 CHECK(cppunivariate::distributions::qzib(-0.4144123335362325,8,0.75,0.5,true,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.33927154541015625,8,0.75,0.5,false,false)==5);
 CHECK(cppunivariate::distributions::qzib(-1.0809544733371008,8,0.75,0.5,false,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.8164596557617188,8,0.75,0.5,true,false)==6);
 CHECK(cppunivariate::distributions::qzib(-0.20277777898440663,8,0.75,0.5,true,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.18354034423828125,8,0.75,0.5,false,false)==6);
 CHECK(cppunivariate::distributions::qzib(-1.695320776043932,8,0.75,0.5,false,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.9499435424804688,8,0.75,0.5,true,false)==7);
 CHECK(cppunivariate::distributions::qzib(-0.05135272512144925,8,0.75,0.5,true,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.05005645751953125,8,0.75,0.5,false,false)==7);
 CHECK(cppunivariate::distributions::qzib(-2.9946037601741926,8,0.75,0.5,false,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.2922351360321045,10,0.25,0.25,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-1.2301965403781283,10,0.25,0.25,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.7077648639678955,10,0.25,0.25,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.3456433534898621,10,0.25,0.25,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.43301892280578613,10,0.25,0.25,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.8369738503045445,10,0.25,0.25,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.5669810771942139,10,0.25,0.25,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.5674293493664728,10,0.25,0.25,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.6441946029663086,10,0.25,0.25,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.439754420005404,10,0.25,0.25,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.3558053970336914,10,0.25,0.25,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-1.0333713351366902,10,0.25,0.25,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.8319063186645508,10,0.25,0.25,true,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.1840354422594854,10,0.25,0.25,true,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.16809368133544922,10,0.25,0.25,false,false)==3);
 CHECK(cppunivariate::distributions::qzib(-1.7832338279983169,10,0.25,0.25,false,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.9414048194885254,10,0.25,0.25,true,false)==4);
 CHECK(cppunivariate::distributions::qzib(-0.0603820305375205,10,0.25,0.25,true,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.05859518051147461,10,0.25,0.25,false,false)==4);
 CHECK(cppunivariate::distributions::qzib(-2.8371028296128706,10,0.25,0.25,false,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.9852042198181152,10,0.25,0.25,true,false)==5);
 CHECK(cppunivariate::distributions::qzib(-0.014906329535309546,10,0.25,0.25,true,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.014795780181884766,10,0.25,0.25,false,false)==5);
 CHECK(cppunivariate::distributions::qzib(-4.2134132617129385,10,0.25,0.25,false,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.997370719909668,10,0.25,0.25,true,false)==6);
 CHECK(cppunivariate::distributions::qzib(-0.002632742718039198,10,0.25,0.25,true,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.0026292800903320312,10,0.25,0.25,false,false)==6);
 CHECK(cppunivariate::distributions::qzib(-5.941045200175109,10,0.25,0.25,false,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.9996881484985352,10,0.25,0.25,true,false)==7);
 CHECK(cppunivariate::distributions::qzib(-0.00031190013725601915,10,0.25,0.25,true,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.00031185150146484375,10,0.25,0.25,false,false)==7);
 CHECK(cppunivariate::distributions::qzib(-8.072983440301654,10,0.25,0.25,false,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.9999778270721436,10,0.25,0.25,true,false)==8);
 CHECK(cppunivariate::distributions::qzib(-2.2173173679443927e-05,10,0.25,0.25,true,true)==8);
 CHECK(cppunivariate::distributions::qzib(2.2172927856445312e-05,10,0.25,0.25,false,false)==8);
 CHECK(cppunivariate::distributions::qzib(-10.71663847916554,10,0.25,0.25,false,true)==8);
 CHECK(cppunivariate::distributions::qzib(0.9999992847442627,10,0.25,0.25,true,false)==9);
 CHECK(cppunivariate::distributions::qzib(-7.152559931001943e-07,10,0.25,0.25,true,true)==9);
 CHECK(cppunivariate::distributions::qzib(7.152557373046875e-07,10,0.25,0.25,false,false)==9);
 CHECK(cppunivariate::distributions::qzib(-14.150625683650688,10,0.25,0.25,false,true)==9);
 CHECK(cppunivariate::distributions::qzib(0.5281567573547363,10,0.25,0.5,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.6383621504092479,10,0.25,0.5,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.47184324264526367,10,0.25,0.5,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.7511084615980265,10,0.25,0.5,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.6220126152038574,10,0.25,0.5,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.4747949047704007,10,0.25,0.5,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.3779873847961426,10,0.25,0.5,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.9728944574746372,10,0.25,0.5,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.7627964019775391,10,0.25,0.5,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.2707641221291644,10,0.25,0.5,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.23720359802246094,10,0.25,0.5,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-1.4388364432448546,10,0.25,0.5,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.8879375457763672,10,0.25,0.5,true,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.11885386979625098,10,0.25,0.5,true,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.11206245422363281,10,0.25,0.5,false,false)==3);
 CHECK(cppunivariate::distributions::qzib(-2.1886989361064813,10,0.25,0.5,false,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.9609365463256836,10,0.25,0.5,true,false)==4);
 CHECK(cppunivariate::distributions::qzib(-0.03984690098925718,10,0.25,0.5,true,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.039063453674316406,10,0.25,0.5,false,false)==4);
 CHECK(cppunivariate::distributions::qzib(-3.2425679377210352,10,0.25,0.5,false,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.9901361465454102,10,0.25,0.5,true,false)==5);
 CHECK(cppunivariate::distributions::qzib(-0.009912823545711675,10,0.25,0.5,true,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.009863853454589844,10,0.25,0.5,false,false)==5);
 CHECK(cppunivariate::distributions::qzib(-4.618878369821102,10,0.25,0.5,false,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.9982471466064453,10,0.25,0.5,true,false)==6);
 CHECK(cppunivariate::distributions::qzib(-0.0017543914386388133,10,0.25,0.5,true,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.0017528533935546875,10,0.25,0.5,false,false)==6);
 CHECK(cppunivariate::distributions::qzib(-6.346510308283274,10,0.25,0.5,false,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.9997920989990234,10,0.25,0.5,true,false)==7);
 CHECK(cppunivariate::distributions::qzib(-0.00020792261538548943,10,0.25,0.5,true,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.0002079010009765625,10,0.25,0.5,false,false)==7);
 CHECK(cppunivariate::distributions::qzib(-8.478448548409817,10,0.25,0.5,false,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.9999852180480957,10,0.25,0.5,true,false)==8);
 CHECK(cppunivariate::distributions::qzib(-1.4782061158424586e-05,10,0.25,0.5,true,true)==8);
 CHECK(cppunivariate::distributions::qzib(1.4781951904296875e-05,10,0.25,0.5,false,false)==8);
 CHECK(cppunivariate::distributions::qzib(-11.122103587273704,10,0.25,0.5,false,true)==8);
 CHECK(cppunivariate::distributions::qzib(0.9999995231628418,10,0.25,0.5,true,false)==9);
 CHECK(cppunivariate::distributions::qzib(-4.768372718899988e-07,10,0.25,0.5,true,true)==9);
 CHECK(cppunivariate::distributions::qzib(4.76837158203125e-07,10,0.25,0.5,false,false)==9);
 CHECK(cppunivariate::distributions::qzib(-14.556090791758852,10,0.25,0.5,false,true)==9);
 CHECK(cppunivariate::distributions::qzib(0.250732421875,10,0.5,0.25,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-1.3833689567907854,10,0.5,0.25,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.749267578125,10,0.5,0.25,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.28865911209960754,10,0.5,0.25,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.258056640625,10,0.5,0.25,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-1.3545761808491061,10,0.5,0.25,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.741943359375,10,0.5,0.25,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.29848237380255066,10,0.5,0.25,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.291015625,10,0.5,0.25,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-1.2343783190940487,10,0.5,0.25,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.708984375,10,0.5,0.25,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.343921790774657,10,0.5,0.25,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.37890625,10,0.5,0.25,true,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.9704664659761797,10,0.5,0.25,true,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.62109375,10,0.5,0.25,false,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.47627324225933093,10,0.5,0.25,false,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.53271484375,10,0.5,0.25,true,false)==4);
 CHECK(cppunivariate::distributions::qzib(-0.6297690003263275,10,0.5,0.25,true,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.46728515625,10,0.5,0.25,false,false)==4);
 CHECK(cppunivariate::distributions::qzib(-0.7608155947064441,10,0.5,0.25,false,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.71728515625,10,0.5,0.25,true,false)==5);
 CHECK(cppunivariate::distributions::qzib(-0.3322818099855211,10,0.5,0.25,true,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.28271484375,10,0.5,0.25,false,false)==5);
 CHECK(cppunivariate::distributions::qzib(-1.263316508586403,10,0.5,0.25,false,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.87109375,10,0.5,0.25,true,false)==6);
 CHECK(cppunivariate::distributions::qzib(-0.13800567301944372,10,0.5,0.25,true,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.12890625,10,0.5,0.25,false,false)==6);
 CHECK(cppunivariate::distributions::qzib(-2.048669883013082,10,0.5,0.25,false,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.958984375,10,0.5,0.25,true,false)==7);
 CHECK(cppunivariate::distributions::qzib(-0.04188049724498721,10,0.5,0.25,true,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.041015625,10,0.5,0.25,false,false)==7);
 CHECK(cppunivariate::distributions::qzib(-3.1938021873160847,10,0.5,0.25,false,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.991943359375,10,0.5,0.25,true,false)==8);
 CHECK(cppunivariate::distributions::qzib(-0.008089270731616966,10,0.5,0.25,true,true)==8);
 CHECK(cppunivariate::distributions::qzib(0.008056640625,10,0.5,0.25,false,false)==8);
 CHECK(cppunivariate::distributions::qzib(-4.821258605252863,10,0.5,0.25,false,true)==8);
 CHECK(cppunivariate::distributions::qzib(0.999267578125,10,0.5,0.25,true,false)==9);
 CHECK(cppunivariate::distributions::qzib(-0.0007326902269407109,10,0.5,0.25,true,true)==9);
 CHECK(cppunivariate::distributions::qzib(0.000732421875,10,0.5,0.25,false,false)==9);
 CHECK(cppunivariate::distributions::qzib(-7.219153878051234,10,0.5,0.25,false,true)==9);
 CHECK(cppunivariate::distributions::qzib(0.50048828125,10,0.5,0.5,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.6921710945868899,10,0.5,0.5,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.49951171875,10,0.5,0.5,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.6941242202077719,10,0.5,0.5,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.50537109375,10,0.5,0.5,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.682462280459929,10,0.5,0.5,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.49462890625,10,0.5,0.5,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.703947481910715,10,0.5,0.5,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.52734375,10,0.5,0.5,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.639902666041133,10,0.5,0.5,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.47265625,10,0.5,0.5,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.7493868988828214,10,0.5,0.5,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.5859375,10,0.5,0.5,true,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.5345421503833068,10,0.5,0.5,true,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.4140625,10,0.5,0.5,false,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.8817383503674954,10,0.5,0.5,false,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.6884765625,10,0.5,0.5,true,false)==4);
 CHECK(cppunivariate::distributions::qzib(-0.3732740027871844,10,0.5,0.5,true,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.3115234375,10,0.5,0.5,false,false)==4);
 CHECK(cppunivariate::distributions::qzib(-1.1662807028146085,10,0.5,0.5,false,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.8115234375,10,0.5,0.5,true,false)==5);
 CHECK(cppunivariate::distributions::qzib(-0.2088420107440049,10,0.5,0.5,true,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.1884765625,10,0.5,0.5,false,false)==5);
 CHECK(cppunivariate::distributions::qzib(-1.6687816166945675,10,0.5,0.5,false,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.9140625,10,0.5,0.5,true,false)==6);
 CHECK(cppunivariate::distributions::qzib(-0.08985632912186105,10,0.5,0.5,true,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.0859375,10,0.5,0.5,false,false)==6);
 CHECK(cppunivariate::distributions::qzib(-2.4541349911212467,10,0.5,0.5,false,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.97265625,10,0.5,0.5,true,false)==7);
 CHECK(cppunivariate::distributions::qzib(-0.027724548014854862,10,0.5,0.5,true,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.02734375,10,0.5,0.5,false,false)==7);
 CHECK(cppunivariate::distributions::qzib(-3.5992672954242493,10,0.5,0.5,false,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.99462890625,10,0.5,0.5,true,false)==8);
 CHECK(cppunivariate::distributions::qzib(-0.005385569932592585,10,0.5,0.5,true,true)==8);
 CHECK(cppunivariate::distributions::qzib(0.00537109375,10,0.5,0.5,false,false)==8);
 CHECK(cppunivariate::distributions::qzib(-5.226723713361028,10,0.5,0.5,false,true)==8);
 CHECK(cppunivariate::distributions::qzib(0.99951171875,10,0.5,0.5,true,false)==9);
 CHECK(cppunivariate::distributions::qzib(-0.0004884004981088745,10,0.5,0.5,true,true)==9);
 CHECK(cppunivariate::distributions::qzib(0.00048828125,10,0.5,0.5,false,false)==9);
 CHECK(cppunivariate::distributions::qzib(-7.6246189861593985,10,0.5,0.5,false,true)==9);
 CHECK(cppunivariate::distributions::qzib(0.2500007152557373,10,0.75,0.25,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-1.386291500101034,10,0.75,0.25,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.7499992847442627,10,0.75,0.25,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.28768302612655205,10,0.75,0.25,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.25002217292785645,10,0.75,0.25,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-1.3862056733413421,10,0.75,0.25,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.7499778270721436,10,0.75,0.25,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.28771163679261036,10,0.75,0.25,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.25031185150146484,10,0.75,0.25,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-1.3850477324785107,10,0.75,0.25,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.7496881484985352,10,0.75,0.25,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.28809796092335677,10,0.75,0.25,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.25262928009033203,10,0.75,0.25,true,false)==3);
 CHECK(cppunivariate::distributions::qzib(-1.375832160936433,10,0.75,0.25,true,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.747370719909668,10,0.75,0.25,false,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.29119393862848486,10,0.75,0.25,false,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.26479578018188477,10,0.75,0.25,true,false)==4);
 CHECK(cppunivariate::distributions::qzib(-1.3287963909152765,10,0.75,0.25,true,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.7352042198181152,10,0.75,0.25,false,false)==4);
 CHECK(cppunivariate::distributions::qzib(-0.307606968269815,10,0.75,0.25,false,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.3085951805114746,10,0.75,0.25,true,false)==5);
 CHECK(cppunivariate::distributions::qzib(-1.175724956441798,10,0.75,0.25,true,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.6914048194885254,10,0.75,0.25,false,false)==5);
 CHECK(cppunivariate::distributions::qzib(-0.3690297808962211,10,0.75,0.25,false,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.4180936813354492,10,0.75,0.25,true,false)==6);
 CHECK(cppunivariate::distributions::qzib(-0.8720497535406836,10,0.75,0.25,true,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.5819063186645508,10,0.75,0.25,false,false)==6);
 CHECK(cppunivariate::distributions::qzib(-0.5414458087007727,10,0.75,0.25,false,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.6058053970336914,10,0.75,0.25,true,false)==7);
 CHECK(cppunivariate::distributions::qzib(-0.501196471492251,10,0.75,0.25,true,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.3941946029663086,10,0.75,0.25,false,false)==7);
 CHECK(cppunivariate::distributions::qzib(-0.930910575462496,10,0.75,0.25,false,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.8169810771942139,10,0.75,0.25,true,false)==8);
 CHECK(cppunivariate::distributions::qzib(-0.2021393457193535,10,0.75,0.25,true,true)==8);
 CHECK(cppunivariate::distributions::qzib(0.18301892280578613,10,0.75,0.25,false,false)==8);
 CHECK(cppunivariate::distributions::qzib(-1.698165728176163,10,0.75,0.25,false,true)==8);
 CHECK(cppunivariate::distributions::qzib(0.9577648639678955,10,0.75,0.25,true,false)==9);
 CHECK(cppunivariate::distributions::qzib(-0.04315297584768019,10,0.75,0.25,true,true)==9);
 CHECK(cppunivariate::distributions::qzib(0.04223513603210449,10,0.75,0.25,false,false)==9);
 CHECK(cppunivariate::distributions::qzib(-3.16450279696959,10,0.75,0.25,false,true)==9);
 CHECK(cppunivariate::distributions::qzib(0.5000004768371582,10,0.75,0.5,true,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.6931462268860836,10,0.75,0.5,true,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.4999995231628418,10,0.75,0.5,false,false)==0);
 CHECK(cppunivariate::distributions::qzib(-0.6931481342347164,10,0.75,0.5,false,true)==0);
 CHECK(cppunivariate::distributions::qzib(0.5000147819519043,10,0.75,0.5,true,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.6931176170931403,10,0.75,0.5,true,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.4999852180480957,10,0.75,0.5,false,false)==1);
 CHECK(cppunivariate::distributions::qzib(-0.6931767449007747,10,0.75,0.5,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(0.5002079010009766,10,0.75,0.5,true,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.6927314649796892,10,0.75,0.5,true,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.49979209899902344,10,0.75,0.5,false,false)==2);
 CHECK(cppunivariate::distributions::qzib(-0.6935630690315212,10,0.75,0.5,false,true)==2);
 CHECK(cppunivariate::distributions::qzib(0.5017528533935547,10,0.75,0.5,true,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.689647604438841,10,0.75,0.5,true,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.4982471466064453,10,0.75,0.5,false,false)==3);
 CHECK(cppunivariate::distributions::qzib(-0.6966590467366492,10,0.75,0.5,false,true)==3);
 CHECK(cppunivariate::distributions::qzib(0.5098638534545898,10,0.75,0.5,true,false)==4);
 CHECK(cppunivariate::distributions::qzib(-0.6736115429129383,10,0.75,0.5,true,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.49013614654541016,10,0.75,0.5,false,false)==4);
 CHECK(cppunivariate::distributions::qzib(-0.7130720763779794,10,0.75,0.5,false,true)==4);
 CHECK(cppunivariate::distributions::qzib(0.5390634536743164,10,0.75,0.5,true,false)==5);
 CHECK(cppunivariate::distributions::qzib(-0.6179219901889589,10,0.75,0.5,true,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.4609365463256836,10,0.75,0.5,false,false)==5);
 CHECK(cppunivariate::distributions::qzib(-0.7744948890043856,10,0.75,0.5,false,true)==5);
 CHECK(cppunivariate::distributions::qzib(0.6120624542236328,10,0.75,0.5,true,false)==6);
 CHECK(cppunivariate::distributions::qzib(-0.4909209522914743,10,0.75,0.5,true,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.3879375457763672,10,0.75,0.5,false,false)==6);
 CHECK(cppunivariate::distributions::qzib(-0.9469109168089371,10,0.75,0.5,false,true)==6);
 CHECK(cppunivariate::distributions::qzib(0.7372035980224609,10,0.75,0.5,true,false)==7);
 CHECK(cppunivariate::distributions::qzib(-0.30489117253850273,10,0.75,0.5,true,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.26279640197753906,10,0.75,0.5,false,false)==7);
 CHECK(cppunivariate::distributions::qzib(-1.3363756835706604,10,0.75,0.5,false,true)==7);
 CHECK(cppunivariate::distributions::qzib(0.8779873847961426,10,0.75,0.5,true,false)==8);
 CHECK(cppunivariate::distributions::qzib(-0.13012305356397555,10,0.75,0.5,true,true)==8);
 CHECK(cppunivariate::distributions::qzib(0.12201261520385742,10,0.75,0.5,false,false)==8);
 CHECK(cppunivariate::distributions::qzib(-2.1036308362843275,10,0.75,0.5,false,true)==8);
 CHECK(cppunivariate::distributions::qzib(0.9718432426452637,10,0.75,0.5,true,false)==9);
 CHECK(cppunivariate::distributions::qzib(-0.028560760526286307,10,0.75,0.5,true,true)==9);
 CHECK(cppunivariate::distributions::qzib(0.028156757354736328,10,0.75,0.5,false,false)==9);
 CHECK(cppunivariate::distributions::qzib(-3.5699679050777546,10,0.75,0.5,false,true)==9);
}
