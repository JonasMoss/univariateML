#include "test_support.hpp"
#include "cppunivariate/distributions/nbinom/zinb_mle.hpp"
#include "cppunivariate/distributions/nbinom/zinb_dpqr.hpp"
#include "cppunivariate/distributions/pois/zip_dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <vector>

TEST_CASE("zinb: golden and independent natural parameter scores") {
  const auto g = mltest::load_distribution_golden("zinb");
  std::vector<int> x;
  for (double v : g.data) x.push_back(static_cast<int>(v));
  const auto fit = cppunivariate::zinb_mle({x.data(), x.size()});
  REQUIRE(fit.status == cppunivariate::ZinbStatus::Ok);
  CHECK(fit.size == doctest::Approx(g.estimates.at("size")).epsilon(mltest::kGolden_zinb).scale(0));
  CHECK(fit.prob == doctest::Approx(g.estimates.at("prob")).epsilon(mltest::kGolden_zinb).scale(0));
  CHECK(fit.pi == doctest::Approx(g.estimates.at("pi")).epsilon(mltest::kGolden_zinb).scale(0));
  CHECK(fit.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_zinb).scale(0));
  auto ll = [&](const std::vector<double> &p) {
    double out = 0;
    for (int v : x) out += cppunivariate::distributions::dzinb(v,p[0],p[1],p[2],true);
    return out;
  };
  std::vector<double> p{fit.size, fit.prob, fit.pi};
  double norm2 = 0;
  for (int j = 0; j < 3; ++j) {
    auto a=p,b=p; const double h=1e-5;
    a[j]+=h; b[j]-=h;
    const double score=(ll(a)-ll(b))/(2*h); norm2+=score*score;
  }
  CHECK(std::sqrt(norm2) < 1e-3);
  const auto fixed = cppunivariate::zinb_mle({x.data(),x.size()},1e-10,200,2.5);
  CHECK(fixed.status == cppunivariate::ZinbStatus::Ok);
  CHECK(fixed.size == 2.5);
}

TEST_CASE("zinb: generated face and attained NB fits") {
  cppunivariate::distributions::StdRng<> rng(123), poisson_rng(5);
  std::vector<int> zip, pois, nb;
  for (int i=0;i<5000;++i) {
    zip.push_back(static_cast<int>(cppunivariate::distributions::rzip(rng,4,.4)));
    pois.push_back(static_cast<int>(poisson_rng.rpois(4)));
    nb.push_back(static_cast<int>(cppunivariate::distributions::rnbinom(rng,2,.4)));
  }
  const auto zip_fit=cppunivariate::zinb_mle({zip.data(),zip.size()});
  CHECK((zip_fit.status==cppunivariate::ZinbStatus::ZipLimit ||
    (zip_fit.status==cppunivariate::ZinbStatus::IterationFailure &&
     zip_fit.certificate_failure==cppunivariate::ZinbCertificateFailure::Budget)));
  if(zip_fit.status==cppunivariate::ZinbStatus::IterationFailure) {
    CHECK(zip_fit.stop_detail.budget_exhausted);
    CHECK_FALSE(zip_fit.stop_detail.candidates.empty());
    CHECK(std::isinf(zip_fit.stop_detail.loglik_upper));
  }
  CHECK(cppunivariate::zinb_mle({pois.data(),pois.size()}).status == cppunivariate::ZinbStatus::PoissonLimit);
  const auto fit = cppunivariate::zinb_mle({nb.data(),nb.size()});
  CHECK(fit.status == cppunivariate::ZinbStatus::Ok);
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const auto boundary = cppunivariate::zinb_mle({nb.data(),nb.size()},1e-10,200,nan,nan,0);
  REQUIRE(boundary.status == cppunivariate::ZinbStatus::Ok);
  CHECK(fit.logLik >= boundary.logLik - 1e-7);
  CHECK(cppunivariate::zinb_mle({pois.data(),pois.size()},1e-10,200,2).status == cppunivariate::ZinbStatus::Ok);
}

TEST_CASE("zinb: held positive-size degenerate representative and stable density") {
  const std::vector<int> x{0,0,0};
  // The size-zero d/p/q/r seam belongs to TASK-78. Retain coverage of
  // the held positive-size representative here; P3 checks size zero below.
  const auto fit=cppunivariate::zinb_mle({x.data(),x.size()},1e-10,200,1);
  CHECK(fit.logLik == 0);
  CHECK(cppunivariate::distributions::dzinb(0,fit.size,fit.prob,fit.pi,true)==0);
  CHECK(cppunivariate::distributions::pzinb(0,fit.size,fit.prob,fit.pi,true,false)==1);
  CHECK(cppunivariate::distributions::qzinb(1,fit.size,fit.prob,fit.pi,true,false)==0);
  CHECK(std::isfinite(cppunivariate::distributions::dzinb(10000,2,.5,.1,true)));
}

// Independent high-precision oracle; see quality/truth.py.
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("zinb: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("zinb",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dzinb(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pzinb(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qzinb(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rzinb(rng, p[0], p[1], p[2]); });
}

TEST_CASE("zinb: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("zinb");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<8;++mask) {
   const auto r=cppunivariate::zinb_mle({x.data(),x.size()},1e-10,200,(mask & 1)?g.estimates.at("size"):nan,(mask & 2)?g.estimates.at("prob"):nan,(mask & 4)?g.estimates.at("pi"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.size==g.estimates.at("size"));
   if(mask & 2)CHECK(r.prob==g.estimates.at("prob"));
   if(mask & 4)CHECK(r.pi==g.estimates.at("pi"));
 }
}

TEST_CASE("zinb: P2 held-domain and non-existence statuses") {
 const std::vector<int> x{0,1,5},z{0,0};
 const double nan=std::numeric_limits<double>::quiet_NaN();
 CHECK(cppunivariate::zinb_mle({z.data(),z.size()},1e-10,200,nan,.4,.3).status==cppunivariate::ZinbStatus::Ok);
 CHECK(cppunivariate::zinb_mle({x.data(),x.size()},1e-10,200,nan,1,nan).status==cppunivariate::ZinbStatus::InvalidInput);
 CHECK(cppunivariate::zinb_mle({x.data(),x.size()},1e-10,200,nan,nan,1).status==cppunivariate::ZinbStatus::InvalidInput);
 CHECK(cppunivariate::zinb_mle({x.data(),x.size()},1e-10,1,nan,.4,nan).status==cppunivariate::ZinbStatus::IterationFailure);
}


TEST_CASE("zinb: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // s=mpf(2.0); p=mpf(0.4); pi=mpf(0.3)
  // d=lambda k:(1-pi)*gamma(k+s)/(gamma(s)*gamma(k+1))*p**s*(1-p)**k; S=lambda k:(1-pi)*betainc(k+1,s,0,1-p,regularized=True)
  // print(log(d(mpf(1e+100)))); print(log(S(mpf(134.0))))
  // lo,hi=0,1
  // while log(S(hi))>-10000: hi*=2
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::dzinb(1e+100, 2.0, 0.4, 0.3, true) == doctest::Approx(-5.108256237659907e+99).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pzinb(134.0, 2.0, 0.4, 0.3, false, true) == doctest::Approx(-65.31080096711501).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qzinb(-10000, 2.0, 0.4, 0.3, false, true) == 19593.0);
  CHECK(cppunivariate::distributions::dzinb(0.5, 2.0, 0.4, 0.3, false) == 0);
}

TEST_CASE("zinb: P3 attained size zero preserves holds") {
 const std::vector<int> z{0,0,0};
 const auto r=cppunivariate::zinb_mle({z.data(),z.size()},1e-10,200,0,.4,.2);
 CHECK(r.status==cppunivariate::ZinbStatus::Ok);
 CHECK(r.size==0); CHECK(r.prob==.4); CHECK(r.logLik==0);
}

TEST_CASE("zinb: zero boundary is the attained point mass") {
  const double inf=std::numeric_limits<double>::infinity();
  const double nan=std::numeric_limits<double>::quiet_NaN();
  cppunivariate::distributions::StdRng<> rng(290);
  for(double x : {-inf,-1.0,-.5,0.0,.5,1.0,inf}) {
    const double mass=x==0?1:0;
    CHECK(cppunivariate::distributions::dzinb(x,0,.4,.3,false)==mass);
    CHECK(cppunivariate::distributions::dzinb(x,0,.4,.3,true)==(mass==1?0:-inf));
    for(bool lower : {false,true}) {
      const double cdf=(x>=0)==lower?1:0;
      CHECK(cppunivariate::distributions::pzinb(x,0,.4,.3,lower,false)==cdf);
      CHECK(cppunivariate::distributions::pzinb(x,0,.4,.3,lower,true)==(cdf==1?0:-inf));
    }
  }
  for(bool lower : {false,true}) {
    for(double p : {0.0,.1,.5,1.0})
      CHECK(cppunivariate::distributions::qzinb(p,0,.4,.3,lower,false)==0);
    for(double p : {-inf,-10000.0,-1.0,0.0})
      CHECK(cppunivariate::distributions::qzinb(p,0,.4,.3,lower,true)==0);
    CHECK(std::isnan(cppunivariate::distributions::qzinb(nan,0,.4,.3,lower,true)));
    CHECK(std::isnan(cppunivariate::distributions::pzinb(nan,0,.4,.3,lower,true)));
  }
  CHECK(std::isnan(cppunivariate::distributions::dzinb(nan,0,.4,.3,false)));
  for(int i=0;i<20;++i) CHECK(cppunivariate::distributions::rzinb(rng,0,.4,.3)==0);
}

#include "cppunivariate/distributions/nbinom/zinb_certificate.hpp"
TEST_CASE("zinb: exhaustive held-size competing maxima and tie certificate") {
  std::vector<int> x(100,0);x.back()=10;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(const auto& entry:std::vector<std::vector<double>>{{.7,.957450207},{.8,.506823257326}}) {
    const auto fit=cppunivariate::zinb_fixed_size_mle({x.data(),x.size()},10,nan,entry[0]);
    REQUIRE(fit.fit.status==cppunivariate::ZinbStatus::Ok);
    REQUIRE(fit.stop_detail.candidates.size()==2);
    CHECK(fit.fit.prob==(entry[0]==.7?doctest::Approx(.957450207).epsilon(1e-8):doctest::Approx(.506823257326).epsilon(1e-8)));
    CHECK(fit.stop_detail.loglik_lower<=fit.fit.logLik);
    CHECK(fit.stop_detail.loglik_upper>=fit.fit.logLik);
    for(const auto& candidate:fit.stop_detail.candidates) {
      double ll=0;
      for(int v:x)ll+=cppunivariate::distributions::dzinb(v,10,candidate.coef[1],entry[0],true);
      CHECK(ll>=candidate.loglik_lower);CHECK(ll<=candidate.loglik_upper);
    }
  }
  const auto tie=cppunivariate::zinb_fixed_size_mle({x.data(),x.size()},10,nan,.76839477568758077746);
  CHECK(tie.fit.status==cppunivariate::ZinbStatus::IterationFailure);
  CHECK(tie.failure==cppunivariate::ZinbCertificateFailure::ComparisonTie);
  CHECK(tie.stop_detail.comparison_tie);
  CHECK(tie.stop_detail.candidates.size()==2);
  const auto budget=cppunivariate::zinb_fixed_size_mle({x.data(),x.size()},10,nan,.7,1e-12,1);
  CHECK(budget.failure==cppunivariate::ZinbCertificateFailure::Budget);
  CHECK(budget.stop_detail.budget_exhausted);
}

TEST_CASE("zinb: fixed-size certificate holds and golden likelihood") {
  const auto g=mltest::load_distribution_golden("zinb");
  std::vector<int> x;for(double v:g.data)x.push_back(int(v));
  const double nan=std::numeric_limits<double>::quiet_NaN(),r=g.estimates.at("size");
  for(unsigned mask=0;mask<4;++mask) {
    const double p=(mask&1)?g.estimates.at("prob"):nan,pi=(mask&2)?g.estimates.at("pi"):nan;
    const auto fit=cppunivariate::zinb_fixed_size_mle({x.data(),x.size()},r,p,pi);
    REQUIRE(fit.fit.status==cppunivariate::ZinbStatus::Ok);
    CHECK(fit.fit.size==r);
    if(mask&1)CHECK(fit.fit.prob==p);
    if(mask&2)CHECK(fit.fit.pi==pi);
    CHECK(fit.fit.logLik>=g.logLik-1e-8);
    CHECK(fit.fit.prob==doctest::Approx(g.estimates.at("prob")).epsilon(2e-7));
    CHECK(fit.fit.pi==doctest::Approx(g.estimates.at("pi")).epsilon(2e-7));
  }
  const std::vector<int> z{0,0,0},positive{0,1,2};
  CHECK(cppunivariate::zinb_fixed_size_mle({z.data(),z.size()},0,.4,.3).fit.logLik==0);
  CHECK(cppunivariate::zinb_fixed_size_mle({positive.data(),positive.size()},0).fit.status==cppunivariate::ZinbStatus::InvalidInput);
  CHECK(cppunivariate::zinb_fixed_size_mle({positive.data(),positive.size()},1,nan,0).fit.status==cppunivariate::ZinbStatus::Ok);
}

TEST_CASE("zinb: held-probability size branch certificate") {
  const auto g=mltest::load_distribution_golden("zinb");
  std::vector<int> x;for(double v:g.data)x.push_back(int(v));
  const double nan=std::numeric_limits<double>::quiet_NaN(),p=g.estimates.at("prob");
  for(double pi:{nan,g.estimates.at("pi"),0.}) {
    const auto fit=cppunivariate::zinb_fixed_prob_mle({x.data(),x.size()},p,pi);
    REQUIRE(fit.fit.status==cppunivariate::ZinbStatus::Ok);
    CHECK(fit.fit.prob==p);
    if(!std::isnan(pi))CHECK(fit.fit.pi==pi);
    const auto ref=cppunivariate::zinb_mle({x.data(),x.size()},1e-10,200,nan,p,pi);
    CHECK(fit.fit.logLik>=ref.logLik-1e-7);
    if(pi!=0)CHECK(fit.fit.size==doctest::Approx(g.estimates.at("size")).epsilon(2e-7));
    CHECK(fit.stop_detail.loglik_lower<=fit.fit.logLik);
    CHECK(fit.stop_detail.loglik_upper>=fit.fit.logLik);
  }
  const auto budget=cppunivariate::zinb_fixed_prob_mle({x.data(),x.size()},p,nan,1e-12,1);
  CHECK(budget.failure==cppunivariate::ZinbCertificateFailure::Budget);
  const std::vector<int> z{0,0,0};
  CHECK(cppunivariate::zinb_fixed_prob_mle({z.data(),z.size()},.4,.3).fit.logLik==0);
}

TEST_CASE("zinb: scalar certificate resolution and zero-likelihood evaluation") {
  const std::vector<int> z{0,0,0},large{0,100001},bad{-1,2},ones{0,0,1,1};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto zero=cppunivariate::zinb_fixed_size_mle({z.data(),z.size()},1e6,.1,0);
  REQUIRE(zero.fit.status==cppunivariate::ZinbStatus::Ok);
  CHECK(zero.fit.logLik==doctest::Approx(3e6*std::log(.1)).epsilon(1e-12));
  const auto work=cppunivariate::zinb_fixed_size_mle({large.data(),large.size()},2);
  CHECK(work.fit.status==cppunivariate::ZinbStatus::IterationFailure);
  CHECK(work.failure==cppunivariate::ZinbCertificateFailure::Workload);
  CHECK(cppunivariate::zinb_fixed_prob_mle({large.data(),large.size()},.4).fit.status==cppunivariate::ZinbStatus::IterationFailure);
  CHECK(cppunivariate::zinb_fixed_size_mle({bad.data(),bad.size()},2).fit.status==cppunivariate::ZinbStatus::InvalidInput);
  CHECK(cppunivariate::zinb_fixed_size_mle({ones.data(),ones.size()},2,nan,nan,-1).fit.status==cppunivariate::ZinbStatus::InvalidInput);
  const auto one=cppunivariate::zinb_fixed_size_mle({ones.data(),ones.size()},2);
  REQUIRE(one.fit.status==cppunivariate::ZinbStatus::Ok);
  CHECK(one.fit.pi==0);
  CHECK(one.fit.prob==.8);
}

TEST_CASE("zinb: default global certificate and same-inflation ZIP payload") {
  const auto g=mltest::load_distribution_golden("zinb");
  std::vector<int> x;for(double v:g.data)x.push_back(int(v));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto fit=cppunivariate::zinb_mle({x.data(),x.size()},1e-10,200,nan,nan,g.estimates.at("pi"));
  REQUIRE(fit.status==cppunivariate::ZinbStatus::Ok);
  CHECK(fit.certificate_failure==cppunivariate::ZinbCertificateFailure::None);
  CHECK(fit.stop_detail.loglik_lower<=fit.logLik);
  CHECK(fit.stop_detail.loglik_upper>=fit.logLik);
  std::vector<int> boundary(100,0);for(int i=50;i<100;++i)boundary[i]=4;
  const auto face=cppunivariate::zinb_mle({boundary.data(),boundary.size()},1e-10,200,nan,nan,.5);
  REQUIRE(face.status==cppunivariate::ZinbStatus::ZipLimit);
  CHECK(face.stop_detail.limit_model==cppunivariate::StopModel::Zip);
  REQUIRE(face.stop_detail.limit_coef.size()==2);
  CHECK(face.stop_detail.limit_coef[1]==.5);
  CHECK(face.stop_detail.loglik_lower<=face.stop_detail.limit_loglik);
  CHECK(face.stop_detail.loglik_upper>=face.stop_detail.limit_loglik);
  const std::vector<int> zeros(99,0);auto tied=zeros;tied.push_back(10);
  const auto tie=cppunivariate::zinb_mle({tied.data(),tied.size()},1e-10,200,10,nan,.76839477568758077746);
  CHECK(tie.status==cppunivariate::ZinbStatus::IterationFailure);
  CHECK(tie.certificate_failure==cppunivariate::ZinbCertificateFailure::ComparisonTie);
  CHECK(tie.stop_detail.candidates.size()==2);
}

TEST_CASE("zinb: positive Poisson-tail coefficient forbids a false ZIP limit") {
  std::vector<int> x(5000,0);x.insert(x.end(),3958,1);x.insert(x.end(),4063,2);x.insert(x.end(),1979,4);
  // The positive-count Poisson mean is two. This signed rate bracket gives
  // A-S*lambda/2 > .7, so finite size strictly improves on the ZIP face.
  const double lo=1.59362,hi=1.59363;
  CHECK(lo/(-std::expm1(-lo))<2-1e-10);
  CHECK(hi/(-std::expm1(-hi))>2+1e-10);
  CHECK(15937-10000*hi>0.69);
  const auto fit=cppunivariate::zinb_mle({x.data(),x.size()});
  CHECK((fit.status==cppunivariate::ZinbStatus::Ok ||
    (fit.status==cppunivariate::ZinbStatus::IterationFailure &&
     fit.certificate_failure==cppunivariate::ZinbCertificateFailure::Budget)));
  CHECK(fit.status!=cppunivariate::ZinbStatus::ZipLimit);
  if(fit.status==cppunivariate::ZinbStatus::IterationFailure) {
    CHECK(fit.iterations>0);
    CHECK(fit.stop_detail.budget_exhausted);
  }
}

TEST_CASE("zinb: near-point-mass zero likelihood uses its positive complement") {
  const std::vector<int> x{0,0,0};
  const double p=std::nextafter(1.,0.),pi=.3;
  const auto fit=cppunivariate::zinb_mle({x.data(),x.size()},1e-10,200,1,p,pi);
  REQUIRE(fit.status==cppunivariate::ZinbStatus::Ok);
  CHECK(fit.logLik==doctest::Approx(3*std::log1p(-(1-pi)*(1-p))).epsilon(1e-12).scale(1e-20));
}

TEST_CASE("zinb: certified zero-deficit delegate clears proposal diagnostics") {
  const std::vector<int> x(100000,100000);
  const auto fit=cppunivariate::zinb_mle({x.data(),x.size()});
  CHECK(fit.status==cppunivariate::ZinbStatus::PoissonLimit);
  CHECK(fit.certificate_failure==cppunivariate::ZinbCertificateFailure::None);
  REQUIRE(fit.stop_detail.limit_coef.size()==1);
  CHECK(fit.stop_detail.limit_coef[0]==100000);
}

#include "cppunivariate/distributions/nbinom/zinb_dpqr.hpp"
TEST_CASE("zinb: quantile search guards and representability") {
  CHECK(std::isnan(cppunivariate::distributions::qzinb(.5,1e308,.1,.1,true,false)));
}

TEST_CASE("zinb: half-inflation log quantile uses the supplied-scale band") {
  const double got=cppunivariate::distributions::qzinb(-0.6931471805599453,1e8,.5,.5,true,true);
  CHECK(got==0);
}

#include "roundtrips.hpp"
TEST_CASE("zinb: modifier four-mode round trips") {
 mltest::modifier_roundtrips("zinb",
 [](double x,const std::vector<double>& p,bool l,bool g){return cppunivariate::distributions::pzinb(x,p[0],p[1],p[2],l,g);},
 [](double x,const std::vector<double>& p,bool l,bool g){return cppunivariate::distributions::qzinb(x,p[0],p[1],p[2],l,g);});
}

TEST_CASE("zinb: geometric inflation modifier round trips") {
  for(double pi : {0.,.25,.5,1.})
    mltest::modifier_roundtrips("ztgeom",
      [=](double x,const std::vector<double>& p,bool l,bool g) {
        return cppunivariate::distributions::pzinb(x,1,p[0],pi,l,g);
      },
      [=](double x,const std::vector<double>& p,bool l,bool g) {
        return cppunivariate::distributions::qzinb(x,1,p[0],pi,l,g);
      });
}
