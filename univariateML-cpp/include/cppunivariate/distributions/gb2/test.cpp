#include "test_support.hpp"
#include "cppunivariate/distributions/gb2/dpqr.hpp"
#include "cppunivariate/distributions/trgamma/dpqr.hpp"
#include <vector>
TEST_CASE("gb2: golden and full score gate") {
  const auto g=mltest::load_distribution_golden("gb2");
  const auto r=cppunivariate::gb2_mle(mltest::as_span(g.data));
  REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
  std::vector<double> budget_data;
  for(int i=0;i<200;++i) {
    const double u=cppunivariate::special::beta_quantile((i+.5)/200,.4,.3,true);
    budget_data.push_back(std::sqrt(u/(1-u)));
  }
  CHECK(cppunivariate::gb2_mle(mltest::as_span(budget_data),1e-7,1).status==cppunivariate::Gb2Status::CertificateBudget);
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_gb2).scale(0));
  CHECK(r.shape1==doctest::Approx(g.estimates.at("shape1")).epsilon(mltest::kGolden_gb2).scale(0));
  CHECK(r.shape2==doctest::Approx(g.estimates.at("shape2")).epsilon(mltest::kGolden_gb2).scale(0));
  CHECK(r.shape3==doctest::Approx(g.estimates.at("shape3")).epsilon(mltest::kGolden_gb2).scale(0));
  CHECK(r.scale==doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_gb2).scale(0));
  std::vector<double> v{r.shape1,r.shape2,r.shape3,r.scale};
  for(int j=0;j<4;++j) {
    auto p=v,m=v;p[j]*=std::exp(1e-5);m[j]*=std::exp(-1e-5);
    double d=0;
    for(double x:g.data)d+=cppunivariate::distributions::dgb2(x,p[0],p[1],p[2],p[3],true)-
      cppunivariate::distributions::dgb2(x,m[0],m[1],m[2],m[3],true);
    CHECK(std::abs(d/2e-5)<1e-3);
  }
}
TEST_CASE("gb2: stable density and exact double Pareto kink profile") {
  CHECK(cppunivariate::distributions::dgb2(1e-300,2,5,3,1,true)==doctest::Approx(-9666.763).epsilon(1e-6));
  CHECK(cppunivariate::distributions::dgb2(1e300,2,5,3,1,true)==doctest::Approx(-7594.436).epsilon(1e-6));
  std::vector<double> logs{-2,-.8,-.2,0,.1,.7,1.5};
  cppunivariate::trgamma_detail::LogData d;
  REQUIRE(cppunivariate::trgamma_detail::standardize(mltest::as_span(logs),d));
  const double ll=cppunivariate::gb2_detail::double_pareto(d,1);
  CHECK(ll==doctest::Approx(-9.173878).epsilon(1e-6));
}
TEST_CASE("gb2: analytic envelope scores and Hessians for inner constraints") {
  const std::vector<double> y{-2,-.8,-.2,0,.1,.7,1.5};
  cppunivariate::trgamma_detail::LogData d;
  REQUIRE(cppunivariate::trgamma_detail::standardize(mltest::as_span(y),d));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double a:{nan,1.7}) for(double b:{nan,1.2}) {
    const auto f=cppunivariate::gb2_detail::profile(d,.7,1.3,a,b);
    REQUIRE(f.ok);
    for(int j=0;j<2;++j) {
      const double h=1e-4;
      const auto plus=cppunivariate::gb2_detail::profile(d,.7*std::exp(j==0?h:0),1.3*std::exp(j==1?h:0),a,b);
      const auto minus=cppunivariate::gb2_detail::profile(d,.7*std::exp(j==0?-h:0),1.3*std::exp(j==1?-h:0),a,b);
      CHECK((plus.ll-minus.ll)/(2*h*d.n)==doctest::Approx(f.g[j]).epsilon(1e-5));
      for(int i=0;i<2;++i)
        CHECK((plus.g[i]-minus.g[i])/(2*h)==doctest::Approx(f.h[2*i+j]).epsilon(1e-5));
    }
  }
}

#include "cppunivariate/distributions/gb2/dpqr.hpp"
#include "cppunivariate/distributions/trgamma/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
TEST_CASE("gb2: probability endpoints and sampler CDF") {
  CHECK(cppunivariate::distributions::qgb2(0,20,2,20,1,true,false)==0);
  CHECK(std::isinf(cppunivariate::distributions::qgb2(0,20,2,20,1,false,false))==true);
  CHECK(cppunivariate::distributions::pgb2(-1,20,2,20,1,true,false)==0);
  CHECK(cppunivariate::distributions::pgb2(std::numeric_limits<double>::infinity(),20,2,20,1,true,false)==1);
  CHECK(std::isnan(cppunivariate::distributions::qgb2(.1,20,2,20,1,true,true)));
  cppunivariate::distributions::StdRng<> rng(7813);
  std::vector<double> samples;
  for(int i=0;i<20000;++i) samples.push_back(cppunivariate::distributions::rgb2(rng,20,2,20,1));
  for(double point:{.5,1.,2.}) {
    int count=0;for(double x:samples) if(x<=point) ++count;
    const double prob=cppunivariate::distributions::pgb2(point,20,2,20,1,true,false);
    CHECK(std::abs(count/20000.-prob)<=5*std::sqrt(prob*(1-prob)/20000.));
  }
}
// from mpmath import mp; mp.dps=100; x=mp.mpf('1e-100'); z=x*x/(1+x*x)
// float(mp.log(mp.betainc(20,20,0,z,regularized=True)))
TEST_CASE("gb2: log beta tails beyond probability underflow") {
  // from mpmath import mp; mp.dps=100
  // -mp.mpf('1e20')*mp.log1p(1/mp.mpf('1e20')) == -1 to double precision
  CHECK(cppunivariate::distributions::pgb2(1e20,1,1,1e20,1,true,true)==doctest::Approx(-1).epsilon(2e-13));
  CHECK(cppunivariate::distributions::pgb2(1e-100,20,2,20,1,true,true)==doctest::Approx(-9185.384112363492).epsilon(2e-14));
  for(double lp:{-27.631021115928547,-69.07755278982137,-230.25850929940458,-690.7755278982137,-800.,-10000.}) for(bool lower:{true,false}) {
    const double q=cppunivariate::distributions::qgb2(lp,20,2,20,1,lower,true);
    REQUIRE(q>0);
    REQUIRE(std::isfinite(q));
    CHECK(cppunivariate::distributions::pgb2(q,20,2,20,1,lower,true)==doctest::Approx(lp).epsilon(2e-11));
  }
}

TEST_CASE("P2 gb2 constant conditional shape fits") {
 std::vector<double> x(5,2.0);
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(int mask=0;mask<3;++mask) {
   double q=mask==0?nan:2,a=mask==2?nan:3,p=mask==1?nan:2;
   auto r=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,300,q,a,p,1);
   REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
   CHECK(std::isfinite(r.logLik));
   CHECK(r.scale==1);
   if(mask!=0) CHECK(r.shape1==q);
   if(mask!=1) CHECK(r.shape3==p);
   if(mask!=2) CHECK(r.shape2==a);
 }
}


TEST_CASE("gb2: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("gb2",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dgb2(x, p[0], p[1], p[2], p[3], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pgb2(x, p[0], p[1], p[2], p[3], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qgb2(x, p[0], p[1], p[2], p[3], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rgb2(rng, p[0], p[1], p[2], p[3]); });
}

// Generating mpmath snippet (binary64 inputs retained exactly):
// import mpmath as mp; mp.mp.dps=100
// a,b,k=mp.mpf(0.4),mp.mpf(0.3),mp.mpf(2)
// print(mp.inf if k*a<1 else (-mp.inf if k*a>1 else mp.log(k)-mp.log(mp.beta(a,b))))
// f=lambda t: mp.log(mp.betainc(a,b,0,mp.exp(t)/(1+mp.exp(t)),regularized=True))+800
// t=mp.findroot(f,-2000); print(mp.exp(t/k))
TEST_CASE("gb2: targeted tail and endpoint regressions") {
  CHECK(cppunivariate::distributions::dgb2(0.0, 0.3, 2.0, 0.4, 1.0, true) == INFINITY);
  CHECK(cppunivariate::distributions::qgb2(-800.0, 0.3, 2.0, 0.4, 1.0, true, true) == doctest::Approx(0.0).epsilon(1e-10));
}

// import mpmath as mp; mp.mp.dps=100
// At shape2*shape3=1: log f(0)=log(shape2)-log(scale)-log(beta(shape3,shape1)).
// print(mp.log(2)-mp.log(3)-mp.log(mp.beta(mp.mpf('.5'),2)))
TEST_CASE("gb2: targeted tail finite and zero endpoint limits") {
  CHECK(cppunivariate::distributions::dgb2(0,2,2,0.5,3,false)
        == doctest::Approx(0.5).epsilon(1e-10));
  CHECK(cppunivariate::distributions::dgb2(0,2,2,1,3,true)==-INFINITY);
}

TEST_CASE("gb2: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("gb2");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::gb2_mle(x,1e-8,1).status==cppunivariate::Gb2Status::CertificateBudget);
}

TEST_CASE("gb2: beta logit inversion fails when a finite bracket cannot be represented") {
  CHECK(std::isnan(cppunivariate::missing_detail::beta_logit_q(-1e308,1e-308,2,true)));
}

TEST_CASE("gb2: bounded member seeds retain the full-data repeated-sample fit") {
  const auto g=mltest::load_distribution_golden("gb2");
  std::vector<double> repeated;
  for(int j=0;j<3;++j)repeated.insert(repeated.end(),g.data.begin(),g.data.end());
  const auto fit=cppunivariate::gb2_mle(mltest::as_span(repeated));
  REQUIRE(fit.status==cppunivariate::Gb2Status::Ok);
  CHECK(fit.logLik==doctest::Approx(3*g.logLik).epsilon(1e-12));
  CHECK(fit.shape1==doctest::Approx(g.estimates.at("shape1")).epsilon(mltest::kGolden_gb2).scale(0));
  CHECK(fit.shape2==doctest::Approx(g.estimates.at("shape2")).epsilon(mltest::kGolden_gb2).scale(0));
  CHECK(fit.shape3==doctest::Approx(g.estimates.at("shape3")).epsilon(mltest::kGolden_gb2).scale(0));
  CHECK(fit.scale==doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_gb2).scale(0));
}

TEST_CASE("gb2: face comparison cannot resolve rounded extreme objectives") {
  const double magnitude=1e99;
  CHECK(cppunivariate::compare_face(-magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(-magnitude,-2*magnitude,2e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-2*magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(-10.,-10.,2e-8)==cppunivariate::FaceComparison::FirstWins);
}

TEST_CASE("gb2: audited huge held-parameter magnitude") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x{1,2};
  cppunivariate::span s{x.data(),x.size()};
  const auto r=cppunivariate::gb2_mle(s,1e-7,300,nan,1,1e100,nan);
  CHECK((r.status==cppunivariate::Gb2Status::Ok || r.status==cppunivariate::Gb2Status::SearchUnresolved || r.status==cppunivariate::Gb2Status::ScaleOutOfRange));
  CHECK(r.status!=cppunivariate::Gb2Status::TrGammaBoundary);
}

TEST_CASE("GB2 held unit beta shapes use Burr submodels") {
 const std::vector<double> x{1,2,3,4};
 const auto r=cppunivariate::gb2_mle(mltest::as_span(x),1e-10,1000,NAN,2,1,NAN);
 const auto b=cppunivariate::burr_mle_holds(mltest::as_span(x),NAN,2,NAN,1e-10,1000);
 REQUIRE(r.status==cppunivariate::Gb2Status::WeibullBoundary);
 CHECK(r.stop_detail.limit_coef==b.stop_detail.limit_coef);
 CHECK(r.stop_detail.limit_loglik==doctest::Approx(b.stop_detail.limit_loglik));
 std::vector<double> reciprocal;for(double v:x)reciprocal.push_back(1/v);
 const auto inv=cppunivariate::gb2_mle(mltest::as_span(reciprocal),1e-10,1000,1,2,NAN,NAN);
 CHECK(inv.status==cppunivariate::Gb2Status::InvWeibullBoundary);
 CHECK(inv.stop_detail.limit_coef[1]==doctest::Approx(1/r.stop_detail.limit_coef[1]));
}

TEST_CASE("GB2 unit beta shapes preserve finite certified joint coefficients") {
 const auto g=mltest::load_distribution_golden("burr");
 const auto r=cppunivariate::gb2_mle(mltest::as_span(g.data),1e-8,1000,NAN,NAN,1,NAN);
 REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
 CHECK(r.shape1==doctest::Approx(g.estimates.at("shape1")).epsilon(mltest::kBurrGoldenTol));
 CHECK(r.shape2==doctest::Approx(g.estimates.at("shape2")).epsilon(mltest::kBurrGoldenTol));
 CHECK(r.shape3==1);
 CHECK(r.scale==doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kBurrGoldenTol));
 double ll=0,jac=0;std::vector<double> reciprocal;
 for(double v:g.data){ll+=cppunivariate::distributions::dgb2(v,r.shape1,r.shape2,r.shape3,r.scale,true);reciprocal.push_back(1/v);jac+=2*std::log(v);}
 CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-8));
 const auto inv=cppunivariate::gb2_mle(mltest::as_span(reciprocal),1e-8,1000,1,NAN,NAN,NAN);
 REQUIRE(inv.status==cppunivariate::Gb2Status::Ok);
 CHECK(inv.shape1==1);CHECK(inv.shape2==doctest::Approx(r.shape2).epsilon(1e-6));
 CHECK(inv.shape3==doctest::Approx(r.shape1).epsilon(1e-6));
 CHECK(inv.scale==doctest::Approx(1/r.scale).epsilon(1e-6));
 CHECK(inv.logLik==doctest::Approx(r.logLik+jac).epsilon(1e-8));
}

TEST_CASE("GB2 held unit beta shape and scale inherit scalar certificate") {
 const std::vector<double> x{.1,.2,.4,.8};
 const auto a=cppunivariate::gb2_mle(mltest::as_span(x),1e-9,1000,NAN,NAN,1,1);
 const auto b=cppunivariate::burr_mle_fixed_scale(mltest::as_span(x),1,1e-9);
 REQUIRE(a.status==cppunivariate::Gb2Status::Ok);
 CHECK(a.shape1==b.shape1);CHECK(a.shape2==b.shape2);CHECK(a.logLik==b.logLik);
 const auto stop=cppunivariate::gb2_mle(mltest::as_span(x),1e-9,1,NAN,NAN,1,1);
 CHECK(stop.status==cppunivariate::Gb2Status::Ok);
 CHECK(std::isfinite(stop.stop_detail.loglik_lower));
}

TEST_CASE("gb2: transformed-gamma budget keeps its face witness coordinates") {
  const auto g=mltest::load_distribution_golden("trgamma");
  const auto r=cppunivariate::gb2_mle(mltest::as_span(g.data),1e-8,1);
  REQUIRE(r.status==cppunivariate::Gb2Status::CertificateBudget);
  REQUIRE(r.stop_detail.budget_exhausted);
  REQUIRE_FALSE(r.stop_detail.candidates.empty());
  bool found=false;
  for(const auto& c:r.stop_detail.candidates) {
    if(c.model!=cppunivariate::StopModel::Trgamma)continue;
    found=true;
    REQUIRE(cppunivariate::valid_stop_coefficients(c.model,c.coef));
    double ll=0;
    for(double v:g.data)ll+=cppunivariate::distributions::dtrgamma(v,c.coef[0],c.coef[1],c.coef[2],true);
    CHECK(ll>=c.loglik_lower-1e-8);
    CHECK(ll<=c.loglik_upper+1e-8);
  }
  CHECK(found);
  CHECK(r.stop_detail.loglik_upper==INFINITY);
}

#include "cppunivariate/distributions/invtrgamma/dpqr.hpp"

TEST_CASE("GB2 held power transports non-unit beta-prime certificates") {
 const std::vector<double> y{.03,.1,.3,.7,1,2,5,20};
 for(double a:{.7,1.,2.})for(bool reciprocal:{false,true}) {
  std::vector<double>x;for(double v:y)x.push_back(std::pow(v,reciprocal?-1/a:1/a));
  const auto parent=cppunivariate::betapr_mle_holds(mltest::as_span(y),2,NAN,NAN,1e-7,1000);
  const auto r=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,1000,reciprocal?2:NAN,a,reciprocal?NAN:2,NAN);
  REQUIRE(parent.status==cppunivariate::BetaPrimeStatus::Ok);
  REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
  CHECK(r.shape1==doctest::Approx(reciprocal?2:parent.beta).epsilon(1e-6));
  CHECK(r.shape3==doctest::Approx(reciprocal?parent.beta:2).epsilon(1e-6));
  CHECK(r.shape2==a);
  CHECK(r.scale==doctest::Approx(std::pow(parent.scale,reciprocal?-1/a:1/a)).epsilon(1e-6));
  double ll=0;for(double v:x)ll+=cppunivariate::distributions::dgb2(v,r.shape1,r.shape2,r.shape3,r.scale,true);
  CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-10));
  CHECK(ll>=r.stop_detail.loglik_lower-1e-8);
  CHECK(ll<=r.stop_detail.loglik_upper+1e-8);
  for(const auto& c:r.stop_detail.candidates) {
   REQUIRE(cppunivariate::valid_stop_coefficients(c.model,c.coef));
   double value=0;
   for(double v:x) value+=c.model==cppunivariate::StopModel::None?
    cppunivariate::distributions::dgb2(v,c.coef[0],c.coef[1],c.coef[2],c.coef[3],true):
    c.model==cppunivariate::StopModel::Trgamma?
    cppunivariate::distributions::dtrgamma(v,c.coef[0],c.coef[1],c.coef[2],true):
    cppunivariate::distributions::dinvtrgamma(v,c.coef[0],c.coef[1],c.coef[2],true);
   CHECK(value>=c.loglik_lower-1e-8);CHECK(value<=c.loglik_upper+1e-8);
  }
 }
}

TEST_CASE("GB2 held power transports gamma faces and exhausted budgets") {
 for(bool inverse:{false,true}) {
  const std::vector<double>x(8,2);
  const auto r=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,300,inverse?2:NAN,2,inverse?NAN:2,NAN);
  REQUIRE(r.status==(inverse?cppunivariate::Gb2Status::InvTrGammaBoundary:cppunivariate::Gb2Status::TrGammaBoundary));
  CHECK(r.stop_detail.limit_model==(inverse?cppunivariate::StopModel::InvTrgamma:cppunivariate::StopModel::Trgamma));
  REQUIRE(r.stop_detail.limit_coef.size()==3);
  CHECK(r.stop_detail.limit_coef[0]==2);CHECK(r.stop_detail.limit_coef[1]==2);
  double ll=0;for(double v:x)ll+=inverse?
   cppunivariate::distributions::dinvtrgamma(v,2,2,r.stop_detail.limit_coef[2],true):
   cppunivariate::distributions::dtrgamma(v,2,2,r.stop_detail.limit_coef[2],true);
  CHECK(ll==doctest::Approx(r.logLik).epsilon(1e-12));
 }
 const std::vector<double>wide{1e-300,1e300};
 CHECK(cppunivariate::gb2_mle(mltest::as_span(wide),1e-7,300,2,10,NAN,NAN).status==cppunivariate::Gb2Status::NumericalFailure);
 const std::vector<double>ordinary{.03,.1,.3,.7,1,2,5,20};
 const auto stopped=cppunivariate::gb2_mle(mltest::as_span(ordinary),1e-7,1,2,2,NAN,NAN);
 CHECK(stopped.status!=cppunivariate::Gb2Status::Ok);
 CHECK(stopped.iterations<=128);
}

TEST_CASE("GB2 held-power nonunique profiles retain both separated witnesses") {
 for(double t:{5.6463,5.646438395028704,5.6466}) {
  std::vector<double>x{1.9901008294052028e-5,2.209857117039297e-5,.0002450722434289249,
   12.288342947780086,942.976056905759,16.239512580216314,2.898690421966857,
   1.2684036923492177,400970.63293690106*std::exp(t)};
  for(double& v:x)v=std::sqrt(v);
  const auto r=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,1000,NAN,2,.22690393135372058,NAN);
  if(t==5.646438395028704) {
   REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
   CHECK_FALSE(r.certified);
   bool left=false,right=false;
   for(const auto& c:r.stop_detail.candidates)if(c.model==cppunivariate::StopModel::None) {
    REQUIRE(c.coef.size()==4);left=left||c.coef[0]<.12;right=right||c.coef[0]>.12;
   }
   CHECK(left);CHECK(right);
  } else {
   REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
   CHECK((r.shape1>.12)==(t<5.6464));
  }
 }
}

TEST_CASE("GB2 unresolved reciprocal certificate does not select an empty gamma face") {
 const std::vector<double>x{.03,.1,.3,.7,1,2,5,20};
 const auto r=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,1,2,2,NAN,NAN);
 REQUIRE(r.status!=cppunivariate::Gb2Status::Ok);
 CHECK(r.stop_detail.limit_coef.empty());
 CHECK(r.stop_detail.limit_model==cppunivariate::StopModel::None);
}

TEST_CASE("GB2 held-power transport rejects overflowing Jacobian arithmetic") {
 const std::vector<double>x(2,1e300);
 const auto r=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,300,NAN,1e308,2,NAN);
 CHECK(r.status==cppunivariate::Gb2Status::NumericalFailure);
 CHECK(r.stop_detail.limit_coef.empty());
}

TEST_CASE("GB2 held beta shapes certify power and scale coefficient accuracy") {
 const std::vector<double>x{.03,.1,.3,.7,1,2,5,20};
 for(int subset=0;subset<3;++subset) {
  const auto r=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,300,2,subset==2?2:NAN,3,subset==1?1:NAN);
  REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
  REQUIRE(r.stop_detail.candidates.size()==1);
  double ll=0;for(double v:x)ll+=cppunivariate::distributions::dgb2(v,r.shape1,r.shape2,r.shape3,r.scale,true);
  CHECK(ll>=r.stop_detail.loglik_lower);CHECK(ll<=r.stop_detail.loglik_upper);
  if(subset==0) {
   CHECK(r.shape2==doctest::Approx(.50129405191849408).epsilon(1e-10));
   CHECK(r.scale==doctest::Approx(.28999624187971024).epsilon(1e-10));
  }
  auto y=x;for(double&v:y)v=7/v;
  const auto s=cppunivariate::gb2_mle(mltest::as_span(y),1e-7,300,3,subset==2?2:NAN,2,subset==1?7:NAN);
  REQUIRE(s.status==cppunivariate::Gb2Status::Ok);
  CHECK(s.shape2==doctest::Approx(r.shape2).epsilon(1e-10));
  CHECK(s.scale==doctest::Approx(7/r.scale).epsilon(1e-10));
 }
 const auto low=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,1,2,NAN,3,NAN);
 CHECK(low.status==cppunivariate::Gb2Status::NumericalFailure);
 CHECK_FALSE(low.stop_detail.budget_exhausted);
 CHECK(low.stop_detail.candidates.size()==1);
 CHECK(cppunivariate::gb2_mle(mltest::as_span(x),1e-30,300,2,NAN,3,NAN).status!=cppunivariate::Gb2Status::Ok);
}

TEST_CASE("GB2 subunit held beta shape certifies strict endpoint domination") {
 const std::vector<double>x{.1,.2,.4,.8};
 for(bool inverse:{false,true}) {
  auto y=x;if(inverse)for(double&v:y)v=1/v;
  const auto r=cppunivariate::gb2_mle(mltest::as_span(y),1e-7,1,inverse?NAN:.4,NAN,inverse?.4:NAN,1);
  REQUIRE(r.status==(inverse?cppunivariate::Gb2Status::ParetoBoundary:cppunivariate::Gb2Status::PowerBoundary));
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  CHECK(r.stop_detail.limit_coef[inverse?1:0]==1);
  double sum=0;for(double v:x)sum-=std::log(v);
  CHECK(r.stop_detail.limit_coef[inverse?0:1]==doctest::Approx(4/sum).epsilon(1e-14));
  for(double a:{1.,10.,100.}) {
   const double shape=r.stop_detail.limit_coef[inverse?0:1]/a;
   double ll=0;for(double v:y)ll+=cppunivariate::distributions::dgb2(v,inverse?shape:.4,a,inverse?.4:shape,1,true);
   CHECK(ll<r.stop_detail.loglik_upper);
   if(a==100)CHECK(r.logLik-ll<.1);
  }
 }
 // A tie or a sample crossing the endpoint is outside this theorem.
 const std::vector<double>cross{.5,2};
 const auto r=cppunivariate::gb2_mle(mltest::as_span(cross),1e-7,1,.4,NAN,NAN,1);
 CHECK(r.stop_detail.limit_model!=cppunivariate::StopModel::Power);
}

TEST_CASE("GB2 held beta shapes reject subnormal scale coefficient spacing") {
 std::vector<double>x{.03,.1,.3,.7,1,2,5,20};
 for(double&v:x)v*=1e-320;
 const auto r=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,300,2,NAN,3,NAN);
 CHECK(r.status==cppunivariate::Gb2Status::NumericalFailure);
}

TEST_CASE("GB2 held beta shapes do not classify log collapse as constant data") {
 const std::vector<double>x{1e300,std::nextafter(1e300,INFINITY)};
 REQUIRE(x[0]!=x[1]);
 REQUIRE(std::log(x[0])==std::log(x[1]));
 CHECK(cppunivariate::gb2_mle(mltest::as_span(x),1e-7,300,2,NAN,3,NAN).status==cppunivariate::Gb2Status::NumericalFailure);
}

TEST_CASE("GB2 constant held-shape scale cannot hide an unrepresentable log ratio") {
 const std::vector<double>x(5,2);
 const auto r=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,300,10,1e-300,std::nextafter(10.,INFINITY),NAN);
 CHECK(r.status==cppunivariate::Gb2Status::NumericalFailure);
 for(bool held_power:{false,true}) {
  const auto s=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,300,2,held_power?3:NAN,3,held_power?NAN:1);
  CHECK(s.status==cppunivariate::Gb2Status::Ok);
  CHECK(s.stop_detail.candidates.size()==1);
 }
}

TEST_CASE("GB2 subunit held beta shape retains the exact endpoint tie penalty") {
 const std::vector<double>x{.25,.5,1,1};
 for(bool inverse:{false,true}) {
  auto y=x;if(inverse)for(double&v:y)v=1/v;
  const auto r=cppunivariate::gb2_mle(mltest::as_span(y),1e-7,1,inverse?NAN:.4,NAN,inverse?.4:NAN,1);
  REQUIRE(r.status==(inverse?cppunivariate::Gb2Status::ParetoBoundary:cppunivariate::Gb2Status::PowerBoundary));
  const double rate=4/(3*std::log(2.));
  double logs=0;for(double v:y)logs+=std::log(v);
  const double ll=4*std::log(rate)-4-logs-.8*std::log(2.);
  CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-12));
  CHECK(ll>=r.stop_detail.loglik_lower);CHECK(ll<=r.stop_detail.loglik_upper);
  double finite=0;for(double v:y)finite+=cppunivariate::distributions::dgb2(v,inverse?rate/1000:.4,1000,inverse?.4:rate/1000,1,true);
  CHECK(finite<ll);CHECK(ll-finite<.05);
 }
}

TEST_CASE("GB2 constant mixed-power finite regimes have certified finite maxima") {
 const std::vector<double>below(5,.5),above(5,2);
 const auto r=cppunivariate::gb2_mle(mltest::as_span(below),1e-7,300,2,NAN,NAN,1);
 const auto s=cppunivariate::gb2_mle(mltest::as_span(above),1e-7,300,NAN,NAN,2,1);
 REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
 REQUIRE(s.status==cppunivariate::Gb2Status::Ok);
 CHECK(r.shape2==doctest::Approx(s.shape2).epsilon(1e-7));
 CHECK(r.shape3==doctest::Approx(s.shape1).epsilon(1e-7));
 const double u=r.shape2*std::log(2.),p=r.shape3;
 CHECK(std::abs(1/p+1/(p+1)-std::log1p(std::exp(u)))<1e-6);
 CHECK(std::abs(1/u-p+(p+2)/(1+std::exp(u)))<1e-6);
 CHECK(r.stop_detail.loglik_lower>5*(-std::log(std::log(2.))-1+std::log(2.)));
 CHECK(r.stop_detail.loglik_upper-r.stop_detail.loglik_lower<1e-6);
}

TEST_CASE("GB2 held power cannot amplify collapsed logs into a certificate") {
 const std::vector<double>x{1e300,std::nextafter(1e300,INFINITY)};
 CHECK(cppunivariate::gb2_mle(mltest::as_span(x),1e-7,300,2,1e100,3,NAN).status==cppunivariate::Gb2Status::NumericalFailure);
}

TEST_CASE("GB2 held-scale mixed profiles certify both beta-shape orientations") {
 const std::vector<double>x{.03,.1,.3,.7,1,2,5,20};
 for(bool inverse:{false,true}) {
  const auto r=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,300,inverse?NAN:2,NAN,inverse?3:NAN,1);
  REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
  CHECK(r.shape2==doctest::Approx(inverse?.43155118384080654:.57834363213651174).epsilon(1e-7));
  CHECK((inverse?r.shape1:r.shape3)==doctest::Approx(inverse?3.3085427329308605:1.7944752190424211).epsilon(1e-7));
  double ll=0;for(double v:x)ll+=cppunivariate::distributions::dgb2(v,r.shape1,r.shape2,r.shape3,r.scale,true);
  CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-10));
  CHECK(r.stop_detail.loglik_lower<=r.stop_detail.loglik_upper);
  const auto low=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,1,inverse?NAN:2,NAN,inverse?3:NAN,1);
  CHECK(low.status!=cppunivariate::Gb2Status::Ok);
 }
}

TEST_CASE("GB2 endpoint ties can dominate even with held opposite shape above one") {
 const std::vector<double>x{.25,.5,1,1};
 const auto r=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,1,1.1,NAN,NAN,1);
 REQUIRE(r.status==cppunivariate::Gb2Status::PowerBoundary);
 const double rate=4/(3*std::log(2.));
 CHECK(r.logLik==doctest::Approx(4*std::log(rate)-4+3*std::log(2.)-2.2*std::log(2.)).epsilon(1e-12));
}

TEST_CASE("GB2 mixed-power coefficient certificates meet independent golden precision") {
 const std::vector<double>x{.03,.1,.3,.7,1,2,5,20};
 for(bool inverse:{false,true}) {
  const auto r=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,300,inverse?NAN:2,NAN,inverse?3:NAN,1);
  REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
  CHECK(r.shape2==doctest::Approx(inverse?.431551183840810324:.578343632137284815).epsilon(5e-10));
  CHECK((inverse?r.shape1:r.shape3)==doctest::Approx(inverse?3.308542732930816109:1.794475219041363996).epsilon(5e-10));
 }
 const std::vector<double>constant(5,.5);
 const auto r=cppunivariate::gb2_mle(mltest::as_span(constant),1e-7,300,2,NAN,NAN,1);
 REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
 CHECK(r.shape2==doctest::Approx(4.929391514647968975).epsilon(5e-10));
 CHECK(r.shape3==doctest::Approx(.3679117637748129625).epsilon(5e-10));
}

TEST_CASE("gb2: unit-shape delegation preserves the inverse-Burr uncertified flag") {
  const double e=std::numeric_limits<double>::epsilon();
  const std::vector<double> data{1,1+e,1+2*e,1+3*e,1+4*e};
  const auto fit=cppunivariate::gb2_mle(mltest::as_span(data),1e-10,100,1,NAN,.855,NAN);
  REQUIRE(fit.status==cppunivariate::Gb2Status::Ok);
  CHECK_FALSE(fit.certified);
  CHECK(std::isfinite(fit.logLik));
}

TEST_CASE("gb2 differentiated log beta retains a small-shape score beside a 1e18 shape") {
 const std::array<double,4> p{{std::log(.7),std::log(1e18),0,0}};
 auto z=cppunivariate::local_accuracy::variables(p);
 auto f=cppunivariate::local_accuracy::logbeta(cppunivariate::local_accuracy::exp(z[0]),cppunivariate::local_accuracy::exp(z[1]));
 CHECK(f.v==doctest::Approx(-28.75170492519331).epsilon(1e-13));
 CHECK(f.g[0]==doctest::Approx(-29.86658865931353).epsilon(1e-12));
 CHECK(f.g[1]==doctest::Approx(-0.7).epsilon(1e-12));
 CHECK(f.h[0]==doctest::Approx(-28.47790457253317).epsilon(1e-12).scale(1));
 CHECK(f.h[1]==doctest::Approx(-0.7).epsilon(1e-12).scale(1));
 CHECK(f.h[5]==doctest::Approx(1.05e-19).epsilon(1e-12).scale(1));
}

TEST_CASE("gb2 local accuracy keeps concentrated beta and gamma-limit coordinates") {
 {
 const std::vector<double> x{3.6286401871558374,3.629003123761874,3.6293660603679108,3.6297289969739475,3.6304548701860213};
 const auto r=cppunivariate::gb2_mle({x.data(),x.size()},1e-10,300,NAN,1.123220989970799,NAN,1.0079365599415882);
 REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
 CHECK(r.shape1==doctest::Approx(33132624.910532456).epsilon(1e-10));
 CHECK(r.shape2==doctest::Approx(1.123220989970799).epsilon(1e-10));
 CHECK(r.shape3==doctest::Approx(139708562.23074487).epsilon(1e-10));
 CHECK(r.scale==doctest::Approx(1.0079365599415882).epsilon(1e-10));
 }
 {
 const std::vector<double> x{0.26339769553123515,0.4562181912566571,0.5889751523246344,0.6968847982831573,0.7901930865937284,0.8735913273001527,0.9496965626030057,1.0220156829711253,1.1113791565452917,1.2432875885353978,1.4740804330726338,2.1259918711975656};
 const auto r=cppunivariate::gb2_mle({x.data(),x.size()},1e-10,300,NAN,1.123220989970799,NAN,NAN);
 REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
 CHECK(r.shape1==doctest::Approx(317.0723737123441).epsilon(1e-10));
 CHECK(r.shape2==doctest::Approx(1.123220989970799).epsilon(1e-10));
 CHECK(r.shape3==doctest::Approx(3.3230705772385902).epsilon(1e-10));
 CHECK(r.scale==doctest::Approx(56.56454728300938).epsilon(1e-10));
 }
}

TEST_CASE("gb2 centered large-shape kernel retains an ordinary held-scale estimate") {
 const std::vector<double> x{1e-6,2e-6,3e-6,4.9999999999999996e-6,9e-6};
 const auto r=cppunivariate::gb2_mle({x.data(),x.size()},1e-10,300,NAN,NAN,NAN,1.0079365599415882);
 REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
 CHECK(r.shape1==doctest::Approx(597.100007345069).epsilon(1e-10));
 CHECK(r.shape2==doctest::Approx(.19427423066143805).epsilon(1e-10));
 CHECK(r.shape3==doctest::Approx(51.063150568462).epsilon(1e-10));
}

TEST_CASE("GB2 transports the uncertified unit-p Burr result") {
 const std::vector<double> x{std::exp(-1.1),std::exp(-1.)};const auto d=mltest::as_span(x);
 const auto p=cppunivariate::burr_mle_holds(d,NAN,NAN,1,1e-8,1000);
 const auto r=cppunivariate::gb2_mle(d,1e-8,1000,NAN,NAN,1,1);
 REQUIRE(p.status==cppunivariate::BurrStatus::Ok);REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
 CHECK_FALSE(p.certified);CHECK(r.certified==p.certified);
 CHECK(r.shape1==p.shape1);CHECK(r.shape2==p.shape2);CHECK(r.scale==p.scale);
 CHECK(r.logLik==doctest::Approx(p.logLik).epsilon(1e-12));
}

// The exact binary64 product 1e8*1e-8 exceeds one, although it rounds to one.
TEST_CASE("gb2: exact power side at the support endpoint") {
  CHECK(cppunivariate::distributions::dgb2(0,1,1e8,1e-8,1,true) == -std::numeric_limits<double>::infinity());
}

// Independent 1600-digit exact-input reference; beta-smoke.py.
TEST_CASE("gb2: centered large-shape density") {
  CHECK(cppunivariate::distributions::dgb2(1.0,100000000.0,1,100000000.0,1,true)==
        doctest::Approx(7.944828247241538).epsilon(1e-10).scale(0));
}

TEST_CASE("gb2: portable small-shape complementary tail") {
 CHECK(cppunivariate::distributions::pgb2(5e-324,2.0,1.0,2e-08,1.0,false,true)==doctest::Approx(-11.116252837780058).epsilon(1e-10));
}

TEST_CASE("gb2: exact-input density from likelihood seam replay") {
 CHECK(cppunivariate::distributions::dgb2(0.7827803638567952,1642761704942913.5,7.824192388751234e-08,1642761640781011.5,1.0079365599415882,true)==doctest::Approx(-0.019107341623514542).epsilon(1e-10));
}

TEST_CASE("GB2 small-power delegate propagates native coefficient accuracy") {
  const std::vector<double> x{0.07853504032045648,1.8052817030312258,0.12054995594558136,0.05961693580046997,0.14214378925732452,0.46699577379446316,0.3941816843849057,0.14479889797301285,0.13492813217740945,0.08682208942239882,2.6691108495205547,0.25048745811949463,0.06576521164344847,0.09295818895722506,0.18625008795501544,0.11679827023309848,0.3968583792568256,0.16425058159935238,0.16545980657609644,0.2231156583477691,0.055960658522496536,0.06985316110810778,0.8328618407335308,0.10661167241044933,0.21890378545392292,0.026508936077348097,0.14351526939649442,0.6981722842289294,0.11046937793985945,0.1046580495300785,0.26313407851523785,0.3890843617835916,0.08650261545083472,0.28025233774673086,0.8884815824856529,1.524018361103345,0.084642660196654,0.6405821605598184,0.3324744078770744,0.44785370391512136,0.04181051568641651,0.2404439040760986,0.02582952407461531,0.3821417205995134,0.05140332538412839,0.016295185513419352,0.17370187268483633,0.1647940529836234,0.7124372971730193,0.042086503500102256,0.24430568489599463,0.06859277094070175,0.06904138709221884,0.815832205050862,0.5282718102159853,0.08181572724471921,2.0944026714447435,1.0502437293557096,0.07579904828755943,0.05434983549584179,1.9909394766284128,0.1906863440272058,0.5849868739285353,0.030470391614962816,0.06404929407285288,0.08380914514336918,1.6930664596676608,0.32096954036737496,0.06588427889787211,0.1027124289024075,0.05599823033210029,0.35983916015097844,0.07173457116343251,0.25330393765282,0.47262690847930916,0.10332626597896134,0.0201952833871855,0.6707901688536437,0.16858917843754093,0.32952631370983704,0.04465485731622312,0.10934541787301413,0.14389008987731416,0.5025690032906194,4.529005885777762,0.04538731336963416,2.760116314387518,0.04962399957369404,0.08388987561364676,44.601555292678334,0.0923886319299493,0.08119153808345454,0.1650332666991911,0.04887882704311675,0.2578977813531764,0.033145800870656675,0.07147306251644694,0.8122245761888282,0.10477521304718565,0.5292146234511944};
  const auto r=cppunivariate::gb2_mle(mltest::as_span(x),1e-10,64,NAN,.5,0.6611055385779123,NAN);
  REQUIRE(r.status==cppunivariate::Gb2Status::Ok);CHECK_FALSE(r.certified);
  // Independent high-precision stationary point of the original density.
  CHECK(r.shape1==doctest::Approx(903.42459848503567900853253819297095918584572070560521301785890537).scale(0.).epsilon(1e-10));
  CHECK(r.scale==doctest::Approx(625653.11035128516325077184133372236564421401924238062787607748625).scale(0.).epsilon(1e-10));
  CHECK(r.shape2==0.5);
  CHECK(r.shape3==0.6611055385779123);
}

TEST_CASE("Comparable beta shapes gb2 transport propagates small-power error") {
  const std::vector<double> x{1.3733876795328398e-88,2.937594362709469e-65,2.701378714528384e-52,1.2699376077132869e-42,1.3673899352578955e-34,1.5970627146986006e-27,4.555442832854519e-21,5.19854194459634e-15,3.3274256113905883e-09,0.0015450781141231093,647.2164681254823,300532638.97973114,192361629598590.03,2.195176268680371e+20,6.261494873039616e+26,7.313202870783376e+33,7.8744025999969e+41,3.701813428166952e+51,3.40414596614879e+64,7.281265260368941e+87};
  cppunivariate::Gb2Result r{20000.458802500446893780565764404618*(1+1e-5),1e-4,20000.45880248738,1.0000000065331644083491598702927446*(1+1e-5),0,0,cppunivariate::Gb2Status::Ok};r.certified=false;
  REQUIRE(cppunivariate::gb2_transport_accuracy(mltest::as_span(x),0,r,{{true,false,false,true}},1e-10,1000,true));
  CHECK(r.shape1==doctest::Approx(20000.458802500446893780565764404618).scale(0.).epsilon(1e-10));
  CHECK(r.scale==doctest::Approx(1.0000000065331644083491598702927446).scale(0.).epsilon(1e-10));
  CHECK(r.shape2==1e-4);CHECK(r.shape3==20000.45880248738);
}
