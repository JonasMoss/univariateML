#include "test_support.hpp"
#include "cppunivariate/distributions/expweibull/dpqr.hpp"
#include "cppunivariate/distributions/weibull/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <vector>

TEST_CASE("expweibull: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("expweibull",
    [](double x,const std::vector<double>& p,bool l){return cppunivariate::distributions::dexpweibull(x,p[0],p[1],p[2],l);},
    [](double x,const std::vector<double>& p,bool t,bool l){return cppunivariate::distributions::pexpweibull(x,p[0],p[1],p[2],t,l);},
    [](double x,const std::vector<double>& p,bool t,bool l){return cppunivariate::distributions::qexpweibull(x,p[0],p[1],p[2],t,l);},
    // At b=1000,c=1e200, a few ulps of h amplify to 2.7e-10
    // relative error in a natural probability of 1e-300.
    [](cppunivariate::distributions::StdRng<>& rng,const std::vector<double>& p){return cppunivariate::distributions::rexpweibull(rng,p[0],p[1],p[2]);},1e-10);
}
TEST_CASE("expweibull: sampler uses the analytic inverse and has uniform PIT") {
  cppunivariate::distributions::StdRng<> rng(179), control(179);
  double sum=0, squares=0;
  for(int i=0;i<10000;++i) {
    const double u=control.runif(0,1);
    const double x=cppunivariate::distributions::rexpweibull(rng,2,1.5,2.5);
    CHECK(x==cppunivariate::distributions::qexpweibull(u,2,1.5,2.5,true,false));
    const double f=cppunivariate::distributions::pexpweibull(x,2,1.5,2.5,true,false);
    sum+=f; squares+=f*f;
  }
  CHECK(std::abs(sum/10000-.5)<.012);
  CHECK(std::abs(squares/10000-1.0/3)<.012);
}
TEST_CASE("expweibull: endpoints invalid inputs and Weibull reduction") {
  const double inf=std::numeric_limits<double>::infinity();
  for(bool lower:{false,true}) for(bool log:{false,true}) {
    CHECK(cppunivariate::distributions::qexpweibull(log ? -inf : 0,2,3,4,lower,log)==(lower ? 0 : inf));
    CHECK(cppunivariate::distributions::qexpweibull(log ? 0 : 1,2,3,4,lower,log)==(lower ? inf : 0));
    CHECK(std::isnan(cppunivariate::distributions::qexpweibull(log ? .1 : 1.1,2,3,4,lower,log)));
    for(double x:{.01,1.,10.,100.})
      CHECK(cppunivariate::distributions::pexpweibull(x,2,3,1,lower,log)==doctest::Approx(cppunivariate::distributions::pweibull(x,3,2,lower,log)).epsilon(1e-12));
  }
  CHECK(cppunivariate::distributions::dexpweibull(0,2,2,.5,false)==.5);
  CHECK(cppunivariate::distributions::dexpweibull(0,2,2,.1,false)==inf);
  CHECK(cppunivariate::distributions::dexpweibull(0,2,2,1,false)==0);
  CHECK(cppunivariate::distributions::dexpweibull(0,2,1000,.001,false)==0);
  CHECK(std::isnan(cppunivariate::distributions::pexpweibull(1,0,2,3,true,false)));
  CHECK(cppunivariate::distributions::dexpweibull(-1,2,3,4,true)==-inf);
}
TEST_CASE("expweibull: underflowed powers and extreme shapes retain log tails") {
  CHECK(cppunivariate::distributions::dexpweibull(1e-200,1,2,.5,true)==doctest::Approx(0).epsilon(1e-12));
  CHECK(cppunivariate::distributions::pexpweibull(1000,1,1,1e300,false,true)==doctest::Approx(std::log(1e300)-1000).epsilon(1e-13));
  for(double b:{.01,1.,100.,1e200}) for(double c:{1e-200,.5,2.,1e200})
    for(bool lower:{false,true}) for(double lp:{-1e-300,-.01,-1.,-100.,-1000.,-10000.}) {
      const double x=cppunivariate::distributions::qexpweibull(lp,1,b,c,lower,true);
      CHECK_FALSE(std::isnan(x));
      if(x>0 && std::isfinite(x) && b<1e10) {
        const double back=cppunivariate::distributions::pexpweibull(x,1,b,c,lower,true);
        INFO("relative="<<(back/lp-1)<<" shape="<<b<<" exponent="<<c<<" lower="<<lower<<" lp="<<lp<<" x="<<x);
        const double left=cppunivariate::distributions::pexpweibull(std::nextafter(x,0.0),1,b,c,lower,true);
        const double right=cppunivariate::distributions::pexpweibull(std::nextafter(x,INFINITY),1,b,c,lower,true);
        CHECK((std::abs(back-lp)<=5e-10*std::abs(lp) ||
              (lp>=std::min(left,right) && lp<=std::max(left,right))));
      }
    }
}

TEST_CASE("expweibull: extreme reciprocal shapes rescue intermediate range") {
  // b*z overflows, but c*b*z and the limiting density remain finite.
  CHECK(cppunivariate::distributions::pexpweibull(1e-100,1,1e308,1e-308,true,true)==doctest::Approx(-100*std::log(10.0)).epsilon(1e-14));
  CHECK(std::abs(cppunivariate::distributions::dexpweibull(1e-100,1,1e308,1e-308,true))<1e-12);
  CHECK(cppunivariate::distributions::qexpweibull(-.5,1,1e308,1e-308,true,true)==doctest::Approx(std::exp(-.5)).epsilon(2e-13));
  // Close scale ratios must retain a one-ulp displacement before multiplying b.
  const double a=1e200, x=std::nextafter(a,INFINITY);
  const double lh=1e16*std::log1p((x-a)/a);
  CHECK(cppunivariate::distributions::pexpweibull(x,a,1e16,1,false,true)==doctest::Approx(-std::exp(lh)).epsilon(1e-14));
}

// mpmath, 100 digits, exact binary64 inputs:
// h=mp.mpf(1e-200)**2; lp=mp.mpf(c)*mp.log(-mp.expm1(-h));
// mp.log(-mp.expm1(lp)). Full values are retained in the job evidence.
TEST_CASE("expweibull: subnormal exponents preserve log survival") {
  CHECK(cppunivariate::distributions::pexpweibull(1e-200,1,2,
      std::numeric_limits<double>::denorm_min(),false,true)==
      doctest::Approx(-737.6145749290254).epsilon(2e-15));
  CHECK(cppunivariate::distributions::pexpweibull(1e-200,1,2,1e-320,false,true)==
      doctest::Approx(-730.001743898618).epsilon(2e-15));
}

TEST_CASE("expweibull: exact-ratio hazard meets natural probability tolerance") {
 CHECK(cppunivariate::distributions::pexpweibull(2.0122736097839593,2,1000,1e200,true,false)==
  doctest::Approx(1.0000000086536384e-300).epsilon(1e-10).scale(0));
}

#include "cppunivariate/distributions/expweibull/mle.hpp"
TEST_CASE("expweibull: all holds density agreement and conditional certificates") {
  std::vector<double> x;
  for(int i=0;i<100;++i)x.push_back(cppunivariate::distributions::qexpweibull((i+.5)/100,2,1.5,2.5,true,false));
  for(int mask=0;mask<8;++mask) {
    const auto r=cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,100,mask&1?2:NAN,mask&2?1.5:NAN,mask&4?2.5:NAN);
    REQUIRE(r.status==cppunivariate::ExpweibullStatus::Ok);
    CHECK(r.certified==bool((mask&4) || (mask&3)==3));
    if(mask&1)CHECK(r.scale==2);
    if(mask&2)CHECK(r.shape==1.5);
    if(mask&4)CHECK(r.exponent==2.5);
    double ll=0;for(double v:x)ll+=cppunivariate::distributions::dexpweibull(v,r.scale,r.shape,r.exponent,true);
    CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-11));
  }
}
TEST_CASE("expweibull: Weibull submodel and rescaling") {
  const std::vector<double> x{.4,.7,1.2,1.3,2.4,2.8,4.,6.};
  const auto w=cppunivariate::weibull_mle(mltest::as_span(x),1e-10,100);
  const auto r=cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,100,NAN,NAN,1);
  REQUIRE(r.status==cppunivariate::ExpweibullStatus::Ok);
  CHECK(r.certified);
  CHECK(r.shape==doctest::Approx(w.shape).epsilon(1e-10));
  CHECK(r.scale==doctest::Approx(w.scale).epsilon(1e-10));
  for(double factor:{1e-100,1e100}) {
    auto y=x;for(double& v:y)v*=factor;
    const auto s=cppunivariate::expweibull_mle(mltest::as_span(y),1e-10,100,NAN,NAN,1);
    REQUIRE(s.status==cppunivariate::ExpweibullStatus::Ok);
    CHECK(s.scale/factor==doctest::Approx(r.scale).epsilon(1e-10));
    CHECK(s.shape==doctest::Approx(r.shape).epsilon(1e-10));
    CHECK(std::abs(s.logLik-r.logLik+x.size()*std::log(factor))<1e-9);
  }
}
TEST_CASE("expweibull: concentration depends on free coordinates") {
  const std::vector<double> x(5,2.);
  for(int mask:{0,2,4})CHECK(cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,100,mask&1?2:NAN,mask&2?1.5:NAN,mask&4?2.5:NAN).status==cppunivariate::ExpweibullStatus::Unbounded);
  CHECK(cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,100,2).status==cppunivariate::ExpweibullStatus::Unbounded);
  CHECK(cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,100,1).status==cppunivariate::ExpweibullStatus::Unbounded);
  for(int mask:{3,6,7}) {
    const auto r=cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,100,mask&1?2:NAN,mask&2?1.5:NAN,mask&4?2.5:NAN);
    CHECK(r.status==cppunivariate::ExpweibullStatus::Ok);
  }
  // A held scale above constant data has a finite unattained envelope.
  CHECK(cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,100,3).status==cppunivariate::ExpweibullStatus::PowerBoundary);
}
TEST_CASE("expweibull: held-endpoint power envelope including repeated ties") {
  for(const std::vector<double>& x:{std::vector<double>{1,1.5,2},std::vector<double>{1,2,3,3}}) {
    const auto r=cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,100,3);
    const bool tied=x.back()==3;
    REQUIRE(r.status==(tied?cppunivariate::ExpweibullStatus::PowerBoundaryTied:cppunivariate::ExpweibullStatus::PowerBoundary));
    CHECK(r.certified);
    double gap=0,logs=0;int ties=0;for(double v:x){gap+=std::log(3/v);logs+=std::log(v);ties+=v==3;}
    const double beta=x.size()/gap;
    CHECK(r.logLik==doctest::Approx(x.size()*(std::log(beta)-1)-logs-ties*std::log(std::expm1(1.))).epsilon(1e-12));
    CHECK(r.stop_detail.limit_model==(tied?cppunivariate::StopModel::None:cppunivariate::StopModel::Power));
    for(double b:{10.,100.,1000.}) {
      double ll=0;for(double v:x)ll+=cppunivariate::distributions::dexpweibull(v,3,b,beta/b,true);
      CHECK(ll<=r.logLik+1e-10);
      if(b==10.)CHECK(ll<r.logLik);
    }
  }
}
TEST_CASE("expweibull: invalid inputs tight tolerance and exhausted search") {
  const std::vector<double> x{.5,1,2,3,5};
  CHECK(cppunivariate::expweibull_mle({}).status==cppunivariate::ExpweibullStatus::InvalidData);
  CHECK(cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,100,-1).status==cppunivariate::ExpweibullStatus::InvalidFixed);
  CHECK(cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,1).status!=cppunivariate::ExpweibullStatus::Ok);
  CHECK(cppunivariate::expweibull_mle(mltest::as_span(x),1e-30).status!=cppunivariate::ExpweibullStatus::Ok);
}
TEST_CASE("expweibull: golden") {
  const auto g=mltest::load_distribution_golden("expweibull");
  const auto r=cppunivariate::expweibull_mle(mltest::as_span(g.data));
  REQUIRE(r.status==cppunivariate::ExpweibullStatus::Ok);
  CHECK(r.scale==doctest::Approx(g.estimates.at("scale")).epsilon(1e-10));
  CHECK(r.shape==doctest::Approx(g.estimates.at("shape")).epsilon(1e-10));
  CHECK(r.exponent==doctest::Approx(g.estimates.at("exponent")).epsilon(1e-10));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
}
TEST_CASE("expweibull: held shape uses its natural scale on nearly constant data") {
  std::vector<double> x;
  for(double z:{0.,1e-6,2e-6,3e-6,5e-6})x.push_back(std::exp(z));
  const auto r=cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,100,NAN,1.5,2.5);
  REQUIRE(r.status==cppunivariate::ExpweibullStatus::Ok);
  CHECK(r.certified);
  double ll=0;for(double v:x)ll+=cppunivariate::distributions::dexpweibull(v,r.scale,r.shape,r.exponent,true);
  CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-10));
}
TEST_CASE("expweibull: single-free shape with remote held scale") {
  for(double value:{1e-100,1e100}) {
    const std::vector<double> x(4,value);
    const auto r=cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,100,1,NAN,2.5);
    REQUIRE(r.status==cppunivariate::ExpweibullStatus::Ok);
    CHECK(r.certified);
    double ll=0;for(double v:x)ll+=cppunivariate::distributions::dexpweibull(v,r.scale,r.shape,r.exponent,true);
    CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-10));
  }
}
TEST_CASE("expweibull: held coordinates do not constrain exponent accuracy") {
  const std::vector<double> x{.5,1,2,4};
  const auto r=cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,100,2,1e-8);
  REQUIRE(r.status==cppunivariate::ExpweibullStatus::Ok);
  double sum=0;for(double v:x)sum+=std::log(-std::expm1(-std::pow(v/2,1e-8)));
  CHECK(r.exponent==doctest::Approx(-static_cast<double>(x.size())/sum).epsilon(1e-10));
  CHECK(r.scale==2);
  CHECK(r.shape==1e-8);
}
TEST_CASE("expweibull: public likelihood restoration at adjacent observations") {
  const std::vector<double> x{1,std::nextafter(1.,2.),std::nextafter(std::nextafter(1.,2.),2.)};
  const auto r=cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,100,NAN,NAN,1);
  if(r.status==cppunivariate::ExpweibullStatus::Ok) {
    double ll=0;for(double v:x)ll+=cppunivariate::distributions::dexpweibull(v,r.scale,r.shape,r.exponent,true);
    CHECK(std::abs(r.logLik-ll)<=1e-10*std::max(1.,std::abs(r.logLik)));
  } else CHECK(r.status==cppunivariate::ExpweibullStatus::AccuracyFailure);
}
TEST_CASE("expweibull: tiny held Weibull shape retains the closed scale equation") {
  const std::vector<double> x{.5,1,2,4};
  const auto w=cppunivariate::weibull_mle(mltest::as_span(x),1e-10,100,NAN,1e-8,NAN);
  const auto r=cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,100,NAN,1e-8,1);
  REQUIRE(w.status==cppunivariate::WeibullStatus::Ok);
  REQUIRE(r.status==cppunivariate::ExpweibullStatus::Ok);
  CHECK(r.scale==w.scale);
  CHECK(r.shape==1e-8);
  CHECK(r.certified);
}
TEST_CASE("expweibull: inconclusive arithmetic allowance retains a verified local root") {
  // Frozen binary64 inputs; independent 100-digit root in the evaluation data.
  const std::vector<double> x{1,1.0000010000005,1.000002000002,1.0000030000044999,1.0000050000125};
  const auto r=cppunivariate::expweibull_mle(mltest::as_span(x));
#if !defined(BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS) && LDBL_MANT_DIG>DBL_MANT_DIG
  REQUIRE(r.status==cppunivariate::ExpweibullStatus::Ok);
#endif
  if(r.status==cppunivariate::ExpweibullStatus::Ok) {
    CHECK_FALSE(r.certified);
    CHECK(r.scale==doctest::Approx(std::exp(-.0001425075388296187915)).epsilon(1e-10));
    CHECK(r.shape==doctest::Approx(std::exp(10.066943359084830593)).epsilon(1e-10));
    CHECK(r.exponent==doctest::Approx(std::exp(29.646289260797796367)).epsilon(1e-10));
  } else {
    CHECK(r.status==cppunivariate::ExpweibullStatus::AccuracyFailure);
    CHECK(r.stop_detail.accuracy_stop==cppunivariate::AccuracyStop::Conditioning);
    CHECK_FALSE(r.stop_detail.candidates.empty());
  }
}
TEST_CASE("expweibull: relaxed coefficient tolerance is honored") {
  const std::vector<double> x{1,1.0000010000005,1.000002000002,1.0000030000044999,1.0000050000125};
  const auto r=cppunivariate::expweibull_mle(mltest::as_span(x),1e-6);
  REQUIRE(r.status==cppunivariate::ExpweibullStatus::Ok);
  CHECK_FALSE(r.certified);
  CHECK(r.scale==doctest::Approx(std::exp(-.0001425075388296187915)).epsilon(1e-6));
  CHECK(r.shape==doctest::Approx(std::exp(10.066943359084830593)).epsilon(1e-6));
  CHECK(r.exponent==doctest::Approx(std::exp(29.646289260797796367)).epsilon(1e-6));
}
TEST_CASE("expweibull: subnormal held Weibull shape has a scale enclosure") {
  const std::vector<double> x{.5,1,2,4};
  const double b=std::numeric_limits<double>::denorm_min();
  const auto r=cppunivariate::expweibull_mle(mltest::as_span(x),1e-10,100,NAN,b,1);
  REQUIRE(r.status==cppunivariate::ExpweibullStatus::Ok);
  CHECK(r.scale==doctest::Approx(std::sqrt(2.)).epsilon(1e-10));
  CHECK(r.shape==b);
  CHECK(r.exponent==1);
  CHECK(r.certified);
}
TEST_CASE("expweibull: exact scale roots and sub-spacing requests") {
  const std::vector<double> constant(5,2.5),fractional{1,1,2,2,2};
  const auto exact=cppunivariate::expweibull_mle(mltest::as_span(constant),1e-30,100,NAN,1.5,1);
  REQUIRE(exact.status==cppunivariate::ExpweibullStatus::Ok);
  CHECK(exact.scale==2.5);
  CHECK(exact.certified);
  const auto fraction=cppunivariate::expweibull_mle(mltest::as_span(fractional),1e-30,100,NAN,1,1);
  CHECK(fraction.status==cppunivariate::ExpweibullStatus::AccuracyFailure);
  CHECK(fraction.stop_detail.accuracy_stop==cppunivariate::AccuracyStop::Tolerance);
}
