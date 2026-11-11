#include "test_support.hpp"
#include "cppunivariate/distributions/gengamma/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>

TEST_CASE("gengamma: normal seam and reflection") {
  for (double w : {-6.,-1.,0.,1.,6.}) {
    const double x=std::exp(w);
    const double center=cppunivariate::distributions::pgengamma(x,0,1,0,true,true);
    for (double q : {1e-8,1e-6,1e-4}) {
      const double positive=cppunivariate::distributions::pgengamma(x,0,1,q,true,true);
      const double negative=cppunivariate::distributions::pgengamma(x,0,1,-q,true,true);
      CHECK(std::abs(positive-center)<q*100);
      CHECK(std::abs(negative-center)<q*100);
      const double reflected=cppunivariate::distributions::pgengamma(1/x,0,1,-q,false,true);
      CHECK(positive==doctest::Approx(reflected).epsilon(1e-11));
    }
  }
  CHECK(cppunivariate::distributions::pgengamma(std::exp(1.),0,1,1e-8,true,false)==
        doctest::Approx(0.841344747278396568).epsilon(2e-12));
}

TEST_CASE("gengamma: seeded sampler matches CDF at fixed landmarks") {
  for (double q : {-1.,-0.01,-1e-8,0.,1e-8,0.01,1.}) {
    cppunivariate::distributions::StdRng<> rng(70191);
    constexpr int n=12000;
    int count[3]={0,0,0};
    const double cut[3]={std::exp(-1.),1.,std::exp(1.)};
    for (int i=0;i<n;++i) {
      const double x=cppunivariate::distributions::rgengamma(rng,0,1,q);
      REQUIRE(std::isfinite(x));
      CHECK(x>0);
      for(int j=0;j<3;++j) if(x<=cut[j]) ++count[j];
    }
    for(int j=0;j<3;++j) {
      const double p=cppunivariate::distributions::pgengamma(cut[j],0,1,q,true,false);
      const double z=std::abs(static_cast<double>(count[j])/n-p);
      CHECK(z<5*std::sqrt(p*(1-p)/n)+1.0/n);
    }
  }
}

TEST_CASE("gengamma: high precision density, tails, quantiles and sampling") {
  mltest::check_dpqr_truth("gengamma",
    [](double x,const std::vector<double> &t,bool log) {
      return cppunivariate::distributions::dgengamma(x,t[0],t[1],t[2],log);
    },
    [](double x,const std::vector<double> &t,bool lower,bool log) {
      return cppunivariate::distributions::pgengamma(x,t[0],t[1],t[2],lower,log);
    },
    [](double p,const std::vector<double> &t,bool lower,bool log) {
      return cppunivariate::distributions::qgengamma(p,t[0],t[1],t[2],lower,log);
    },
    [](auto &rng,const std::vector<double> &t) {
      return cppunivariate::distributions::rgengamma(rng,t[0],t[1],t[2]);
    });
}

TEST_CASE("gengamma: very large signed shape retains log gamma tails") {
  const double q=1e200, mu=-2*std::log(q), sigma=q, loga=mu;
  // These coordinates make X a unit-scale Gamma(q^-2) variate.  At x=1,
  // its upper tail tends to q^-2 * E1(1), with relative error O(q^-2).
  const double log_e1_at_one=std::log(0.21938393439552027368);
  const double upper=cppunivariate::distributions::pgengamma(1,mu,sigma,q,false,true);
  CHECK(upper==doctest::Approx(loga+log_e1_at_one).epsilon(1e-13));
  const double density=cppunivariate::distributions::dgengamma(1,mu,sigma,q,true);
  CHECK(density==doctest::Approx(loga-1).epsilon(1e-13));
  const double x=cppunivariate::distributions::qgengamma(upper,mu,sigma,q,false,true);
  CHECK(x==doctest::Approx(1).epsilon(1e-10));
  const double reflected=cppunivariate::distributions::pgengamma(1,-mu,sigma,-q,true,true);
  CHECK(reflected==doctest::Approx(upper).epsilon(1e-13));
  CHECK(cppunivariate::distributions::qgengamma(upper,-mu,sigma,-q,true,true)==
        doctest::Approx(1).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qgengamma(-10000,0,1e-204,q,true,true)==
        doctest::Approx(std::exp(-1.)).epsilon(1e-11));
}

TEST_CASE("gengamma: numerical transition and subnormal shape") {
  for (double sign : {-1.,1.}) {
    const double q=sign*0.25;
    const double next=std::nextafter(q,sign*1.);
    for (double w : {-6.,0.,1.,6.}) {
      const double x=std::exp(w);
      const double a=cppunivariate::distributions::pgengamma(x,0,1,q,true,true);
      const double b=cppunivariate::distributions::pgengamma(x,0,1,next,true,true);
      CHECK(a==doctest::Approx(b).epsilon(2e-11));
    }
    const double tiny=sign*1e-320;
    const double p=cppunivariate::distributions::pgengamma(std::exp(1.),0,1,tiny,true,false);
    CHECK(p==doctest::Approx(0.8413447460685429486).epsilon(1e-12));
    cppunivariate::distributions::StdRng<> rng(7432);
    CHECK(std::isfinite(cppunivariate::distributions::rgengamma(rng,0,1,tiny)));
  }
}

TEST_CASE("gengamma: huge shape and tiny scale retain standardized tails") {
  const double q=1e200, sigma=1e-200, mu=1e200;
  const double lower=cppunivariate::distributions::pgengamma(1,mu,sigma,q,true,true);
  const double density=cppunivariate::distributions::dgengamma(1,mu,sigma,q,true);
  CHECK(lower==doctest::Approx(-1e200).epsilon(1e-13));
  CHECK(density==doctest::Approx(-1e200).epsilon(1e-13));
}

TEST_CASE("gengamma: large-shape transition") {
  const double q=1e10, next=std::nextafter(q,cppunivariate::special::kPosInf);
  for (double shape : {q,next}) {
    const double mu=-2*std::log(shape), sigma=shape;
    const double upper=cppunivariate::distributions::pgengamma(1,mu,sigma,shape,false,true);
    CHECK(upper==doctest::Approx(mu+std::log(0.21938393439552027368)).epsilon(1e-12));
  }
}

TEST_CASE("gengamma: small gamma shape preserves the upper tail below gamma underflow") {
  const double shape=1e8, mu=-2*std::log(shape), sigma=shape;
  const double x=std::exp(-40.);
  const double lp=cppunivariate::distributions::pgengamma(x,mu,sigma,shape,false,true);
  const double reference=mu+std::log(40.-0.57721566490153286061);
  CHECK(lp==doctest::Approx(reference).epsilon(1e-12));
  CHECK(cppunivariate::distributions::qgengamma(lp,mu,sigma,shape,false,true)==
        doctest::Approx(x).epsilon(1e-9));
}

TEST_CASE("gengamma: overflowed coordinates and exact zero endpoint") {
  const double q=1e8, sigma=1e-200, mu=5e115;
  const double expected=-5e307;
  CHECK(cppunivariate::distributions::pgengamma(1,mu,sigma,q,true,true)==
        doctest::Approx(expected).epsilon(1e-13));
  CHECK(cppunivariate::distributions::dgengamma(1,mu,sigma,q,true)==
        doctest::Approx(expected).epsilon(1e-13));
  const double tiny=std::ldexp(1.,-600), large=std::ldexp(1.,600);
  CHECK(cppunivariate::distributions::dgengamma(0,0,large,tiny,true)==
        cppunivariate::special::kPosInf);
}

TEST_CASE("gengamma: overflowed shape product retains finite log tails") {
  for (double sign : {-1.,1.}) {
    const double Q=sign*1e8, mu=sign*5e307;
    const double expected=-5e299;
    CHECK(cppunivariate::distributions::dgengamma(1,mu,1,Q,true)==
          doctest::Approx(expected).epsilon(1e-13));
    CHECK(cppunivariate::distributions::pgengamma(1,mu,1,Q,Q>0,true)==
          doctest::Approx(expected).epsilon(1e-13));
  }
}

TEST_CASE("gengamma: large-shape inverse crosses the E1 transition") {
  const double Q=1e12, mu=-2*std::log(Q), sigma=Q;
  for (double lx : {-36.2,-36.1,-36.0,-35.9,-35.8}) {
    const double x=std::exp(lx);
    const double lp=cppunivariate::distributions::pgengamma(x,mu,sigma,Q,false,true);
    CHECK(cppunivariate::distributions::qgengamma(lp,mu,sigma,Q,false,true)==
          doctest::Approx(x).epsilon(1e-9));
  }
}

TEST_CASE("gengamma: log-location and log-scale transformation") {
  const double mu=.75, sigma=1.3;
  for (double Q : {-3.,-.01,0.,.01,3.}) {
    for (double w : {-3.,0.,2.}) {
      const double x=std::exp(mu+sigma*w), base=std::exp(w);
      const double d=cppunivariate::distributions::dgengamma(x,mu,sigma,Q,true);
      const double d0=cppunivariate::distributions::dgengamma(base,0,1,Q,true);
      CHECK(d==doctest::Approx(d0+w-std::log(sigma)-std::log(x)).epsilon(1e-11));
      for (bool lower : {false,true}) {
        const double p=cppunivariate::distributions::pgengamma(x,mu,sigma,Q,lower,true);
        const double p0=cppunivariate::distributions::pgengamma(base,0,1,Q,lower,true);
        CHECK(p==doctest::Approx(p0).epsilon(1e-11));
        if (p<0 && std::isfinite(p))
          CHECK(cppunivariate::distributions::qgengamma(p,mu,sigma,Q,lower,true)==
                doctest::Approx(x).epsilon(1e-9));
      }
    }
  }
}

TEST_CASE("gengamma: gamma quantile keeps sub-ulp correction through scaling") {
  for (double sigma : {1e6,std::ldexp(1.,1000)}) {
    const double mu=36*sigma;
    const double correction=0.5*std::exp(-36.)*sigma;
    const double upper=cppunivariate::distributions::qgengamma(-36,mu,sigma,1,true,true);
    if (std::isinf(std::exp(correction))) CHECK(upper==cppunivariate::special::kPosInf);
    else CHECK(upper==doctest::Approx(std::exp(correction)).epsilon(1e-13));
    CHECK(cppunivariate::distributions::qgengamma(-36,-mu,sigma,-1,false,true)==
          doctest::Approx(std::exp(-correction)).epsilon(1e-13));
  }
}

TEST_CASE("gengamma: seeded log moments") {
  constexpr int n=12000;
  constexpr double euler=0.57721566490153286061;
  constexpr double pi2_over_six=1.6449340668482264365;
  for (double Q : {-1.,-.01,0.,.01,1.}) {
    cppunivariate::distributions::StdRng<> rng(20261110);
    double sum=0, sum2=0;
    for (int i=0;i<n;++i) {
      const double v=std::log(cppunivariate::distributions::rgengamma(rng,0,1,Q));
      REQUIRE(std::isfinite(v));
      sum+=v; sum2+=v*v;
    }
    const double mean=std::abs(Q)==1 ? -euler/Q : -Q/2-Q*Q*Q/12;
    const double variance=std::abs(Q)==1 ? pi2_over_six : 1+Q*Q/2;
    CHECK(std::abs(sum/n-mean)<6*std::sqrt(variance/n));
    CHECK(std::abs(sum2/n-(variance+mean*mean))<(std::abs(Q)==1 ? .25 : .12));
  }
}
