#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/tlambda/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <vector>
TEST_CASE("tlambda: logistic, uniform and log-tail inversion") {
  for(double l:std::vector<double>{-1,-.5,0,1e-14,-1e-14,.14,.5,1,2,5}) {
    for(double lp:std::vector<double>{-100,-30,-5,-1}) {
      const double q=cppunivariate::distributions::qtlambda(lp,0,1,l,false,true);
      if(l<=0 || lp>-30) CHECK(cppunivariate::distributions::ptlambda(q,0,1,l,false,true)==doctest::Approx(lp).epsilon(2e-8));
      CHECK(cppunivariate::distributions::dtlambda(-q,0,1,l,true)==cppunivariate::distributions::dtlambda(q,0,1,l,true));
    }
  }
  CHECK(cppunivariate::distributions::ptlambda(100,0,1,0,true,true)/(-std::exp(-100))==doctest::Approx(1.0).epsilon(1e-12));
  CHECK(cppunivariate::distributions::dtlambda(0,0,1,0)==doctest::Approx(.25));
  CHECK(cppunivariate::distributions::dtlambda(1,0,1,1)==.5);
  CHECK(cppunivariate::distributions::dtlambda(.5,0,1,2)==1);
  CHECK(cppunivariate::distributions::dtlambda(2,0,1,.5)==0);
}
TEST_CASE("tlambda: golden and fixed shape") {
  auto g=mltest::load_distribution_golden("tlambda");
  auto f=cppunivariate::tlambda_mle(mltest::as_span(g.data));
  REQUIRE(f.status==cppunivariate::TlambdaStatus::Ok);
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(1e-9));
  CHECK(f.lambda==doctest::Approx(g.estimates.at("lambda")).epsilon(1e-6));
  double norm2=0;
  for(int j=0;j<3;++j) {
    std::array<double,3> a{f.mu,f.sigma,f.lambda},b=a;
    const double h=1e-5*(1+std::abs(a[j]));a[j]+=h;b[j]-=h;
    const double score=(cppunivariate::tlambda_detail::likelihood(mltest::as_span(g.data),a[0],a[1],a[2])-cppunivariate::tlambda_detail::likelihood(mltest::as_span(g.data),b[0],b[1],b[2]))/(2*h);
    norm2+=score*score;
  }
  CHECK(std::sqrt(norm2)<1e-3);
  const std::vector<double> x={-2,-1,0,1,2};
  for(double l:std::vector<double>{1,2}) {
    auto u=cppunivariate::tlambda_mle(mltest::as_span(x),1e-7,1000,l);
    CHECK(u.mu==0);CHECK(u.sigma==2*l);CHECK(u.logLik==doctest::Approx(-5*std::log(4.0)));
  }
  CHECK(cppunivariate::tlambda_mle(mltest::as_span(x),1e-7,1000,INFINITY).status==cppunivariate::TlambdaStatus::InvalidShape);
  const std::vector<double> collapsed={0,1e-300,2e-300,1e300};
  CHECK(cppunivariate::tlambda_mle(mltest::as_span(collapsed)).status==cppunivariate::TlambdaStatus::InvalidData);
  const std::vector<double> tied={0,0,0,1,2};
  CHECK(cppunivariate::tlambda_mle(mltest::as_span(tied)).status==cppunivariate::TlambdaStatus::InvalidData);
}
TEST_CASE("tlambda: heavy-tail standardized overflow and seeded sampling") {
  CHECK(std::isfinite(cppunivariate::distributions::dtlambda(1e300,0,1e-300,-1,true)));
  CHECK(cppunivariate::distributions::dtlambda(1e300,0,1e-300,-1,true)==doctest::Approx(-900*std::log(10.0)));
  CHECK(cppunivariate::distributions::ptlambda(5e307,0,1,1e-308,false,true)/(-std::log(2.0)/1e-308)==doctest::Approx(1.0).epsilon(1e-12));
  CHECK(cppunivariate::distributions::ptlambda(1e308,-1e308,1e308,.1)==doctest::Approx(cppunivariate::distributions::ptlambda(2,0,1,.1)).epsilon(1e-12));
  CHECK(cppunivariate::distributions::qtlambda(1,-1e308,1e308,.5)/1e308==doctest::Approx(1.0).epsilon(1e-12));
  cppunivariate::distributions::StdRng<> rng(123);
  for(int j=0;j<10;++j) CHECK(std::isfinite(cppunivariate::distributions::rtlambda(rng,0,1,.14)));
}

#include "cppunivariate/distributions/tlambda/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("tlambda: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("tlambda",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dtlambda(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ptlambda(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qtlambda(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rtlambda(rng, p[0], p[1], p[2]); });
}

TEST_CASE("tlambda: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("tlambda");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<8;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::tlambda_mle(mltest::as_span(g.data),1e-8,2000,((mask & 4) ? g.estimates.at("lambda") : nan),((mask & 1) ? g.estimates.at("mu") : nan),((mask & 2) ? g.estimates.at("sigma") : nan));
    if(mask & 1)CHECK(r.mu==g.estimates.at("mu"));
    if(mask & 2)CHECK(r.sigma==g.estimates.at("sigma"));
    if(mask & 4)CHECK(r.lambda==g.estimates.at("lambda"));
    CHECK(r.status==cppunivariate::TlambdaStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("tlambda: held-location half ties can attain their maximum") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x={0,1};
  CHECK(cppunivariate::tlambda_mle(mltest::as_span(x),1e-7,1000,nan,0,nan).status==cppunivariate::TlambdaStatus::Unbounded);
}

TEST_CASE("tlambda: P3 restrictions and closed faces") {
 const std::vector<double> x{-2,-1,0,1,2};const auto data=mltest::as_span(x);
 const auto f=cppunivariate::tlambda_mle(data);REQUIRE(f.status==cppunivariate::TlambdaStatus::Ok);
 CHECK(f.lambda==1);CHECK(f.restricted==32u);
 const auto h=cppunivariate::tlambda_mle(data,1e-7,1000,1);CHECK(h.restricted==0u);
}
TEST_CASE("tlambda: held shapes outside both free restrictions are evaluation") {
 const std::vector<double> x{-2,-1,0,1,2};
 for(double l:{-2.0,3.0}) {
   const auto r=cppunivariate::tlambda_mle(mltest::as_span(x),1e-7,1000,l,0,10);
   CHECK(r.status==cppunivariate::TlambdaStatus::Ok);CHECK(r.lambda==l);
   CHECK(std::isfinite(r.logLik));CHECK(r.restricted==0u);
 }
}


// Generating mpmath snippet (binary64 inputs retained exactly):
// import mpmath as mp, math; mp.mp.dps=100
// l=mp.mpf(0.14)
// for p in [mp.mpf(float(-math.log(2))),mp.mpf(-10000)]:
//     c=mp.log1p(-mp.exp(p))
//     print((mp.expm1(l*p)-mp.expm1(l*c))/l)  # negate for upper tail
TEST_CASE("tlambda: targeted tail and endpoint regressions") {
  CHECK(cppunivariate::distributions::qtlambda(-0.6931471805599453, 0.0, 1.0, 0.14, true, true) == doctest::Approx(4.209158811285494e-17).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qtlambda(-10000.0, 0.0, 1.0, 0.14, false, true) == doctest::Approx(7.142857142857142).epsilon(1e-10));
}

// Generating mpmath snippet at the exact binary64 endpoint neighbour:
// import mpmath as mp; mp.mp.dps=100
// m,s,a=map(mp.mpf,[0.0, 1.0, 5e-10]); x=mp.mpf(-1999999999.9999998)
// f=lambda u: (mp.expm1(a*u)-mp.expm1(a*mp.log1p(-mp.exp(u))))/a-(x-m)/s
// u=mp.findroot(f,mp.log(1+a*(x-m)/s)/a)
// v=mp.log1p(-mp.exp(u)); print(-mp.log(s)-mp.log(mp.exp((a-1)*u)+mp.exp((a-1)*v)),u)
TEST_CASE("tlambda: exact binary64 finite-endpoint neighbour") {
  CHECK(cppunivariate::distributions::dtlambda(-1999999999.9999998, 0.0, 1.0, 5e-10, true) == doctest::Approx(-74809499294.06802).epsilon(1e-10));
  CHECK(cppunivariate::distributions::ptlambda(-1999999999.9999998, 0.0, 1.0, 5e-10, true, true) == doctest::Approx(-74809499331.47278).epsilon(1e-10));
}

// import mpmath as mp; mp.mp.dps=100
// f(Q(u))=1/(u**(l-1)+(1-u)**(l-1)); u->1 gives .5 for l=1, else 1.
// API convention: apply this limit at the rounded q(0/1) endpoint as well.
TEST_CASE("tlambda: targeted tail rounded finite-density endpoint convention") {
  for(double l:std::vector<double>{1,2,5}) {
    const double x=cppunivariate::distributions::qtlambda(1,0,1,l);
    CHECK(cppunivariate::distributions::dtlambda(x,0,1,l)==(l==1?0.5:1));
  }
}

TEST_CASE("tlambda: affine cancellation survives overflowing unscaled products") {
  for(double sign : {-1.0, 1.0}) {
    const double mu=-sign*1e308;
    const double endpoint=cppunivariate::distributions::qtlambda(sign>0?1:0,mu,1e308,.5);
    CHECK(endpoint/1e308==doctest::Approx(sign).epsilon(1e-12));
    const double p=sign>0?.999:.001;
    const double x=cppunivariate::distributions::qtlambda(p,mu,1e308,.5);
    REQUIRE(std::isfinite(x));
    CHECK(cppunivariate::distributions::ptlambda(x,mu,1e308,.5)==doctest::Approx(p).epsilon(1e-12));
  }
}

TEST_CASE("tlambda: certificate inverse brackets retain quantile-generated tails") {
  for(double l:{-3.0,-1.0,-.1,0.0,1e-12,.14,.9,1.5,3.0}) {
    for(double p:{1e-30,1e-8,.1,.499999,.5}) {
      const double z=-cppunivariate::distributions::qtlambda(p,0,1,l);
      if(l>0 && z>=1/l)continue; // quantile rounded to the support endpoint
      const auto b=cppunivariate::tlambda_cert_detail::inverse(z,l,1000);
      CAPTURE(l);CAPTURE(p);CAPTURE(z);
      REQUIRE(b.ok);
      const double t=-std::log(p),pad=2e-8*(1+t);
      CHECK(b.lo<=t+pad);CHECK(b.hi>=t-pad);
      CHECK(b.hi-b.lo<=1e-11*(1+t));
    }
  }
}
TEST_CASE("tlambda: concavity certifies every held nonnegative shape nuisance subset") {
  const std::vector<double> x{-2,-1,-.2,.4,1,3};
  const double nan=NAN;
  for(double l:{0.0,.14,.5,.9})for(int mask:{0,1,2,3}) {
    CAPTURE(l);CAPTURE(mask);
    const auto r=cppunivariate::tlambda_mle(mltest::as_span(x),1e-7,1000,l,mask&1?.2:nan,mask&2?4:nan);
    REQUIRE(r.status==cppunivariate::TlambdaStatus::Ok);CHECK(r.certified);
    CHECK(r.certificate_lower<=r.logLik+1e-9);
    CHECK(r.certificate_upper>=r.logLik-1e-9);
    CHECK(r.certificate_upper-r.certificate_lower<=1e-7*x.size());
    if(mask&1)CHECK(r.mu==.2);if(mask&2)CHECK(r.sigma==4);
    // Independent feasible alternatives need not be stationary or close.
    for(double dm:{-.3,0.,.3})for(double factor:{.8,1.,1.2}) {
      const double ll=cppunivariate::tlambda_detail::likelihood(mltest::as_span(x),mask&1?.2:r.mu+dm,mask&2?4:r.sigma*factor,l);
      CHECK(ll<=r.certificate_upper+1e-8);
    }
  }
  const std::vector<double> constant{2,2,2};
  auto c=cppunivariate::tlambda_mle(mltest::as_span(constant),1e-7,1000,.5,0,nan);
  REQUIRE(c.status==cppunivariate::TlambdaStatus::Ok);CHECK(c.certified);
  CHECK(cppunivariate::tlambda_mle(mltest::as_span(constant),1e-7,1000,.5).status==cppunivariate::TlambdaStatus::Unbounded);
  const auto small=cppunivariate::tlambda_mle(mltest::as_span(x),1e-7,1,.14);
  CHECK(small.status==cppunivariate::TlambdaStatus::CertificateFailure);CHECK_FALSE(small.stop_detail.budget_exhausted);
}
TEST_CASE("tlambda: exact uniforms include plateaus and check closed support") {
  const std::vector<double> x{-.25,.25};
  for(double l:{1.,2.}) {
    const auto r=cppunivariate::tlambda_mle(mltest::as_span(x),1e-7,1000,l,NAN,l);
    REQUIRE(r.status==cppunivariate::TlambdaStatus::Ok);CHECK(r.certified);CHECK(r.mu==0);
    CHECK(r.logLik==doctest::Approx(-2*std::log(2.)));
    const auto edge=cppunivariate::tlambda_mle(mltest::as_span(x),1e-7,1000,l,.75,l);
    REQUIRE(edge.status==cppunivariate::TlambdaStatus::Ok);CHECK(edge.certified);
  }
}
TEST_CASE("tlambda: convex contact maxima include exact ties") {
  const std::vector<double> symmetric{-.25,.25};
  const auto r=cppunivariate::tlambda_mle(mltest::as_span(symmetric),1e-7,1000,1.5,NAN,1);
  REQUIRE(r.status==cppunivariate::TlambdaStatus::CertificateFailure);
  CHECK(r.stop_detail.comparison_tie);REQUIRE(r.stop_detail.candidates.size()==2);
  CHECK(r.stop_detail.candidates[0].coef[0]==doctest::Approx(-5./12));
  CHECK(r.stop_detail.candidates[1].coef[0]==doctest::Approx(5./12));
  const std::vector<double> x{-1,-.8,.3,2};
  const auto s=cppunivariate::tlambda_mle(mltest::as_span(x),1e-7,1000,1.5,0,NAN);
  REQUIRE(s.status==cppunivariate::TlambdaStatus::Ok);CHECK(s.certified);CHECK(s.sigma==3);
  const auto joint=cppunivariate::tlambda_mle(mltest::as_span(x),1e-6,2000,1.5);
  REQUIRE(joint.status==cppunivariate::TlambdaStatus::Ok);CHECK(joint.certified);
  CHECK(joint.certificate_upper-joint.certificate_lower<=1e-6*x.size()+1e-9);
}
TEST_CASE("tlambda: exact collapse decisions retain original-unit faces") {
  const std::vector<double> equal{2,2,5,9};
  const auto r=cppunivariate::tlambda_mle(mltest::as_span(equal),1e-7,1000,-1);
  REQUIRE(r.status==cppunivariate::TlambdaStatus::CollapseLimit);
  CHECK(r.stop_detail.limit_coef==std::vector<double>{2,0,-1});
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(-6*std::log(2.)-2*std::log(3.)-2*std::log(7.)));
  const std::vector<double> held{0,1,2};
  CHECK(cppunivariate::tlambda_mle(mltest::as_span(held),1e-7,1000,-2,0,NAN).status==cppunivariate::TlambdaStatus::CollapseLimit);
  CHECK(cppunivariate::tlambda_mle(mltest::as_span(held),1e-7,1000,-3,0,NAN).status==cppunivariate::TlambdaStatus::Unbounded);
  const auto unresolved=cppunivariate::tlambda_mle(mltest::as_span(held),1e-7,1000,-2);
  CHECK(unresolved.status==cppunivariate::TlambdaStatus::CertificateFailure);
  CHECK(std::isfinite(unresolved.stop_detail.limit_loglik));
}
TEST_CASE("tlambda: negative scalar and shape-only boxes bound global likelihood") {
  const std::vector<double> x{-2,-1,.5,2};
  for(int mask:{1,2,3}) {
    const auto r=cppunivariate::tlambda_mle(mltest::as_span(x),1e-6,2000,mask==3?NAN:-1,mask&1?0:NAN,mask&2?1:NAN);
    CAPTURE(mask);CAPTURE(r.iterations);
    REQUIRE(r.status==cppunivariate::TlambdaStatus::Ok);CHECK(r.certified);
    CHECK(r.certificate_upper-r.certificate_lower<=1e-6*x.size()+1e-9);
  }
  // The negative-shape midpoint can be a local minimum: never use concavity.
  const std::vector<double> modes{-80./9,80./9};
  const auto m=cppunivariate::tlambda_mle(mltest::as_span(modes),1e-6,2000,-1,NAN,1);
  REQUIRE(m.status==cppunivariate::TlambdaStatus::Ok);CHECK(m.certified);
  CHECK(std::abs(m.mu)>1);
  const auto budget=cppunivariate::tlambda_mle(mltest::as_span(modes),1e-12,64,-1,NAN,1);
  CHECK(budget.status==cppunivariate::TlambdaStatus::CertificateFailure);CHECK(budget.stop_detail.budget_exhausted);
}

TEST_CASE("tlambda: free-shape half ties retain faces without claiming nonattainment") {
  const std::vector<double> x{0,0,1,2};
  const auto r=cppunivariate::tlambda_mle(mltest::as_span(x));
  REQUIRE(r.status==cppunivariate::TlambdaStatus::CertificateFailure);
  CHECK_FALSE(r.stop_detail.budget_exhausted);CHECK(std::isfinite(r.stop_detail.limit_loglik));
  CHECK(r.stop_detail.loglik_upper==INFINITY);REQUIRE(r.stop_detail.candidates.size()==1);
  CHECK(r.stop_detail.candidates[0].loglik_lower>r.stop_detail.limit_loglik);
}

TEST_CASE("tlambda: shapes above two have decreasing scale densities and contact certificates") {
  const std::vector<double> x{-2,-1,.5,2};
  for(double l:{2.5,3.,5.}) {
    const auto scale=cppunivariate::tlambda_mle(mltest::as_span(x),1e-6,1000,l,0,NAN);
    REQUIRE(scale.status==cppunivariate::TlambdaStatus::Ok);CHECK(scale.certified);CHECK(scale.sigma==2*l);
    for(double s:{2.1*l,3*l,5*l})CHECK(cppunivariate::tlambda_detail::likelihood(mltest::as_span(x),0,s,l)<=scale.certificate_upper);
    const auto location=cppunivariate::tlambda_mle(mltest::as_span(x),1e-6,2000,l,NAN,3*l);
    REQUIRE(location.status==cppunivariate::TlambdaStatus::Ok);CHECK(location.certified);
    const auto joint=cppunivariate::tlambda_mle(mltest::as_span(x),1e-6,2000,l);
    REQUIRE(joint.status==cppunivariate::TlambdaStatus::Ok);CHECK(joint.certified);
    for(double m:{-1.,-.5,0.,.5,1.}) {
      const double s=l*std::max(std::abs(-2-m),std::abs(2-m));
      CHECK(cppunivariate::tlambda_detail::likelihood(mltest::as_span(x),m,s,l)<=joint.certificate_upper+1e-9);
    }
  }
}

TEST_CASE("tlambda: likelihood enclosures survive cancellation of normalization terms") {
  const std::vector<double> x(1024,0);
  const auto r=cppunivariate::tlambda_mle(mltest::as_span(x),1e-7,1000,-998,0,std::ldexp(1.0,-1000));
  REQUIRE(r.status==cppunivariate::TlambdaStatus::Ok);CHECK(r.certified);
  // g_lambda(0)=2^(lambda-2)=sigma, hence every density is exactly one.
  CHECK(r.certificate_lower<=0);CHECK(r.certificate_upper>=0);
  CHECK(r.logLik>=r.certificate_lower);CHECK(r.logLik<=r.certificate_upper);
}

TEST_CASE("tlambda: unrepresentable uniform midpoints stop instead of widening silently") {
  const double lo=1,hi=std::nextafter(lo,INFINITY);
  const std::vector<double> x{lo,hi};
  for(double l:{1.,2.}) {
    const auto r=cppunivariate::tlambda_mle(mltest::as_span(x),1e-7,1000,l);
    CHECK(r.status==cppunivariate::TlambdaStatus::CertificateFailure);CHECK_FALSE(r.certified);
  }
  double top=lo;for(int i=0;i<3;++i)top=std::nextafter(top,INFINITY);
  CHECK(cppunivariate::tlambda_detail::uniform_fit(lo,top,4,1,1000).status==cppunivariate::TlambdaStatus::CertificateFailure);
}

// 420-digit independent bisection references and binary64 input records:
// dev/research/evaluations/2026-11-08-tlambda-speed-impl-data/check_inverse.py
TEST_CASE("tlambda: fitting inverse verifies centre tail and endpoint regimes") {
  const std::vector<std::array<double,5>> rows{
    {-32.0,1e+100,NAN,-7.303882662568884,241.02812786477318},
    {-0.7,1e+300,NAN,-986.3126470775358,1676.7315000318108},
    {-1.0,10.0,NAN,-2.4068567723080196,4.823470652913973},
    {-1e-12,10.0,NAN,-10.000045398849219,10.000090797758437},
    {0.0,10.0,NAN,-10.000045398899218,10.000090797798434},
    {1e-12,0.1,-1.000310945187316e-13,-0.7443966600736074,1.3887933201464484},
    {0.14,1e-20,0.0,-0.6931471805599453,1.2892537558414983},
    {0.5,2.0,-744.4400719213812,-1488.8801438427624,744.4400719213812},
    {0.9,1.11111111111111,-34.538776394910684,-38.39771139809301,3.8610418271520444},
    {0.999999999999,1.000000000001,-230.25850929940458,-230.9516564800795,0.6931471806754188},
    {1.5,0.1,-0.16251892949777494,-0.8457623164193767,0.3440503704037633},
    {3.0,0.1,-0.35667494393873245,-1.1735938032645383,-0.5572737977379317},
    {16.0,0.0625,-744.4400719213812,-747.212660643621,-5e-324},
  };
  for(const auto& row:rows){
    CAPTURE(row[0]);CAPTURE(row[1]);CAPTURE(row[2]);
    const auto r=cppunivariate::tlambda_fit_detail::inverse(row[1],row[0],row[2]);
    REQUIRE(r.ok);
    CHECK(std::abs(r.logp-row[3])<=5e-13*(1+std::abs(row[3])));
    CHECK(std::abs(r.logd-row[4])<=5e-13*(1+std::abs(row[4])));
  }
  const double x=std::nextafter(2.,0.);
  CHECK(cppunivariate::tlambda_fit_detail::logdensity(x,0,1,.5)==doctest::Approx(cppunivariate::distributions::dtlambda(x,0,1,.5,true)).epsilon(1e-12));
  CHECK_FALSE(cppunivariate::tlambda_fit_detail::inverse(1.,.14,NAN,1).ok);
}
TEST_CASE("tlambda: profile golden coefficients meet the relative accuracy contract") {
  const auto g=mltest::load_distribution_golden("tlambda");
  const std::array<double,3> ref{0.04752470769410265657093355,1.098815160848591024380682,0.1844784658509907723491377};
  for(int mask:{0,1,2}){
    const auto f=cppunivariate::tlambda_mle(mltest::as_span(g.data),1e-10,2000,NAN,mask==1?ref[0]:NAN,mask==2?ref[1]:NAN);
    REQUIRE(f.status==cppunivariate::TlambdaStatus::Ok);
    CHECK_FALSE(f.certified);
    const std::array<double,3> got{f.mu,f.sigma,f.lambda};
    for(int j=0;j<3;++j)CHECK(std::abs(got[j]/ref[j]-1)<1e-10);
    CHECK(std::abs(f.logLik+911.4099114230672826896986)<2e-10);
  }
}
TEST_CASE("tlambda: profile routes retain feasible likelihood and both fitted faces") {
  for(double shape:{-.8,.14,.7}) {
    std::vector<double> x;
    for(int i=0;i<100;++i)x.push_back(cppunivariate::distributions::qtlambda((i+.5)/100,0,1,shape));
    const auto data=mltest::as_span(x);
    const double generating=cppunivariate::tlambda_detail::likelihood(data,0,1,shape);
    for(int mask:{0,1,2,4}) {
      const auto f=cppunivariate::tlambda_mle(data,1e-10,2000,mask==4?shape:NAN,mask==1?0:NAN,mask==2?1:NAN);
      REQUIRE(f.status==cppunivariate::TlambdaStatus::Ok);
      CHECK(f.logLik>=generating-1e-7);
    }
  }
  for(double shape:{-2.,1.}){
    std::vector<double> x;
    for(int i=0;i<31;++i)x.push_back(cppunivariate::distributions::qtlambda((i+.5)/31,0,1,shape));
    const auto f=cppunivariate::tlambda_mle(mltest::as_span(x),1e-10,2000);
    REQUIRE(f.status==cppunivariate::TlambdaStatus::Ok);
    CHECK(f.lambda==(shape<0?-1:1));
    CHECK(f.restricted==(shape<0?16u:32u));
  }
}

TEST_CASE("tlambda: profile affine frame retains extreme-tail information") {
  // At lambda=-1 the exact density is 1/(sqrt(z*z+4)*(sqrt(z*z+4)+2)).
  // Its tail score is -2/z+O(z^-2), curvature 2/z^2+O(z^-3).
  const std::vector<double> x{1e150,2e150};
  const auto v=cppunivariate::tlambda_fit_detail::value(x,1,0,-1,1000);
  REQUIRE(v.ok);
  CHECK(std::abs(v.gu/(-2)-1)<1e-10);
  CHECK(std::abs(v.gv/(3e-150)-1)<1e-10);
  CHECK(std::abs(v.huu/2-1)<1e-10);
  CHECK(std::abs(v.huv/(-3e-150)-1)<1e-10);
  CHECK(std::abs(v.hvv/(2.5e-300)-1)<1e-10);
}
