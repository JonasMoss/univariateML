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
TEST_CASE("tlambda: d/p/q/r against the truth table" * doctest::may_fail()) {
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
