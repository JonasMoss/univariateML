#include "test_support.hpp"
#include "cppunivariate/distributions/triang/mle.hpp"
#include "cppunivariate/distributions/triang/dpqr.hpp"
#include <random>
#include <vector>

TEST_CASE("triang: golden") {
  const auto g=mltest::load_distribution_golden("triang");
  const auto r=cppunivariate::triang_mle(mltest::as_span(g.data));
  CHECK(r.status==cppunivariate::TriangStatus::Ok);
  CHECK(r.a==doctest::Approx(g.estimates.at("a")).epsilon(1e-12));
  CHECK(r.b==doctest::Approx(g.estimates.at("b")).epsilon(1e-12));
  CHECK(r.c==doctest::Approx(g.estimates.at("c")).epsilon(1e-12));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-12));
}
TEST_CASE("triang: corners, ties, incompatible and constant samples") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x{0,1};
  auto r=cppunivariate::triang_mle(mltest::as_span(x));
  CHECK(r.status==cppunivariate::TriangStatus::Ok);
  CHECK(r.not_unique);
  CHECK(r.c==0); CHECK(r.a==0);
  CHECK(r.b==doctest::Approx(1.5));
  CHECK(std::exp(r.logLik)==doctest::Approx(16.0/27));
  CHECK(cppunivariate::triang_mle(mltest::as_span(x),1e-12,100,0,1).status==cppunivariate::TriangStatus::Incompatible);
  x={2,2,2};
  CHECK(cppunivariate::triang_mle(mltest::as_span(x)).status==cppunivariate::TriangStatus::Unbounded);
  CHECK(cppunivariate::triang_mle(mltest::as_span(x),1e-12,100,0,nan).status==cppunivariate::TriangStatus::Ok);
  for(bool reflect:{false,true}) {
    x.clear();
    for(int i=0;i<100;++i) {
      const double z=cppunivariate::distributions::qtriang(i/100.0,0,1,0);
      x.push_back(reflect?1-z:z);
    }
    r=cppunivariate::triang_mle(mltest::as_span(x));
    CHECK(r.status==cppunivariate::TriangStatus::Ok);
    CHECK(r.c==(reflect?r.b:r.a));
  }
}
TEST_CASE("triang: every fixed subset and reference density") {
  const std::vector<double> x{.02,.2,.3,.4,.45,.65,.9};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=0;mask<8;++mask) {
    auto r=cppunivariate::triang_mle(mltest::as_span(x),1e-12,100,
      mask&1?0:nan,mask&2?1:nan,mask&4?.4:nan);
    CHECK(r.status==cppunivariate::TriangStatus::Ok);
    if(mask&1) CHECK(r.a==0);
    if(mask&2) CHECK(r.b==1);
    if(mask&4) CHECK(r.c==.4);
    double ll=0;
    for(double z:x) ll+=cppunivariate::distributions::dtriang(z,r.a,r.b,r.c,true);
    CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-12));
  }
  for(double c:{-1.,2.}) {
    auto r=cppunivariate::triang_mle(mltest::as_span(x),1e-12,100,nan,nan,c);
    CHECK(r.status==cppunivariate::TriangStatus::Ok);
    CHECK((c<0?r.a:r.b)==c);
  }
}
TEST_CASE("triang: certified pruning agrees with exhaustive enumeration") {
  std::mt19937 gen(20261028);
  std::uniform_real_distribution<double> u(0,1);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int seed=0;seed<200;++seed) {
    std::vector<double>x;
    for(int i=0;i<5+seed%70;++i) {
      double z=cppunivariate::distributions::qtriang(u(gen),0,1,(seed%11)/10.0);
      if(seed%4==0) z=std::round(z*20)/20;
      x.push_back(z);
    }
    const double a=seed%3==0?-.1:nan, b=seed%5==0?1.1:nan;
    const auto fast=cppunivariate::triang_mle(mltest::as_span(x),1e-12,100,a,b);
    const auto brute=cppunivariate::triang_mle(mltest::as_span(x),1e-12,100,a,b,nan,false);
    CHECK(fast.status==cppunivariate::TriangStatus::Ok);
    CHECK(fast.status==brute.status);
    CHECK(std::abs(fast.logLik-brute.logLik)<1e-10);
    CHECK(fast.not_unique==brute.not_unique);
    CHECK(fast.c==brute.c);
  }
}
TEST_CASE("triang: CDF quantile and corner density") {
  for(double c:{0.,.4,1.}) for(double p:{0.,.1,.5,.9,1.}) {
    const auto q=cppunivariate::distributions::qtriang(p,0,1,c);
    CHECK(cppunivariate::distributions::ptriang(q,0,1,c)==doctest::Approx(p));
  }
  CHECK(cppunivariate::distributions::dtriang(0,0,1,0)==2);
  CHECK(cppunivariate::distributions::dtriang(1,0,1,1)==2);
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("triang: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("triang",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dtriang(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ptriang(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qtriang(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rtriang(rng, p[0], p[1], p[2]); });
}

TEST_CASE("triang: constant data with held exterior mode") {
  const std::vector<double> x(5,.4);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double b:{nan,2.}) {
    const auto r=cppunivariate::triang_mle(mltest::as_span(x),1e-12,100,nan,b,-1);
    REQUIRE(r.status==cppunivariate::TriangStatus::Ok);
    CHECK(r.a==-1); CHECK(r.b==doctest::Approx(std::isnan(b)?1.8:2));
    CHECK(r.c==-1);
  }
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// logF=lambda x:m.log(2)+2*m.log(x); Q=lambda l:m.exp((l-m.log(2))/2)
TEST_CASE("triang: extreme tail regressions") {
  CHECK(cppunivariate::distributions::ptriang(1e-300,0,1,0.5,true,true) == doctest::Approx(2*std::log(1e-300)+std::log(2.0)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qtriang(-800,0,1,0.5,true,true) == doctest::Approx(std::exp((-800-std::log(2.0))/2)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qtriang(-INFINITY,0,1,0.5,true,true) == doctest::Approx(0).epsilon(1e-12).scale(1e-300));
}

// mpmath: m.mp.dps=400; x=m.mpf(float.fromhex('0x0.0000000000001p-1022'))
// For mode 0, F(x)=x*(2-x); for mode 1, S(1-e)=e*(2-e).
TEST_CASE("triang: endpoint modes retain the opposite small tail") {
  const double tiny=std::numeric_limits<double>::denorm_min();
  CHECK(cppunivariate::distributions::ptriang(tiny,0,1,0,true,true) == doctest::Approx(std::log(tiny)+std::log(2.0)).epsilon(1e-12));
  const double x=std::nextafter(1.0,0.0), e=1-x;
  CHECK(cppunivariate::distributions::ptriang(x,0,1,1,false,true) == doctest::Approx(std::log(e)+std::log(2-e)).epsilon(1e-12));
  CHECK(cppunivariate::distributions::dtriang(tiny,0,1e300,1e300,true) == doctest::Approx(std::log(2.0)+std::log(tiny)-2*std::log(1e300)).epsilon(1e-12));
}
