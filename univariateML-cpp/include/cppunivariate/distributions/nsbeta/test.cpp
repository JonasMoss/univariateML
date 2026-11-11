#include "test_support.hpp"
#include "cppunivariate/distributions/nsbeta/dpqr.hpp"
#include "cppunivariate/distributions/nsbeta/mle.hpp"
TEST_CASE("nsbeta: exact affine likelihood") {
 const std::vector<double> y{.05,.1,.2,.4,.6,.7,.85};
 std::vector<double> x(y); for(double &v:x) v=2+3*v;
 auto a=cppunivariate::nsbeta_mle(cppunivariate::span{x.data(),x.size()},2,5,1e-10,1000);
 auto b=cppunivariate::beta_mle(cppunivariate::span{y.data(),y.size()},1e-10,1000);
 CHECK(a.logLik == doctest::Approx(b.logLik-y.size()*std::log(3)));
 CHECK(a.shape1 == doctest::Approx(b.alpha));
}

TEST_CASE("nsbeta: scalar density parameter mapping") {
 CHECK(cppunivariate::distributions::dnsbeta(.5,2,3,0,1,false) == doctest::Approx(1.5));
}

#include "cppunivariate/distributions/nsbeta/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
TEST_CASE("nsbeta: probability endpoints and sampler CDF") {
  CHECK(cppunivariate::distributions::qnsbeta(0,20,20,0,1,true,false)==0);
  CHECK(std::isinf(cppunivariate::distributions::qnsbeta(0,20,20,0,1,false,false))==false);
  CHECK(cppunivariate::distributions::pnsbeta(-1,20,20,0,1,true,false)==0);
  CHECK(cppunivariate::distributions::pnsbeta(std::numeric_limits<double>::infinity(),20,20,0,1,true,false)==1);
  CHECK(std::isnan(cppunivariate::distributions::qnsbeta(.1,20,20,0,1,true,true)));
  cppunivariate::distributions::StdRng<> rng(7813);
  std::vector<double> samples;
  for(int i=0;i<20000;++i) samples.push_back(cppunivariate::distributions::rnsbeta(rng,20,20,0,1));
  for(double point:{.4,.5,.6}) {
    int count=0;for(double x:samples) if(x<=point) ++count;
    const double prob=cppunivariate::distributions::pnsbeta(point,20,20,0,1,true,false);
    CHECK(std::abs(count/20000.-prob)<=5*std::sqrt(prob*(1-prob)/20000.));
  }
}
// from mpmath import mp; mp.dps=100; float(mp.betainc(20,20,0,mp.mpf(1e-6),regularized=True))
TEST_CASE("nsbeta: independent extreme value") {
 CHECK(cppunivariate::distributions::pnsbeta(1e-06,20,20,0,1,true,false)==doctest::Approx(6.8922017237834511e-110).epsilon(2e-13));
}

TEST_CASE("P2 nsbeta golden holds") {
 auto g=mltest::load_distribution_golden("nsbeta");
 const double nan=std::numeric_limits<double>::quiet_NaN();
for(int mask=1;mask<4;++mask){double a=mask&1?g.estimates.at("shape1"):nan,b=mask&2?g.estimates.at("shape2"):nan;auto r=cppunivariate::nsbeta_mle(mltest::as_span(g.data),g.estimates.at("min"),g.estimates.at("max"),1e-10,1000,a,b);REQUIRE(r.status==cppunivariate::BetaStatus::Ok);
 if(mask&1)CHECK(r.shape1==a);
 if(mask&2)CHECK(r.shape2==b);
 CHECK(r.logLik>=g.logLik-1e-5);}
}


TEST_CASE("nsbeta: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("nsbeta",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dnsbeta(x, p[0], p[1], p[2], p[3], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pnsbeta(x, p[0], p[1], p[2], p[3], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qnsbeta(x, p[0], p[1], p[2], p[3], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rnsbeta(rng, p[0], p[1], p[2], p[3]); });
}

// import mpmath as mp, math; mp.mp.dps=100
// x=mp.mpf(math.nextafter(5.,2.)); y=(5-x)/3
// print(3*mp.log(y)+mp.log(4-3*y))  # Beta(2,3) upper tail
// print(mp.log(12)+mp.log(1-y)+2*mp.log(y)-mp.log(3))
TEST_CASE("nsbeta: targeted tail affine endpoint distances") {
  const double x=std::nextafter(5.0,2.0);
  CHECK(cppunivariate::distributions::pnsbeta(x,2,3,2,5,false,true)
        ==doctest::Approx(-105.88161958887623).epsilon(1e-10));
  CHECK(cppunivariate::distributions::dnsbeta(x,2,3,2,5,true)
        ==doctest::Approx(-70.12564827221085).epsilon(1e-10));
  CHECK(cppunivariate::distributions::dnsbeta(2,1,3,2,5,true)==0);
  CHECK(cppunivariate::distributions::qnsbeta(0,2,3,2,5,false,false)==5);
}

TEST_CASE("nsbeta: TASK-132 empty and invalid delegates preserve non-success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2},std::vector<double>{-1,2}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::nsbeta_mle(x,0,1,1e-8,200).status != cppunivariate::BetaStatus::Ok);
  }
}

TEST_CASE("nsbeta held-shape solve forwards the iteration budget") {
  const std::vector<double> x{1e-200,2e-200,3e-200,5e-200,9e-200};
  const auto limited=cppunivariate::nsbeta_mle(mltest::as_span(x),0,1,1e-10,1,2,NAN);
  CHECK(limited.status==cppunivariate::BetaStatus::IterationFailure);
  const auto solved=cppunivariate::nsbeta_mle(mltest::as_span(x),0,1,1e-10,1000,2,NAN);
  REQUIRE(solved.status==cppunivariate::BetaStatus::Ok);
  CHECK(solved.shape2>1e199);
  CHECK(solved.iterations>1);
}

// Raw score roots checked independently at 350 and 700 digits.
TEST_CASE("nsbeta: TASK-101.7.3 raw likelihood regressions") {
  struct Fixture { int index; std::vector<double> x; double a,b,lo,hi,ll; };
  const std::vector<Fixture> fixtures{
    {5348,{3.279712609634442e-151,7.163848556685424e-151,3.247469053643058e-151,1.3428490451384639e-151,5.302279618623525e-151,5.9296803360997277e-151,5.249832294577309e-151,3.503576195388067e-151,5.430978029647745e-151,7.340437563236251e-151,8.133244607203068e-152,2.243783898794992e-151},2.0063421787377256,4.7349382987329286e+150,0.0,1.0,4145.549562030085},
    {10791,{1e-17,2e-17,3e-17,5.0000000000000005e-17,9.000000000000001e-17},2.0063421787377256,5.015855446844314e+16,0.0,1.0,184.38678752333837},
    {10792,{1e-200,2e-200,3e-200,5e-200,9e-200},2.0063421787377256,5.015855446844314e+199,0.0,1.0,2291.25214761289},
    {10799,{0.999999999999,0.999999999998,0.999999999997,0.999999999995,0.999999999991},761512504326.9779,3.0460502693524503,0.0,1.0,126.54970076195625},
    {11011,{0.9999999999999999,0.9999999999999998,0.9999999999999997,0.9999999999999994,0.999999999999999},6859095429003744.0,3.0460502693524503,0.0,1.0,172.07865397295913},
  };
  for(const auto& f:fixtures) {
    CAPTURE(f.index);
    const bool first=f.index==5348 || f.index==10791 || f.index==10792;
    const auto r=cppunivariate::nsbeta_mle(mltest::as_span(f.x),f.lo,f.hi,1e-10,1000,first?f.a:NAN,first?NAN:f.b);
    REQUIRE(r.status==cppunivariate::BetaStatus::Ok);
    CHECK(r.shape1==doctest::Approx(f.a).epsilon(1e-10));
    CHECK(r.shape2==doctest::Approx(f.b).epsilon(1e-10));
    CHECK(r.min==doctest::Approx(f.lo).epsilon(1e-10));
    CHECK(r.max==doctest::Approx(f.hi).epsilon(1e-10));
    CHECK(r.logLik==doctest::Approx(f.ll).epsilon(1e-10));
  }
}

TEST_CASE("nsbeta all-held overflow remains a non-success evaluation") {
  const std::vector<double> x{.5};
  const auto r=cppunivariate::nsbeta_mle(mltest::as_span(x),0,1,1e-10,1000,1e308,1e308);
  CHECK(r.status==cppunivariate::BetaStatus::IterationFailure);
}

TEST_CASE("nsbeta held-shape score rejects insufficient subnormal signal") {
  for(double unit:{1e-320,1e-311}) {
    const std::vector<double> x{unit,2*unit,3*unit,5*unit,9*unit};
    const auto r=cppunivariate::nsbeta_mle(mltest::as_span(x),0,1,1e-10,1000,1e-320,NAN);
    CHECK(r.status==cppunivariate::BetaStatus::IterationFailure);
  }
}

TEST_CASE("nsbeta tiny held shape with a resolved score remains estimable") {
  const std::vector<double> x{.1,.2,.3,.5,.9};
  double target=0;for(double v:x)target-=std::log1p(-v)/x.size();
  const double held=1e-320;
  const auto r=cppunivariate::nsbeta_mle(mltest::as_span(x),0,1,1e-10,1000,held,NAN);
  REQUIRE(r.status==cppunivariate::BetaStatus::Ok);
  // psi(b+h)-psi(b) ~ h/b^2 here. The omitted relative correction
  // is order sqrt(h), below 1e-150; form the square roots separately.
  CHECK(r.shape2==doctest::Approx(std::sqrt(held)/std::sqrt(target)).epsilon(1e-10));
  double ll=x.size()*std::log(held);
  for(double v:x)ll-=std::log(v)+std::log1p(-v);
  CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-10));
}

// Independent central log-probability expansion; exact binary64 input.
TEST_CASE("nsbeta: central inverse retains a translated residual") {
  CHECK(cppunivariate::distributions::qnsbeta(-0.6931471805599453,1,1,-1,1,true,true) ==
        doctest::Approx(2.3190468138462996e-17).epsilon(1e-10).scale(0));
}

// The true upper log tail is approximately -1e274 (Laplace relative remainder O(1e-274)).
// Rounded beta coordinates would give half the mass; the named unresolved region returns NaN.
TEST_CASE("nsbeta: concentrating coordinate bridge reports its precision region") {
  CHECK(std::isnan(cppunivariate::distributions::pnsbeta(1e-17,1e308,1e308,-1,1,false,true)));
}

// Independent 1600-digit exact-input reference; beta-smoke.py.
TEST_CASE("nsbeta: centered large-shape density") {
  CHECK(cppunivariate::distributions::dnsbeta(0.0,100000000.0,100000000.0,-1.0,1.0,true)==
        doctest::Approx(8.637975427801482).epsilon(1e-10).scale(0));
}
