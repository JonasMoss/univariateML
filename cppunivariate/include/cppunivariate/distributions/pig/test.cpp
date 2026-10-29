#include "test_support.hpp"
#include "cppunivariate/distributions/pig/mle.hpp"
#include <vector>
#include <limits>
namespace {
cppunivariate::ispan pig_span(const std::vector<int>& x) { return {x.data(),x.size()}; }
}
TEST_CASE("pig: golden and independent score gate") {
  const auto g=mltest::load_distribution_golden("pig");
  std::vector<int> x;
  for (double v:g.data) x.push_back(static_cast<int>(v));
  const auto r=cppunivariate::pig_mle(pig_span(x));
  REQUIRE(r.status==cppunivariate::PigStatus::Ok);
  CHECK(r.mean==doctest::Approx(g.estimates.at("mean")).epsilon(1e-8));
  CHECK(r.shape==doctest::Approx(g.estimates.at("shape")).epsilon(1e-6));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  const cppunivariate::pig_detail::Counts c(pig_span(x));
  const double h=1e-5;
  const double sm=(c.likelihood(r.mean*std::exp(h),r.shape)-c.likelihood(r.mean*std::exp(-h),r.shape))/(2*h);
  const double ss=(c.likelihood(r.mean,r.shape*std::exp(h))-c.likelihood(r.mean,r.shape*std::exp(-h)))/(2*h);
  CHECK(std::hypot(sm,ss)<1e-3);
}
TEST_CASE("pig: stable masses at actuar failure points") {
  CHECK(cppunivariate::distributions::dpig(2,2,1e20,true)==doctest::Approx(-1.306852819440055).epsilon(1e-13));
  CHECK(cppunivariate::distributions::dpig(1000,1000,1e12,true)==doctest::Approx(-4.372900006026047).epsilon(1e-11));
  CHECK(cppunivariate::distributions::dpig(20,2,3,true)==doctest::Approx(-9.665144620247714).epsilon(1e-13));
  CHECK(cppunivariate::distributions::dpig(10000,2,.001,true)==doctest::Approx(-19.43764866483945).epsilon(1e-11));
  CHECK(cppunivariate::distributions::dpig(2,std::numeric_limits<double>::infinity(),3,true)==doctest::Approx(-2.3948252303866533).epsilon(1e-8));
}
TEST_CASE("pig: faces and fixed subsets") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for (const std::vector<int> x: {std::vector<int>{1,1,1}, {0,1,1,2}, {0,2}, {0,0,0}}) {
    const auto r=cppunivariate::pig_mle(pig_span(x));
    CHECK(r.status==(x.back()==0?cppunivariate::PigStatus::Ok:cppunivariate::PigStatus::PoissonLimit));
  }
  const std::vector<int> x{0,0,0,0,10};
  const auto r=cppunivariate::pig_mle(pig_span(x));
  CHECK(r.status==cppunivariate::PigStatus::Ok);
  CHECK(r.mean==2);
  CHECK(r.shape==doctest::Approx(.036799).epsilon(1e-4));
  const auto fm=cppunivariate::pig_mle(pig_span(x),1e-9,200,3,nan);
  const auto fs=cppunivariate::pig_mle(pig_span(x),1e-9,200,nan,3);
  const auto both=cppunivariate::pig_mle(pig_span(x),1e-9,200,3,3);
  CHECK(fm.status==cppunivariate::PigStatus::Ok);
  CHECK(fs.status==cppunivariate::PigStatus::Ok);
  CHECK(both.status==cppunivariate::PigStatus::Ok);
  CHECK(fm.mean==3);
  CHECK(fs.shape==3);
  CHECK(fs.logLik>cppunivariate::pig_detail::Counts(pig_span(x)).likelihood(std::numeric_limits<double>::infinity(),3));
  CHECK(cppunivariate::pig_mle(pig_span(x),1e-9,200,0,3).status==cppunivariate::PigStatus::InvalidInput);
  CHECK(cppunivariate::pig_mle(pig_span(x),1e-15,1).status==cppunivariate::PigStatus::SearchFailure);
}
TEST_CASE("pig: free fits can reach reference underflow region") {
  const std::vector<int> x{960,1040};
  const auto r=cppunivariate::pig_mle(pig_span(x));
  REQUIRE(r.status==cppunivariate::PigStatus::Ok);
  CHECK(r.mean==1000);
  CHECK(std::isfinite(r.logLik));
  CHECK(cppunivariate::pig_detail::log_masses(1,r.mean,r.shape).back() < -745);
  const std::vector<int> y{0,1,2,3};
  const auto fixed=cppunivariate::pig_mle(pig_span(y),1e-9,200,
    std::numeric_limits<double>::quiet_NaN(),1e20);
  REQUIRE(fixed.status==cppunivariate::PigStatus::Ok);
  CHECK(fixed.mean==doctest::Approx(1.5).epsilon(1e-6));
}

#include "cppunivariate/distributions/pig/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
TEST_CASE("pig: probability endpoints and sampler CDF") {
  CHECK(cppunivariate::distributions::qpig(0,2,3,true,false)==0);
  CHECK(std::isinf(cppunivariate::distributions::qpig(0,2,3,false,false))==true);
  CHECK(cppunivariate::distributions::ppig(-1,2,3,true,false)==0);
  CHECK(cppunivariate::distributions::ppig(std::numeric_limits<double>::infinity(),2,3,true,false)==1);
  CHECK(std::isnan(cppunivariate::distributions::qpig(.1,2,3,true,true)));
  cppunivariate::distributions::StdRng<> rng(7813);
  std::vector<double> samples;
  for(int i=0;i<20000;++i) samples.push_back(cppunivariate::distributions::rpig(rng,2,3));
  for(double point:{0.,2.,5.}) {
    int count=0;for(double x:samples) if(x<=point) ++count;
    const double prob=cppunivariate::distributions::ppig(point,2,3,true,false);
    CHECK(std::abs(count/20000.-prob)<=5*std::sqrt(prob*(1-prob)/20000.));
  }
}
// from mpmath import mp; mp.dps=100
// def pmf(k):
//     k=mp.mpf(k)
//     return (mp.sqrt(6/mp.pi)*mp.exp(mp.mpf('1.5'))
//       *(mp.mpf(3)/mp.mpf('2.75'))**((k-mp.mpf('.5'))/2)
//       *mp.besselk(k-mp.mpf('.5'),mp.sqrt(mp.mpf('8.25')))/mp.factorial(k))
// mp.log(mp.fsum(pmf(k) for k in range(100,900)))
TEST_CASE("pig: direct upper log sum and discrete quantile brackets") {
  CHECK(cppunivariate::distributions::ppig(99,2,3,false,true)==doctest::Approx(-36.218853990576662).epsilon(2e-14));
  for(double lp:{-27.631021115928547,-69.07755278982137,-230.25850929940458,-690.7755278982137,-800.,-10000.}) for(bool lower:{true,false}) {
    const double q=cppunivariate::distributions::qpig(lp,2,3,lower,true);
    REQUIRE(std::isfinite(q));
    if(lower) {CHECK(cppunivariate::distributions::ppig(q,2,3,true,true)>=lp);CHECK(cppunivariate::distributions::ppig(q-1,2,3,true,true)<lp);}
    else {CHECK(cppunivariate::distributions::ppig(q,2,3,false,true)<=lp);CHECK(cppunivariate::distributions::ppig(q-1,2,3,false,true)>lp);}
  }
}

TEST_CASE("pig: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("pig");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<4;++mask) {
   const auto r=cppunivariate::pig_mle({x.data(),x.size()},1e-10,200,(mask & 1)?g.estimates.at("mean"):nan,(mask & 2)?g.estimates.at("shape"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.mean==g.estimates.at("mean"));
   if(mask & 2)CHECK(r.shape==g.estimates.at("shape"));
 }
}


// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("pig: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("pig",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dpig(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ppig(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qpig(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rpig(rng, p[0], p[1]); });
}


TEST_CASE("PIG P3 zero shape preserves held means") {
 const std::vector<int> z{0,0}, pos{0,1};
 auto r=cppunivariate::pig_mle({z.data(),z.size()});
 CHECK(r.status==cppunivariate::PigStatus::Ok); CHECK(r.mean==1); CHECK(r.shape==0); CHECK(r.logLik==0);
 r=cppunivariate::pig_mle({z.data(),z.size()},1e-9,200,2,0);
 CHECK(r.status==cppunivariate::PigStatus::Ok); CHECK(r.mean==2);
 CHECK(cppunivariate::pig_mle({pos.data(),pos.size()},1e-9,200,2,0).status==cppunivariate::PigStatus::InvalidInput);
}
