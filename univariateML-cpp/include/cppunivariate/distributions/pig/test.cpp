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
  CHECK(r.mean==doctest::Approx(g.estimates.at("mean")).epsilon(mltest::kGolden_pig).scale(0));
  CHECK(r.shape==doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kGolden_pig).scale(0));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_pig).scale(0));
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
TEST_CASE("pig: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("pig",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dpig(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ppig(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qpig(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rpig(rng, p[0], p[1]); });
}

// mpmath, mp.dps=100: A=mp.mpf(8)/11; mp.log(A)
// At x=1e100 the Bessel Taylor and gamma-ratio corrections are O(1e-100).
// Transition oracle (mp.dps=100), with m=mp.mpf(20), s=mp.mpf(3):
// A=2*m*m/(s+2*m*m); c=s*(2+s/m**2)/4; n=mp.mpf(100002)
// cs=[mp.mpf(1)]
// for j in range(1,30): cs.append(-cs[-1]*c/(j*(n-mp.mpf('.5')-j)))
// def f(v):
//   h=(1-A)/(1-A*mp.exp(-v/n))
//   return mp.exp(-v)*h**mp.mpf('1.5')*mp.polyval(cs[::-1],h)
// const=mp.log(mp.sqrt(s/(2*mp.pi)))+s/m-mp.log(A)/2
// lu=(const+n*mp.log(A)+mp.loggamma(n-mp.mpf('.5'))-mp.loggamma(n+1)
//   -mp.log(1-A)+mp.log(mp.quad(f,[0,1,4,16,64,256])))
// Next Taylor term <1e-160; the v>256 integral contributes <1e-111.
TEST_CASE("pig: large order masses and upper log quantiles") {
  CHECK(cppunivariate::distributions::dpig(100001,20,3,true)==doctest::Approx(-391.7895462691783452).epsilon(2e-14));
  CHECK(cppunivariate::distributions::ppig(100001,20,3,false,true)==doctest::Approx(-386.2075431663518493).epsilon(2e-14));
  CHECK(cppunivariate::distributions::dpig(1e100,2,3,true)/1e100==doctest::Approx(-.31845373111853461581).epsilon(2e-15));
  CHECK(cppunivariate::distributions::ppig(1e100,2,3,false,true)/1e100==doctest::Approx(-.31845373111853461581).epsilon(2e-15));
  CHECK(cppunivariate::distributions::ppig(1e100,2,3,true,true)==0);
  for(double lp:{-10000.,-1e6}) {
    const double q=cppunivariate::distributions::qpig(lp,20,3,false,true);
    REQUIRE(std::isfinite(q));
    CHECK(cppunivariate::distributions::ppig(q,20,3,false,true)<=lp);
    CHECK(cppunivariate::distributions::ppig(q-1,20,3,false,true)>lp);
  }
  CHECK(std::isfinite(cppunivariate::distributions::dpig(std::numeric_limits<double>::max(),2,3,true)));
}



TEST_CASE("PIG P3 zero shape preserves held means") {
 const std::vector<int> z{0,0}, pos{0,1};
 auto r=cppunivariate::pig_mle({z.data(),z.size()});
 CHECK(r.status==cppunivariate::PigStatus::Ok); CHECK(r.mean==1); CHECK(r.shape==0); CHECK(r.logLik==0);
 r=cppunivariate::pig_mle({z.data(),z.size()},1e-9,200,2,0);
 CHECK(r.status==cppunivariate::PigStatus::Ok); CHECK(r.mean==2);
 CHECK(cppunivariate::pig_mle({pos.data(),pos.size()},1e-9,200,2,0).status==cppunivariate::PigStatus::InvalidInput);
}

TEST_CASE("pig: zero boundary is the attained point mass") {
  const double inf=std::numeric_limits<double>::infinity();
  const double nan=std::numeric_limits<double>::quiet_NaN();
  cppunivariate::distributions::StdRng<> rng(290);
  for(double x : {-inf,-1.0,-.5,0.0,.5,1.0,inf}) {
    const double mass=x==0?1:0;
    CHECK(cppunivariate::distributions::dpig(x,2,0,false)==mass);
    CHECK(cppunivariate::distributions::dpig(x,2,0,true)==(mass==1?0:-inf));
    for(bool lower : {false,true}) {
      const double cdf=(x>=0)==lower?1:0;
      CHECK(cppunivariate::distributions::ppig(x,2,0,lower,false)==cdf);
      CHECK(cppunivariate::distributions::ppig(x,2,0,lower,true)==(cdf==1?0:-inf));
    }
  }
  for(bool lower : {false,true}) {
    for(double p : {0.0,.1,.5,1.0})
      CHECK(cppunivariate::distributions::qpig(p,2,0,lower,false)==0);
    for(double p : {-inf,-10000.0,-1.0,0.0})
      CHECK(cppunivariate::distributions::qpig(p,2,0,lower,true)==0);
    CHECK(std::isnan(cppunivariate::distributions::qpig(nan,2,0,lower,true)));
    CHECK(std::isnan(cppunivariate::distributions::ppig(nan,2,0,lower,true)));
  }
  CHECK(std::isnan(cppunivariate::distributions::dpig(nan,2,0,false)));
  for(int i=0;i<20;++i) CHECK(cppunivariate::distributions::rpig(rng,2,0)==0);
}

TEST_CASE("pig: face comparison cannot resolve rounded extreme objectives") {
  const double magnitude=1e100;
  CHECK(cppunivariate::compare_face(-magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(-magnitude,-2*magnitude,2e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-2*magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(-10.,-10.,2e-8)==cppunivariate::FaceComparison::FirstWins);
}

TEST_CASE("pig: audited huge held-parameter magnitude") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<int> x{1,2};
  cppunivariate::ispan s{x.data(),x.size()};
  const auto r=cppunivariate::pig_mle(s,1e-9,200,1e100,nan);
  CHECK((r.status==cppunivariate::PigStatus::Ok || r.status==cppunivariate::PigStatus::SearchFailure));
  CHECK(r.status!=cppunivariate::PigStatus::PoissonLimit);
}

TEST_CASE("face comparison: nonfinite values and finite extremes") {
  const double inf=std::numeric_limits<double>::infinity();
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double big=std::numeric_limits<double>::max();
  CHECK(cppunivariate::compare_face(nan,0,1e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(-inf,-inf,1e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(0,-inf,1e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-inf,0,1e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(big,-big,1e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-big,big,1e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(0,1e-8,1e-8)==cppunivariate::FaceComparison::NumericalTie);
}

TEST_CASE("pig: held-shape certificate has a unique signed root") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(const std::vector<int> x : {std::vector<int>{1}, {0,0,0,0,10}, {1,2,3,5}, {0,1,2,3}}) {
    const cppunivariate::pig_detail::Counts data(pig_span(x));
    for(double shape : {1e-300,1e-20,.01,3.,1e20,1e300}) {
      const auto r=cppunivariate::pig_mle(pig_span(x),1e-9,200,nan,shape);
      REQUIRE(r.status==cppunivariate::PigStatus::Ok);
      CHECK(r.mean>=data.average/2*(1-1e-9));
      CHECK(r.mean<2*data.average);
      CHECK(r.shape==shape);
      CHECK(r.logLik==doctest::Approx(data.likelihood(r.mean,shape)).epsilon(1e-13));
      for(double scale : {.25,.5,.9,1.1,2.,4.})
        CHECK(data.likelihood(r.mean*scale,shape)<=r.logLik+1e-10*x.size());
    }
  }
  // x=1: Q_0=1, so D=1 and the score equation is mu=(1+q)/2.
  // shape=1/9 and mu=2/3 give q=1/3 exactly.
  const std::vector<int> one{1};
  const auto r=cppunivariate::pig_mle(pig_span(one),1e-10,200,nan,1./9);
  REQUIRE(r.status==cppunivariate::PigStatus::Ok);
  CHECK(r.mean==doctest::Approx(2./3).epsilon(1e-10));
}

TEST_CASE("pig: held-shape certificate never accepts an unchecked iterate") {
  const std::vector<int> x{0,0,0,0,10};
  const auto stopped=cppunivariate::pig_mle(pig_span(x),1e-12,1,NAN,3);
  CHECK(stopped.status==cppunivariate::PigStatus::SearchFailure);
  CHECK_FALSE(stopped.stop_detail.budget_exhausted);
  REQUIRE(stopped.stop_detail.candidates.size()==1);
  CHECK(stopped.stop_detail.candidates[0].coef[1]==3);
  CHECK(std::isinf(stopped.stop_detail.loglik_upper));
  const auto too_tight=cppunivariate::pig_mle(pig_span(x),1e-30,200,NAN,3);
  CHECK(too_tight.status==cppunivariate::PigStatus::SearchFailure);
  const std::vector<int> huge{std::numeric_limits<int>::max()};
  const auto budget=cppunivariate::pig_mle(pig_span(huge),1e-9,1,NAN,3);
  CHECK(budget.status==cppunivariate::PigStatus::SearchFailure);
  CHECK(budget.stop_detail.budget_exhausted);
  CHECK(budget.stop_detail.candidates.empty());
}

TEST_CASE("pig: Bessel degree recurrence agrees with explicit polynomials") {
  // Q_1(y)=1+y, Q_2(y)=1+3y+3y^2, y=1/z.
  for(double z : {.001,.1,1.,10.,1000.}) {
    const double y=1/z;
    const std::vector<int> x{0,1,2,3};
    const cppunivariate::pig_detail::Counts data(pig_span(x));
    const double exact=(6+y/(1+y)+(3*y+6*y*y)/(1+3*y+3*y*y))/4;
    CHECK(cppunivariate::pig_detail::mean_certificate_degree(data,std::log(z))==
      doctest::Approx(exact).epsilon(1e-13));
  }
}

TEST_CASE("pig: held-shape fit matches independent 90-digit factorial oracle") {
  // Generated by dev/research/evaluations/2026-11-06-pig-cert-data/oracle.py.
  const std::vector<int> x{0,0,0,0,10};
  const std::vector<double> shapes{1e-20,.01,3,1e20};
  const std::vector<double> means{1.90000000005656854249,1.95416471760289943742,
    2.42165778251079467625,2.00000000000000000014};
  const std::vector<double> likelihoods{-27.35986861598906181955,-7.19009147587577648792,
    -10.95251365292649097306,-18.17294076747606220035};
  for(std::size_t i=0;i<shapes.size();++i) {
    const auto r=cppunivariate::pig_mle(pig_span(x),1e-10,200,NAN,shapes[i]);
    REQUIRE(r.status==cppunivariate::PigStatus::Ok);
    CHECK(r.mean==doctest::Approx(means[i]).epsilon(1e-10));
    CHECK(r.logLik==doctest::Approx(likelihoods[i]).epsilon(1e-12));
  }
}

TEST_CASE("pig: held-shape zero supremum has exact face details") {
  const std::vector<int> x{0,0,0};
  const auto r=cppunivariate::pig_mle(pig_span(x),1e-9,200,NAN,3);
  CHECK(r.status==cppunivariate::PigStatus::ZeroLimit);
  CHECK(r.stop_detail.limit_model==cppunivariate::StopModel::Pois);
  REQUIRE(r.stop_detail.limit_coef.size()==1);
  CHECK(r.stop_detail.limit_coef[0]==0);
  CHECK(r.stop_detail.limit_loglik==0);
  CHECK(r.stop_detail.loglik_lower==0);
  CHECK(r.stop_detail.loglik_upper==0);
}

TEST_CASE("pig: held-shape certificate protects the mass recurrence integer endpoint") {
  const std::vector<int> huge{std::numeric_limits<int>::max()};
  const auto r=cppunivariate::pig_mle(pig_span(huge),1e-9,100000,NAN,3);
  CHECK(r.status==cppunivariate::PigStatus::SearchFailure);
  CHECK_FALSE(r.stop_detail.budget_exhausted);
  CHECK(r.stop_detail.candidates.empty());
}

TEST_CASE("pig: held-mean binary-count shape is a certified closed form") {
  for(const std::vector<int> x : {std::vector<int>{1}, {0,1}, {0,1,1}}) {
    const cppunivariate::pig_detail::Counts data(pig_span(x));
    for(double mu : {.1,1.,2.,3.,10.,1e300}) {
      const auto r=cppunivariate::pig_mle(pig_span(x),1e-9,1,mu,NAN);
      if(mu<=2*data.average) {
        REQUIRE(r.status==cppunivariate::PigStatus::PoissonLimit);
        CHECK(r.stop_detail.limit_model==cppunivariate::StopModel::Pois);
        REQUIRE(r.stop_detail.limit_coef.size()==1);
        CHECK(r.stop_detail.limit_coef[0]==mu);
        CHECK(r.stop_detail.loglik_lower<=r.logLik);
        CHECK(r.stop_detail.loglik_upper>=r.logLik);
      } else {
        REQUIRE(r.status==cppunivariate::PigStatus::Ok);
        CHECK(r.mean==mu);
        CHECK(r.iterations==0);
        const double w=std::sqrt(1-2*data.average/mu);
        const double shape=2*data.average*data.average/(w*(1+w)*(1+w));
        CHECK(r.shape==doctest::Approx(shape).epsilon(1e-13));
        for(double factor : {.1,.5,.9,1.1,2.,10.})
          CHECK(data.likelihood(mu,r.shape*factor)<=r.logLik+1e-11);
      }
    }
  }
}

TEST_CASE("pig: binary Poisson seam uses the exact product residual") {
  const std::vector<int> one{1},thirds{0,1,1};
  const auto at=cppunivariate::pig_mle(pig_span(one),1e-9,1,2,NAN);
  CHECK(at.status==cppunivariate::PigStatus::PoissonLimit);
  const auto below=cppunivariate::pig_mle(pig_span(one),1e-9,1,std::nextafter(2.,0.),NAN);
  CHECK(below.status==cppunivariate::PigStatus::PoissonLimit);
  const auto above=cppunivariate::pig_mle(pig_span(one),1e-9,1,std::nextafter(2.,3.),NAN);
  REQUIRE(above.status==cppunivariate::PigStatus::Ok);
  CHECK(std::isfinite(above.shape));
  // Both products can round to 4: their FMA residuals have opposite signs.
  const double mu=4./3,larger=std::nextafter(mu,2.);
  CHECK(mu*3==4);
  CHECK(larger*3==4);
  CHECK(std::fma(mu,3.,-4.)<0);
  CHECK(std::fma(larger,3.,-4.)>0);
  CHECK(cppunivariate::pig_mle(pig_span(thirds),1e-9,1,mu,NAN).status==cppunivariate::PigStatus::PoissonLimit);
  CHECK(cppunivariate::pig_mle(pig_span(thirds),1e-9,1,larger,NAN).status==cppunivariate::PigStatus::Ok);
}

TEST_CASE("pig: globally certified held-mean existence and Poisson boundaries") {
  const std::vector<int> x{0,1,2,3};
  for(double mu : {.5,1.,1.5,2.,3.}) {
    const auto r=cppunivariate::pig_mle(pig_span(x),1e-9,200,mu,NAN);
    if(mu>=1 && mu<=2) {
      REQUIRE(r.status==cppunivariate::PigStatus::PoissonLimit);
      CHECK(r.stop_detail.limit_coef[0]==mu);
      CHECK(r.stop_detail.loglik_upper>=r.logLik);
    } else {
      REQUIRE(r.status==cppunivariate::PigStatus::Ok);
      const cppunivariate::pig_detail::Counts data(pig_span(x));
      for(double factor : {.1,.5,.9,1.1,2.,10.})
        CHECK(data.likelihood(mu,r.shape*factor)<=r.logLik+1e-10);
    }
  }
  const std::vector<int> uncertain{0,1,2};
  const auto tie=cppunivariate::pig_mle(pig_span(uncertain),1e-9,200,1+std::sqrt(1./3),NAN);
  CHECK(tie.status==cppunivariate::PigStatus::SearchFailure);
  CHECK(tie.stop_detail.comparison_tie);
  REQUIRE(tie.stop_detail.candidates.size()==1);
  CHECK(tie.stop_detail.candidates[0].model==cppunivariate::StopModel::Pois);
  CHECK(std::isinf(tie.stop_detail.loglik_upper));
}

TEST_CASE("pig: analytic concavity retains small finite Poisson improvements") {
  std::vector<int> x(10002,0);x.insert(x.end(),10000,2);x.push_back(3);
  const cppunivariate::pig_detail::Counts data(pig_span(x));
  const auto r=cppunivariate::pig_mle(pig_span(x));
  REQUIRE(r.status==cppunivariate::PigStatus::Ok);
  CHECK(r.mean==1);
  CHECK(std::isfinite(r.shape));
  CHECK(r.logLik>data.poisson(1));
  CHECK(r.logLik-data.poisson(1)<1e-8*x.size());
}

TEST_CASE("pig: recurrence moments preserve the Poisson scaled-degree limit") {
  const std::vector<int> x{0,1,2,3};
  const cppunivariate::pig_detail::Counts data(pig_span(x));
  for(double z : {.001,.1,1.,10.,1000.}) {
    const auto m=cppunivariate::pig_detail::shape_moments(data,std::log(z));
    const double y=1/z;
    const double expected=(y/(1+y)+(3*y+6*y*y)/(1+3*y+3*y*y))/4;
    CHECK(m.degree==doctest::Approx(expected).epsilon(1e-13));
    CHECK(m.scaled_degree==doctest::Approx(z*expected).epsilon(1e-13));
  }
  const auto at_infinity=cppunivariate::pig_detail::shape_moments(data,1000);
  CHECK(at_infinity.degree==0);
  CHECK(at_infinity.scaled_degree==1); // average k*(k-1)/2
}

TEST_CASE("pig: count budgets guard every subset without hiding analytic faces") {
  const int huge=std::numeric_limits<int>::max();
  const std::vector<int> over{0,0,huge},constant{huge};
  for(unsigned mask=0;mask<4;++mask) {
    const auto r=cppunivariate::pig_mle(pig_span(over),1e-9,1,(mask&1)?3:NAN,(mask&2)?3:NAN);
    CHECK(r.status==cppunivariate::PigStatus::SearchFailure);
    CHECK(r.stop_detail.budget_exhausted);
  }
  const auto face=cppunivariate::pig_mle(pig_span(constant),1e-9,1);
  CHECK(face.status==cppunivariate::PigStatus::PoissonLimit);
  CHECK(std::isfinite(face.logLik));
}

TEST_CASE("pig: global shape certificates match 180-digit profile oracles") {
  // shape-oracle.py: independent finite coefficients, 220 high-precision
  // bisections; separate Bessel-K likelihood check for counts below 100.
  struct Fixture {std::vector<std::pair<int,int>> counts;double held,mean,shape,ll;};
  const std::vector<Fixture> fixtures{
    {{{0,4},{10,1}},NAN,2.0,0.036798885007980536,-7.026586378878994},
    {{{0,4},{10,1}},3.0,3.0,0.03610063696291194,-7.0316229651747655},
    {{{0,1},{1,1},{2,1},{3,1}},0.5,0.5,0.6960898083055955,-8.296365479712742},
    {{{0,1},{1,1},{2,1},{3,1}},3.0,3.0,2.3672070492847226,-7.043127314605914},
    {{{1,1},{2,1}},1e+100,1e+100,0.8201941016011037,-4.014692697829875},
    {{{2,1},{5,1}},0.0001,0.0001,4.000384012288317e-09,-25.416382474780118},
    {{{10,1},{40,1}},NAN,25.0,48.603705013091535,-8.101749823126013},
    {{{960,1},{1040,1}},NAN,1000.0,1665003.2886574783,-10.215382098531208},
    {{{0,10002},{2,10000},{3,1}},NAN,1.0,6666.416994630708,-26936.26345258837},
    {{{0,114},{1,145},{2,100},{3,55},{4,37},{5,16},{6,14},{7,8},{8,5},{9,3},{10,1},{17,2}},NAN,1.978,3.5119672752062097,-930.1830192843388},
  };
  for(const auto& f:fixtures) {
    std::vector<int> x;
    for(const auto& kv:f.counts)x.insert(x.end(),kv.second,kv.first);
    const auto r=cppunivariate::pig_mle(pig_span(x),1e-9,200,f.held,NAN);
    REQUIRE(r.status==cppunivariate::PigStatus::Ok);
    CHECK(r.mean==doctest::Approx(f.mean).epsilon(1e-12));
    CHECK(r.shape==doctest::Approx(f.shape).epsilon(1e-6));
    CHECK(r.logLik==doctest::Approx(f.ll).epsilon(1e-11));
  }
}

TEST_CASE("pig: certified point polish preserves the R golden tolerance") {
  const auto g=mltest::load_distribution_golden("pig");
  std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
  const auto r=cppunivariate::pig_mle(pig_span(x));
  REQUIRE(r.status==cppunivariate::PigStatus::Ok);
  CHECK(std::abs(r.shape-g.estimates.at("shape"))<=1e-7*std::abs(g.estimates.at("shape")));
  const auto stopped=cppunivariate::pig_mle(pig_span(x),1e-30,30);
  CHECK(stopped.status==cppunivariate::PigStatus::SearchFailure);
  CHECK_FALSE(stopped.stop_detail.budget_exhausted);
}

#include "cppunivariate/core/inference_join.hpp"
TEST_CASE("pig: analytic covariance derivatives and subset projection") {
  const std::array<double,8> theta{2,3};
  const std::vector<double> x{0,1,2,3,5};
  const auto full=cppunivariate::join_family_inference({x.data(),x.size()},"pig",theta,3);
  REQUIRE(full.status==cppunivariate::InferenceStatus::Ok);
  for(unsigned mask=0;mask<(1u<<2);++mask) {
    std::size_t emitted=0;
    const auto r=cppunivariate::join_family_inference({x.data(),x.size()},"pig",theta,mask,[&](const auto&){++emitted;});
    REQUIRE(r.status==cppunivariate::InferenceStatus::Ok);
    CHECK(emitted==x.size());
    for(std::size_t i=0;i<r.dimension;++i) {
      const auto a=r.free_indices[i];CHECK(r.score[i]==doctest::Approx(full.score[a]));
      for(std::size_t j=0;j<r.dimension;++j) {
        const auto b=r.free_indices[j];
        CHECK(r.observed[i*r.dimension+j]==doctest::Approx(full.observed[a*2+b]));
        CHECK(r.opg[i*r.dimension+j]==doctest::Approx(full.opg[a*2+b]));
      }
    }
  }
  auto bad=theta;bad[1]=0;
  CHECK(cppunivariate::join_family_inference({x.data(),x.size()},"pig",bad,1).status==cppunivariate::InferenceStatus::BoundaryOrLimit);
  CHECK(cppunivariate::join_family_inference({x.data(),x.size()},"pig",theta,1u<<2).status==cppunivariate::InferenceStatus::InvalidSubset);
}

#include "cppunivariate/distributions/pig/dpqr.hpp"
TEST_CASE("pig: failed integer inversion reports NaN") {
  CHECK(std::isnan(cppunivariate::distributions::qpig(.5,1e8,1e8,true,false)));
}

TEST_CASE("pig: Poisson-side score retains coefficient accuracy") {
  std::vector<int> x(10002,0);x.insert(x.end(),10000,2);x.push_back(3);
  const auto r=cppunivariate::pig_mle({x.data(),x.size()});
  REQUIRE(r.status==cppunivariate::PigStatus::Ok);
  // Independent 90-digit closed Bessel-polynomial score, retained in
  // dev/research/evaluations/2026-11-10-acc-newton-data/pig-face.py.
  CHECK(r.shape==doctest::Approx(6666.41699463070802775319433).epsilon(1e-10).scale(0));
}

TEST_CASE("pig: unrepresentable subnormal shape is not Ok") {
  const std::vector<int> x{0,2};
  const auto r=cppunivariate::pig_mle({x.data(),x.size()},1e-10,200,1e-158,NAN);
  CHECK(r.status==cppunivariate::PigStatus::AccuracyFailure);
}

TEST_CASE("pig: the zero-count CDF is its atom even at tiny shape") {
  CHECK(cppunivariate::distributions::ppig(0,1,1e-8,true,true)==
        doctest::Approx(-0.0001414113565908629).epsilon(1e-12).scale(0));
}

TEST_CASE("pig: extreme count-tail regression") {
  CHECK(cppunivariate::distributions::ppig(1,1,1e-8,true,true) == doctest::Approx(-7.070317853112745e-05).epsilon(1e-10).scale(0));
  CHECK(cppunivariate::distributions::ppig(2,1e8,1,true,true) == doctest::Approx(-0.6562700043193149).epsilon(1e-11));
}

TEST_CASE("pig: representable large integration coordinate") {
  // n*(1-A)<1e16 while n*(1-A)/A>1e16: a representable coordinate
  // must reach the remainder check rather than an arbitrary size refusal.
  const double upper=cppunivariate::distributions::ppig(2.0004e19,10,.1,false,true);
  CHECK(std::isfinite(upper));
  // Independent positive-mass/geometric bounds put the exact log tail
  // between -9999500333187631.753 and -9999500333187624.150.
  CHECK(upper == doctest::Approx(-9999500333187624.0).epsilon(1e-12).scale(0));
}

TEST_CASE("pig: slow reference discrete quantile round trips on every truth point") {
  mltest::discrete_roundtrips("pig",
    [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::ppig(x,t[0], t[1],l,g);},
    [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qpig(x,t[0], t[1],l,g);});
}

TEST_CASE("pig: cached CDF preserves uncached values across parameters and scales") {
  for(double mean:{1.,2.}) for(double shape:{.3,3.})
    for(bool lower:{false,true}) for(bool log:{false,true}) {
      const double expected=cppunivariate::distributions::pig_probability_uncached(7,mean,shape,lower,log);
      CHECK(cppunivariate::distributions::ppig(7,mean,shape,lower,log)==expected);
      CHECK(cppunivariate::distributions::ppig(7,mean,shape,lower,log)==expected);
    }
}
