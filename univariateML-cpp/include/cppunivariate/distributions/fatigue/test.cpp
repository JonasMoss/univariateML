#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/fatigue/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

TEST_CASE("fatigue") {
  auto g = mltest::load_distribution_golden("fatigue");
  auto r = cppunivariate::fatigue_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.alpha == doctest::Approx(g.estimates.at("alpha")).epsilon(mltest::kGolden_fatigue).scale(0));
  CHECK(r.beta == doctest::Approx(g.estimates.at("beta")).epsilon(mltest::kGolden_fatigue).scale(0));
  // Wrapper hardcodes mu=0; golden also stores 0.
  CHECK(g.estimates.at("mu") == 0.0);
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_fatigue).scale(0));
}

#undef C_APPROX
#undef C_APPROX_FD

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("fatigue: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("fatigue",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dfatigue(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pfatigue(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qfatigue(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rfatigue(rng, p[0], p[1], p[2]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// F=lambda x:m.erfc(-(m.sqrt(x)-1/m.sqrt(x))/m.sqrt(2))/2
TEST_CASE("fatigue: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pfatigue(1,1,1,0,true,true) == doctest::Approx(-std::log(2.0)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qfatigue(-INFINITY,1,1,0,true,true) == doctest::Approx(0).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::pfatigue(INFINITY,1,1,0,false,true) == -INFINITY);
}

// mpmath: mp.dps=100; z=findroot(lambda z:log(erfc(z/sqrt(2))/2)+10000,141)
// h=3*z/2; lower=2/(sqrt(1+h*h)+h)**2; upper=2*(sqrt(1+h*h)+h)**2
TEST_CASE("fatigue: normal transform in extreme log tails") {
  CHECK(cppunivariate::distributions::qfatigue(-10000,3,2,0,true,true) == doctest::Approx(1.1117514052021604e-5).epsilon(1e-12));
  CHECK(cppunivariate::distributions::qfatigue(-10000,3,2,0,false,true) == doctest::Approx(359792.6641948019).epsilon(1e-12));
  CHECK(cppunivariate::distributions::pfatigue(359792.6641948019,3,2,0,false,true) == doctest::Approx(-10000).epsilon(1e-12));
}


TEST_CASE("fatigue: exact held-beta alpha and held-alpha profile") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{.2,.2,1,3};
  auto r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200,nan,1);
  double ss=0;for(double v:x)ss+=v+1/v-2;
  CHECK(r.beta==1);CHECK(r.alpha==doctest::Approx(std::sqrt(ss/4)));
  for(double a:{.7,3.}) {
    r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200,a);
    CHECK(r.alpha==a);CHECK(std::isfinite(r.logLik));
    double ll=0;for(double v:x)ll+=cppunivariate::distributions::dfatigue(v,a,r.beta,0,true);
    CHECK(r.logLik==doctest::Approx(ll));
  }
  const std::vector<double> c{1,1};
  CHECK(std::isnan(cppunivariate::fatigue_mle(mltest::as_span(c),1e-10,200,nan,1).logLik));
  CHECK(std::isfinite(cppunivariate::fatigue_mle(mltest::as_span(c),1e-10,200,3,1).logLik));
}

TEST_CASE("fatigue: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("fatigue");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::fatigue_mle(x,1e-8,1).status==cppunivariate::FatigueStatus::SearchFailure);
}

TEST_CASE("fatigue core threshold transform matches former adapter") {
  const auto g=mltest::load_distribution_golden("fatigue");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=0;mask<4;++mask) {
    std::vector<double> shifted=g.data;
    for(auto &v:shifted)v-=-0.25;
    const double a=(mask&1)?0.8:nan,b=(mask&2)?1.5:nan;
    const auto old=cppunivariate::fatigue_mle(mltest::as_span(shifted),mltest::kFitTol,100,a,b);
    const auto r=cppunivariate::fatigue_mle_shifted(mltest::as_span(g.data),-0.25,mltest::kFitTol,100,a,b);
    CHECK(r.status==old.status);
    CHECK(r.alpha==old.alpha);
    CHECK(r.beta==old.beta);
    CHECK(r.logLik==old.logLik);
  }
}

TEST_CASE("fatigue certificate: reciprocal conditional maxima are a structured tie") {
  const std::vector<double> x{1};
  const auto r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200,3);
  CHECK(r.status==cppunivariate::FatigueStatus::SearchFailure);
  REQUIRE(r.stop_detail.comparison_tie);
  REQUIRE(r.stop_detail.candidates.size()==2);
  CHECK(r.stop_detail.candidates[0].coef[1]==doctest::Approx((7-3*std::sqrt(5.))/2).epsilon(1e-12));
  CHECK(r.stop_detail.candidates[1].coef[1]==doctest::Approx((7+3*std::sqrt(5.))/2).epsilon(1e-12));
  CHECK(r.stop_detail.candidates[0].loglik_lower==doctest::Approx(r.stop_detail.candidates[1].loglik_lower));
  CHECK(cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200,2).beta==1);
}

TEST_CASE("fatigue certificate: rational inclusion holds over dispersed scales") {
  const std::vector<double> x{.001,.02,.3,1,2,40};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double a:{nan,.7,3.,8.}) {
    const auto r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200,a);
    REQUIRE(r.status==cppunivariate::FatigueStatus::Ok);
    double score=0;
    for(double v:x)score+=1/(v+r.beta)-1/(2*r.beta)+v/(2*r.alpha*r.alpha*r.beta*r.beta)-1/(2*r.alpha*r.alpha*v);
    CHECK(std::abs(score*r.beta)<1e-9);
  }
}

TEST_CASE("fatigue certificate: nonconstant reciprocal data retain the two-mode tie") {
  const std::vector<double> x{.5,2};
  const auto r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,300,3);
  CHECK(r.status==cppunivariate::FatigueStatus::SearchFailure);
  CHECK(r.stop_detail.comparison_tie);
  REQUIRE(r.stop_detail.candidates.size()>=2);
  CHECK(r.stop_detail.candidates[0].coef[1]*r.stop_detail.candidates[1].coef[1]==doctest::Approx(1).epsilon(1e-10));
}

TEST_CASE("fatigue certificate: nearly coincident mean bounds are solved in an offset coordinate") {
  // TASK-214.39: was a numerical stop. For x_i = 1 + k eps, k = 0..4: alpha = sqrt(2) eps, beta = 1 + 2 eps.
  const double e=std::numeric_limits<double>::epsilon();
  const std::vector<double> x{1,1+e,1+2*e,1+3*e,1+4*e};
  const auto r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,300);
  REQUIRE(r.status==cppunivariate::FatigueStatus::Ok);
  CHECK_FALSE(r.certified);
  CHECK_FALSE(r.stop_detail.comparison_tie);
  CHECK_FALSE(r.stop_detail.budget_exhausted);
  CHECK(std::abs(r.alpha/(std::sqrt(2.0)*e)-1)<=1e-10);
  CHECK(r.beta==1+2*e);
  REQUIRE(r.stop_detail.candidates.size()==1);
  CHECK(r.stop_detail.candidates[0].coef[0]>0);
}

TEST_CASE("fatigue certificate: held huge alpha avoids squaring alpha") {
  const std::vector<double> x{1e308};
  const auto r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200,1e155,1);
  REQUIRE(r.status==cppunivariate::FatigueStatus::Ok);
  CHECK(r.logLik==doctest::Approx(cppunivariate::distributions::dfatigue(x[0],1e155,1,0,true)).epsilon(1e-12));
}

TEST_CASE("fatigue certificate: rounded threshold shifts cannot assert nonexistence") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{1,2},one{1};
  CHECK(cppunivariate::fatigue_mle_shifted(mltest::as_span(x),-1e300,1e-10,200).status==cppunivariate::FatigueStatus::SearchFailure);
  CHECK(cppunivariate::fatigue_mle_shifted(mltest::as_span(one),.1,1e-10,200,nan,.9).status==cppunivariate::FatigueStatus::SearchFailure);
  CHECK(cppunivariate::fatigue_mle_shifted(mltest::as_span(one),0,1e-10,200,nan,1).status==cppunivariate::FatigueStatus::Unbounded);
}

TEST_CASE("fatigue certificate: subnormal rescaling must retain coefficient accuracy") {
  const double unit=1e-320;
  const std::vector<double> x{unit,3*unit,4*unit};
  const auto r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200);
  CHECK(r.status==cppunivariate::FatigueStatus::SearchFailure);
}

TEST_CASE("fatigue: exact-input extreme arithmetic regression") {
  CHECK(cppunivariate::distributions::qfatigue(-0.6931471805599453,100000000.0,1.0,0.0,true,true) == doctest::Approx(1.0000000029064942).epsilon(1e-10).scale(0));
}

TEST_CASE("fatigue: physical inverse survives latent underflow") {
 CHECK(cppunivariate::distributions::qfatigue(-.6931471805599453,4.9406564584124654e-324,1e308,-1e308,true,true)==doctest::Approx(1.4359989127576843e-32).epsilon(1e-9).scale(0));
}

TEST_CASE("fatigue: the analytic constant-sample pair is a certified estimate") {
  // Constant sample with alpha held above 2: the closed-form beta roots x*r and
  // x/r are evaluated as an Ok fit would be; the first listed is returned.
  const std::vector<double> one{1};
  const auto r=cppunivariate::fatigue_mle_shifted(mltest::as_span(one),0,1e-10,200,3.0);
  REQUIRE(r.status==cppunivariate::FatigueStatus::SearchFailure);
  REQUIRE(r.stop_detail.comparison_tie);
  const auto o=cppunivariate::outcome(r);
  CHECK(o.kind==cppunivariate::OutcomeKind::Estimate);
  CHECK(o.certified);
  CHECK(o.uniqueness_known);
  CHECK_FALSE(o.unique);
  const auto& c=r.stop_detail.candidates[cppunivariate::comparison_choice(r)];
  CHECK(c.loglik_lower==c.loglik_upper);
  CHECK(c.coef[0]==3.0);
}

TEST_CASE("fatigue transformed-data range failures have an arithmetic outcome") {
  const std::vector<double> x{1,2};
  const auto r=cppunivariate::fatigue_mle_shifted(mltest::as_span(x),-1e300,1e-10,200);
  CHECK(r.transformation_failure);
  CHECK(cppunivariate::outcome(r).reason==cppunivariate::OutcomeReason::Evaluation);
}

TEST_CASE("fatigue: alpha held above 2 with a near-constant sample lists both reciprocal roots (TASK-214.40)") {
  // Archive 001458, 001459, 001461, 001462 and 009817: the profile has two
  // separated maxima, the scale beta and its reciprocal twin, whose
  // likelihoods agree within the certificate's tie margin 1e-10 n.
  const double alpha=0x1.6ddf01bbcd369p+1;
  const std::vector<std::vector<double>> samples{
    {0x1.cf2797fd493b0p-1,0x1.cf27984afd927p-1,0x1.cf279898b1e9fp-1,0x1.cf2798e666417p-1,0x1.cf279981cef06p-1},
    {0x1.cf279898adef7p-1,0x1.cf279898afecbp-1,0x1.cf279898b1e9fp-1,0x1.cf279898b3e73p-1,0x1.cf279898b7e1bp-1},
    {0x1.ffffff543388fp-1,0x1.ffffffaa19c47p-1,0x1.0000000000000p+0,0x1.0000002af31dcp+0,0x1.00000080d9595p+0},
    {0x1.fffffffffb9a2p-1,0x1.fffffffffdcd1p-1,0x1.0000000000000p+0,0x1.0000000001198p+0,0x1.00000000034c7p+0},
    {0x1.ffffc5e52104ep-1,0x1.ffffd966946a6p-1,0x1.ffffe4bf1ae42p-1,0x1.ffffed983b132p-1,0x1.fffff54ee8bd7p-1,0x1.fffffc7d349c1p-1,
     0x1.000001c165b50p+0,0x1.000005588bbdcp+0,0x1.00000933e2cb1p+0,0x1.00000da073479p+0,0x1.0000134cb73f4p+0,0x1.00001d0d72c9ap+0}};
  for(const auto& x:samples) {
    const auto r=cppunivariate::fatigue_mle_shifted(mltest::as_span(x),0,1e-10,100,alpha);
    const auto o=cppunivariate::outcome(r);
    CHECK(o.kind==cppunivariate::OutcomeKind::Estimate);
    CHECK_FALSE(o.certified);
    CHECK_FALSE(o.uniqueness_known);
    // Both roots are listed and are reciprocal twins up to the sample scale.
    REQUIRE(r.stop_detail.candidates.size()==2);
    const double b0=r.stop_detail.candidates[0].coef[1],b1=r.stop_detail.candidates[1].coef[1];
    CHECK(std::abs(std::log(b0/b1))>1);
    CHECK(std::abs(r.stop_detail.candidates[0].loglik_lower-r.stop_detail.candidates[1].loglik_lower)<=1e-10*x.size());
    CHECK(r.stop_detail.candidates[0].loglik_lower>=r.stop_detail.candidates[1].loglik_lower);
  }
}

TEST_CASE("fatigue near-constant sample with mu held returns the rounded maximizer, uncertified") {
  // TASK-214.39: alpha = (sqrt 2 / 3) eps = 1.005e-16 and beta = c + (c+ - c) / 3, which rounds to c.
  // alpha is the maximizer's, not the best alpha at the rounded beta (22% larger).
  const std::vector<double> x{0x1.0a95766f46a5ep+1,0x1.0a95766f46a5ep+1,0x1.0a95766f46a5fp+1};
  const auto r=cppunivariate::fatigue_mle_shifted(mltest::as_span(x),0.0,1e-10,100);
  REQUIRE(r.status==cppunivariate::FatigueStatus::Ok);
  CHECK_FALSE(r.certified);
  CHECK(cppunivariate::outcome(r).kind==cppunivariate::OutcomeKind::Estimate);
  CHECK(std::abs(r.alpha/1.00517138574689837891345158203e-16-1)<=1e-10);
  CHECK(std::abs(r.beta/2.08268623765370290238744625337-1)<=1e-10);
  CHECK(r.beta==x[0]);
  CHECK(r.logLik==doctest::Approx(103.300819121222).epsilon(1e-10));  // value at the returned point; supremum 104.0508
}

TEST_CASE("fatigue: the joint fixed-threshold score has one certified maximum") {
  const std::vector<double> x{2., 3., 8., 102.};
  const auto fit = cppunivariate::fatigue_mle_shifted(mltest::as_span(x), 1., 1e-10, 100);
  REQUIRE(fit.status == cppunivariate::FatigueStatus::Ok);
  CHECK(fit.certified);
  CHECK(fit.singleton_proved);
  const double harmonic = 4/(1. + 1./2 + 1./7 + 1./101);
  const double arithmetic = (1. + 2 + 7 + 101)/4;
  CHECK(fit.beta > harmonic);
  CHECK(fit.beta < (harmonic + arithmetic)/2);
  const double xi = 4/(1/(1 + fit.beta) + 1/(2 + fit.beta)
                     + 1/(7 + fit.beta) + 1/(101 + fit.beta));
  const double delta = fit.beta - harmonic;
  CHECK(std::abs(harmonic*(arithmetic - harmonic) - delta*(xi - delta)) < 1e-8);
}
