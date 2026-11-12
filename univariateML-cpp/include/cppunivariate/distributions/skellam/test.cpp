#include "test_support.hpp"
#include "cppunivariate/distributions/skellam/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
TEST_CASE("skellam: mass, tails and extreme log quantiles") {
  CHECK(cppunivariate::distributions::dskellam(0,1,1,true)==doctest::Approx(-1.1760064585170438));
  CHECK(cppunivariate::distributions::dskellam(0,1e12,1e12,true)==doctest::Approx(-15.081022681448863));
  CHECK(cppunivariate::distributions::dskellam(-1,0,0)==0);
  CHECK(cppunivariate::distributions::dskellam(0,0,0)==1);
  CHECK(cppunivariate::distributions::dskellam(1.5,2,3)==0);
  double sum=0;for(int k=-80;k<=80;++k) sum+=cppunivariate::distributions::dskellam(k,4,3);
  CHECK(sum==doctest::Approx(1));
  for(bool lower:{true,false}) for(double lp:{-0.01,-1.0,-20.0,-1000.0}) {
    const double q=cppunivariate::distributions::qskellam(lp,4,3,lower,true);
    const double f=cppunivariate::distributions::pskellam(q,4,3,lower,true);
    const double prev=cppunivariate::distributions::pskellam(q-1,4,3,lower,true);
    CHECK((lower ? f>=lp : f<=lp));
    CHECK((lower ? prev<lp : prev>lp));
  }
}
TEST_CASE("skellam: attained faces, fixed subsets, validation") {
  std::vector<double> x{1,1,1,1};
  auto r=cppunivariate::skellam_mle(mltest::as_span(x));
  CHECK(r.status==cppunivariate::SkellamStatus::Ok);CHECK(r.mu1==1);CHECK(r.mu2==0);
  x={-4,-2,-1,0,1,2,5,8};
  r=cppunivariate::skellam_mle(mltest::as_span(x));
  CHECK(r.status==cppunivariate::SkellamStatus::Ok);CHECK(r.mu1-r.mu2==doctest::Approx(1.125));
  for(double h:{-1e-4,1e-4}) {
    double ll=0;for(double v:x) ll+=cppunivariate::distributions::dskellam(v,r.mu1+h,r.mu2,true);
    CHECK(ll<=r.logLik+1e-9);
  }
  CHECK(cppunivariate::skellam_mle(mltest::as_span(x),1e-9,150,0).status==cppunivariate::SkellamStatus::Impossible);
  CHECK(cppunivariate::skellam_mle(mltest::as_span(x),1e-9,1).status==cppunivariate::SkellamStatus::IterationFailure);
  x={0,0};r=cppunivariate::skellam_mle(mltest::as_span(x));CHECK(r.mu1==0);CHECK(r.mu2==0);CHECK(r.logLik==0);
  x={0.5};CHECK(cppunivariate::skellam_mle(mltest::as_span(x)).status==cppunivariate::SkellamStatus::InvalidInput);
}
TEST_CASE("skellam: golden") {
  auto g=mltest::load_distribution_golden("skellam");
  auto r=cppunivariate::skellam_mle(mltest::as_span(g.data));
  CHECK(r.status==cppunivariate::SkellamStatus::Ok);
  CHECK(r.mu1==doctest::Approx(g.estimates.at("mu1")).epsilon(mltest::kGolden_skellam).scale(0));
  CHECK(r.mu2==doctest::Approx(g.estimates.at("mu2")).epsilon(mltest::kGolden_skellam).scale(0));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_skellam).scale(0));
  for(int coordinate=0;coordinate<2;++coordinate) {
    const double h=1e-4*std::max(1.0,coordinate==0?r.mu1:r.mu2);
    double plus=0,minus=0;
    for(double v:g.data) {
      plus+=cppunivariate::distributions::dskellam(v,r.mu1+(coordinate==0?h:0),r.mu2+(coordinate==1?h:0),true);
      minus+=cppunivariate::distributions::dskellam(v,r.mu1-(coordinate==0?h:0),r.mu2-(coordinate==1?h:0),true);
    }
    CHECK(std::abs((plus-minus)/(2*h))<1e-3);
  }
}

TEST_CASE("skellam: standalone sampling and invalid arguments") {
  cppunivariate::distributions::StdRng<> rng(53);
  CHECK(cppunivariate::distributions::rskellam(rng,0,0)==0);
  for(int j=0;j<10;++j) {
    const double v=cppunivariate::distributions::rskellam(rng,4,3);
    CHECK(std::isfinite(v));CHECK(v==std::floor(v));
  }
  CHECK(std::isnan(cppunivariate::distributions::dskellam(0,-1,1)));
  CHECK(std::isnan(cppunivariate::distributions::pskellam(0,INFINITY,1)));
  CHECK(std::isnan(cppunivariate::distributions::qskellam(2,1,1)));
  const std::vector<double> empty;
  CHECK(cppunivariate::skellam_mle(mltest::as_span(empty)).status==cppunivariate::SkellamStatus::InvalidInput);
}

#include "cppunivariate/distributions/skellam/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("skellam: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("skellam",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dskellam(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pskellam(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qskellam(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rskellam(rng, p[0], p[1]); });
}

TEST_CASE("skellam: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("skellam");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::skellam_mle(x,1e-8,1).status==cppunivariate::SkellamStatus::IterationFailure);
}

TEST_CASE("skellam TASK-134 subnormal log tails and near-zero face") {
  const double expected[]={-733.33365531008144,-738.50417158558412,-743.68035323493075};
  for(int k=174;k<=176;++k) {
    const double face=cppunivariate::distributions::pskellam(k,1,0,false,true);
    CHECK(face==doctest::Approx(expected[k-174]).epsilon(2e-13));
    // Coupling: shifting by an independent Poisson(e) changes this tail
    // by a relative factor between exp(-e) and one.
    const double near=cppunivariate::distributions::pskellam(k,1,1e-8,false,true);
    CHECK(near<=face+2e-12);
    CHECK(near>=face-1e-8-2e-12);
    CHECK(cppunivariate::distributions::pskellam(-k-1,1e-8,1,true,true)==near);
  }
}

TEST_CASE("skellam: certified profile dominates a dense finite grid") {
  for(const std::vector<double> x: {std::vector<double>{-4,-2,-1,0,1,2,5,8},
                                  {0,0,1,2,8,20}, {-20,-8,-2,-1,0,0},
                                  {-1,0,1}}) {
    const auto fit=cppunivariate::skellam_mle(mltest::as_span(x));
    REQUIRE(fit.status==cppunivariate::SkellamStatus::Ok);
    double mean=0;for(double v:x) mean+=v;mean/=x.size();
    for(int i=0;i<=240;++i) {
      const double t=std::exp(-12.0+17.0*i/240);
      double ll=0;
      for(double v:x) ll+=cppunivariate::distributions::dskellam(v,t+std::max(mean,0.0),t+std::max(-mean,0.0),true);
      CHECK(ll<=fit.logLik+1e-8*x.size());
    }
  }
}

TEST_CASE("skellam: tail-cover budget retains the feasible rate witness") {
  const std::vector<double> x{-1e9,0,1e9};
  const auto r=cppunivariate::skellam_mle({x.data(),x.size()},1e-9,1);
  CHECK(r.status==cppunivariate::SkellamStatus::IterationFailure);
  REQUIRE(r.stop_detail.budget_exhausted);
  REQUIRE(r.stop_detail.candidates.size()==1);
  const auto& c=r.stop_detail.candidates.front();
  REQUIRE(c.coef.size()==2);
  CHECK(c.coef[0]>0);
  CHECK(c.coef[1]>0);
  double ll=0;
  for(double v:x)ll+=cppunivariate::distributions::dskellam(v,c.coef[0],c.coef[1],true);
  CHECK(ll>=c.loglik_lower);
  CHECK(ll<=c.loglik_upper);
  CHECK(std::isfinite(r.stop_detail.loglik_lower));
  CHECK(r.stop_detail.loglik_upper==INFINITY);
}

TEST_CASE("Skellam nonfinite positive series fails numerically") {
  CHECK(std::isnan(cppunivariate::skellam_detail::logmass(NAN,1,1)));
}

#include "cppunivariate/distributions/skellam/dpqr.hpp"
TEST_CASE("skellam: quantile search guards and representability") {
  CHECK(std::isnan(cppunivariate::distributions::qskellam(.5,1e308,1e308)));
}

TEST_CASE("skellam: large replication preserves the Poisson boundary mean") {
  const double k=2000000001.;const std::vector<double> x(10000000,k);
  const auto r=cppunivariate::skellam_mle(mltest::as_span(x));
  REQUIRE(r.status==cppunivariate::SkellamStatus::Ok);
  CHECK(r.mu1==k);CHECK(r.mu2==0);
}

TEST_CASE("skellam: extreme count-tail regression") {
  CHECK(cppunivariate::distributions::pskellam(99629757,1e8,1,true,false) == doctest::Approx(1.0030217828350785e-300).epsilon(1e-10).scale(0));
  CHECK(cppunivariate::distributions::dskellam(99629757,1e8,1,true) == doctest::Approx(-696.3705538160286).epsilon(1e-12));
}
