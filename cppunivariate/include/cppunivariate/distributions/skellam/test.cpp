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
  CHECK(r.mu1==doctest::Approx(g.estimates.at("mu1")).epsilon(1e-5));
  CHECK(r.mu2==doctest::Approx(g.estimates.at("mu2")).epsilon(1e-5));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-8));
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
// Oracle gap: see this family's truth.py for the missing controlled route.
TEST_CASE("skellam: d/p/q/r against the truth table" * doctest::skip()) {
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
