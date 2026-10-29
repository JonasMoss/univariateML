#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/slash/mle.hpp"
#include <vector>
TEST_CASE("slash: golden and likelihood") {
  const auto g=mltest::load_distribution_golden("slash");
  const auto f=cppunivariate::slash_mle(mltest::as_span(g.data));
  REQUIRE(f.status==cppunivariate::SlashStatus::Ok);
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(1e-7));
  double ll=0;for(double x:g.data)ll+=cppunivariate::distributions::dslash(x,f.mu,f.sigma,true);
  CHECK(ll==doctest::Approx(f.logLik).epsilon(1e-12));
  std::vector<double> theta={f.mu,f.sigma};
  double norm2=0;
  for(std::size_t j=0;j<theta.size();++j) {
    const double h=1e-5*(1+std::abs(theta[j]));auto plus=theta,minus=theta;
    plus[j]+=h;minus[j]-=h;double score=0;
    for(double x:g.data)score+=(cppunivariate::distributions::dslash(x,plus[0],plus[1],true)-cppunivariate::distributions::dslash(x,minus[0],minus[1],true))/(2*h);
    norm2+=score*score;
  }
  CHECK(std::sqrt(norm2)<1e-3);
}
TEST_CASE("slash: direct log tails and quantiles") {
  for(bool lower:{false,true})for(double lp:{-1e-6,-.6931471805599453,-10.,-100.,-600.}) {
    const double q=cppunivariate::distributions::qslash(lp,0,1,lower,true);
    CHECK(cppunivariate::distributions::pslash(q,0,1,lower,true)==doctest::Approx(lp).epsilon(1e-10));
  }
  CHECK(std::isinf(cppunivariate::distributions::qslash(-1000,0,1,true,true)));
  CHECK(std::isinf(cppunivariate::distributions::qslash(0,0,1)));
  CHECK(std::isinf(cppunivariate::distributions::qslash(1,0,1)));
  CHECK(std::isnan(cppunivariate::distributions::dslash(0,0,-1)));
}
TEST_CASE("slash: multiplicity, held scale and invalid inputs") {
  const std::vector<double> x={0,0,0,1},half={0,0,-1,1},two={0,1};
  CHECK(cppunivariate::slash_mle(mltest::as_span(x)).status==cppunivariate::SlashStatus::Unbounded);
  CHECK(cppunivariate::slash_mle(mltest::as_span(x),1e-9,200,1).status==cppunivariate::SlashStatus::Ok);
  CHECK(cppunivariate::slash_mle(mltest::as_span(half)).status==cppunivariate::SlashStatus::CollapseLimit);
  CHECK(cppunivariate::slash_mle(mltest::as_span(two)).status==cppunivariate::SlashStatus::Ok);
  CHECK(cppunivariate::slash_mle({},1e-9,200).status==cppunivariate::SlashStatus::InvalidInput);
  CHECK(cppunivariate::distributions::dslash(0)==doctest::Approx(cppunivariate::distributions::dslash(1e-10)).epsilon(1e-14));
}

#include "cppunivariate/distributions/slash/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("slash: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("slash",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dslash(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pslash(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qslash(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rslash(rng, p[0], p[1]); });
}

TEST_CASE("slash: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("slash");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<4;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::slash_mle(mltest::as_span(g.data),1e-10,1000,((mask & 2) ? g.estimates.at("sigma") : nan),((mask & 1) ? g.estimates.at("mu") : nan));
    if(mask & 1)CHECK(r.mu==g.estimates.at("mu"));
    if(mask & 2)CHECK(r.sigma==g.estimates.at("sigma"));
    CHECK(r.status==cppunivariate::SlashStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("slash: P3 failed refinement cannot inherit a grid point status") {
  const std::vector<double> values{-2,-.3,.1,.8,4};
  CHECK(cppunivariate::slash_mle(mltest::as_span(values),1e-9,1).status==cppunivariate::SlashStatus::IterationFailure);
}
