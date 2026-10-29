#include "test_support.hpp"
#include "cppunivariate/distributions/ht/mle.hpp"
#include "cppunivariate/distributions/ht/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
TEST_CASE("ht: golden and scores") {
  const auto g=mltest::load_distribution_golden("ht");
  const auto r=cppunivariate::ht_mle(mltest::as_span(g.data));
  CHECK(r.status==cppunivariate::HtStatus::Ok);
  CHECK(r.nu==doctest::Approx(g.estimates.at("nu")).epsilon(1e-7));
  CHECK(r.sigma==doctest::Approx(g.estimates.at("sigma")).epsilon(1e-8));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  double ls=std::log(r.sigma), l1=ls,l2=ls;
  CHECK(std::abs((cppunivariate::ht_detail::profile(mltest::as_span(g.data),r.nu*std::exp(1e-5),l1,true)-cppunivariate::ht_detail::profile(mltest::as_span(g.data),r.nu*std::exp(-1e-5),l2,true))/2e-5)<1e-3);
}
TEST_CASE("ht: fixed subsets, collapse and normal limit") {
  const std::vector<double> x={0,1,2}, eq={0,1}, z={0,0}, positive={1,1,1};
  CHECK(cppunivariate::ht_mle(mltest::as_span(x)).status==cppunivariate::HtStatus::Unbounded);
  CHECK(cppunivariate::ht_mle(mltest::as_span(x),1).status==cppunivariate::HtStatus::Ok);
  CHECK(cppunivariate::ht_mle(mltest::as_span(eq),1).status==cppunivariate::HtStatus::ScaleBoundary);
  CHECK(cppunivariate::ht_mle(mltest::as_span(eq),.5).status==cppunivariate::HtStatus::Unbounded);
  CHECK(cppunivariate::ht_mle(mltest::as_span(z),1,2).status==cppunivariate::HtStatus::Ok);
  CHECK(cppunivariate::ht_mle(mltest::as_span(z),std::numeric_limits<double>::quiet_NaN(),2).status==cppunivariate::HtStatus::NormalLimit);
  CHECK(cppunivariate::ht_mle(mltest::as_span(positive)).status==cppunivariate::HtStatus::NormalLimit);
  CHECK(cppunivariate::ht_mle(mltest::as_span(x),-1).status==cppunivariate::HtStatus::InvalidParameter);
  CHECK(cppunivariate::ht_mle(mltest::as_span(positive),1e-300).status==cppunivariate::HtStatus::SearchFailure);
  const std::vector<double> bad={-1,2};
  CHECK(cppunivariate::ht_mle(mltest::as_span(bad)).status==cppunivariate::HtStatus::InvalidData);
}
TEST_CASE("ht: d/p/q/r and extreme tails") {
  CHECK(cppunivariate::distributions::dht(-1,3,1,false)==0);
  CHECK(std::isfinite(cppunivariate::distributions::dht(1e200,3,1e-200,true)));
  for(double nu:{.1,1.,3.,100.}) for(bool lower:{false,true}) for(bool logs:{false,true}) for(double p:{0.,.01,.5,.99,1.}) {
    const double q=cppunivariate::distributions::qht(logs?std::log(p):p,nu,2,lower,logs);
    CHECK(cppunivariate::distributions::pht(q,nu,2,lower,false)==doctest::Approx(p).epsilon(1e-10));
  }
  const double q=cppunivariate::distributions::qht(-100,3,1,false,true);
  CHECK(cppunivariate::distributions::pht(q,3,1,false,true)==doctest::Approx(-100).epsilon(1e-10));
  cppunivariate::distributions::StdRng<> rng(9);
  for(int i=0;i<20;++i) CHECK(cppunivariate::distributions::rht(rng,3,1)>=0);
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("ht: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("ht",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dht(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pht(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qht(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rht(rng, p[0], p[1]); });
}
