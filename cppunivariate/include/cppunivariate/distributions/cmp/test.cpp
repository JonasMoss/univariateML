#include "test_support.hpp"
#include "cppunivariate/distributions/cmp/mle.hpp"
TEST_CASE("CMP independent mpmath density and tail fixtures") {
 const double rows[][6]={
#include "reference.inc"
 };
 for(const auto &r:rows) {
  CHECK(cppunivariate::distributions::dcmp(r[2],r[0],r[1],true)==doctest::Approx(r[3]).epsilon(2e-12));
  CHECK(cppunivariate::distributions::pcmp(r[2],r[0],r[1],true,true)==doctest::Approx(r[4]).epsilon(2e-12));
  CHECK(cppunivariate::distributions::pcmp(r[2],r[0],r[1],false,true)==doctest::Approx(r[5]).epsilon(2e-12));
 }
}
TEST_CASE("CMP golden delegates to existing fitter") {
 const auto g=mltest::load_distribution_golden("cmp");
 const auto f=cppunivariate::cmp_mle({g.data.data(),g.data.size()});
 REQUIRE(f.status==cppunivariate::CmpStatus::Ok);
 CHECK(std::exp(f.eta)==doctest::Approx(g.estimates.at("lambda")).epsilon(1e-8));
 CHECK(f.nu==doctest::Approx(g.estimates.at("nu")).epsilon(1e-8));
 CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
}
TEST_CASE("CMP tiny log complements and invalid domains") {
 CHECK(cppunivariate::distributions::pcmp(0,1e-100,2,true,true)/(-1e-100)==doctest::Approx(1).epsilon(1e-12));
 CHECK(std::isnan(cppunivariate::distributions::dcmp(1,1,0)));
 CHECK(std::isnan(cppunivariate::distributions::qcmp(2,2,1)));
 CHECK(cppunivariate::distributions::dcmp(-1,2,2)==0);
 CHECK(cppunivariate::distributions::qcmp(1,0,2)==0);
}

#include "cppunivariate/distributions/cmp/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
// CMP rebuilds its series for every inverse-CDF draw. The 20000-draw audit
// exceeded five minutes; use 1000 draws/point (timing in the truth report).
TEST_CASE("cmp: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("cmp",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dcmp(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pcmp(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qcmp(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rcmp(rng, p[0], p[1]); }, 1e-10, 1e-9, 1000);
}
