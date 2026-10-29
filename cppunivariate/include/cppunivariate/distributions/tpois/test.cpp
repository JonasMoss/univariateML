#include "test_support.hpp"
#include "cppunivariate/distributions/tpois/mle.hpp"
#include <vector>
TEST_CASE("tpois: unique mean solve and support errors") {
  const std::vector<int> x={1,2,3,4};
  auto f=cppunivariate::tpois_mle({x.data(),x.size()},0,10);
  CHECK(f.status==cppunivariate::truncated_detail::Status::Ok);
  CHECK(std::isfinite(f.logLik));
  CHECK(cppunivariate::tpois_mle({x.data(),x.size()},-INFINITY,INFINITY,1e12).status==cppunivariate::truncated_detail::Status::NumericalFailure);
  auto z=cppunivariate::tpois_mle({x.data(),x.size()},2,10);
  CHECK(z.status==cppunivariate::truncated_detail::Status::InvalidInput);
  const std::vector<int> constant={1,1};
  CHECK(cppunivariate::tpois_mle({constant.data(),constant.size()},0,10).status==cppunivariate::truncated_detail::Status::LowerPointMass);
}

#include "cppunivariate/distributions/tpois/dpqr.hpp"
TEST_CASE("tpois: density normalization and log-tail quantiles") {
  CHECK(cppunivariate::distributions::dtpois(INFINITY,3,0,INFINITY)==0);
  CHECK(cppunivariate::distributions::qtpois(1,0,-INFINITY,INFINITY)==0);
  double total=0;
  for(int x=0;x<=200;++x)total+=cppunivariate::distributions::dtpois(x,3,0,10);
  CHECK(total==doctest::Approx(1).epsilon(1e-12));
  for(double lp : {-1.,-10.,-100.}) {
    const double q=cppunivariate::distributions::qtpois(lp,3,0,10,false,true);
    CHECK(cppunivariate::distributions::ptpois(q,3,0,10,false,true)<=lp);
    CHECK(cppunivariate::distributions::ptpois(q-1,3,0,10,false,true)>lp);
  }
}

TEST_CASE("tpois: golden regression") {
  auto g=mltest::load_distribution_golden("tpois");std::vector<int>x;
  for(double v:g.data)x.push_back(static_cast<int>(v));
  auto f=cppunivariate::tpois_mle({x.data(),x.size()},0,10);
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  CHECK(f.parameter==doctest::Approx(g.estimates.at("lambda")).epsilon(1e-10));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("tpois: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("tpois",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dtpois(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ptpois(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qtpois(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rtpois(rng, p[0], p[1], p[2]); });
}
