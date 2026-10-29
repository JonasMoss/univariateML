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
    CHECK(r.status==(x.back()==0?cppunivariate::PigStatus::ZeroLimit:cppunivariate::PigStatus::PoissonLimit));
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
