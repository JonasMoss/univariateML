#include "test_support.hpp"
#include "cppunivariate/distributions/dgamma/mle.hpp"
#include "cppunivariate/distributions/dgamma/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <vector>
TEST_CASE("dgamma: golden and independent score") {
  const auto g=mltest::load_distribution_golden("dgamma");
  const auto r=cppunivariate::dgamma_mle({g.data.data(),g.data.size()});
  REQUIRE(r.status==cppunivariate::DgammaStatus::Ok);
  CHECK(r.shape==doctest::Approx(g.estimates.at("shape")).epsilon(2e-6));
  CHECK(r.rate==doctest::Approx(g.estimates.at("rate")).epsilon(2e-6));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-9));
  const double h=1e-5;
  double ga=0,gb=0;
  for (double x:g.data) {
    ga+=(cppunivariate::distributions::ddgamma(x,r.shape+h,r.rate,true)-cppunivariate::distributions::ddgamma(x,r.shape-h,r.rate,true))/(2*h);
    gb+=(cppunivariate::distributions::ddgamma(x,r.shape,r.rate+h,true)-cppunivariate::distributions::ddgamma(x,r.shape,r.rate-h,true))/(2*h);
  }
  CHECK(std::hypot(ga,gb)<1e-3);
}
TEST_CASE("dgamma: statuses, held subsets and tails") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> constant{3,3,3}, adjacent{3,3,4}, ordinary{0,1,2,4,8}, invalid{0,1.5};
  CHECK(cppunivariate::dgamma_mle({constant.data(),constant.size()}).status==cppunivariate::DgammaStatus::PointMass);
  CHECK(cppunivariate::dgamma_mle({adjacent.data(),adjacent.size()}).status==cppunivariate::DgammaStatus::TwoPointLimit);
  CHECK(cppunivariate::dgamma_mle({invalid.data(),invalid.size()}).status==cppunivariate::DgammaStatus::InvalidData);
  CHECK(cppunivariate::dgamma_mle({ordinary.data(),ordinary.size()},-1).status==cppunivariate::DgammaStatus::InvalidControl);
  CHECK(cppunivariate::dgamma_mle({ordinary.data(),ordinary.size()},1e-8,400,1,nan).status==cppunivariate::DgammaStatus::Ok);
  CHECK(cppunivariate::dgamma_mle({ordinary.data(),ordinary.size()},1e-8,400,nan,1).status==cppunivariate::DgammaStatus::Ok);
  const auto fixed=cppunivariate::dgamma_mle({constant.data(),constant.size()},1e-8,400,2,1);
  CHECK(fixed.status==cppunivariate::DgammaStatus::Ok);
  CHECK(fixed.iterations==0);
  CHECK(std::isfinite(cppunivariate::distributions::ddgamma(1000,2,1,true)));
  CHECK(cppunivariate::distributions::pdgamma(1.5,2,1)==cppunivariate::distributions::pdgamma(1,2,1));
  cppunivariate::distributions::StdRng<> rng(19);
  const double z=cppunivariate::distributions::rdgamma(rng,2,1);
  CHECK(z==std::floor(z));
}
TEST_CASE("dgamma: held boundaries, profile score and quantiles") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> zero{0,0},x{0,0,1,3,9};
  CHECK(cppunivariate::dgamma_mle({zero.data(),zero.size()},1e-8,400,2,nan).status==cppunivariate::DgammaStatus::PointMass);
  CHECK(cppunivariate::dgamma_mle({zero.data(),zero.size()},1e-8,400,nan,2).status==cppunivariate::DgammaStatus::PointMass);
  const auto r=cppunivariate::dgamma_mle({x.data(),x.size()},1e-8,400,2,nan);
  REQUIRE(r.status==cppunivariate::DgammaStatus::Ok);
  cppunivariate::dnorm_detail::Cells cells; REQUIRE(cells.init({x.data(),x.size()}));
  double derivative=0;
  CHECK(std::abs(cppunivariate::dgamma_detail::rate_score(cells,2,std::log(r.rate),derivative))<1e-9);
  CHECK(derivative<0);
  CHECK(std::isfinite(cppunivariate::distributions::ddgamma(0,1000,1e-300,true)));
  CHECK(cppunivariate::distributions::ddgamma(0,1000,1e-300,true)==doctest::Approx(1000*std::log(1e-300)-std::lgamma(1001.0)).epsilon(1e-12));
  for (double p:std::vector<double>{1e-12,0.1,0.5,0.9}) {
    const double q=cppunivariate::distributions::qdgamma(p,2,1);
    CHECK(cppunivariate::distributions::pdgamma(q,2,1)>=p-1e-15);
    CHECK(cppunivariate::distributions::pdgamma(q-1,2,1)<p);
  }
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("dgamma: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("dgamma",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::ddgamma(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pdgamma(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qdgamma(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rdgamma(rng, p[0], p[1]); });
}
