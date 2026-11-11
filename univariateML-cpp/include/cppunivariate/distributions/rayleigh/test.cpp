#include "cppunivariate/core/constants.hpp"
#include <algorithm>
#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/rayleigh/mle.hpp"
#include "cppunivariate/distributions/rayleigh/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("rrayleigh samples are positive; mean = sigma * sqrt(pi/2)") {
  cppunivariate::distributions::StdRng<> rng(103);
  const int n = 20000;
  const double sigma = 2.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rrayleigh(rng, sigma);
    CHECK(x >= 0.0);
    sum += x;
  }
  const double expected = sigma * std::sqrt(cppunivariate::pi / 2.0);
  CHECK(std::abs(sum / n - expected) < 0.1);
}

TEST_CASE("rayleigh") {
  auto g = mltest::load_distribution_golden("rayleigh");
  auto r = cppunivariate::rayleigh_mle(mltest::as_span(g.data));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sigma")).epsilon(mltest::kGolden_rayleigh).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_rayleigh).scale(0));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("rayleigh: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("rayleigh",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::drayleigh(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::prayleigh(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qrayleigh(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rrayleigh(rng, p[0]); });
}

TEST_CASE("rayleigh: any zero makes the likelihood identically zero") {
  for (auto x : {std::vector<double>{0,0,1,2}, std::vector<double>{0,1}, std::vector<double>{0,0}})
    CHECK(cppunivariate::rayleigh_mle(mltest::as_span(x)).status == cppunivariate::RayleighStatus::ZeroLikelihood);
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// S=lambda x:m.exp(-x*x/2); Q=lambda l:m.sqrt(-2*l)
TEST_CASE("rayleigh: extreme tail regressions") {
  CHECK(cppunivariate::distributions::prayleigh(100,1,false,true) == doctest::Approx(-5000).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qrayleigh(-5000,1,false,true) == doctest::Approx(100).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::prayleigh(1e-300,1,true,true) == doctest::Approx(2*std::log(1e-300)-std::log(2.0)).epsilon(1e-12).scale(1e-300));
}


TEST_CASE("rayleigh P2 every held subset") {
  const auto g=mltest::load_distribution_golden("rayleigh");
  std::vector<double> x=g.data;
  const std::vector<double> held{g.estimates.at("sigma")};
  { const auto r=cppunivariate::rayleigh_mle(mltest::as_span(x),held[0]);
    CHECK(r.sigma==held[0]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::drayleigh(v,p[0],true);return s;};
    std::vector<double> p{r.sigma};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

#include "cppunivariate/distributions/rayleigh/censored.hpp"
#include "censored_reference.hpp"
TEST_CASE("rayleigh: right censoring independent 50-digit reference") {
  for(const auto& c:censored_reference::cases()) {
    if(std::string(c.family)!="rayleigh")continue;
    CAPTURE(c.label); CAPTURE(c.held);
    const auto& v=c.lower;
    std::vector<double> upper=v;
    for(std::size_t i=0;i<v.size();++i)if(!c.exact[i])upper[i]=INFINITY;
    const cppunivariate::CensoredData data{mltest::as_span(v),mltest::as_span(upper)};
    const auto r=cppunivariate::rayleigh_censored_mle(data);
    REQUIRE(r.status==cppunivariate::RayleighCensoredStatus::Ok);
    CHECK(r.sigma/c.coefficient==doctest::Approx(1).epsilon(1e-10));
    CHECK(r.logLik==doctest::Approx(c.logLik).epsilon(1e-12));
    if(std::string(c.label)=="0pct" || std::string(c.label)=="single") {
      const auto exact=cppunivariate::rayleigh_mle(mltest::as_span(v));
      CHECK(r.sigma/exact.sigma==doctest::Approx(1).epsilon(4*std::numeric_limits<double>::epsilon()));
      CHECK(r.logLik==doctest::Approx(exact.logLik).epsilon(2e-13));
    }
  }
}
TEST_CASE("rayleigh: right censoring classifications and refusals") {
  std::vector<double> lower{1,2},upper{INFINITY,INFINITY};
  cppunivariate::CensoredData data{mltest::as_span(lower),mltest::as_span(upper)};
  const auto c=censored_reference::Case{"","",1.,0,0,{},{}};
  CHECK(cppunivariate::rayleigh_censored_mle(data).status==cppunivariate::RayleighCensoredStatus::AllCensored);
  upper[0]=1;
  CHECK(cppunivariate::rayleigh_censored_mle(data).status==cppunivariate::RayleighCensoredStatus::Ok);
  lower[0]=-INFINITY;
  CHECK(cppunivariate::rayleigh_censored_mle(data).status==cppunivariate::RayleighCensoredStatus::NotImplemented);
  lower[0]=1;upper[0]=1.5;
  CHECK(cppunivariate::rayleigh_censored_mle(data).status==cppunivariate::RayleighCensoredStatus::NotImplemented);
  upper[0]=0;
  CHECK(cppunivariate::rayleigh_censored_mle(data).status==cppunivariate::RayleighCensoredStatus::InvalidInput);
}
