#include <algorithm>
#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include "cppunivariate/distributions/lnorm/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("lnorm") {
  auto g = mltest::load_distribution_golden("lnorm");
  auto r = cppunivariate::lognormal_mle(mltest::as_span(g.data));
  CHECK(r.meanlog == doctest::Approx(g.estimates.at("meanlog")).epsilon(mltest::kTol));
  CHECK(r.sdlog == doctest::Approx(g.estimates.at("sdlog")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("lnorm: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/lnorm/fixture-outlier.json");
  auto r = cppunivariate::lognormal_mle(mltest::as_span(g.data));
  CHECK(r.meanlog == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.sdlog == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
}

TEST_CASE("lnorm dpqr: median = exp(meanlog)") {
  CHECK(cppunivariate::distributions::qlnorm(0.5, 0.0, 1.0, true, false) == doctest::Approx(1.0));
  const double q = cppunivariate::distributions::qlnorm(0.4, 1.0, 0.5, true, false);
  CHECK(cppunivariate::distributions::plnorm(q, 1.0, 0.5, true, false) == doctest::Approx(0.4));
}

TEST_CASE("lnorm P2 every held subset") {
  const auto g=mltest::load_distribution_golden("lnorm");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("meanlog"),g.estimates.at("sdlog")};
  { const auto r=cppunivariate::lognormal_mle(mltest::as_span(x),held[0],nan);
    CHECK(r.meanlog==held[0]);
    CHECK(std::isfinite(r.sdlog));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dlnorm(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.meanlog,r.sdlog};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::lognormal_mle(mltest::as_span(x),nan,held[1]);
    CHECK(std::isfinite(r.meanlog));
    CHECK(r.sdlog==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dlnorm(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.meanlog,r.sdlog};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::lognormal_mle(mltest::as_span(x),held[0],held[1]);
    CHECK(r.meanlog==held[0]);
    CHECK(r.sdlog==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dlnorm(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.meanlog,r.sdlog};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("lnorm P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK_FALSE(std::isfinite(cppunivariate::lognormal_mle(mltest::as_span(x),std::log(2.0),nan).logLik));
  CHECK(std::isfinite(cppunivariate::lognormal_mle(mltest::as_span(x),nan,1).logLik));
}

TEST_CASE("lnorm held distant mean uses a scaled-width result") {
  const std::vector<double> x{1,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto r=cppunivariate::lognormal_mle(mltest::as_span(x),1e300,nan);
  CHECK(r.meanlog==1e300);
  CHECK(r.sdlog==doctest::Approx(1e300));
  CHECK(std::isfinite(r.logLik));
}
