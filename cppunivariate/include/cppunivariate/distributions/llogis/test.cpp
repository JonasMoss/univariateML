#include <algorithm>
#include "test_support.hpp"
#include "cppunivariate/distributions/llogis/mle.hpp"
#include "cppunivariate/distributions/llogis/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>

TEST_CASE("rllogis samples are positive; CDF round-trip at p=0.7") {
  cppunivariate::distributions::StdRng<> rng(163);
  for (int i = 0; i < 5000; ++i) {
    const double x = cppunivariate::distributions::rllogis(rng, 2.0, 3.0);
    CHECK(x > 0.0);
  }
  const double q =
      cppunivariate::distributions::qllogis(0.7, 2.0, 3.0, true, false);
  const double back =
      cppunivariate::distributions::pllogis(q, 2.0, 3.0, true, false);
  CHECK(std::abs(back - 0.7) < 1e-10);
}

TEST_CASE("llogis") {
  auto g = mltest::load_distribution_golden("llogis");
  auto r = cppunivariate::loglogistic_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kTol));
  CHECK(r.rate == doctest::Approx(g.estimates.at("rate")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("llogis: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) +
      "/llogis/fixture-outlier.json");
  auto r = cppunivariate::loglogistic_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.rate == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
}

TEST_CASE("llogis P2 every held subset") {
  const auto g=mltest::load_distribution_golden("llogis");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("shape"),g.estimates.at("rate")};
  { const auto r=cppunivariate::loglogistic_mle(mltest::as_span(x),1e-10,200,held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.rate));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dllogis(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.rate};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::loglogistic_mle(mltest::as_span(x),1e-10,200,nan,held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.rate==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dllogis(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.rate};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::loglogistic_mle(mltest::as_span(x),1e-10,200,held[0],held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.rate==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dllogis(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.rate};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("llogis P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK_FALSE(std::isfinite(cppunivariate::loglogistic_mle(mltest::as_span(x),1e-10,100,nan,.5).logLik));
  CHECK(std::isfinite(cppunivariate::loglogistic_mle(mltest::as_span(x),1e-10,100,2,nan).logLik));
  CHECK_FALSE(std::isfinite(cppunivariate::loglogistic_mle(mltest::as_span(x),1e-10,1,nan,1).logLik));
}
