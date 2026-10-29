#include <algorithm>
#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/invgauss/mle.hpp"
#include "cppunivariate/distributions/invgauss/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("rinvgauss samples are positive; sample mean ~ mu") {
  cppunivariate::distributions::StdRng<> rng(151);
  const int n = 20000;
  const double mu = 2.0, shape = 5.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rinvgauss(rng, mu, shape);
    CHECK(x > 0.0);
    sum += x;
  }
  // E[X] = mu for invgauss
  CHECK(std::abs(sum / n - mu) < 0.1);
}

TEST_CASE("invgauss") {
  auto g = mltest::load_distribution_golden("invgauss");
  auto r = cppunivariate::invgauss_mle(mltest::as_span(g.data));
  CHECK(r.mu == doctest::Approx(g.estimates.at("mean")).epsilon(mltest::kTol));
  CHECK(r.lambda == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("invgauss P2 every held subset") {
  const auto g=mltest::load_distribution_golden("invgauss");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("mean"),g.estimates.at("shape")};
  { const auto r=cppunivariate::invgauss_mle(mltest::as_span(x),held[0],nan);
    CHECK(r.mu==held[0]);
    CHECK(std::isfinite(r.lambda));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvgauss(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.mu,r.lambda};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::invgauss_mle(mltest::as_span(x),nan,held[1]);
    CHECK(std::isfinite(r.mu));
    CHECK(r.lambda==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvgauss(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.mu,r.lambda};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::invgauss_mle(mltest::as_span(x),held[0],held[1]);
    CHECK(r.mu==held[0]);
    CHECK(r.lambda==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvgauss(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.mu,r.lambda};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("invgauss P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK_FALSE(std::isfinite(cppunivariate::invgauss_mle(mltest::as_span(x),2,nan).logLik));
  CHECK(std::isfinite(cppunivariate::invgauss_mle(mltest::as_span(x),nan,2).logLik));
}
