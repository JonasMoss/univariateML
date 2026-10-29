#include <algorithm>
#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/pareto/mle.hpp"
#include "cppunivariate/distributions/pareto/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("rpareto samples are >= scale b; mean = a*b/(a-1) for a>1") {
  cppunivariate::distributions::StdRng<> rng(113);
  const int n = 20000;
  const double a = 3.0, b = 2.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rpareto(rng, a, b);
    CHECK(x >= b);
    sum += x;
  }
  const double expected = a * b / (a - 1.0); // = 3.0
  CHECK(std::abs(sum / n - expected) < 0.1);
}

TEST_CASE("pareto") {
  auto g = mltest::load_distribution_golden("pareto");
  auto r = cppunivariate::pareto_mle(mltest::as_span(g.data));
  // golden "a" → ParetoResult.shape, "b" → .min
  CHECK(r.shape == doctest::Approx(g.estimates.at("a")).epsilon(mltest::kTol));
  CHECK(r.min == doctest::Approx(g.estimates.at("b")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("pareto P2 every held subset") {
  const auto g=mltest::load_distribution_golden("pareto");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("a"),g.estimates.at("b")};
  { const auto r=cppunivariate::pareto_mle(mltest::as_span(x),held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.min));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dpareto(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.min};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
  { const auto r=cppunivariate::pareto_mle(mltest::as_span(x),nan,held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.min==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dpareto(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.min};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::pareto_mle(mltest::as_span(x),held[0],held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.min==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dpareto(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.min};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("pareto P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK_FALSE(std::isfinite(cppunivariate::pareto_mle(mltest::as_span(x),nan,2).logLik));
  CHECK_FALSE(std::isfinite(cppunivariate::pareto_mle(mltest::as_span(x),nan,3).logLik));
  CHECK(std::isfinite(cppunivariate::pareto_mle(mltest::as_span(x),2,nan).logLik));
}
