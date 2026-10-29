#include <algorithm>
#include "test_support.hpp"
#include "cppunivariate/distributions/naka/mle.hpp"
#include "cppunivariate/distributions/naka/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>

TEST_CASE("rnaka samples are positive and match analytic mean") {
  cppunivariate::distributions::StdRng<> rng(11);
  const int n = 20000;
  const double shape = 2.0;
  const double scale = 1.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rnaka(rng, shape, scale);
    CHECK(x > 0.0);
    sum += x;
  }
  // E[X] = sqrt(scale/shape) * Gamma(shape+0.5)/Gamma(shape)
  const double expected = std::sqrt(scale / shape) *
                          std::tgamma(shape + 0.5) / std::tgamma(shape);
  CHECK(std::abs(sum / n - expected) < 0.05);
}

TEST_CASE("naka") {
  auto g = mltest::load_distribution_golden("naka");
  auto r = cppunivariate::naka_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kTol));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("naka dpqr support and extreme log probabilities") {
  CHECK(cppunivariate::distributions::pnaka(-1,2,3)==0);
  CHECK(cppunivariate::distributions::pnaka(-1,2,3,false)==1);
  CHECK(cppunivariate::distributions::dnaka(-1,2,3)==0);
  CHECK(std::isinf(cppunivariate::distributions::dnaka(0,.25,2)));
  CHECK(cppunivariate::distributions::dnaka(0,.5,2)==doctest::Approx(1/std::sqrt(M_PI)));
  CHECK(cppunivariate::distributions::dnaka(0,2,2)==0);
  CHECK(std::isnan(cppunivariate::distributions::dnaka(1,-1,2)));
  for (bool lower : {true,false}) {
    for (double lp : {-1000.0,-10000.0,-1e-20}) {
      const double x=cppunivariate::distributions::qnaka(lp,10,2,lower,true);
      CHECK(cppunivariate::distributions::pnaka(x,10,2,lower,true)==doctest::Approx(lp).epsilon(1e-10));
    }
  }
}

TEST_CASE("naka tiny shape upper tail avoids cancellation") {
  const double lx=2*std::log(1e-200)+std::log(1e-25);
  CHECK(cppunivariate::distributions::pnaka(1e-200,1e-25,1,false,true)==
    doctest::Approx(std::log(-std::expm1(1e-25*lx))).epsilon(1e-12));
}

TEST_CASE("naka P2 every held subset") {
  const auto g=mltest::load_distribution_golden("naka");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("shape"),g.estimates.at("scale")};
  { const auto r=cppunivariate::naka_mle(mltest::as_span(x),1e-10,200,held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.scale));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dnaka(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::naka_mle(mltest::as_span(x),1e-10,200,nan,held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dnaka(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::naka_mle(mltest::as_span(x),1e-10,200,held[0],held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dnaka(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("naka P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::naka_mle(mltest::as_span(x),1e-10,100,nan,4).status==cppunivariate::GammaStatus::Degenerate);
  CHECK(std::isfinite(cppunivariate::naka_mle(mltest::as_span(x),1e-10,100,2,nan).logLik));
  CHECK(cppunivariate::naka_mle(mltest::as_span(x),1e-10,1,nan,3).status==cppunivariate::GammaStatus::IterationFailure);
}
