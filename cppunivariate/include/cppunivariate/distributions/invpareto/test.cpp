#include <algorithm>
#include <vector>
#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/invpareto/mle.hpp"
#include "cppunivariate/distributions/invpareto/dpqr.hpp"
TEST_CASE("invpareto: golden, reciprocal agreement and score") {
  auto g=mltest::load_distribution_golden("invpareto");
  auto r=cppunivariate::invpareto_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(1e-12)); CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(1e-12));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(1e-12));
  std::vector<double> y; double jac=0;
  for (double x:g.data) { y.push_back(1/x); jac-=2*std::log(x); }
  auto b=cppunivariate::lomax_mle(mltest::as_span(y), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(b.kappa)); CHECK(r.scale == doctest::Approx(b.lambda));
  CHECK(r.logLik == doctest::Approx(b.logLik+jac).epsilon(1e-12));

  // Independent reference log-density score in log parameters.
  auto cost = [&](double u, double v) {
    double ll=0;
    for (double x : g.data) ll+=cppunivariate::distributions::dinvpareto(x,std::exp(u),std::exp(v),true);
    return ll;
  };
  const double u=std::log(r.shape),v=std::log(r.scale),h=1e-5;
  const double a=(cost(u+h,v)-cost(u-h,v))/(2*h), bscore=(cost(u,v+h)-cost(u,v-h))/(2*h);
  CHECK(std::hypot(a,bscore)<1e-3);

}
TEST_CASE("invpareto: inverse exponential limit") {
  std::vector<double> x{1,1,1,1};
  CHECK(cppunivariate::invpareto_mle(mltest::as_span(x),1e-10,100).status == cppunivariate::LomaxStatus::ExponentialLimit);
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("invpareto: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("invpareto",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dinvpareto(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pinvpareto(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qinvpareto(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rinvpareto(rng, p[0], p[1]); });
}

TEST_CASE("invpareto P2 every held subset") {
  const auto g=mltest::load_distribution_golden("invpareto");
  std::vector<double> x=g.data;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> held{g.estimates.at("shape"),g.estimates.at("scale")};
  { const auto r=cppunivariate::invpareto_mle(mltest::as_span(x),1e-10,200,nan,held[0],nan);
    CHECK(r.shape==held[0]);
    CHECK(std::isfinite(r.scale));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvpareto(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[1]));a[1]+=h;b[1]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::invpareto_mle(mltest::as_span(x),1e-10,200,nan,nan,held[1]);
    CHECK(std::isfinite(r.shape));
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvpareto(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
    {auto a=p,b=p;const double h=1e-5*std::max(1.0,std::abs(p[0]));a[0]+=h;b[0]-=h;CHECK(std::abs((ll(a)-ll(b))/(2*h))<1e-3);}
  }
  { const auto r=cppunivariate::invpareto_mle(mltest::as_span(x),1e-10,200,nan,held[0],held[1]);
    CHECK(r.shape==held[0]);
    CHECK(r.scale==held[1]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvpareto(v,p[0],p[1],true);return s;};
    std::vector<double> p{r.shape,r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("invpareto P2 held boundaries and solver failure") {
  const std::vector<double> x{2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(std::isfinite(cppunivariate::invpareto_mle(mltest::as_span(x),1e-10,100,nan,2,nan).logLik));
  CHECK(std::isfinite(cppunivariate::invpareto_mle(mltest::as_span(x),1e-10,100,nan,nan,2).logLik));
  CHECK(cppunivariate::invpareto_mle(mltest::as_span(x),1e-10,1,nan,2,nan).status==cppunivariate::LomaxStatus::IterationFailure);
}

TEST_CASE("invpareto: P3 held delegate preserves exhaustion") {
  const std::vector<double> values{.3,.8,1.7,4.2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::invpareto_mle(mltest::as_span(values),1e-9,1,nan,2,nan).status==cppunivariate::LomaxStatus::IterationFailure);
}
