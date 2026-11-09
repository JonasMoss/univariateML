#include <algorithm>
#include <vector>
#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/invexp/mle.hpp"
#include "cppunivariate/distributions/invexp/dpqr.hpp"
TEST_CASE("invexp: golden, reciprocal agreement and score") {
  auto g=mltest::load_distribution_golden("invexp");
  auto r=cppunivariate::invexp_mle(mltest::as_span(g.data));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_invexp).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_invexp).scale(0));
  std::vector<double> y; double jac=0;
  for (double x:g.data) { y.push_back(1/x); jac-=2*std::log(x); }
  auto b=cppunivariate::exp_mle(mltest::as_span(y));
  CHECK(r.scale == doctest::Approx(b.rate));
  CHECK(r.logLik == doctest::Approx(b.logLik+jac).epsilon(1e-12));

}

TEST_CASE("invexp P2 every held subset") {
  const auto g=mltest::load_distribution_golden("invexp");
  std::vector<double> x=g.data;
  const std::vector<double> held{g.estimates.at("scale")};
  { const auto r=cppunivariate::invexp_mle(mltest::as_span(x),held[0]);
    CHECK(r.scale==held[0]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dinvexp(v,p[0],true);return s;};
    std::vector<double> p{r.scale};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("invexp: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("invexp",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dinvexp(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pinvexp(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qinvexp(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rinvexp(rng, p[0]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// F=lambda x:m.exp(-1/x); Q=lambda l:-1/l
TEST_CASE("invexp: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pinvexp(1e300,1,false,true) == doctest::Approx(-std::log(1e300)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qinvexp(-10000,1,true,true) == doctest::Approx(0.0001).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::dinvexp(INFINITY,1,true) == -INFINITY);
}

TEST_CASE("invexp: README scale validation") {
  cppunivariate::distributions::StdRng<> rng(94);
  for (double scale : {0.0, -1.0, double(INFINITY), double(NAN)}) {
    CHECK(std::isnan(cppunivariate::distributions::dinvexp(0.5, scale, false)));
    CHECK(std::isnan(cppunivariate::distributions::pinvexp(0.5, scale, false, true)));
    CHECK(std::isnan(cppunivariate::distributions::qinvexp(0.5, scale, false, true)));
    CHECK(std::isnan(cppunivariate::distributions::rinvexp(rng, scale)));
  }
}

TEST_CASE("invexp: finite scale with overflowing reciprocal") {
  const double scale = 1e-320;
  CHECK(std::isfinite(cppunivariate::distributions::dinvexp(scale, scale, true)));
  CHECK(std::isfinite(cppunivariate::distributions::pinvexp(scale, scale, false, true)));
  CHECK(cppunivariate::distributions::qinvexp(0.5, scale, true, false) > 0.0);
}

TEST_CASE("invexp: TASK-132 invalid data never report success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2.0},std::vector<double>{INFINITY,2.0},std::vector<double>{-INFINITY,2.0},std::vector<double>{-1.0,2.0},std::vector<double>{0.0,2.0}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::invexp_mle(x).status != cppunivariate::InvExpStatus::Ok);
  }
}

TEST_CASE("invexp: TASK-132 reciprocal overflow is numerical failure") {
  const std::vector<double> data{1,std::numeric_limits<double>::denorm_min()};
  const cppunivariate::span x{data.data(),data.size()};
  CHECK(cppunivariate::invexp_mle(x).status == cppunivariate::InvExpStatus::IterationFailure);
  CHECK(cppunivariate::invexp_mle(x,1.0).status == cppunivariate::InvExpStatus::IterationFailure);
}
