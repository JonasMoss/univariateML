#include "test_support.hpp"
#include "cppunivariate/distributions/nhyper/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <limits>

TEST_CASE("nhyper: exhaustive small urns and independent likelihood grid") {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  for (int N=1;N<=20;++N) for (int r=1;r<=N;++r)
    for (int x=r;x<=N;++x) {
      const std::vector<double> data{static_cast<double>(x)};
      const auto a = cppunivariate::nhyper_mle(mltest::as_span(data),r,N,nan);
      long double best=-1e100L;
      for (int m=r;m<=N-x+r;++m) {
        const long double ll = std::lgamma(static_cast<long double>(x)) - std::lgamma(static_cast<long double>(r)) - std::lgamma(static_cast<long double>(x-r+1)) +
          std::lgamma(static_cast<long double>(N-x+1)) - std::lgamma(static_cast<long double>(m-r+1)) - std::lgamma(static_cast<long double>(N-m-x+r+1)) -
          std::lgamma(static_cast<long double>(N+1)) + std::lgamma(static_cast<long double>(N-m+1)) + std::lgamma(static_cast<long double>(m+1));
        best=std::max(best,ll);
      }
      CHECK(std::fabs(a.logLik-best)<1e-12);
      CHECK(a.m>=r);
      CHECK(a.m<=N-x+r);
    }
}
TEST_CASE("nhyper: golden and large populations") {
  const auto g=mltest::load_distribution_golden("nhyper");
  const auto a=cppunivariate::nhyper_mle(mltest::as_span(g.data),3,50,std::numeric_limits<double>::quiet_NaN());
  CHECK(a.m==g.estimates.at("m"));
  CHECK(a.logLik==doctest::Approx(g.logLik).epsilon(1e-12));
  CHECK(cppunivariate::nhyper_log_pmf(3,1e12,1e12,3)==doctest::Approx(std::log(.125)).epsilon(1e-11));
}
TEST_CASE("nhyper: scalar tails, quantiles, RNG and invalid inputs") {
  namespace d=cppunivariate::distributions;
  for (double x=2;x<=14;++x) {
    const double p=d::pnhyper(x,12,8,2,true,false);
    CHECK(p+d::pnhyper(x,12,8,2,false,false)==doctest::Approx(1).epsilon(1e-13));
    CHECK(d::qnhyper(p,12,8,2,true,false)==x);
  }
  CHECK(d::qnhyper(-1e-100,12,8,2,true,true)==14);
  CHECK(d::qnhyper(-1e-100,12,8,2,false,true)==2);
  CHECK(d::qnhyper(-1000,12,8,2,true,true)==2);
  CHECK(d::qnhyper(-1000,12,8,2,false,true)==14);
  CHECK(d::dnhyper(2,0,20,2,false)==1);
  CHECK(d::qnhyper(.5,0,20,2,true,false)==2);
  CHECK(d::dnhyper(0,0,0,0,false)==1);
  CHECK(std::isnan(d::dnhyper(0,1,1,2,false)));
  CHECK(std::isnan(d::qnhyper(2,1,1,1,true,false)));
  cppunivariate::distributions::StdRng<> rng(42);
  for (int i=0;i<50;++i) {
    const double v=d::rnhyper(rng,12,8,2);
    CHECK(v>=2); CHECK(v<=14);
  }
}

TEST_CASE("nhyper: statuses, held composition and boundary identification") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{3,4,5}, zero{0}, low{1}, two{2,2};
  const auto fit=[&](double r,double N,double m) {
    return cppunivariate::nhyper_mle(mltest::as_span(x),r,N,m);
  };
  CHECK(fit(2,20,8).status==cppunivariate::NhyperStatus::Ok);
  CHECK(fit(2,20,19).status==cppunivariate::NhyperStatus::Infeasible);
  CHECK(fit(2,4,nan).status==cppunivariate::NhyperStatus::Infeasible);
  CHECK(fit(-1,20,nan).status==cppunivariate::NhyperStatus::InvalidInput);
  CHECK(fit(2,1,nan).status==cppunivariate::NhyperStatus::InvalidInput);
  CHECK(fit(2,20,1).status==cppunivariate::NhyperStatus::InvalidInput);
  CHECK(fit(2,20,21).status==cppunivariate::NhyperStatus::InvalidInput);
  CHECK(fit(2,9007199254740992.0,nan).status==cppunivariate::NhyperStatus::InvalidInput);
  CHECK(cppunivariate::nhyper_mle(mltest::as_span(zero),0,20,nan).status==cppunivariate::NhyperStatus::Unidentified);
  CHECK(cppunivariate::nhyper_mle(mltest::as_span(zero),0,0,nan).status==cppunivariate::NhyperStatus::Ok);
  CHECK(cppunivariate::nhyper_mle(mltest::as_span(low),0,20,nan).status==cppunivariate::NhyperStatus::Infeasible);
  CHECK(cppunivariate::nhyper_mle(mltest::as_span(two),2,20,nan).m==20);
  const std::vector<double> six{6};
  const auto tied=cppunivariate::nhyper_mle(mltest::as_span(six),2,20,nan);
  CHECK(tied.status==cppunivariate::NhyperStatus::Tie);
  CHECK(tied.m==6); CHECK(tied.tie==7);
  const auto large=fit(2,9007199254740991.0,nan);
  CHECK(std::isfinite(large.logLik));
  CHECK(large.iterations<60);
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("nhyper: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("nhyper",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dnhyper(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pnhyper(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qnhyper(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rnhyper(rng, p[0], p[1], p[2]); });
}

TEST_CASE("nhyper: population size is returned unchanged as a coefficient") {
  const std::vector<double> x{3,4,5};
  const auto a = cppunivariate::nhyper_mle(mltest::as_span(x),2,20,
      std::numeric_limits<double>::quiet_NaN());
  CHECK(a.N == 20);
  CHECK(a.m >= 2);
  const auto b = cppunivariate::nhyper_mle(mltest::as_span(x),2,20,8);
  CHECK(b.m == 8);
  CHECK(b.N == 20);
}
