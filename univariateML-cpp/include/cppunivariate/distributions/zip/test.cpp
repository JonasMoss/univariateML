#include "test_support.hpp"
#include "cppunivariate/distributions/zip/mle.hpp"
#include "cppunivariate/distributions/zip/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include "cppunivariate/distributions/pois/mle.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace dist = cppunivariate::distributions;
#define D_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define D_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)
namespace {
 std::vector<int> as_ints(const std::vector<double> &v) {
   std::vector<int> out;
   for (double x : v) out.push_back(static_cast<int>(x));
   return out;
 }
 cppunivariate::ispan as_ispan(const std::vector<int> &v) { return {v.data(), v.size()}; }
 }
 TEST_CASE("zip") {
  auto g = mltest::load_distribution_golden("zip");
  auto v = as_ints(g.data);
  auto r = cppunivariate::zip_mle(as_ispan(v));
  CHECK(r.lambda == D_APPROX(g.estimates.at("lambda")));
  CHECK(r.pi == D_APPROX(g.estimates.at("pi")));
  CHECK(r.logLik == D_APPROX(g.logLik));
}

TEST_CASE("zip ordinary zero inflation") {
  const std::vector<std::vector<int>> samples{
      {0, 1, 2, 1, 1, 2, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3},
      {0, 0, 4}};
  const std::vector<double> lambdas{1.423265, 3.920695};
  const std::vector<double> pis{0.4730435, 0.6599249};
  const std::vector<double> logliks{-23.0097617, -3.52318909};
  for (std::size_t i = 0; i < samples.size(); ++i) {
    const auto r = cppunivariate::zip_mle(as_ispan(samples[i]));
    REQUIRE(r.status == cppunivariate::ZipStatus::Ok);
    CHECK(r.lambda == doctest::Approx(lambdas[i]).epsilon(1e-4));
    CHECK(r.pi == doctest::Approx(pis[i]).epsilon(1e-4));
    CHECK(r.logLik >= logliks[i] - 1e-6);
  }
}

TEST_CASE("zip boundaries and invalid input") {
  const std::vector<int> zeros{0, 0, 0};
  const auto zero = cppunivariate::zip_mle(as_ispan(zeros));
  REQUIRE(zero.status == cppunivariate::ZipStatus::Ok);
  CHECK(zero.lambda == 1.0);
  CHECK(zero.pi == 1.0);
  CHECK(zero.logLik == 0.0);
  for (const std::vector<int> &v : {std::vector<int>{1, 2, 3},
                                  std::vector<int>{0, 1, 1, 2}}) {
    const auto r = cppunivariate::zip_mle(as_ispan(v));
    const auto pois = cppunivariate::pois_mle(as_ispan(v));
    REQUIRE(r.status == cppunivariate::ZipStatus::Ok);
    CHECK(r.pi == 0.0);
    CHECK(r.lambda == pois.lambda);
    CHECK(r.logLik == doctest::Approx(pois.logLik));
  }
  for (const std::vector<int> &v : {std::vector<int>{}, std::vector<int>{-1, 0}}) {
    CHECK(cppunivariate::zip_mle(as_ispan(v)).status ==
          cppunivariate::ZipStatus::InvalidInput);
  }
}

// Independent high-precision oracle; see quality/truth.py.
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("zip: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("zip",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dzip(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pzip(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qzip(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rzip(rng, p[0], p[1]); });
}

TEST_CASE("zip: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("zip");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<4;++mask) {
   const auto r=cppunivariate::zip_mle({x.data(),x.size()},(mask & 1)?g.estimates.at("lambda"):nan,(mask & 2)?g.estimates.at("pi"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.lambda==g.estimates.at("lambda"));
   if(mask & 2)CHECK(r.pi==g.estimates.at("pi"));
 }
}

TEST_CASE("zip: P2 held-domain and non-existence statuses") {
 const std::vector<int> x{0,1,5},z{0,0};
 const double nan=std::numeric_limits<double>::quiet_NaN();
 CHECK(cppunivariate::zip_mle({z.data(),z.size()},nan,.4).status==cppunivariate::ZipStatus::ZeroLimit);
 CHECK(cppunivariate::zip_mle({x.data(),x.size()},0,nan).status==cppunivariate::ZipStatus::InvalidInput);
 CHECK(cppunivariate::zip_mle({x.data(),x.size()},nan,1).status==cppunivariate::ZipStatus::InvalidInput);
 const auto r=cppunivariate::zip_mle({x.data(),x.size()},nan,.4);
 const double w=.6*std::exp(-r.lambda)/(.4+.6*std::exp(-r.lambda));
 CHECK(std::abs(6/r.lambda-2-w)<1e-6);
}


TEST_CASE("zip: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // l=mpf(1.0); pi=mpf(0.5)
  // d=lambda k:(1-pi)*exp(-l)*l**k/factorial(k); S=lambda k:(1-pi)*gammainc(k+1,0,l)/gamma(k+1)
  // print(log(d(mpf(1e+100)))); print(log(S(mpf(108.0))))
  // lo,hi=0,1
  // while log(S(hi))>-10000: hi*=2
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::dzip(1e+100, 1.0, 0.5, true) == doctest::Approx(-2.2925850929940456e+102).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pzip(108.0, 1.0, 0.5, false, true) == doctest::Approx(-407.3063116161621).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qzip(-10000, 1.0, 0.5, false, true) == 1571.0);
  CHECK(cppunivariate::distributions::dzip(0.5, 1.0, 0.5, false) == 0);
}

TEST_CASE("zip: structured stop payloads") {
  const std::vector<int> x{0,0};
  const auto r=cppunivariate::zip_mle({x.data(),x.size()},NAN,.5);
  REQUIRE(r.stop_detail.limit_model==cppunivariate::StopModel::Pois);
  CHECK(r.stop_detail.limit_coef[0]==0);
  CHECK(r.stop_detail.limit_loglik==0);
}

TEST_CASE("zip: certified held-inflation rates and competing peaks") {
  std::vector<int> x(100,0);x.back()=10;
  // Independent 80-digit mpmath roots and likelihoods, retained with the report.
  const std::vector<double> p{.001,.1,.9};
  const std::vector<double> rate{.10010953222725355,.11229195327730168,9.994983435397758};
  const std::vector<double> ll{-48.120846640688325,-47.13592437890727,-14.811337135452884};
  for(std::size_t j=0;j<p.size();++j) {
    const auto r=cppunivariate::zip_mle(as_ispan(x),NAN,p[j]);
    REQUIRE(r.status==cppunivariate::ZipStatus::Ok);
    CHECK(r.lambda==doctest::Approx(rate[j]).epsilon(1e-11));
    CHECK(r.logLik==doctest::Approx(ll[j]).epsilon(1e-12));
  }
  const double tie_pi=.7254971052291191;
  const auto tie=cppunivariate::zip_mle(as_ispan(x),NAN,tie_pi);
  REQUIRE(tie.status==cppunivariate::ZipStatus::Tie);
  REQUIRE(tie.stop_detail.candidates.size()==2);
  CHECK(tie.stop_detail.comparison_tie);
  CHECK(tie.stop_detail.candidates[0].coef[0]==doctest::Approx(.5223041246623353).epsilon(1e-11));
  CHECK(tie.stop_detail.candidates[1].coef[0]==doctest::Approx(9.982727928366345).epsilon(1e-11));
  const double tied_ll=-35.13856131179308;
  for(const auto& c:tie.stop_detail.candidates) {
    CHECK(c.loglik_lower<=tied_ll);
    CHECK(c.loglik_upper>=tied_ll);
  }
  CHECK(tie.stop_detail.loglik_lower<=tied_ll);
  CHECK(tie.stop_detail.loglik_upper>=tied_ll);
  for(double p:{tie_pi-1e-6,tie_pi+1e-6}) {
    const auto r=cppunivariate::zip_mle(as_ispan(x),NAN,p);
    REQUIRE(r.status==cppunivariate::ZipStatus::Ok);
    CHECK((r.lambda>5)==(p>tie_pi));
  }
}

TEST_CASE("zip: certificate budget and turning-point numerical stops") {
  std::vector<int> x(100,0);x.back()=10;
  const auto budget=cppunivariate::zip_mle(as_ispan(x),NAN,.1,1);
  REQUIRE(budget.status==cppunivariate::ZipStatus::Budget);
  CHECK(budget.stop_detail.budget_exhausted);
  CHECK(budget.stop_detail.candidates.size()==2);
  CHECK(std::isfinite(budget.stop_detail.loglik_lower));
  CHECK(budget.stop_detail.loglik_upper>=budget.stop_detail.loglik_lower);
  // A stationary rate at a turning point cannot be classified reliably.
  const auto tangent=cppunivariate::zip_mle(as_ispan(x),NAN,.7901867873967294);
  CHECK(tangent.status==cppunivariate::ZipStatus::NumericalFailure);
  CHECK(tangent.stop_detail.candidates.size()==1);
  CHECK(std::isinf(tangent.stop_detail.loglik_upper));
  const std::vector<int> repeated{0,8}; // d=v-u=4: repeated turning point.
  CHECK(cppunivariate::zip_mle(as_ispan(repeated),NAN,.4).status==cppunivariate::ZipStatus::NumericalFailure);
  const std::vector<int> monotone{0,1,2}; // d<4, exactly one root.
  const auto r=cppunivariate::zip_mle(as_ispan(monotone),NAN,.4);
  REQUIRE(r.status==cppunivariate::ZipStatus::Ok);
  CHECK(r.lambda>1);
  CHECK(r.lambda<1.5);
}

TEST_CASE("zip: stable held likelihoods and analytic submodels") {
  const std::vector<int> x{0,1,2},positive{1,2,3},zeros{0,0};
  const auto held=cppunivariate::zip_mle(as_ispan(x),1000,0);
  REQUIRE(held.status==cppunivariate::ZipStatus::Ok);
  CHECK(std::isfinite(held.logLik));
  CHECK(held.logLik==doctest::Approx(-3000+3*std::log(1000)-std::log(2.0)));
  const auto tiny=cppunivariate::zip_mle(as_ispan(x),2,std::numeric_limits<double>::denorm_min());
  REQUIRE(tiny.status==cppunivariate::ZipStatus::Ok);
  CHECK(std::isfinite(tiny.logLik));
  const auto r=cppunivariate::zip_mle(as_ispan(positive),NAN,.5);
  REQUIRE(r.status==cppunivariate::ZipStatus::Ok);
  CHECK(r.lambda==2);
  CHECK(cppunivariate::zip_mle(as_ispan(zeros),NAN,1).lambda==1);
}

TEST_CASE("zip: held tiny rate preserves the all-zero inflation maximum") {
  const std::vector<int> x{0,0};
  const auto r=cppunivariate::zip_mle(as_ispan(x),1e-20);
  REQUIRE(r.status==cppunivariate::ZipStatus::Ok);
  CHECK(r.pi==1);
  CHECK(r.logLik==0);
}
