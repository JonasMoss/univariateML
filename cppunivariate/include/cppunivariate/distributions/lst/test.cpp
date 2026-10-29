#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/lst/mle.hpp"
#include "cppunivariate/distributions/lst/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include "cppunivariate/core/finite_diff.hpp"
#include <vector>
#include <cmath>

TEST_CASE("lst: golden and analytic score") {
  auto g = mltest::load_distribution_golden("lst");
  auto r = cppunivariate::lst_mle(mltest::as_span(g.data),mltest::kFitTol,mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::LstStatus::Ok);
  CHECK(r.mu == doctest::Approx(g.estimates.at("mu")).epsilon(1e-6));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sigma")).epsilon(1e-6));
  CHECK(r.df == doctest::Approx(g.estimates.at("df")).epsilon(1e-6));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(1e-10));
  cppunivariate::LstCost cost{mltest::as_span(g.data)};
  double p[3] = {r.mu,std::log(r.sigma),std::log(r.df-1.0)};
  double grad[3], numeric[3];
  cost.gradient(p,grad);
  cppunivariate::finite_diff_gradient(cost,p,numeric);
  for (int i=0;i<3;++i) {
    CHECK(std::abs(grad[i]) < 1e-3);
    CHECK(std::abs(numeric[i]) < 1e-3);
    CHECK(std::abs(grad[i]-numeric[i]) < 1e-4);
  }
}
TEST_CASE("lst: analytic gradients away from optimum") {
  const std::vector<double> x = {-3,-1,0,1,8};
  for (double df : {1.1,2.0,10.0,20000.0}) {
    cppunivariate::LstCost cost{mltest::as_span(x)};
    double p[3] = {0.3,std::log(1.2),std::log(df-1)};
    double a[3], b[3];
    cost.gradient(p,a);
    cppunivariate::finite_diff_gradient(cost,p,b);
    for (int j=0;j<3;++j) CHECK(a[j] == doctest::Approx(b[j]).epsilon(1e-5));
  }
}
TEST_CASE("lst: faces and collapse guard") {
  std::vector<double> x;
  for (int i=0;i<200;++i) x.push_back(cppunivariate::special::qt((i+0.5)/200,0.8,true,false));
  auto c = cppunivariate::lst_mle(mltest::as_span(x),1e-7,300);
  CHECK(c.status == cppunivariate::LstStatus::Ok);
  CHECK(c.df == 1.0);
  for (int i=0;i<200;++i) x[i] = cppunivariate::special::qnorm((i+0.5)/200,0,1,true,false);
  CHECK(cppunivariate::lst_mle(mltest::as_span(x),1e-7,300).status == cppunivariate::LstStatus::NormalBoundary);
  for (const std::vector<double> bad : {std::vector<double>{0,1}, {0,0,1,2}, {1,1,1,1}})
    CHECK(cppunivariate::lst_mle(mltest::as_span(bad),1e-7,200).status == cppunivariate::LstStatus::NoMLE);
}
TEST_CASE("lst: dpqr location-scale convention and tails") {
  cppunivariate::distributions::StdRng<> rng(123);
  for (double df : {0.5,1.0,1.5,3.0,10.0}) {
    for (double p : {0.001,0.2,0.8,0.999}) {
      const double q = cppunivariate::distributions::qlst(p,2,3,df,true,false);
      CHECK(cppunivariate::distributions::plst(q,2,3,df,true,false) == doctest::Approx(p));
      CHECK(cppunivariate::distributions::qlst(std::log(p),2,3,df,false,true) ==
            doctest::Approx(cppunivariate::distributions::qlst(p,2,3,df,false,false)));
      CHECK(cppunivariate::distributions::dlst(q,2,3,df,true) == doctest::Approx(cppunivariate::special::dt((q-2)/3,df,true)-std::log(3.0)));
    }
    CHECK(std::isfinite(cppunivariate::distributions::rlst(rng,2,3,df)));
  }
}

TEST_CASE("lst: fixed df score, ties and validation") {
  const std::vector<double> x = {-5,-2,-1,0,0,1,2,9};
  for (double df : {1.0,2.5,10.0}) {
    auto r = cppunivariate::lst_mle_fixed_df(mltest::as_span(x),df,1e-8,5000);
    REQUIRE(r.status == cppunivariate::LstStatus::Ok);
    CHECK(r.df == df);
    cppunivariate::LstCost cost{mltest::as_span(x),df};
    double p[2] = {r.mu,std::log(r.sigma)}, g[2];
    cost.gradient(p,g);
    CHECK(std::hypot(r.sigma*g[0],g[1]) < 1e-8);
    CHECK(r.logLik == doctest::Approx(-cost(p)).epsilon(1e-12));
  }
  for (double df : {0.0,0.5,-1.0,std::numeric_limits<double>::infinity()})
    CHECK(cppunivariate::lst_mle_fixed_df(mltest::as_span(x),df,1e-8,100).status == cppunivariate::LstStatus::InvalidDf);
  const std::vector<double> tied = {0,0,0,1};
  CHECK(cppunivariate::lst_mle_fixed_df(mltest::as_span(tied),2,1e-8,100).status == cppunivariate::LstStatus::Unbounded);
  CHECK(cppunivariate::lst_mle_fixed_df(mltest::as_span(tied),3,1e-8,100).status == cppunivariate::LstStatus::ScaleBoundary);
  CHECK(cppunivariate::lst_mle_fixed_df(mltest::as_span(tied),4,1e-8,5000).status == cppunivariate::LstStatus::Ok);
  CHECK(cppunivariate::lst_mle_fixed_df(mltest::as_span(x),3,1e-12,0).status == cppunivariate::LstStatus::IterationFailure);
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("lst: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("lst",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dlst(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::plst(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qlst(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rlst(rng, p[0], p[1], p[2]); });
}

TEST_CASE("lst: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("lst");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<8;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::lst_mle(mltest::as_span(g.data),1e-8,1000,((mask & 1) ? g.estimates.at("mu") : nan),((mask & 2) ? g.estimates.at("sigma") : nan),((mask & 4) ? g.estimates.at("df") : nan));
    if(mask & 1)CHECK(r.mu==g.estimates.at("mu"));
    if(mask & 2)CHECK(r.sigma==g.estimates.at("sigma"));
    if(mask & 4)CHECK(r.df==g.estimates.at("df"));
    CHECK(r.status==cppunivariate::LstStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("lst: held-location half ties compare the normal face") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x={0,1};
  CHECK(cppunivariate::lst_mle(mltest::as_span(x),1e-8,200,0,nan,nan).status==cppunivariate::LstStatus::NormalBoundary);
}

TEST_CASE("lst: held profiles preserve extreme units") {
  const std::vector<double> x={-2,-.5,0,.7,3};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto base=cppunivariate::lst_mle(mltest::as_span(x),1e-8,200,0,nan,1e20);
  for(double unit:{1e-300,1e300}) {
    auto y=x;for(double& v:y)v*=unit;
    const auto fit=cppunivariate::lst_mle(mltest::as_span(y),1e-8,200,0,nan,1e20);
    CHECK(fit.sigma/unit==doctest::Approx(base.sigma).epsilon(1e-8));
    CHECK(fit.logLik+x.size()*std::log(unit)==doctest::Approx(base.logLik).epsilon(1e-8));
  }
  CHECK(base.sigma==doctest::Approx(std::sqrt(2.748)).epsilon(1e-8));
}


// Generating mpmath snippet (binary64 inputs retained exactly):
// import mpmath as mp; mp.mp.dps=100
// n=mp.mpf(3); x=mp.mpf(1e300)
// print(-mp.log(mp.beta(n/2,mp.mpf('.5')))-mp.log(n)/2-(n+1)/2*mp.log1p(x*x/n))
// ls=lambda y: mp.log(mp.betainc(n/2,mp.mpf('.5'),0,n/(n+mp.exp(2*y)),regularized=True)/2)
// print(mp.exp(mp.findroot(lambda y: ls(y)+800,800/n)))
TEST_CASE("lst: targeted tail and endpoint regressions") {
  CHECK(cppunivariate::distributions::dlst(1e+300, 0.0, 1.0, 3.0, true) == doctest::Approx(-2761.905775865142).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qlst(-800.0, 0.0, 1.0, 3.0, false, true) == doctest::Approx(6.698981179231598e+115).epsilon(1e-10));
}

// import mpmath as mp; mp.mp.dps=100
// n=mp.mpf(3); s=mp.mpf(1e-300); x=mp.mpf(1e300)
// c=(n/2-1)*mp.log(n)-mp.log(mp.beta(n/2,mp.mpf('.5')))
// print(mp.exp(mp.log(s)+(c+2200)/n),c-n*mp.log(x/s))
// The omitted Student tail correction is O((x/s)**-2), below 1e-1200.
TEST_CASE("lst: targeted tail scaling before exponentiation") {
  CHECK(cppunivariate::distributions::qlst(-2200,0,1e-300,3,false,true)
        == doctest::Approx(3.13882410493767e18).epsilon(1e-10));
  CHECK(cppunivariate::distributions::plst(1e300,0,1e-300,3,false,true)
        == doctest::Approx(-4144.5554439502375).epsilon(1e-10));
  CHECK(cppunivariate::distributions::dlst(1e300,0,1e-300,3,true)
        == doctest::Approx(-4834.232359559784).epsilon(1e-10));
}
