// paralogis: golden regression, score residual, start value.

#include "test_support.hpp"

#include "cppunivariate/core/finite_diff.hpp"
#include "cppunivariate/distributions/paralogis/mle.hpp"

#include <cmath>

TEST_CASE("paralogis: golden and score residual") {
  auto g = mltest::load_distribution_golden("paralogis");
  auto r = cppunivariate::paralogis_mle(mltest::as_span(g.data), mltest::kFitTol,
                                        mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::ParalogisStatus::Ok);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kGolden_paralogis).scale(0));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_paralogis).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_paralogis).scale(0));

  // A parameter silently held fixed (the old 1-D fit held scale at 1)
  // leaves a non-zero score component; both components must vanish.
  cppunivariate::ParalogisCost cost{mltest::as_span(g.data)};
  const double phi_hat[2] = {std::log(r.shape), std::log(r.scale)};
  double grad[2];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  CHECK(std::sqrt(grad[0] * grad[0] + grad[1] * grad[1]) < 1e-3);
}

TEST_CASE("paralogis: an explicit shape start reaches the same fit") {
  auto g = mltest::load_distribution_golden("paralogis");
  const auto x = mltest::as_span(g.data);
  auto a = cppunivariate::paralogis_mle(x, 1e-8, 500);
  auto b = cppunivariate::paralogis_mle(x, 1e-8, 500, 2.0 * a.shape);
  CHECK(b.status == cppunivariate::ParalogisStatus::Ok);
  CHECK(b.logLik == doctest::Approx(a.logLik).epsilon(1e-9));
  CHECK(b.shape == doctest::Approx(a.shape).epsilon(1e-4));
}

TEST_CASE("paralogis: historical fixed scale golden and equivariance") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/paralogis/fixed-scale-golden.json");
  auto r = cppunivariate::paralogis_mle_fixed_scale(mltest::as_span(g.data), 1);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(1e-9));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(1e-10));
  CHECK(r.scale == 1);
  for (auto &v : g.data) v *= 7;
  auto moved = cppunivariate::paralogis_mle_fixed_scale(mltest::as_span(g.data), 7);
  CHECK(moved.shape == doctest::Approx(r.shape).epsilon(1e-10));
  CHECK(moved.logLik == doctest::Approx(r.logLik-g.data.size()*std::log(7.0)).epsilon(1e-10));
  for (double bad : {0.0, -1.0, double(INFINITY), double(NAN)})
    CHECK(cppunivariate::paralogis_mle_fixed_scale(mltest::as_span(g.data), bad).status == cppunivariate::ParalogisStatus::InvalidStart);
}

TEST_CASE("paralogis: fixed-scale independent scipy oracle") {
  // scipy minimization of the reference log density, in log parameters.
  std::vector<double> x{.03,.1,.4,.7,1,2,4,12};
  auto r = cppunivariate::paralogis_mle_fixed_scale(mltest::as_span(x), 1);
  CHECK(r.logLik == doctest::Approx(-13.915168948543954).epsilon(1e-10));
}

#include "cppunivariate/distributions/paralogis/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("paralogis: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("paralogis",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dparalogis(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pparalogis(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qparalogis(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rparalogis(rng, p[0], p[1]); });
}

TEST_CASE("P2 paralogis golden holds") {
 auto g=mltest::load_distribution_golden("paralogis");
 const double nan=std::numeric_limits<double>::quiet_NaN();
for(int mask=0;mask<2;++mask){auto r=cppunivariate::paralogis_mle_holds(mltest::as_span(g.data),g.estimates.at("shape"),mask?g.estimates.at("scale"):nan);
 CHECK(r.shape==g.estimates.at("shape"));
 CHECK(r.logLik>=g.logLik-1e-5);}
}


// mpmath reproduction: import mpmath as mp; mp.mp.dps=1200
// F=lambda x: -mp.expm1(-2*mp.log1p(x**2)); Q=lambda l: mp.sqrt(mp.expm1(-mp.log1p(-mp.exp(l))/2))
// Evaluate log tails at the stated decimal x; log densities follow mp.log(mp.diff(F,x))
// (use -mp.diff(S,x) for survival definitions); Q uses the indicated log tail.
TEST_CASE("paralogis: deep tails and log inversion regressions") {
  CHECK(cppunivariate::distributions::pparalogis(1e-200,2,1,true,true) == doctest::Approx(std::log(2.0)-400*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::dparalogis(1e200,2,1,true) == doctest::Approx(std::log(4.0)-1000*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qparalogis(-1000,2,1,true,true) == doctest::Approx(std::exp((-1000-std::log(2.0))/2)).scale(0.0));
}

TEST_CASE("paralogis: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("paralogis");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::paralogis_mle(x,1e-8,1).status==cppunivariate::ParalogisStatus::BfgsFailure);
}

TEST_CASE("paralogis core preserves adapter constant-data rejection") {
  const std::vector<double> x{2,2,2};
  CHECK(cppunivariate::paralogis_mle(mltest::as_span(x),1e-8,100).status==cppunivariate::ParalogisStatus::Unbounded);
}

TEST_CASE("paralogis: compact certificate excludes competing maxima") {
  for(const std::vector<double>& x: {std::vector<double>{.03,.1,.4,.7,1,2,4,12},
       std::vector<double>{.3,.8,1,1,1},std::vector<double>{1.1,1.4,2,3,5}}) {
    auto r=cppunivariate::paralogis_mle(mltest::as_span(x),1e-8,1000);
    REQUIRE(r.status==cppunivariate::ParalogisStatus::Ok);
    auto c=cppunivariate::paralogis_cert_detail::certify(mltest::as_span(x),r.shape,r.scale,false);
    CHECK(c.ok);
    CHECK(c.boxes>0);
    auto stopped=cppunivariate::paralogis_cert_detail::certify(mltest::as_span(x),r.shape,r.scale,false,1);
    CHECK_FALSE(stopped.ok);
    CHECK(stopped.budget);
    // A deliberately insufficient proof budget cannot become a success.
    CHECK(c.shape==doctest::Approx(r.shape).epsilon(1e-8));
  }
}

TEST_CASE("paralogis: held scale certificate includes constant and one-point data") {
  for(double v:{.01,1.,100.}) {
    const std::vector<double> x{v};
    auto r=cppunivariate::paralogis_mle_fixed_scale(mltest::as_span(x),1,1e-10,1000);
    REQUIRE(r.status==cppunivariate::ParalogisStatus::Ok);
    CHECK(r.scale==1);
  }
  const std::vector<double> x{3,3,3};
  auto r=cppunivariate::paralogis_mle_fixed_scale(mltest::as_span(x),3);
  REQUIRE(r.status==cppunivariate::ParalogisStatus::Ok);
  CHECK(r.shape==doctest::Approx(2/std::log(2.)).epsilon(1e-9));
}

TEST_CASE("paralogis: failed proposal retains a bounded likelihood witness") {
  auto g=mltest::load_distribution_golden("paralogis");
  const auto r=cppunivariate::paralogis_mle(mltest::as_span(g.data),1e-10,1);
  CHECK(r.status==cppunivariate::ParalogisStatus::BfgsFailure);
  REQUIRE(r.stop_detail.candidates.size()==1);
  CHECK(r.stop_detail.candidates[0].loglik_lower<=r.logLik);
  CHECK(r.stop_detail.candidates[0].loglik_upper>=r.logLik);
  const std::vector<double> bad{0,0};
  CHECK(cppunivariate::paralogis_mle(mltest::as_span(bad),1e-8,100).status==cppunivariate::ParalogisStatus::InvalidStart);
}

TEST_CASE("paralogis: free common shape holds entrypoint retains the equality constraint") {
  const std::vector<double> x{.03,.1,.4,.7,1,2,4,12};
  const auto data=mltest::as_span(x);
  const auto a=cppunivariate::paralogis_mle_holds(data,NAN,1);
  const auto b=cppunivariate::paralogis_mle_fixed_scale(data,1);
  REQUIRE(a.status==cppunivariate::ParalogisStatus::Ok);
  CHECK(a.shape==doctest::Approx(b.shape).epsilon(1e-10));
  const auto c=cppunivariate::paralogis_mle_holds(data,NAN);
  REQUIRE(c.status==cppunivariate::ParalogisStatus::Ok);
  CHECK(c.logLik>=a.logLik);
}

TEST_CASE("paralogis: normalization overflow is numerical rather than invalid data") {
  const std::vector<double> x{1e-300,1e300,1e300};
  CHECK(cppunivariate::paralogis_mle(mltest::as_span(x),1e-8,100).status==cppunivariate::ParalogisStatus::NumericalFailure);
}

TEST_CASE("paralogis: held-scale competing modes return the certified winner") {
  for(double d:{.01,.003,.001,.0001,.00001}) {
    const std::vector<double> x{std::exp(-d)};
    const auto data=mltest::as_span(x);
    const auto r=cppunivariate::paralogis_mle_fixed_scale(data,1,1e-10,1000);
    REQUIRE(r.status==cppunivariate::ParalogisStatus::Ok);
    CHECK(r.shape>100);
    CHECK(cppunivariate::paralogis_cert_detail::certify(data,r.shape,1,true).ok);
    if(d==.001) {
      CHECK(r.shape==doctest::Approx(9262.778134534776).epsilon(1e-9));
      CHECK(r.logLik==doctest::Approx(8.126729864909168).epsilon(1e-10));
      const auto losing=cppunivariate::paralogis_shape_proposal(data,1);
      CHECK_FALSE(cppunivariate::paralogis_cert_detail::certify(data,losing.shape,1,true).ok);
      CHECK_FALSE(cppunivariate::paralogis_cert_detail::certify(data,r.shape,1,true,1).ok);
    }
  }
}

TEST_CASE("paralogis: binned bounds enclose exact grouped derivatives") {
  std::vector<cppunivariate::burr_cert_detail::Interval> y;
  for(int i=0;i<10000;++i)y.push_back({-.8+double(i)/10000,-.8+double(i)/10000});
  const cppunivariate::paralogis_cert_detail::Data data(y);
  for(bool held:{false,true})for(double a:{.5,3.,100.}) {
    auto d=data;d.held=held;
    std::array<cppunivariate::burr_cert_detail::Interval,3> g,gb;
    cppunivariate::burr_cert_detail::BoxMatrix h,hb;
    cppunivariate::burr_cert_detail::Interval l,lb;
    REQUIRE(cppunivariate::paralogis_cert_detail::derivatives(d,a,0,g,h,l));
    for(int level=0;level<3;++level) {
      REQUIRE(cppunivariate::paralogis_cert_detail::derivatives(d,a,0,gb,hb,lb,level));
      CHECK(lb.lo<=l.lo);CHECK(lb.hi>=l.hi);
      for(int j=0;j<2;++j){CHECK(gb[j].lo<=g[j].lo);CHECK(gb[j].hi>=g[j].hi);}
    }
  }
}
