#include "test_support.hpp"
#include "cppunivariate/distributions/invparalogis/mle.hpp"
#include "cppunivariate/distributions/invparalogis/dpqr.hpp"
TEST_CASE("invparalogis: golden, reciprocal agreement and score") {
  auto g=mltest::load_distribution_golden("invparalogis");
  auto r=cppunivariate::invparalogis_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  // invparalogis_mle is paralogis_mle on 1/x (same finite-difference BFGS,
  // same default gradtol kFitTol); reuse kFdTol like the paralogis golden
  // check for the same reason (test_support.hpp's kFdTol comment).
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kGolden_invparalogis).scale(0)); CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_invparalogis).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_invparalogis).scale(0));
  std::vector<double> y; double jac=0;
  for (double x:g.data) { y.push_back(1/x); jac-=2*std::log(x); }
  auto b=cppunivariate::paralogis_mle(mltest::as_span(y), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(b.shape)); CHECK(r.scale == doctest::Approx(1/b.scale));
  CHECK(r.logLik == doctest::Approx(b.logLik+jac).epsilon(1e-12));
  
  // Independent reference log-density score in log parameters.
  auto cost = [&](double u, double v) {
    double ll=0;
    for (double x : g.data) ll+=cppunivariate::distributions::dinvparalogis(x,std::exp(u),std::exp(v),true);
    return ll;
  };
  const double u=std::log(r.shape),v=std::log(r.scale),h=1e-5;
  const double a=(cost(u+h,v)-cost(u-h,v))/(2*h), bscore=(cost(u,v+h)-cost(u,v-h))/(2*h);
  CHECK(std::hypot(a,bscore)<1e-3);

}
TEST_CASE("invparalogis: fixed scale reciprocal agreement and invalid values") {
  std::vector<double> x{.03,.1,.4,.7,1,2,4,12}, y;
  double jac=0;
  for(double v:x) {y.push_back(1/v);jac-=2*std::log(v);}
  auto r=cppunivariate::invparalogis_mle_fixed_scale(mltest::as_span(x),3.7);
  auto b=cppunivariate::paralogis_mle_fixed_scale(mltest::as_span(y),1/3.7);
  CHECK(r.scale==3.7);
  CHECK(r.shape==doctest::Approx(b.shape).epsilon(1e-12));
  CHECK(r.logLik==doctest::Approx(b.logLik+jac).epsilon(1e-12));
  for(double bad:{0.,-1.,double(INFINITY),double(NAN)})
    CHECK(cppunivariate::invparalogis_mle_fixed_scale(mltest::as_span(x),bad).status==cppunivariate::ParalogisStatus::InvalidStart);
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("invparalogis: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("invparalogis",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dinvparalogis(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pinvparalogis(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qinvparalogis(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rinvparalogis(rng, p[0], p[1]); });
}

TEST_CASE("P2 invparalogis golden holds") {
 auto g=mltest::load_distribution_golden("invparalogis");
 const double nan=std::numeric_limits<double>::quiet_NaN();
for(int mask=0;mask<2;++mask){auto r=cppunivariate::invparalogis_mle_holds(mltest::as_span(g.data),g.estimates.at("shape"),mask?g.estimates.at("scale"):nan);
 CHECK(r.shape==g.estimates.at("shape"));
 CHECK(r.logLik>=g.logLik-1e-5);}
}


// mpmath reproduction: import mpmath as mp; mp.mp.dps=1200
// F=lambda x: (1+x**-2)**-2; Q=lambda l: 1/mp.sqrt(mp.expm1(-mp.log1p(-mp.exp(l))/2))
// Evaluate log tails at the stated decimal x; log densities follow mp.log(mp.diff(F,x))
// (use -mp.diff(S,x) for survival definitions); Q uses the indicated log tail.
TEST_CASE("invparalogis: deep tails and log inversion regressions") {
  CHECK(cppunivariate::distributions::pinvparalogis(1e200,2,1,false,true) == doctest::Approx(std::log(2.0)-400*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::dinvparalogis(1e-200,2,1,true) == doctest::Approx(std::log(4.0)-600*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qinvparalogis(-1000,2,1,false,true) == doctest::Approx(std::exp((1000+std::log(2.0))/2)).scale(0.0));
}

TEST_CASE("invparalogis: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("invparalogis");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::invparalogis_mle(x,1e-8,1).status==cppunivariate::ParalogisStatus::BfgsFailure);
}

TEST_CASE("invparalogis: reciprocal failure witnesses preserve coefficients and bounds") {
  const std::vector<double> x{.03,.1,.4,.7,1,2,4,12};
  std::vector<double> y;double jac=0;
  for(double v:x){y.push_back(1/v);jac-=2*std::log(v);}
  auto a=cppunivariate::paralogis_mle(mltest::as_span(y),1e-10,1);
  auto b=cppunivariate::invparalogis_mle(mltest::as_span(x),1e-10,1);
  REQUIRE(a.status==cppunivariate::ParalogisStatus::BfgsFailure);
  CHECK(b.status==a.status);
  REQUIRE(b.stop_detail.candidates.size()==a.stop_detail.candidates.size());
  CHECK(b.stop_detail.candidates[0].coef[1]==1/a.stop_detail.candidates[0].coef[1]);
  CHECK(b.stop_detail.candidates[0].loglik_lower==a.stop_detail.candidates[0].loglik_lower+jac);
  CHECK(b.stop_detail.candidates[0].loglik_upper==a.stop_detail.candidates[0].loglik_upper+jac);
}

TEST_CASE("invparalogis: free common shape holds entrypoint uses certified delegation") {
  const std::vector<double> x{.03,.1,.4,.7,1,2,4,12};
  const auto data=mltest::as_span(x);
  const auto a=cppunivariate::invparalogis_mle_holds(data,NAN,1);
  const auto b=cppunivariate::invparalogis_mle_fixed_scale(data,1);
  REQUIRE(a.status==cppunivariate::ParalogisStatus::Ok);
  CHECK(a.shape==doctest::Approx(b.shape).epsilon(1e-10));
}

TEST_CASE("invparalogis: reciprocal representation failure is numerical") {
  const std::vector<double> x{1e-320,1};
  CHECK(cppunivariate::invparalogis_mle(mltest::as_span(x),1e-8,100).status==cppunivariate::ParalogisStatus::NumericalFailure);
  CHECK(cppunivariate::invparalogis_mle_fixed_scale(mltest::as_span(x),1).status==cppunivariate::ParalogisStatus::NumericalFailure);
  const std::vector<double> constant{1e-320,1e-320};
  CHECK(cppunivariate::invparalogis_mle(mltest::as_span(constant),1e-8,100).status==cppunivariate::ParalogisStatus::Unbounded);
}

TEST_CASE("invparalogis: rounded reciprocal ties do not imply nonexistence") {
  const std::vector<double> x{1.9,std::nextafter(1.9,INFINITY)};
  REQUIRE(x[0]!=x[1]);
  REQUIRE(1/x[0]==1/x[1]);
  const auto r=cppunivariate::invparalogis_mle(mltest::as_span(x),1e-8,100);
  CHECK(r.status==cppunivariate::ParalogisStatus::NumericalFailure);
  CHECK(r.stop_detail.candidates.empty());
  CHECK_FALSE(std::isinf(r.stop_detail.loglik_lower));
}

TEST_CASE("invparalogis: competing held-scale winner and reciprocal likelihood") {
  const std::vector<double> x{std::exp(.001)};
  const std::vector<double> y{1/x[0]};
  const auto a=cppunivariate::invparalogis_mle_fixed_scale(mltest::as_span(x),1);
  const auto b=cppunivariate::paralogis_mle_fixed_scale(mltest::as_span(y),1);
  REQUIRE(a.status==cppunivariate::ParalogisStatus::Ok);
  CHECK(a.shape==doctest::Approx(b.shape).epsilon(1e-10));
  CHECK(a.logLik==doctest::Approx(b.logLik-2*std::log(x[0])).epsilon(1e-10));
}

TEST_CASE("inverse paralogistic transport keeps the parent flag on both search routes") {
 const std::vector<std::vector<double>> samples{{.9998,.9999,1,1.0001,1.0003},{.99999999999999}};
 for(const auto& raw:samples){
  std::vector<double> x,y;for(double v:raw)x.push_back(1/v);double jac=0;for(double v:x){y.push_back(1/v);jac-=2*std::log(v);}
  const auto p=raw.size()==1?cppunivariate::paralogis_mle_fixed_scale(mltest::as_span(y),1,1e-8,1000,NAN):cppunivariate::paralogis_mle(mltest::as_span(y),1e-8,1000);
  const auto r=raw.size()==1?cppunivariate::invparalogis_mle_fixed_scale(mltest::as_span(x),1,1e-8,1000):cppunivariate::invparalogis_mle(mltest::as_span(x),1e-8,1000);
  REQUIRE(p.status==cppunivariate::ParalogisStatus::Ok);REQUIRE(r.status==p.status);
  CHECK_FALSE(p.certified);CHECK(r.certified==p.certified);CHECK(r.shape==p.shape);CHECK(r.scale==1/p.scale);CHECK(r.logLik==p.logLik+jac);
 }
}
