#include "test_support.hpp"
#include "cppunivariate/distributions/invparalogis/mle.hpp"
#include "cppunivariate/distributions/invparalogis/dpqr.hpp"
TEST_CASE("invparalogis: golden, reciprocal agreement and score") {
  auto g=mltest::load_distribution_golden("invparalogis");
  auto r=cppunivariate::invparalogis_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(1e-6)); CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(1e-6));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(1e-6));
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
