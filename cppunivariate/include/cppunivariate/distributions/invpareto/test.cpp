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
  CHECK(cppunivariate::invpareto_mle(mltest::as_span(x),1e-10,100).status == cppunivariate::LomaxStatus::MeanVarianceCondition);
}
