#include "test_support.hpp"
#include "cppunivariate/distributions/foldnorm/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <limits>
TEST_CASE("foldnorm golden and second moment identity") {
  const auto g=mltest::load_distribution_golden("foldnorm");
  const auto r=cppunivariate::foldnorm_mle({g.data.data(),g.data.size()});
  CHECK(r.status==cppunivariate::FoldnormStatus::Ok);
  CHECK(r.mu==doctest::Approx(g.estimates.at("mu")).epsilon(1e-6));
  CHECK(r.sigma==doctest::Approx(g.estimates.at("sigma")).epsilon(1e-6));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-12));
  double score_mu=0,score_sigma=0;
  for (double z:g.data) {
    const double t=std::tanh(z*r.mu/(r.sigma*r.sigma));
    score_mu+=(z*t-r.mu)/(r.sigma*r.sigma);
    score_sigma+=-1/r.sigma+(z*z+r.mu*r.mu-2*r.mu*z*t)/std::pow(r.sigma,3);
  }
  CHECK(std::abs(score_mu)<1e-3);
  CHECK(std::abs(score_sigma)<1e-3);
  double m2=0;for (double z:g.data) m2+=z*z/g.data.size();
  CHECK(r.mu*r.mu+r.sigma*r.sigma==doctest::Approx(m2).epsilon(1e-12));
}
TEST_CASE("foldnorm boundary, zeros and fixed subsets") {
  const std::vector<double> x{0,0,0,0,1};
  const auto r=cppunivariate::foldnorm_mle({x.data(),x.size()});
  CHECK(r.status==cppunivariate::FoldnormStatus::Ok);
  CHECK(r.mu==0);
  CHECK(r.sigma==doctest::Approx(std::sqrt(.2)));
  const std::vector<double> c{2,2};
  CHECK(cppunivariate::foldnorm_mle({c.data(),c.size()}).status==cppunivariate::FoldnormStatus::Unbounded);
  CHECK(cppunivariate::foldnorm_mle({c.data(),c.size()},1e-10,200,0).sigma==doctest::Approx(2));
  CHECK(cppunivariate::foldnorm_mle({c.data(),c.size()},1e-10,200,0,1).status==cppunivariate::FoldnormStatus::Ok);
  const std::vector<double> constant(10,1);
  CHECK(cppunivariate::foldnorm_mle({constant.data(),constant.size()}).status==cppunivariate::FoldnormStatus::Unbounded);
  const std::vector<double> bad{-1,2};
  CHECK(cppunivariate::foldnorm_mle({bad.data(),bad.size()}).status==cppunivariate::FoldnormStatus::InvalidInput);
}
TEST_CASE("foldnorm density tails inversion and portable RNG") {
  for (double mu:{0.,.1,2.,40.}) for (double s:{.2,1.,3.}) {
    for (double x:{0.,.1,1.,10.}) {
      CHECK(cppunivariate::distributions::dfoldnorm(x,mu,s)==doctest::Approx(cppunivariate::special::dnorm(x,mu,s,false)+cppunivariate::special::dnorm(-x,mu,s,false)).epsilon(1e-12));
      CHECK(cppunivariate::distributions::pfoldnorm(x,mu,s)+cppunivariate::distributions::pfoldnorm(x,mu,s,false)==doctest::Approx(1).epsilon(1e-12));
    }
    for (bool tail:{true,false}) for (double lp:{-1e-10,-.7,-10.,-100.,(tail ? -600. : -1000.)}) {
      const double q=cppunivariate::distributions::qfoldnorm(lp,mu,s,tail,true);
      CHECK(cppunivariate::distributions::pfoldnorm(q,mu,s,tail,true)==doctest::Approx(lp).epsilon(1e-10));
    }
  }
  cppunivariate::distributions::StdRng<> rng(7);
  double sum=0; for (int k=0;k<10000;++k) { const double z=cppunivariate::distributions::rfoldnorm(rng,2,1); CHECK(z>=0);sum+=z*z/10000; }
  CHECK(sum==doctest::Approx(5).epsilon(.04));
}

#include "cppunivariate/distributions/foldnorm/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("foldnorm: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("foldnorm",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dfoldnorm(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pfoldnorm(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qfoldnorm(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rfoldnorm(rng, p[0], p[1]); });
}

TEST_CASE("foldnorm R6 constant free and held-location collapse") {
  for (double value : {0.,2.}) {
    const std::vector<double> x(3,value);
    CHECK(cppunivariate::foldnorm_mle(mltest::as_span(x)).status==cppunivariate::FoldnormStatus::Unbounded);
    CHECK(cppunivariate::foldnorm_mle(mltest::as_span(x),1e-8,100,value).status==cppunivariate::FoldnormStatus::Unbounded);
  }
}

TEST_CASE("foldnorm scaled residuals: normal scale at extreme units") {
  for(double unit:{1e-200,1e200}) {
    const std::vector<double> x={unit,2*unit};
    const auto r=cppunivariate::foldnorm_mle(mltest::as_span(x),1e-10,200,0);
    REQUIRE(r.status==cppunivariate::FoldnormStatus::Ok);
    CHECK(r.sigma/unit==doctest::Approx(std::sqrt(2.5)).epsilon(1e-12));
    CHECK(std::isfinite(r.logLik));
  }
}
