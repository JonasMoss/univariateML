#include "test_support.hpp"
#include "cppunivariate/distributions/sgomp/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
TEST_CASE("sgomp tails and inversion") {
  CHECK(cppunivariate::distributions::dsgomp(1000,1,1,true)==doctest::Approx(-1000+std::log(2.0)));
  CHECK(cppunivariate::distributions::psgomp(1000,1,1,false,true)==doctest::Approx(-1000+std::log(2.0)));
  CHECK(cppunivariate::distributions::psgomp(1e-20,1,1,true,true)==doctest::Approx(std::log(1e-20)-1));
  const double extreme=cppunivariate::distributions::qsgomp(-1e300,1,1e308,true,true);
  CHECK(cppunivariate::distributions::psgomp(extreme,1,1e308,true,true)==doctest::Approx(-1e300).epsilon(1e-12));
  for (double eta:{0.,1e-10,1.,100.,1e100}) for (double lp:{-1.,-20.,-700.,-1000.}) for (bool lower:{false,true}) {
    const double q=cppunivariate::distributions::qsgomp(lp,2,eta,lower,true);
    if (q>0) CHECK(cppunivariate::distributions::psgomp(q,2,eta,lower,true)==doctest::Approx(lp).epsilon(1e-10));
  }
}
TEST_CASE("sgomp fitting faces fixed coordinates and collapse") {
  const std::vector<double> x{0,0,0,1,10},c{2,2,2},z{0,0};
  const auto f=cppunivariate::sgomp_mle({x.data(),x.size()});
  REQUIRE(f.status==cppunivariate::SgompStatus::Ok);
  CHECK(f.eta==0);
  CHECK(f.b==doctest::Approx(5./11));
  CHECK(cppunivariate::sgomp_mle({c.data(),c.size()}).status==cppunivariate::SgompStatus::Collapse);
  CHECK(cppunivariate::sgomp_mle({z.data(),z.size()},1e-10,200,1).eta==0);
  CHECK(cppunivariate::sgomp_mle({c.data(),c.size()},1e-10,200,1).status==cppunivariate::SgompStatus::Ok);
  CHECK(cppunivariate::sgomp_mle({c.data(),c.size()},1e-10,200,cppunivariate::sgomp_detail::nan(),2).status==cppunivariate::SgompStatus::Ok);
  CHECK(cppunivariate::sgomp_mle({x.data(),x.size()},1e-10,1).status==cppunivariate::SgompStatus::IterationFailure);
}
TEST_CASE("sgomp golden and independent score") {
  const auto g=mltest::load_distribution_golden("sgomp");
  const auto f=cppunivariate::sgomp_mle({g.data.data(),g.data.size()});
  REQUIRE(f.status==cppunivariate::SgompStatus::Ok);
  CHECK(f.b==doctest::Approx(g.estimates.at("b")).epsilon(1e-8));
  CHECK(f.eta==doctest::Approx(g.estimates.at("eta")).epsilon(1e-8));
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  double sb=0,se=0;
  for (double x:g.data) {
    const double v=std::exp(-f.b*x),a=-std::expm1(-f.b*x);
    sb+=1/f.b-x+f.eta*x*v+f.eta*x*v/(1+f.eta*a);
    se+=-v+a/(1+f.eta*a);
  }
  CHECK(std::abs(sb)<1e-3); CHECK(std::abs(se)<1e-3);
}
TEST_CASE("sgomp standalone sampler uses the same distribution") {
  cppunivariate::distributions::StdRng<> rng(719);
  double mean=0;
  for (int i=0;i<5000;++i) {
    const double x=cppunivariate::distributions::rsgomp(rng,2,3);
    mean+=cppunivariate::distributions::psgomp(x,2,3);
  }
  CHECK(std::abs(mean/5000-.5)<.02);
}

#include "cppunivariate/distributions/sgomp/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("sgomp: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("sgomp",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dsgomp(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::psgomp(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qsgomp(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rsgomp(rng, p[0], p[1]); });
}
