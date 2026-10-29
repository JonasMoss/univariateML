#include "test_support.hpp"
#include "cppunivariate/distributions/cmb/mle.hpp"
#include <vector>
TEST_CASE("cmb golden and moment score") {
  const auto g=mltest::load_distribution_golden("cmb");
  std::vector<int> x; for (double v:g.data) x.push_back(static_cast<int>(v));
  const auto f=cppunivariate::cmb_mle({x.data(),x.size()},10);
  REQUIRE(f.status==cppunivariate::CmbStatus::Ok);
  CHECK(f.prob==doctest::Approx(g.estimates.at("prob")).epsilon(1e-8));
  CHECK(f.nu==doctest::Approx(g.estimates.at("nu")).epsilon(1e-8));
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  const auto t=cppunivariate::cmb_detail::Table(10,std::log(f.prob/(1-f.prob)),f.nu);
  double m=0,h=0; for (int v:x) {m+=v; h+=cppunivariate::cmb_detail::logchoose(10,v);}
  CHECK(std::abs(m/x.size()-t.mean)<1e-8);
  CHECK(std::abs(h/x.size()-t.mh)<1e-8);
}
TEST_CASE("cmb binomial and all finite-family faces") {
  for (int N:{1,5,20}) for (double p:{.01,.3,.99}) {
    double sum=0;
    for (int k=0;k<=N;++k) {
      const double lp=cppunivariate::cmb_detail::logchoose(N,k)+k*std::log(p)+(N-k)*std::log1p(-p);
      const double w=cppunivariate::distributions::dcmb(k,N,p,1);
      CHECK(w==doctest::Approx(std::exp(lp)).epsilon(1e-11)); sum+=w;
    }
    CHECK(sum==doctest::Approx(1).epsilon(1e-12));
  }
  const std::vector<int> a{2,2},b{2,3},c{0,5},d{0,1,4,5};
  CHECK(cppunivariate::cmb_mle({a.data(),a.size()},5).status==cppunivariate::CmbStatus::PointMass);
  CHECK(cppunivariate::cmb_mle({b.data(),b.size()},5).status==cppunivariate::CmbStatus::AdjacentLimit);
  CHECK(cppunivariate::cmb_mle({c.data(),c.size()},5).status==cppunivariate::CmbStatus::EndpointLimit);
  CHECK(cppunivariate::cmb_mle({d.data(),d.size()},5).status==cppunivariate::CmbStatus::Ok);
}

#include "cppunivariate/distributions/cmb/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("cmb: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("cmb",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dcmb(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pcmb(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qcmb(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rcmb(rng, p[0], p[1], p[2]); });
}
