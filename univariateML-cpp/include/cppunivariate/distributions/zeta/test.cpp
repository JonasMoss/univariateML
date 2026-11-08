#include "test_support.hpp"
#include "cppunivariate/distributions/zeta/mle.hpp"
#include "cppunivariate/distributions/zeta/dpqr.hpp"
#include <vector>
namespace {
TEST_CASE("zeta: golden, boundary and invalid data") {
  const auto g=mltest::load_distribution_golden("zeta");
  std::vector<int> x;
  for (double k:g.data) x.push_back(static_cast<int>(k));
  const auto fit=cppunivariate::zeta_mle({x.data(),x.size()},1e-10,200);
  CHECK(fit.status==cppunivariate::ZetaStatus::Ok);
  CHECK(fit.s==doctest::Approx(g.estimates.at("s")).epsilon(1e-8));
  CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  const std::vector<int> one={1,1},constant={2,2},bad={0,1};
  CHECK(cppunivariate::zeta_mle({one.data(),one.size()}).status==cppunivariate::ZetaStatus::PointMass);
  CHECK(cppunivariate::zeta_mle({constant.data(),constant.size()}).status==cppunivariate::ZetaStatus::Ok);
  CHECK(cppunivariate::zeta_mle({bad.data(),bad.size()}).status==cppunivariate::ZetaStatus::InvalidInput);
  CHECK(cppunivariate::zeta_mle({constant.data(),constant.size()},1e-14,1).status==cppunivariate::ZetaStatus::IterationFailure);
}
TEST_CASE("zeta: direct sums and inverse inequalities") {
  for (double shape:{1.1,2.0,10.0}) {
    double total=0;
    for (int k=1;k<=1000;++k) {
      total+=cppunivariate::distributions::dzeta(k,shape);
      if (k==1 || k==10 || k==1000)
        CHECK(cppunivariate::distributions::pzeta(k+.75,shape)==doctest::Approx(total).epsilon(2e-11));
    }
    for (double u:{.001,.1,.5,.9,.999}) {
      const double q=cppunivariate::distributions::qzeta(u,shape);
      if (q<9e15) {
        CHECK(cppunivariate::distributions::pzeta(q,shape)>=u-2e-15);
        CHECK(cppunivariate::distributions::pzeta(q-1,shape)<u+2e-15);
      }
    }
    CHECK(std::isfinite(cppunivariate::distributions::pzeta(1e200,shape,false,true)));
  }
}
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("zeta: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("zeta",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dzeta(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pzeta(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qzeta(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rzeta(rng, p[0]); });
}

TEST_CASE("zeta: held coefficient bypasses the all-ones escape") {
  const std::vector<int> x={1,1,1};
  const auto f=cppunivariate::zeta_mle({x.data(),x.size()},1e-10,200,2.3);
  CHECK(f.status==cppunivariate::ZetaStatus::Ok);
  CHECK(f.s==2.3);
  CHECK(std::isfinite(f.logLik));
  CHECK(f.iterations==0);
  CHECK(cppunivariate::zeta_mle({x.data(),x.size()},1e-10,200,0).status==cppunivariate::ZetaStatus::InvalidInput);
}

TEST_CASE("zeta: structured stop payloads") {
  const std::vector<int> x{1,1};
  const auto r=cppunivariate::zeta_mle({x.data(),x.size()});
  REQUIRE(r.stop_detail.limit_coef.size()==1);
  CHECK(r.stop_detail.limit_model==cppunivariate::StopModel::PointMass);
  CHECK(r.stop_detail.limit_coef[0]==1);
  CHECK(r.stop_detail.limit_loglik==0);
  CHECK(r.stop_detail.candidates.size()==1);
}

#include "cppunivariate/distributions/zeta/dpqr.hpp"
TEST_CASE("zeta: quantile search guards and representability") {
  CHECK(cppunivariate::distributions::qzeta(.9,1.001)==INFINITY);
}
