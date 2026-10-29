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
