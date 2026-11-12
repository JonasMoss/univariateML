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
  CHECK(fit.s==doctest::Approx(g.estimates.at("s")).epsilon(mltest::kGolden_zeta).scale(0));
  CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_zeta).scale(0));
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

#include "truncation.hpp"
TEST_CASE("finite zeta windows widen the exponent domain") {
  std::vector<int> x{1,2,3,4,4,4};
  const auto r=cppunivariate::zeta_window_mle({x.data(),x.size()},1,4);
  CHECK(r.status==cppunivariate::ZetaWindowStatus::Ok);
  CHECK(r.s<0);
  double total=0;
  for(int k=1;k<=4;++k)total+=cppunivariate::zeta_window_eval('d',k,4,r.s);
  CHECK(total==doctest::Approx(1));
  std::vector<int> edge{4,4};
  CHECK(cppunivariate::zeta_window_mle({edge.data(),edge.size()},1,4).status==cppunivariate::ZetaWindowStatus::UpperLimit);
  CHECK(cppunivariate::zeta_window_mle({x.data(),x.size()},1,4,-2).s==-2);
  CHECK(cppunivariate::zeta_window_mle({x.data(),x.size()},1,4,NAN,1e-10,1).status==cppunivariate::ZetaWindowStatus::NumericalFailure);
}

TEST_CASE("finite zeta endpoint counts and log complements remain resolved") {
  const int N=2147483647;
  std::vector<int> x{N-1,N};
  const auto r=cppunivariate::zeta_window_mle({x.data(),x.size()},1,N);
  CHECK(r.status==cppunivariate::ZetaWindowStatus::Ok);
  CHECK(r.s<0);
  const double denominator=double(N)*(double(N)+1)*(2.*N+1)/6;
  CHECK(cppunivariate::zeta_window_eval('d',1,N,-2,true,true)==doctest::Approx(-std::log(denominator)).epsilon(1e-12));
  const double tail=cppunivariate::zeta_window_eval('p',1,N,-2,false,true);
  CHECK(tail<0);
  CHECK(tail/std::log1p(-1/denominator)==doctest::Approx(1).epsilon(1e-12));
  const double mode=cppunivariate::zeta_window_eval('d',4,4,-100,true,true);
  const double exact=-std::log1p(std::pow(.75,100)+std::pow(.5,100)+std::pow(.25,100));
  CHECK(mode/exact==doctest::Approx(1).epsilon(1e-12));
}

TEST_CASE("zeta finite window: huge held exponent retains the endpoint law") {
  const std::vector<int> x{1,1};
  const auto fit=cppunivariate::zeta_window_mle({x.data(),x.size()},1,2147483647.,1e308);
  CHECK(fit.status==cppunivariate::ZetaWindowStatus::Ok);
  CHECK(fit.s==1e308);
  CHECK(fit.logLik==0);
  const auto concentrated=cppunivariate::zeta_window_mle({x.data(),x.size()},1,2147483647.,100.);
  CHECK(concentrated.logLik==doctest::Approx(-2*std::pow(2.,-100)).scale(0.).epsilon(1e-10));
}

TEST_CASE("zeta finite window: uniform likelihood at four million observations") {
  std::vector<int> x;x.reserve(4000000);
  for(int k=1;k<=4;++k)x.insert(x.end(),1000000,k);
  const auto fit=cppunivariate::zeta_window_mle({x.data(),x.size()},1,4);
  CHECK(fit.status==cppunivariate::ZetaWindowStatus::Ok);
  CHECK(std::abs(fit.s)<=1e-10);
  CHECK(fit.logLik==doctest::Approx(-double(x.size())*std::log(4.)).epsilon(1e-10));
}

TEST_CASE("zeta finite window: normalizer bounds across integer powers") {
  const int N=3000;const std::vector<int>x{N-1,N};
  for(double r:{7.,std::nextafter(7.,INFINITY),8.,std::nextafter(8.,INFINITY)}) {
    double den=0;for(int k=1;k<=N;++k)den+=std::pow(double(k)/N,r);
    const auto fit=cppunivariate::zeta_window_mle({x.data(),x.size()},1,N,-r);
    CHECK(fit.status==cppunivariate::ZetaWindowStatus::Ok);
    CHECK(fit.logLik==doctest::Approx(r*std::log1p(-1./N)-2*std::log(den)).epsilon(1e-10));
    CHECK(std::isfinite(cppunivariate::zeta_window_eval('q',.5,N,-r)));
  }
}

TEST_CASE("zeta finite window: rounded score roots must justify requested precision") {
  const std::vector<int> x{1,2,3,4};
  const auto tight=cppunivariate::zeta_window_mle({x.data(),x.size()},1,4,NAN,1e-20,400);
  CHECK((tight.status==cppunivariate::ZetaWindowStatus::Ok || tight.status==cppunivariate::ZetaWindowStatus::NumericalFailure));
  if(tight.status==cppunivariate::ZetaWindowStatus::Ok)CHECK(std::abs(tight.s)<=1e-20);
  const auto held=cppunivariate::zeta_window_mle({x.data(),x.size()},1,4,0,1e-20,400);
  CHECK(held.status==cppunivariate::ZetaWindowStatus::Ok);
  CHECK(held.s==0);
}
