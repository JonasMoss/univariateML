#include "test_support.hpp"
#include "cppunivariate/distributions/yulesimon/mle.hpp"
#include "cppunivariate/distributions/yulesimon/dpqr.hpp"
#include <vector>
namespace {
TEST_CASE("yulesimon: golden, boundary and invalid data") {
  const auto g=mltest::load_distribution_golden("yulesimon");
  std::vector<int> x;
  for (double k:g.data) x.push_back(static_cast<int>(k));
  const auto fit=cppunivariate::yulesimon_mle({x.data(),x.size()},1e-10,200);
  CHECK(fit.status==cppunivariate::YulesimonStatus::Ok);
  CHECK(fit.rho==doctest::Approx(g.estimates.at("rho")).epsilon(1e-8));
  CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  const std::vector<int> one={1,1},constant={2,2},bad={0,1};
  CHECK(cppunivariate::yulesimon_mle({one.data(),one.size()}).status==cppunivariate::YulesimonStatus::PointMass);
  CHECK(cppunivariate::yulesimon_mle({constant.data(),constant.size()}).status==cppunivariate::YulesimonStatus::Ok);
  CHECK(cppunivariate::yulesimon_mle({bad.data(),bad.size()}).status==cppunivariate::YulesimonStatus::InvalidInput);
  CHECK(cppunivariate::yulesimon_mle({constant.data(),constant.size()},1e-14,1).status==cppunivariate::YulesimonStatus::IterationFailure);
}
TEST_CASE("yulesimon: direct sums and inverse inequalities") {
  for (double shape:{.1,1.0,10.0}) {
    double total=0;
    for (int k=1;k<=1000;++k) {
      total+=cppunivariate::distributions::dyulesimon(k,shape);
      if (k==1 || k==10 || k==1000)
        CHECK(cppunivariate::distributions::pyulesimon(k+.75,shape)==doctest::Approx(total).epsilon(2e-11));
    }
    for (double u:{.001,.1,.5,.9,.999}) {
      const double q=cppunivariate::distributions::qyulesimon(u,shape);
      if (q<9e15) {
        CHECK(cppunivariate::distributions::pyulesimon(q,shape)>=u-2e-15);
        CHECK(cppunivariate::distributions::pyulesimon(q-1,shape)<u+2e-15);
      }
    }
    CHECK(std::isfinite(cppunivariate::distributions::pyulesimon(1e200,shape,false,true)));
  }
}
}
TEST_CASE("yulesimon: tiny shape and huge counts preserve gamma increments") {
  const double rho=1e-300,k=1e200;
  const double expected=rho*(std::log(k)+0.5772156649015329);
  CHECK(cppunivariate::distributions::pyulesimon(k,rho)/expected==doctest::Approx(1).epsilon(1e-12));
  CHECK(cppunivariate::distributions::pyulesimon(k,rho,false,true)/(-expected)==doctest::Approx(1).epsilon(1e-12));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("yulesimon: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("yulesimon",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dyulesimon(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pyulesimon(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qyulesimon(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::ryulesimon(rng, p[0]); });
}

TEST_CASE("yulesimon: held coefficient bypasses the all-ones escape") {
  const std::vector<int> x={1,1,1};
  const auto f=cppunivariate::yulesimon_mle({x.data(),x.size()},1e-10,200,2.3);
  CHECK(f.status==cppunivariate::YulesimonStatus::Ok);
  CHECK(f.rho==2.3);
  CHECK(std::isfinite(f.logLik));
  CHECK(f.iterations==0);
  CHECK(cppunivariate::yulesimon_mle({x.data(),x.size()},1e-10,200,0).status==cppunivariate::YulesimonStatus::InvalidInput);
}

// rho=1: P(X=k)=1/(k(k+1)), P(X>k)=1/(k+1).
TEST_CASE("yulesimon: unit shape quantiles allow adjacent answers at jumps") {
  const auto adjacent=[](double x,double lo) {return x==lo || x==lo+1;};
  CHECK(cppunivariate::distributions::qyulesimon(.83,1,true,false)==5);
  CHECK(cppunivariate::distributions::qyulesimon(.17,1,false,false)==5);

  CHECK(adjacent(cppunivariate::distributions::qyulesimon(-0.6931471805599453,1,true,true),1));
  CHECK(adjacent(cppunivariate::distributions::qyulesimon(-4.605170185988092,1,false,true),99));
  CHECK(adjacent(cppunivariate::distributions::qyulesimon(1e-6,1,false,false),999999));
  CHECK(adjacent(cppunivariate::distributions::qyulesimon(.5,1,true,false),1));
  for(double k:{1.,2.,99.,999999.,1e100}) {
    CHECK(cppunivariate::distributions::pyulesimon(k,1,false,true)==doctest::Approx(-std::log(k+1)).epsilon(1e-13));
    CHECK(cppunivariate::distributions::dyulesimon(k,1,true)==doctest::Approx(-std::log(k)-std::log(k+1)).epsilon(1e-13));
  }
}

TEST_CASE("yulesimon: structured stop payloads") {
  const std::vector<int> x{1,1};
  const auto r=cppunivariate::yulesimon_mle({x.data(),x.size()});
  REQUIRE(r.stop_detail.limit_coef.size()==1);
  CHECK(r.stop_detail.limit_model==cppunivariate::StopModel::PointMass);
  CHECK(r.stop_detail.limit_coef[0]==1);
  CHECK(r.stop_detail.limit_loglik==0);
  CHECK(r.stop_detail.candidates.size()==1);
}

#include "cppunivariate/distributions/yulesimon/dpqr.hpp"
TEST_CASE("yulesimon: quantile search guards and representability") {
  CHECK(std::isnan(cppunivariate::distributions::qyulesimon(.9,.001)));
}
