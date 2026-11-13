#include "test_support.hpp"
#include "cppunivariate/distributions/pois/tpois_mle.hpp"
#include <vector>
TEST_CASE("tpois: unique mean solve and support errors") {
  const std::vector<int> x={1,2,3,4};
  auto f=cppunivariate::tpois_mle({x.data(),x.size()},0,10);
  CHECK(f.status==cppunivariate::truncated_detail::Status::Ok);
  CHECK(std::isfinite(f.logLik));
  CHECK(cppunivariate::tpois_mle({x.data(),x.size()},-INFINITY,INFINITY,1e12).status==cppunivariate::truncated_detail::Status::NumericalFailure);
  auto z=cppunivariate::tpois_mle({x.data(),x.size()},2,10);
  CHECK(z.status==cppunivariate::truncated_detail::Status::InvalidInput);
  const std::vector<int> constant={1,1};
  CHECK(cppunivariate::tpois_mle({constant.data(),constant.size()},0,10).status==cppunivariate::truncated_detail::Status::LowerPointMass);
}

#include "cppunivariate/distributions/pois/tpois_dpqr.hpp"
TEST_CASE("tpois: density normalization and log-tail quantiles") {
  CHECK(cppunivariate::distributions::dtpois(INFINITY,3,0,INFINITY)==0);
  CHECK(cppunivariate::distributions::qtpois(1,0,-INFINITY,INFINITY)==0);
  double total=0;
  for(int x=0;x<=200;++x)total+=cppunivariate::distributions::dtpois(x,3,0,10);
  CHECK(total==doctest::Approx(1).epsilon(1e-12));
  for(double lp : {-1.,-10.,-100.}) {
    const double q=cppunivariate::distributions::qtpois(lp,3,0,10,false,true);
    CHECK(cppunivariate::distributions::ptpois(q,3,0,10,false,true)<=lp);
    CHECK(cppunivariate::distributions::ptpois(q-1,3,0,10,false,true)>lp);
  }
}

TEST_CASE("tpois: golden regression") {
  auto g=mltest::load_distribution_golden("tpois");std::vector<int>x;
  for(double v:g.data)x.push_back(static_cast<int>(v));
  auto f=cppunivariate::tpois_mle({x.data(),x.size()},0,10);
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_tpois).scale(0));
  CHECK(f.parameter==doctest::Approx(g.estimates.at("lambda")).epsilon(mltest::kGolden_tpois).scale(0));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("tpois: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("tpois",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dtpois(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ptpois(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qtpois(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rtpois(rng, p[0], p[1], p[2]); });
}

TEST_CASE("tpois: all hold subsets, singleton representatives and escapes") {
  const std::vector<int> x={1,1,2,4},constant={3,3},zeros={0,0};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double a:{nan,-1.})for(double b:{nan,10.})for(double l:{nan,2.3}) {
    const auto f=cppunivariate::tpois_mle({x.data(),x.size()},a,b,l);
    CHECK(f.status==cppunivariate::truncated_detail::Status::Ok);
    if(!std::isnan(l))CHECK(f.parameter==l);
    double ll=0;for(int v:x)ll+=cppunivariate::distributions::dtpois(v,f.parameter,std::isnan(a)?0:a,std::isnan(b)?4:b,true);
    CHECK(f.logLik==doctest::Approx(ll).epsilon(1e-10));
  }
  const auto single=cppunivariate::tpois_mle({constant.data(),constant.size()},nan,nan);
  CHECK(single.status==cppunivariate::truncated_detail::Status::Ok);
  CHECK(single.parameter==1);
  CHECK(single.logLik==0);
  CHECK(cppunivariate::tpois_mle({constant.data(),constant.size()},nan,10).status==cppunivariate::truncated_detail::Status::LowerPointMass);
  CHECK(cppunivariate::tpois_mle({constant.data(),constant.size()},-1,nan).status==cppunivariate::truncated_detail::Status::UpperPointMass);
  CHECK(cppunivariate::tpois_mle({constant.data(),constant.size()},nan,nan,0).status==cppunivariate::truncated_detail::Status::InvalidInput);
  CHECK(cppunivariate::tpois_mle({zeros.data(),zeros.size()},nan,nan,0).status==cppunivariate::truncated_detail::Status::Ok);
}

TEST_CASE("tpois: structured stop payloads") {
  const std::vector<int> x{1,1};
  const auto r=cppunivariate::tpois_mle({x.data(),x.size()},0,10);
  REQUIRE(r.stop_detail.limit_model==cppunivariate::StopModel::PointMass);
  CHECK(r.stop_detail.limit_coef[0]==1);
  const auto f=cppunivariate::tpois_mle({x.data(),x.size()},-1,1);
  REQUIRE(f.stop_detail.limit_model==cppunivariate::StopModel::PointMass);
  CHECK(f.stop_detail.limit_loglik==0);
}

#include "cppunivariate/distributions/pois/tpois_dpqr.hpp"
TEST_CASE("tpois: quantile search guards and representability") {
  CHECK(std::isnan(cppunivariate::distributions::qtpois(.5,1e308,-INFINITY,INFINITY)));
}

TEST_CASE("tpois: conditional normalizer retains its mode anchor") {
 CHECK(cppunivariate::distributions::dtpois(1000004,926275.8860827014,999999,1000011,true)==doctest::Approx(-2.4047244421197274).epsilon(1e-10));
}

TEST_CASE("tpois: estimated large-count bounds logLik uses relative conditional masses") {
  const std::vector<int> x{1000000,1000001,1000003,1000008,1000011};
  const auto r=cppunivariate::tpois_mle({x.data(),x.size()},NAN,NAN);
  REQUIRE(r.status==cppunivariate::truncated_detail::Status::Ok);
  double ll=0;
  for(int v:x)ll+=cppunivariate::distributions::dtpois(v,r.parameter,999999,1000011,true);
  CHECK(std::abs(r.logLik-ll)<=1e-10*std::max(1.,std::abs(ll)));
  CHECK(r.logLik==doctest::Approx(-12.253430677034801).epsilon(1e-12));
}
