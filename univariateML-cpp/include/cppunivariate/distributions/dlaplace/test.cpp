#include "test_support.hpp"
#include "cppunivariate/distributions/dlaplace/mle.hpp"
#include <vector>
TEST_CASE("dlaplace: closed form and fixed subsets") {
  std::vector<int>x={-2,0,1,4};
  auto f=cppunivariate::dlaplace_mle({x.data(),x.size()});
  CHECK(f.status==cppunivariate::DlaplaceStatus::Ok);
  CHECK(f.location==0);CHECK(f.not_unique);
  CHECK(f.scale==doctest::Approx(1.75/(std::hypot(1.,1.75)+1)));
  CHECK(cppunivariate::dlaplace_mle({x.data(),x.size()},.5).status==cppunivariate::DlaplaceStatus::InvalidInput);
  std::vector<int>z={2,2};
  CHECK(cppunivariate::dlaplace_mle({z.data(),z.size()}).status==cppunivariate::DlaplaceStatus::PointMass);
  CHECK(cppunivariate::dlaplace_mle({z.data(),z.size()},0).status==cppunivariate::DlaplaceStatus::Ok);
}

#include "cppunivariate/distributions/dlaplace/dpqr.hpp"
TEST_CASE("dlaplace: density normalization and log-tail quantiles") {
  CHECK(std::isnan(cppunivariate::distributions::ddlaplace(std::numeric_limits<double>::quiet_NaN(),0,.4)));
  double total=0;
  for(int x=-200;x<=200;++x)total+=cppunivariate::distributions::ddlaplace(x,0,.4);
  CHECK(total==doctest::Approx(1).epsilon(1e-12));
  for(double lp : {-1.,-10.,-100.}) {
    const double q=cppunivariate::distributions::qdlaplace(lp,0,.4,false,true);
    CHECK(cppunivariate::distributions::pdlaplace(q,0,.4,false,true)<=lp);
    CHECK(cppunivariate::distributions::pdlaplace(q-1,0,.4,false,true)>lp);
  }
}

TEST_CASE("dlaplace: golden regression") {
  auto g=mltest::load_distribution_golden("dlaplace");std::vector<int>x;
  for(double v:g.data)x.push_back(static_cast<int>(v));
  auto f=cppunivariate::dlaplace_mle({x.data(),x.size()});
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_dlaplace).scale(0));
  CHECK(f.scale==doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_dlaplace).scale(0));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("dlaplace: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("dlaplace",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::ddlaplace(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pdlaplace(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qdlaplace(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rdlaplace(rng, p[0], p[1]); });
}

TEST_CASE("dlaplace: structured stop payloads") {
  const std::vector<int> x{1,1};
  const auto r=cppunivariate::dlaplace_mle({x.data(),x.size()});
  REQUIRE(r.stop_detail.limit_coef.size()==1);
  CHECK(r.stop_detail.limit_model==cppunivariate::StopModel::PointMass);
  CHECK(r.stop_detail.limit_coef[0]==1);
  CHECK(r.stop_detail.limit_loglik==0);
  CHECK(r.stop_detail.candidates.size()==1);
}

#include "cppunivariate/distributions/dlaplace/dpqr.hpp"
TEST_CASE("dlaplace: quantile search guards and representability") {
  CHECK(mltest::truth_lattice_quantile(cppunivariate::distributions::qdlaplace(.25,1e308,.5),.25,true,
      [](double x) {return cppunivariate::distributions::pdlaplace(x,1e308,.5,true,false);}));
  CHECK(cppunivariate::distributions::qdlaplace(.5,0,.5)==0);
}

TEST_CASE("dlaplace: large geometric tails use the supplied probability scale") {
  for (bool lower : {false,true}) for (bool log : {false,true}) {
    const double u=lower ? .999 : .001;
    const double p=log ? std::log(u) : u;
    const double h=cppunivariate::distributions::qdlaplace(p,0,std::nextafter(1.0,0.0),lower,log);
    REQUIRE(std::isfinite(h));
    REQUIRE(h>9007199254740992.0);
    CHECK(mltest::truth_lattice_quantile(h,p,lower,[&](double x) {
      return cppunivariate::distributions::pdlaplace(x,0,std::nextafter(1.0,0.0),lower,log);
    }));
  }
}

TEST_CASE("dlaplace: overflowing distances retain finite geometric log tails") {
  const double scale=std::nextafter(1.0,0.0);
  // Independent 100-digit geometric formula; see pdnorm-fix-data/check.py.
  const double expected=-2.2204460492503132285e292;
  for (double sign : {-1.0,1.0}) {
    const double x=sign*1e308, location=-x;
    CHECK(cppunivariate::distributions::ddlaplace(x,location,scale,true)
          ==doctest::Approx(expected).epsilon(3e-15));
    CHECK(cppunivariate::distributions::pdlaplace(x,location,scale,sign<0,true)
          ==doctest::Approx(expected).epsilon(3e-15));
    CHECK(cppunivariate::distributions::pdlaplace(x,location,scale,sign>0,true)==0);
    CHECK(cppunivariate::distributions::pdlaplace(x,location,scale,sign<0,false)==0);
    CHECK(cppunivariate::distributions::ddlaplace(x,location,scale,false)==0);
  }
}
