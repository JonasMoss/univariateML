// doctest main for the per-distribution test.cpp files.
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include "test_support.hpp"

TEST_CASE("truth table loader: nonfinite and subnormal cells") {
  const auto rows=mltest::truth_matrix(
      R"({"cells":[["Inf","-Inf","NaN",4.9406564584124654e-324],[-0.0,1e300]]})",
      "cells");
  REQUIRE(rows.size()==2);
  REQUIRE(rows[0].size()==4);
  CHECK(rows[0][0]==std::numeric_limits<double>::infinity());
  CHECK(rows[0][1]==-std::numeric_limits<double>::infinity());
  CHECK(std::isnan(rows[0][2]));
  CHECK(rows[0][3]==std::numeric_limits<double>::denorm_min());
  CHECK(std::signbit(rows[1][0]));
  CHECK(rows[1][1]==1e300);
  CHECK(mltest::truth_matrix(R"({"cells":[[invalid]]})","cells").empty());
}

TEST_CASE("truth table loader accepts range errors and rejects trailing junk") {
  const auto rows = mltest::truth_matrix(
      R"({"cells":[[1e-400,-1e-400,1e400,-1e400]]})", "cells");
  REQUIRE(rows.size() == 1);
  REQUIRE(rows[0].size() == 4);
  CHECK(rows[0][0] == 0.0);
  CHECK(std::signbit(rows[0][1]));
  CHECK(rows[0][2] == std::numeric_limits<double>::infinity());
  CHECK(rows[0][3] == -std::numeric_limits<double>::infinity());
  CHECK(mltest::truth_matrix(R"({"cells":[[1abc]]})", "cells").empty());
  CHECK(mltest::truth_matrix("{\"cells\":[[\"1", "cells").empty());
}

TEST_CASE("truth checks require the minimal finite representable quantile") {
  const double h=1e30;
  const auto cdf=[&](double x) {return x<h ? .25 : .75;};
  CHECK(mltest::truth_lattice_quantile(h,.5,true,cdf));
  CHECK_FALSE(mltest::truth_lattice_quantile(NAN,.5,true,cdf));
  CHECK_FALSE(mltest::truth_lattice_quantile(INFINITY,.5,true,cdf));
  CHECK_FALSE(mltest::truth_lattice_quantile(std::nextafter(h,INFINITY),.5,true,cdf));
  CHECK_FALSE(mltest::truth_lattice_quantile(std::nextafter(h,-INFINITY),.5,true,cdf));
  CHECK(mltest::truth_lattice_quantile(h,.5,false,[&](double x) {return 1-cdf(x);}));
}

#include "cppunivariate/core/likelihood_polish.hpp"
TEST_CASE("polish method floor survives public recording and excludes evaluation breakdown") {
  std::array<double,3> q{0.,0.,0.};
  const std::array<bool,3> free{true,false,false};
  cppunivariate::StopDetail detail;
  const auto objective=[](const std::array<cppunivariate::accuracy_detail::Jet,3>& z) {
    return -z[0]*z[0];
  };
  CHECK_FALSE(cppunivariate::accuracy_detail::polish(q,free,5e-16,100,objective,detail));
  cppunivariate::record_accuracy(detail,5e-16,{1.,1000000.},0.,{1.,0.});
  CHECK(detail.accuracy_stop==cppunivariate::AccuracyStop::Tolerance);
  CHECK(detail.attainable_tol==4*std::numeric_limits<double>::epsilon());
  detail.accuracy_stop=cppunivariate::AccuracyStop::Tolerance;detail.attainable_tol=1e-8;
  cppunivariate::record_accuracy(detail,1e-10,{1.},0.,{1.});
  CHECK(detail.accuracy_stop==cppunivariate::AccuracyStop::Tolerance);
  CHECK(detail.attainable_tol==1e-8);
  const auto invalid=[](const std::array<cppunivariate::accuracy_detail::Jet,3>&) {
    return cppunivariate::accuracy_detail::Jet(NAN);
  };
  CHECK_FALSE(cppunivariate::accuracy_detail::polish(q,free,5e-16,100,invalid,detail));
  CHECK(detail.accuracy_stop==cppunivariate::AccuracyStop::None);
}

TEST_CASE("discrete quantile fuzz: scales, endpoints and bands wider than atoms") {
  const double e=64*std::numeric_limits<double>::epsilon();
  CHECK(cppunivariate::special::discrete_reached(.5,.5,true,false)==1);
  CHECK(cppunivariate::special::discrete_reached(.5,.5+e/4,true,false)==1);
  CHECK(cppunivariate::special::discrete_reached(.5,.5-e/4,false,false)==1);
  CHECK(cppunivariate::special::discrete_reached(.5,.5+e,true,false)==0);
  CHECK(cppunivariate::special::discrete_reached(-1,-1,true,true)==1);
  CHECK(cppunivariate::special::discrete_reached(-1,-1,false,true)==1);
  CHECK(cppunivariate::special::discrete_reached(-1,-1+e/2,true,true)==1);
  CHECK(cppunivariate::special::discrete_reached(-1,-1-e/2,false,true)==1);
  const double tiny=std::numeric_limits<double>::denorm_min();
  CHECK(cppunivariate::special::discrete_reached(0,tiny,true,false)==0);
  CHECK(cppunivariate::special::discrete_reached(tiny,tiny,true,false)==1);
  const double almost_one=std::nextafter(1.,0.);
  CHECK(cppunivariate::special::discrete_reached(1,almost_one,false,false)==0);
  CHECK(cppunivariate::special::discrete_reached(-std::numeric_limits<double>::max(),-std::numeric_limits<double>::max(),true,true)==1);
  CHECK(cppunivariate::special::discrete_reached(NAN,.5,true,false)==-1);
  CHECK(cppunivariate::special::discrete_reached(.5,NAN,true,false)==-1);
}

TEST_CASE("discrete quantile band may cross more than one atom") {
  const double eps=std::numeric_limits<double>::epsilon();
  const double target=.5+100*eps;
  const double q=cppunivariate::special::discrete_search(0,200,[&](double k) {
    return cppunivariate::special::discrete_reached(.5+k*eps,target,true,false);
  });
  CHECK(q==68);
  CHECK(cppunivariate::special::discrete_reached(-10000,-10000+1e-11,true,true)==1);
}

TEST_CASE("discrete quantile preserves failure when either computed tail is NaN") {
  const auto target=cppunivariate::special::discrete_log_tails(.5,true,false);
  CHECK(std::isnan(cppunivariate::special::discrete_quantile(target,0,10,[](double) {
    return cppunivariate::special::LogTails{std::log(.75),NAN,true};
  },.5,true,false)));
}
