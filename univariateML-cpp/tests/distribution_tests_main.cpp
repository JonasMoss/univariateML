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
