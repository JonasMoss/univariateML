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

TEST_CASE("truth checks: numerical stops are scoped to unrepresentable integer quantiles") {
  CHECK(mltest::truth_lattice_failure(NAN,1e30,.5,false,true));
  CHECK(mltest::truth_lattice_failure(NAN,INFINITY,-800,true,true));
  CHECK_FALSE(mltest::truth_lattice_failure(NAN,1e30,.5,false,false));
  CHECK_FALSE(mltest::truth_lattice_failure(NAN,10,.5,false,true));
  CHECK_FALSE(mltest::truth_lattice_failure(NAN,INFINITY,1,false,true));
  CHECK_FALSE(mltest::truth_lattice_failure(NAN,INFINITY,0,true,true));
  CHECK_FALSE(mltest::truth_lattice_failure(1e40,1e30,.5,false,true));
}
