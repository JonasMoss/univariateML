#include "test_support.hpp"
#include "cppunivariate/distributions/tbinom/mle.hpp"
#include <vector>
TEST_CASE("tbinom: unique mean solve and support errors") {
  const std::vector<int> x={1,2,3,4};
  auto f=cppunivariate::tbinom_mle({x.data(),x.size()},0,10,12);
  CHECK(f.status==cppunivariate::truncated_detail::Status::Ok);
  CHECK(std::isfinite(f.logLik));
  CHECK(cppunivariate::tbinom_mle({x.data(),x.size()},-INFINITY,INFINITY,1e308).status==cppunivariate::truncated_detail::Status::NumericalFailure);
  auto z=cppunivariate::tbinom_mle({x.data(),x.size()},2,10,12);
  CHECK(z.status==cppunivariate::truncated_detail::Status::InvalidInput);
  const std::vector<int> constant={1,1};
  CHECK(cppunivariate::tbinom_mle({constant.data(),constant.size()},0,10,12).status==cppunivariate::truncated_detail::Status::LowerPointMass);
}

#include "cppunivariate/distributions/tbinom/dpqr.hpp"
TEST_CASE("tbinom: density normalization and log-tail quantiles") {
  double total=0;
  for(int x=0;x<=200;++x)total+=cppunivariate::distributions::dtbinom(x, .4, 0, 10, 12);
  CHECK(total==doctest::Approx(1).epsilon(1e-12));
  for(double lp : {-1.,-10.,-100.}) {
    const double q=cppunivariate::distributions::qtbinom(lp, .4, 0, 10, 12, false, true);
    CHECK(cppunivariate::distributions::ptbinom(q, .4, 0, 10, 12, false, true)<=lp);
    CHECK(cppunivariate::distributions::ptbinom(q-1, .4, 0, 10, 12, false, true)>lp);
  }
}

TEST_CASE("tbinom: golden regression") {
  auto g=mltest::load_distribution_golden("tbinom");std::vector<int>x;
  for(double v:g.data)x.push_back(static_cast<int>(v));
  auto f=cppunivariate::tbinom_mle({x.data(),x.size()},0,10,12);
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  CHECK(f.parameter==doctest::Approx(g.estimates.at("prob")).epsilon(1e-10));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("tbinom: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("tbinom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dtbinom(x, p[0], p[1], p[2], p[3], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ptbinom(x, p[0], p[1], p[2], p[3], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qtbinom(x, p[0], p[1], p[2], p[3], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rtbinom(rng, p[0], p[1], p[2], p[3]); });
}

TEST_CASE("tbinom: free bounds and fixed-probability integer tail") {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 const std::vector<int> x{1,2,2,3,4},constant{3,3};
 const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},nan,nan,8);
 CHECK(f.a==0);CHECK(f.b==4);CHECK(f.size==8);
 CHECK(f.status==cppunivariate::TbinomStatus::Ok);
 const auto h=cppunivariate::tbinom_design_mle({x.data(),x.size()},nan,nan,nan,.4);
 REQUIRE(h.status==cppunivariate::TbinomStatus::Ok);
 for(int k=4;k<100;++k)CHECK(h.logLik>=cppunivariate::tbinom_mle({x.data(),x.size()},0,4,k,.4).logLik-1e-9);
 const auto c=cppunivariate::tbinom_design_mle({constant.data(),constant.size()},nan,nan,8);
 CHECK(c.prob==.5);CHECK(c.logLik==0);
 CHECK(cppunivariate::tbinom_design_mle({constant.data(),constant.size()},-1,3,nan,.4).status==cppunivariate::TbinomStatus::UpperPointMass);
}

#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>

// Free probability AND free size: the four previously not-implemented
// `holds:` subsets ({}, {a}, {b}, {a, b}), implemented from
// notes/evaluations/2026-10-29-p0b.md section 6 (tbinom) and
// notes/evaluations/2026-10-29-p2b-binom-report.md.

TEST_CASE("tbinom: free probability and free size, a constant sample attains likelihood one") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<int> c3{5,5,5};
  auto f=cppunivariate::tbinom_design_mle({c3.data(),c3.size()},nan,nan,nan);
  CHECK(f.status==cppunivariate::TbinomStatus::Ok);
  CHECK(f.prob==1);CHECK(f.size==5);CHECK(f.logLik==0);
  // A held lower bound that still admits size = max(x) gives the same fit.
  f=cppunivariate::tbinom_design_mle({c3.data(),c3.size()},0,nan,nan);
  CHECK(f.status==cppunivariate::TbinomStatus::Ok);
  CHECK(f.prob==1);CHECK(f.logLik==0);
  // An infeasible held lower bound (excludes the data) stops.
  CHECK(cppunivariate::tbinom_design_mle({c3.data(),c3.size()},5,nan,nan).status==cppunivariate::TbinomStatus::InvalidInput);
}

TEST_CASE("tbinom: free probability and free size attains on regular samples, all four bound subsets") {
  cppunivariate::distributions::StdRng<> rng(44);
  std::vector<int> x;
  for (int i=0;i<60;++i)
    x.push_back(static_cast<int>(cppunivariate::distributions::rtbinom(rng,.4,0,10,12)));
  const int xmin=*std::min_element(x.begin(),x.end()), xmax=*std::max_element(x.begin(),x.end());
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_a[]={nan,0,nan,0}, held_b[]={nan,nan,10,10};
  for (int i=0;i<4;++i) {
    const double a=held_a[i], b=held_b[i];
    const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},a,b,nan);
    REQUIRE(f.status==cppunivariate::TbinomStatus::Ok);
    CHECK(f.prob>0);CHECK(f.prob<1);
    CHECK(f.size>=xmax);
    // Independent oracle: the already-certified fixed-size profile over
    // prob (truncated_detail::fit via tbinom_mle), brute-forced over a
    // size range well past the attained optimum.
    const double aa=std::isnan(a)?double(xmin)-1:a, bb=std::isnan(b)?double(xmax):b;
    double best=-std::numeric_limits<double>::infinity();
    for (int k=xmax;k<=xmax+2000;++k) {
      auto r=cppunivariate::tbinom_mle({x.data(),x.size()},aa,bb,k);
      if (r.status==cppunivariate::truncated_detail::Status::Ok && r.logLik>best) best=r.logLik;
    }
    CHECK(f.logLik>=best-1e-6*x.size());
    // The fixed-size profile at the fitter's own chosen size agrees exactly.
    auto at_size=cppunivariate::tbinom_mle({x.data(),x.size()},aa,bb,f.size);
    CHECK(at_size.logLik==doctest::Approx(f.logLik).epsilon(1e-9));
  }
}

TEST_CASE("tbinom: free probability and free size escapes to the truncated Poisson face on thin samples") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<int> thin{2,3,4};
  // held {}: a, b both estimated (a = min(x) - 1 = 1, b = max(x) = 4).
  auto f=cppunivariate::tbinom_design_mle({thin.data(),thin.size()},nan,nan,nan);
  CHECK(f.status==cppunivariate::TbinomStatus::TpoisLimit);
  auto tp=cppunivariate::truncated_detail::fit({thin.data(),thin.size()},1,4,-1);
  REQUIRE(tp.status==cppunivariate::truncated_detail::Status::Ok);
  CHECK(f.logLik<=tp.logLik+1e-6);
  // A wider lower conditioning window changes existence: the same thin
  // sample has an attained finite fit, strictly better than its new face.
  f=cppunivariate::tbinom_design_mle({thin.data(),thin.size()},0,nan,nan);
  CHECK(f.status==cppunivariate::TbinomStatus::Ok);
  tp=cppunivariate::truncated_detail::fit({thin.data(),thin.size()},0,4,-1);
  REQUIRE(tp.status==cppunivariate::truncated_detail::Status::Ok);
  CHECK(f.logLik>tp.logLik+1e-3);
  CHECK(cppunivariate::tbinom_design_mle({thin.data(),thin.size()},0,5,nan).status==cppunivariate::TbinomStatus::Ok);
  // Infeasible bounds (excludes an observation) stop instead of searching.
  CHECK(cppunivariate::tbinom_design_mle({thin.data(),thin.size()},3,nan,nan).status==cppunivariate::TbinomStatus::InvalidInput);
}

TEST_CASE("tbinom: Poisson-scale profile stays accurate past the inherited cap") {
  const std::vector<int> x{1,1,2,3,5,7};
  const auto face=cppunivariate::tbinom_detail::profile({x.data(),x.size()},1,10,-1);
  REQUIRE(face.status==cppunivariate::truncated_detail::Status::Ok);
  for(double N : {1e8,1e10,1e12,1e14}) {
    const auto f=cppunivariate::tbinom_detail::profile({x.data(),x.size()},1,10,N);
    REQUIRE(f.status==cppunivariate::truncated_detail::Status::Ok);
    CHECK(f.logLik==doctest::Approx(face.logLik).epsilon(1e-7));
    CHECK(N*f.parameter==doctest::Approx(face.parameter).epsilon(1e-7));
  }
}

TEST_CASE("tbinom: two-point saturated profile is attained, not a Poisson escape") {
  const std::vector<int> x{2,2,3,3};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},1,3,nan);
  REQUIRE(f.status==cppunivariate::TbinomStatus::Ok);
  CHECK(f.size==3);
  CHECK(f.logLik==doctest::Approx(-4*std::log(2.)).epsilon(1e-12));
  const std::vector<int> huge{2000001,2000002};
  CHECK(cppunivariate::tbinom_design_mle({huge.data(),huge.size()},2000000,2000005,nan).status==cppunivariate::TbinomStatus::NumericalFailure);
}

TEST_CASE("tbinom attained point sampler ignores zero uniform") {
  // At success probability one the (possibly truncated) law is delta_size.
  struct ZeroRng { double runif(double,double) { return 0; } } rng;
  CHECK(cppunivariate::distributions::rtbinom(rng,1,-1,6,5)==5);
}
