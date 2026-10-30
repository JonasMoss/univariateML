// Shared helpers for the per-distribution doctest files
// (cppunivariate/include/cppunivariate/distributions/<id>/test.cpp), which
// CMake globs into the distribution_tests executable.

#pragma once

#include "doctest.h"
#include "json_loader.hpp"

#include <string>

#ifndef CPPUNIV_DISTRIBUTIONS_DIR
#error "CPPUNIV_DISTRIBUTIONS_DIR must be defined by the build (tests/CMakeLists.txt)"
#endif

#include <cfloat>

namespace mltest {

// Relative tolerance against goldens. Closed-form/Newton-on-an-explicit-
// score fitters reproduce estimates deterministically to within a few ULP
// on any machine; kTol is the floor for those.
constexpr double kTol = 1e-10;
// With double-width intermediates the Bhattacharjee simplex stops at a
// slightly different point: relative coefficient changes were <=1.61e-8,
// and its independent score norm was 5.73e-6 (gate 1e-3). Applies under the
// proxy macro and on platforms whose long double is double (macOS arm64, MSVC).
#if defined(BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS) || LDBL_MANT_DIG <= DBL_MANT_DIG
constexpr double kBhattGoldenTol = 5e-8;
#else
constexpr double kBhattGoldenTol = 1e-12;
#endif

// Defaults of the R adapters (reltol / gradtol / abstol, iterlim). Also the
// shared default `gradtol` for every finite-difference BFGS fitter below.
constexpr double kFitTol = 1.220703e-4;
constexpr int kFitMaxit = 100;

// Golden-regression rule for finite-difference BFGS fitters (std, ged,
// sstd, sged, paralogis, lst, invparalogis): cross-machine drift
// (simbox vs local, 2026-10-29) was much smaller than the fitters' declared
// convergence tolerance (kFitTol = 1.220703e-4). std's nu differed by
// 2.5e-6 relative in the C++ check; mean relative difference in R golden
// checks was 9.4e-6. Using kFdTol = 3e-5 (~3x the largest observed drift)
// keeps golden tests as regression canaries, not noise followers. The
// fitters' declared gradtol (kFitTol) is the upper bound: comparing tighter
// than that would be meaningless. Families with their own, different default
// `gradtol` (burr, invburr: 1e-8) are not routed through this constant --
// each golden check there uses its own fitter's `tol` argument directly,
// documented at the call site.
constexpr double kFdTol = 3e-5;

inline Golden load_distribution_golden(const std::string &id) {
  return load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/" + id + "/golden.json");
}

}  // namespace mltest

#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <chrono>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <limits>
#include <vector>

#ifndef MLTEST_TRUTH_REPORT_DIR
#define MLTEST_TRUTH_REPORT_DIR ""
#endif

namespace mltest {

inline bool truth_close(double actual, double expected, double tolerance) {
  if (std::isnan(expected)) return std::isnan(actual);
  if (actual == expected) return true;
  if (!std::isfinite(actual) || !std::isfinite(expected)) return false;
  return std::abs(actual-expected) <= tolerance*std::max(1.0,std::abs(expected));
}

struct TruthFailure {
  std::size_t count = 0;
  double worst = -1;
  std::size_t point = 0;
  double at = 0, actual = 0, expected = 0;
};

// D(x, theta, log), P(x, theta, lower, log), Q(u, theta, lower, log),
// R(rng, theta). The family owns the signature adapters in its test.cpp.
// report_directory is used by the standalone audit runner, never by ctest.
template <typename D, typename P, typename Q, typename R>
inline void check_dpqr_truth(const std::string &id, D density, P probability,
                            Q quantile, R sample, double log_tolerance = 1e-10,
                            double q_tolerance = 1e-9,
                            std::size_t draws = 20000,
                            const std::string &report_directory = MLTEST_TRUTH_REPORT_DIR) {
  const std::string filename = std::string(CPPUNIV_DISTRIBUTIONS_DIR)+"/"+id+"/truth.json";
  const std::string source = detail::slurp(filename);
  const auto params = truth_matrix(source,"parameters");
  const auto values = truth_matrix(source,"values");
  const auto qs = truth_matrix(source,"quantiles");
  REQUIRE_FALSE(params.empty());
  REQUIRE_FALSE(values.empty());
  REQUIRE_FALSE(qs.empty());
  std::map<std::string, TruthFailure> failures;
  // The library contract forbids exceptions. A test-only guard records an
  // unexpected Boost exception and lets the rest of the audit run.
  const auto evaluate=[](const auto &call) {
    double value=std::numeric_limits<double>::quiet_NaN();
    CHECK_NOTHROW(value=call());
    return value;
  };
  const auto check = [&](const std::string &kind, std::size_t point, double at,
                         double actual, double expected, bool ok) {
    INFO(id << " " << kind << " point=" << point << " input=" << at
            << " ours=" << actual << " truth=" << expected);
    CHECK(ok);
    auto &f = failures[kind];
    if (!ok) {
      ++f.count;
      const bool relative=kind=="lower" || kind=="upper" || kind.compare(0,2,"q_")==0;
      const double scale=kind=="r_cdf" ?
          5*std::sqrt(expected*(1-expected)/draws)+1.0/draws :
          (relative ? std::abs(expected) : std::max(1.0,std::abs(expected)));
      const double error = std::isfinite(actual) && std::isfinite(expected) && scale>0 ?
          std::abs(actual-expected)/scale : std::numeric_limits<double>::infinity();
      if (error > f.worst) {
        f.worst=error; f.point=point; f.at=at;
        f.actual=actual; f.expected=expected;
      }
    }
  };
  for (const auto &row : values) {
    REQUIRE(row.size()==5);
    const std::size_t point = static_cast<std::size_t>(row[0]);
    REQUIRE(point<params.size());
    const auto &theta=params[point];
    const double d=evaluate([&] {return density(row[1],theta,true);});
    check("log_d",point,row[1],d,row[2],truth_close(d,row[2],log_tolerance));
    for (int tail=0;tail<2;++tail) {
      const bool lower=tail==0;
      const double expected=row[3+tail];
      const double p=evaluate([&] {return probability(row[1],theta,lower,true);});
      check(lower?"log_lower":"log_upper",point,row[1],p,expected,truth_close(p,expected,log_tolerance));
      const double natural=std::exp(expected);
      if (natural>1e-300) {
        const double got=evaluate([&] {return probability(row[1],theta,lower,false);});
        check(lower?"lower":"upper",point,row[1],got,natural,
              std::isfinite(got) && std::abs(got-natural)<=log_tolerance*natural);
      }
    }
  }
  for (const auto &row:qs) {
    REQUIRE(row.size()==9);
    const std::size_t point=static_cast<std::size_t>(row[0]);
    REQUIRE(point<params.size());
    const double got=evaluate([&] {return quantile(row[3],params[point],row[1]!=0,row[2]!=0);});
    const double expected=row[4];
    const bool relative = got==expected || (std::isfinite(got) && std::isfinite(expected) &&
        std::abs(got-expected)<=std::max(q_tolerance*std::abs(expected),row[8]));
    // The oracle supplies double brackets or two acceptable lattice points.
    const bool bracket = row[7]!=0 ? (got==row[5] || got==row[6]) :
        (got>=row[5] && got<=row[6]);
    check(std::string(row[1]!=0?"q_lower":"q_upper")+(row[2]!=0?"_log":""),
          point,row[3],got,expected,relative||bracket);
  }
  const auto start=std::chrono::steady_clock::now();
  const auto sampler_points = truth_matrix(source,"sampler_points");
  REQUIRE(sampler_points.size()==2);
  for (const auto &sampler_point : sampler_points) {
    REQUIRE(sampler_point.size()==1);
    const std::size_t point=static_cast<std::size_t>(sampler_point[0]);
    REQUIRE(point<params.size());
    cppunivariate::distributions::StdRng<> rng(20261029+point);
    std::vector<double> samples;
    samples.reserve(draws);
    CHECK_NOTHROW([&] {
      for (std::size_t i=0;i<draws;++i) samples.push_back(sample(rng,params[point]));
    }());
    const std::size_t invalid=draws-samples.size()+static_cast<std::size_t>(std::count_if(samples.begin(),samples.end(),
        [](double x) {return std::isnan(x);}));
    check("r_valid",point,0,static_cast<double>(invalid),0,invalid==0);
    samples.erase(std::remove_if(samples.begin(),samples.end(),
        [](double x) {return std::isnan(x);}),samples.end());
    std::sort(samples.begin(),samples.end());
    for (const auto &row:values) {
      if (static_cast<std::size_t>(row[0])!=point) continue;
      const double p=std::exp(row[3]);
      if (p<1e-3 || p>1-1e-3) continue;
      const double observed=static_cast<double>(std::upper_bound(samples.begin(),samples.end(),row[1])-samples.begin())/draws;
      const double tolerance=5*std::sqrt(p*(1-p)/draws)+1.0/draws;
      check("r_cdf",point,row[1],observed,p,std::abs(observed-p)<=tolerance);
    }
  }
  if (!report_directory.empty()) {
    std::ofstream out(report_directory+"/"+id+".csv");
    out << std::setprecision(17);
    for (const auto &entry:failures) {
      const auto &f=entry.second;
      out << id << ',' << entry.first << ',' << f.count << ',' << f.point << ':'
          << f.at << ',' << f.actual << ',' << f.expected << '\n';
    }
    std::ofstream timing(report_directory+"/"+id+".seconds");
    timing << std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count() << '\n';
  }
}

} // namespace mltest

namespace mltest {

// Standalone audit for APIs that currently expose only d. The owning family's
// test.cpp is left untouched until its p/q/r implementation arrives.
template <typename D>
inline void check_density_truth(const std::string &id, D density,
                               const std::string &report_directory = MLTEST_TRUTH_REPORT_DIR) {
  const std::string source=detail::slurp(std::string(CPPUNIV_DISTRIBUTIONS_DIR)+"/"+id+"/truth.json");
  const auto params=truth_matrix(source,"parameters");
  const auto values=truth_matrix(source,"values");
  REQUIRE_FALSE(params.empty());
  REQUIRE_FALSE(values.empty());
  TruthFailure failure;
  for (const auto &row:values) {
    REQUIRE(row.size()==5);
    const std::size_t point=static_cast<std::size_t>(row[0]);
    REQUIRE(point<params.size());
    double actual=std::numeric_limits<double>::quiet_NaN();
    CHECK_NOTHROW(actual=density(row[1],params[point],true));
    const bool ok=truth_close(actual,row[2],1e-10);
    INFO(id << " log_d point=" << point << " x=" << row[1]
            << " ours=" << actual << " truth=" << row[2]);
    CHECK(ok);
    if (!ok) {
      ++failure.count;
      const double error=std::isfinite(actual) && std::isfinite(row[2]) ?
          std::abs(actual-row[2])/std::max(1.0,std::abs(row[2])) :
          std::numeric_limits<double>::infinity();
      if (error>failure.worst) {
        failure.worst=error; failure.point=point; failure.at=row[1];
        failure.actual=actual; failure.expected=row[2];
      }
    }
  }
  if (!report_directory.empty()) {
    std::ofstream out(report_directory+"/"+id+".csv");
    out << std::setprecision(17) << id << ",log_d," << failure.count << ','
        << failure.point << ':' << failure.at << ',' << failure.actual << ','
        << failure.expected << '\n';
  }
}

} // namespace mltest
