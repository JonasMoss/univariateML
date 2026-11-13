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

// Rounding floor for analytic comparisons outside golden tests.
constexpr double kTol = 1e-10;
// With double-width intermediates the Bhattacharjee simplex stops at a
// slightly different point: relative coefficient changes were <=1.61e-8 on
// the workstation, 2.27e-7 on simbox (GCC 13 and 15) and 4.39e-7 in the macOS
// R golden drift; its independent score norm was 5.73e-6 (gate 1e-3). As for
// the R golden table: at least 4x the largest observed drift. Applies under
// the proxy macro and on platforms whose long double is double (macOS arm64, MSVC).
#if defined(__EMSCRIPTEN__)
constexpr double kBhattGoldenTol = 5e-7;
#elif defined(BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS) || LDBL_MANT_DIG <= DBL_MANT_DIG
constexpr double kBhattGoldenTol = 2e-6;
#else
constexpr double kBhattGoldenTol = 1e-12;
#endif

// Finite-difference BFGS with gradtol 1e-8: observed arm64 golden
// coefficient drift <= 2.37e-7; score and likelihood are checked separately.
constexpr double kBurrGoldenTol = 1e-6;

// The shifted Burr double-only baseline differs by <=4.18e-7 relatively;
// independent total log-coordinate score norm is 2.02e-5 (gate 1e-3),
// and its independent likelihood differs from the golden by 1.1e-12.
// TASK-132 report and dev/validation/pareto4-precision.cpp reproduce this.
#if defined(__EMSCRIPTEN__) || defined(BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS) || LDBL_MANT_DIG <= DBL_MANT_DIG
constexpr double kPareto4GoldenTol = 5e-7;
#else
constexpr double kPareto4GoldenTol = 1e-8;
#endif

// Independent golden references: componentwise relative accuracy measured on
// native GCC, double intermediates, R and WASM. See the TASK-149.1 report and
// its retained captures. TASK-150.2.1 tightens eight families using the
// 2026-11-10-acc-newton report. These empirical allowances are not error bounds.
constexpr double kGolden_bbinom = 1e-15;
constexpr double kGolden_beta = 5e-15;
constexpr double kGolden_betapr = 1e-11;
constexpr double kGolden_binom = 2e-16;
// bnbinom: joint analytic Newton refinement; measured native/double/R/WASM reference error.
constexpr double kGolden_bnbinom = 1.5e-13;
constexpr double kGolden_burr = 2e-14;
constexpr double kGolden_cauchy = 3e-12;
constexpr double kGolden_cmb = 2e-13;
constexpr double kGolden_cmp = 1e-15;
// dgamma: analytic Newton correction; measured native/double/R/WASM reference checks.
constexpr double kGolden_dgamma = 5e-15;
constexpr double kGolden_dlaplace = 2e-16;
constexpr double kGolden_dnorm = 1e-12;
constexpr double kGolden_dunif = 3e-16;
constexpr double kGolden_dweibull = 2e-16;
// emg: analytic Newton correction; measured native/double/R/WASM reference checks.
constexpr double kGolden_emg = 5e-14;
constexpr double kGolden_exp = 5e-16;
// expweibull: local coefficient accuracy contract.
constexpr double kGolden_expweibull = 1e-10;
constexpr double kGolden_fatigue = 5e-13;
constexpr double kGolden_foldnorm = 1e-14;
constexpr double kGolden_fpareto = 1e-11;
constexpr double kGolden_frechet = 2e-14;
constexpr double kGolden_gamma = 1e-15;
// gb2: joint analytic Newton refinement; measured native/double/R/WASM reference error.
constexpr double kGolden_gb2 = 8e-14;
constexpr double kGolden_ged = 3e-11;
// genbeta: analytic Newton correction; measured native/double/R/WASM reference checks.
constexpr double kGolden_genbeta = 1e-14;
// gengamma: 50-digit reference; measured native relative coefficient error 8.5e-16.
constexpr double kGolden_gengamma = 5e-14;
constexpr double kGolden_geom = 5e-15;
constexpr double kGolden_gev = 1e-13;
constexpr double kGolden_gompertz = 5e-13;
constexpr double kGolden_gpd = 2e-15;
constexpr double kGolden_gumbel = 2e-15;
constexpr double kGolden_hcauchy = 5e-15;
constexpr double kGolden_hnorm = 2e-16;
// ht: analytic Newton correction; measured native/double/R/WASM reference checks.
constexpr double kGolden_ht = 1e-14;
constexpr double kGolden_huber = 1e-12;
constexpr double kGolden_hyper = 2e-16;
constexpr double kGolden_invburr = 1e-15;
constexpr double kGolden_invexp = 1e-15;
constexpr double kGolden_invgamma = 2e-15;
constexpr double kGolden_invgauss = 2e-15;
constexpr double kGolden_invparalogis = 3e-15;
constexpr double kGolden_invpareto = 1e-10;
constexpr double kGolden_invtrgamma = 1e-13;
constexpr double kGolden_invweibull = 1e-14;
constexpr double kGolden_kumar = 5e-16;
constexpr double kGolden_laplace = 1e-15;
constexpr double kGolden_lgamma = 2e-15;
constexpr double kGolden_lgser = 5e-16;
constexpr double kGolden_llogis = 2e-13;
constexpr double kGolden_lnorm = 1e-14;
constexpr double kGolden_logis = 5e-14;
constexpr double kGolden_logitnorm = 2e-15;
constexpr double kGolden_lomax = 2e-15;
constexpr double kGolden_lst = 2e-13;
constexpr double kGolden_naka = 2e-15;
constexpr double kGolden_nbinom = 1e-10;
constexpr double kGolden_nhyper = 2e-16;
constexpr double kGolden_norm = 2e-14;
constexpr double kGolden_nsbeta = 2e-15;
constexpr double kGolden_paralogis = 2e-15;
constexpr double kGolden_pareto = 5e-16;
constexpr double kGolden_pareto2 = 2e-15;
// pareto3: analytic Newton correction; measured native/double/R/WASM reference checks.
constexpr double kGolden_pareto3 = 1e-15;
constexpr double kGolden_pareto4 = 5e-15;
constexpr double kGolden_pig = 1e-12;
constexpr double kGolden_pois = 3e-15;
constexpr double kGolden_power = 2e-15;
constexpr double kGolden_rayleigh = 2e-16;
// sged: joint analytic Newton refinement; measured native/double/R/WASM reference error.
constexpr double kGolden_sged = 3e-14;
// sgomp: analytic Newton correction; measured native/double/R/WASM reference checks.
constexpr double kGolden_sgomp = 1e-15;
// skellam: analytic Newton correction; measured native/double/R/WASM reference checks.
constexpr double kGolden_skellam = 5e-15;
// slash: joint analytic Newton refinement; measured native/double/R/WASM reference error.
constexpr double kGolden_slash = 3e-13;
// sn: analytic Newton correction; measured native/double/R/WASM reference checks.
constexpr double kGolden_sn = 2e-14;
constexpr double kGolden_snorm = 3e-13;
// sstd: joint analytic Newton refinement; measured native/double/R/WASM reference error.
constexpr double kGolden_sstd = 4e-13;
// st: joint analytic Newton refinement; measured native/double/R/WASM reference error.
constexpr double kGolden_st = 1.2e-13;
constexpr double kGolden_std = 3e-13;
constexpr double kGolden_tbinom = 5e-15;
constexpr double kGolden_tlambda = 1e-14;
constexpr double kGolden_tnorm = 2e-14;
constexpr double kGolden_tpois = 3e-15;
constexpr double kGolden_trgamma = 3e-14;
constexpr double kGolden_triang = 1e-12;
constexpr double kGolden_unif = 1e-16;
constexpr double kGolden_weibull = 5e-15;
constexpr double kGolden_yulesimon = 5e-11;
constexpr double kGolden_zeta = 1e-11;
constexpr double kGolden_zib = 5e-15;
// zinb: analytic Newton correction; measured native/double/R/WASM reference checks.
constexpr double kGolden_zinb = 1e-14;
constexpr double kGolden_zip = 5e-15;
constexpr double kGolden_zipf = 1e-13;
constexpr double kGolden_zmbinom = 5e-16;
constexpr double kGolden_zmgeom = 2e-15;
constexpr double kGolden_zmlogarithmic = 5e-17;
// zmnbinom: analytic Newton correction; measured native/double/R/WASM reference checks.
constexpr double kGolden_zmnbinom = 1e-14;
constexpr double kGolden_zmpois = 5e-15;
constexpr double kGolden_ztbinom = 1e-15;
constexpr double kGolden_ztgeom = 1e-17;
// ztnbinom: analytic Newton correction; measured native/double/R/WASM reference checks.
constexpr double kGolden_ztnbinom = 1e-14;
constexpr double kGolden_ztpois = 5e-16;
constexpr double kGolden_bhatt = 2e-6; // Deferred to bhatt-allfree.

// Defaults of the R adapters (reltol / gradtol / abstol, iterlim). Also the
// shared default `gradtol` for every finite-difference BFGS fitter below.
constexpr double kFitTol = 1.220703e-4;
constexpr int kFitMaxit = 100;

// Legacy allowance for non-golden finite-difference comparisons.
constexpr double kFdTol = 3e-5;

inline std::string fixture_directory(const std::string& id) {
  const std::string ordinary=std::string(CPPUNIV_DISTRIBUTIONS_DIR)+"/"+id;
  std::ifstream check(ordinary+"/README.md");
  return check.good()?ordinary:std::string(CPPUNIV_MODIFIER_FIXTURES_DIR)+"/"+id;
}
inline Golden load_distribution_golden(const std::string &id) {
  return load_golden(fixture_directory(id) + "/golden.json");
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

// Check the computed CDF in the caller's selected tail and probability scale.
// Unlike an oracle tolerance, this rejects every nonminimal representable answer.
template <typename P>
inline bool truth_lattice_quantile(double actual, double p, bool lower, P probability,
                                   bool allow_infinity=false) {
  if (!std::isfinite(actual) && !(allow_infinity && actual==INFINITY)) return false;
  const double at=probability(actual);
  const double before=probability(std::nextafter(actual,-INFINITY));
  return lower ? at>=p && before<p : at<=p && before>p;
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
  const std::string filename = fixture_directory(id)+"/truth.json";
  const std::string source = detail::slurp(filename);
  const auto params = truth_matrix(source,"parameters");
  const auto values = truth_matrix(source,"values");
  const auto qs = truth_matrix(source,"quantiles");
  const std::string dossier=detail::slurp(fixture_directory(id)+"/README.md");
  const bool discrete=dossier.find("type: integer")!=std::string::npos;
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
    // A saturated endpoint probability must not rescue a wrong endpoint.
    // Continuous interior roots may instead be ill-conditioned in x.
    bool backward=false;
    if (!discrete && !relative && !bracket && std::isfinite(got) &&
        std::isfinite(expected) && (row[2]!=0 ? std::isfinite(row[3]) && row[3]<0 :
                                   row[3]>0 && row[3]<1)) {
      const double at=evaluate([&] {return probability(got,params[point],row[1]!=0,row[2]!=0);});
      const double scale=row[2]!=0 ? std::max(1.0,std::abs(row[3])) : std::abs(row[3]);
      backward=std::isfinite(at) && std::abs(at-row[3])<=1e-12*scale;
    }
    check(std::string(row[1]!=0?"q_lower":"q_upper")+(row[2]!=0?"_log":""),
          point,row[3],got,expected,discrete && std::abs(expected)>9007199254740992.0 &&
          (row[2]!=0 ? std::isfinite(row[3]) && row[3]<0 : row[3]>0 && row[3]<1)
          ? (relative||bracket) && truth_lattice_quantile(got,row[3],row[1]!=0,[&](double x) {
              return probability(x,params[point],row[1]!=0,row[2]!=0);
            },std::isinf(expected)) : relative||bracket||backward);
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
  const std::string source=detail::slurp(fixture_directory(id)+"/truth.json");
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
