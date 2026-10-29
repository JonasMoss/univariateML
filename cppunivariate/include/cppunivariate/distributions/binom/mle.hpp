// Binomial MLE.
//
// If `size_or_nan` is finite, treat it as a user-supplied total-trials
// value: size is fixed and prob is the closed-form mean(x)/size; the
// caller's responsibility to validate size >= max(x).
//
// Otherwise (size_or_nan is NaN) auto-fit with a bracketed integer profile
// search. Status
// enum surfaces SizeTooSmall, Underdispersed, IterationFailure.

#pragma once

#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <map>
#include <vector>

namespace cppunivariate {

enum class BinomStatus {
  Ok = 0,
  SizeTooSmall,
  Underdispersed,
  IterationFailure,
  InvalidInput
};

struct BinomResult {
  double size;
  double prob;
  double logLik;
  int iterations;
  BinomStatus status;
};

inline double binom_lchoose(double n, double k) {
  return cppunivariate::special::lgamma(n + 1.0) - cppunivariate::special::lgamma(k + 1.0) -
         cppunivariate::special::lgamma(n - k + 1.0);
}

inline BinomResult binom_mle(ispan x, double size_or_nan, double tol,
                             int maxit, double held_prob = std::numeric_limits<double>::quiet_NaN()) {
  (void)tol;
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, BinomStatus::IterationFailure};

  double sum_x = 0.0, sum_x_sq = 0.0;
  int max_x = x[0];
  for (auto v : x) {
    sum_x += static_cast<double>(v);
    sum_x_sq += static_cast<double>(v) * static_cast<double>(v);
    if (v > max_x) max_x = v;
  }
  const double dn = static_cast<double>(n);
  const double x_bar = sum_x / dn;
  const double var_pop = sum_x_sq / dn - x_bar * x_bar;

  if (!std::isnan(held_prob)) {
    const auto bad = [&]() { return BinomResult{kNaN,kNaN,kNaN,0,BinomStatus::InvalidInput}; };
    if (!(held_prob>=0 && held_prob<=1) || !std::isfinite(held_prob) || maxit<1 || !(tol>0)) return bad();
    for (int v:x) if (v<0) return bad();
    double size=size_or_nan;
    int iterations=0;
    if (!std::isnan(size) && (!std::isfinite(size) || size<max_x || size!=std::floor(size))) return bad();
    if (std::isnan(size)) {
      if (held_prob==0) { if (max_x>0) return bad(); size=0; }
      else if (held_prob==1) { if (var_pop!=0) return bad(); size=max_x; }
      else {
        // Adjacent log likelihood ratio decreases strictly in integer size.
        const auto delta=[&](double k) {
          double d=dn*std::log1p(-held_prob);
          for (int v:x) d-=std::log1p(-v/(k+1));
          return d;
        };
        double lo=max_x,hi=std::max(1.,lo);
        while (delta(hi)>0) {
          if (++iterations>=maxit || hi>=4503599627370496.) return BinomResult{kNaN,kNaN,kNaN,iterations,BinomStatus::IterationFailure};
          hi*=2;
        }
        while (lo<hi) {
          if (++iterations>=maxit) return BinomResult{kNaN,kNaN,kNaN,iterations,BinomStatus::IterationFailure};
          const double mid=std::floor(lo+(hi-lo)/2);
          if (delta(mid)>0) lo=mid+1; else hi=mid;
        }
        size=lo;
      }
    }
    double ll=0;
    for (int v:x) {
      if ((held_prob==0 && v!=0) || (held_prob==1 && v!=size)) return bad();
      ll+=binom_lchoose(size,v);
      if (v>0) ll+=v*std::log(held_prob);
      if (size>v) ll+=(size-v)*std::log1p(-held_prob);
    }
    return {size,held_prob,ll,iterations,BinomStatus::Ok};
  }

  // Build sorted unique values + counts (replaces arma::unique + match).
  std::map<int, int> count_map;
  for (auto v : x) ++count_map[v];
  std::vector<int> uniq;
  std::vector<int> counts;
  uniq.reserve(count_map.size());
  counts.reserve(count_map.size());
  for (const auto &kv : count_map) {
    uniq.push_back(kv.first);
    counts.push_back(kv.second);
  }

  auto log_likelihood = [&](double size) {
    // Continuous extension x*log(x) = 0 at x = 0.
    if (x_bar == 0.0 || size == x_bar) return 0.0;
    if (static_cast<double>(max_x) / size < 1e-5) {
      // Poisson-scale form of the exact binomial likelihood. Expand only
      // sum_{j=0}^{x-1} log(1-j/size), with error O(x (x/size)^4).
      double ll = 0.0;
      for (std::size_t i = 0; i < uniq.size(); ++i) {
        const double k = static_cast<double>(uniq[i]);
        const double q = (k - 1.0) / size;
        const double correction = -0.5 * k * q -
            k * q * ((2.0 * k - 1.0) / size) / 12.0 -
            k * k / size * q * q / 12.0;
        ll += counts[i] * (k * std::log(x_bar) - std::lgamma(k + 1.0) +
                          (size - k) * std::log1p(-x_bar / size) + correction);
      }
      return ll;
    }
    const double size_m_x = size - x_bar;
    double val = dn * x_bar * std::log(x_bar) +
                 dn * size_m_x * std::log(size_m_x) - dn * size * std::log(size);
    for (std::size_t i = 0; i < uniq.size(); ++i) {
      val += static_cast<double>(counts[i]) *
             binom_lchoose(size, static_cast<double>(uniq[i]));
    }
    return val;
  };

  // User-supplied size.
  if (std::isfinite(size_or_nan)) {
    if (size_or_nan < static_cast<double>(max_x)) {
      return {kNaN, kNaN, kNaN, 0, BinomStatus::SizeTooSmall};
    }
    const double prob = size_or_nan == 0.0 ? 0.0 : x_bar / size_or_nan;
    return {size_or_nan, prob, log_likelihood(size_or_nan), 0,
            BinomStatus::Ok};
  }

  // All-zero data: size is not identifiable; choose size=0, prob=0.
  if (x_bar == 0.0) return {0.0, 0.0, 0.0, 0, BinomStatus::Ok};

  if (uniq.size() == 1) return {static_cast<double>(max_x),1.0,0.0,0,BinomStatus::Ok};

  if (x_bar / var_pop <= 1.0) {
    return {kNaN, kNaN, kNaN, 0, BinomStatus::Underdispersed};
  }

  // DeRiggi's unimodality gives a unimodal integer profile. Bracket its
  // first decreasing step, then bisect those adjacent differences.
  // Use extended precision: the differences become small in the Poisson tail.
  auto increment = [&](long long k) {
    const long double z=k, m=static_cast<long double>(sum_x)/n;
    long double d=0;
    for (std::size_t i=0;i<uniq.size();++i)
      d+=static_cast<long double>(counts[i])*std::log1p(
        static_cast<long double>(uniq[i])/(z+1-uniq[i]));
    d+=static_cast<long double>(n)*(-m*std::log1p(1/z)
       +(z+1-m)*std::log1p(-m/(z+1))-(z-m)*std::log1p(-m/z));
    return d;
  };
  long long lo=max_x, hi=lo;
  int iterations=0;
  const long long limit=1000000LL;
  if (hi>limit) return {kNaN,kNaN,kNaN,0,BinomStatus::IterationFailure};
  while(increment(hi)>0) {
    if (++iterations>=maxit || hi>=limit) return {kNaN,kNaN,kNaN,iterations,BinomStatus::IterationFailure};
    lo=hi+1;
    hi=std::min(limit,hi*2);
  }
  while(lo<hi) {
    if (++iterations>=maxit) return {kNaN,kNaN,kNaN,iterations,BinomStatus::IterationFailure};
    const long long mid=lo+(hi-lo)/2;
    if(increment(mid)>0) lo=mid+1; else hi=mid;
  }
  const double best_size=static_cast<double>(lo);
  return {best_size,x_bar/best_size,log_likelihood(best_size),iterations,BinomStatus::Ok};
}

}  // namespace cppunivariate
