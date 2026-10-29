// Binomial MLE.
//
// If `size_or_nan` is finite, treat it as a user-supplied total-trials
// value: size is fixed and prob is the closed-form mean(x)/size; the
// caller's responsibility to validate size >= max(x).
//
// Otherwise (size_or_nan is NaN) auto-fit: compute the dispersion check
// (var/mean ratio), Newton-on-size with absolute convergence, and pick
// the best of {init, floor(opt), ceil(opt)} by log-likelihood. Status
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
  IterationFailure
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
                             int maxit) {
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

  auto f_over_df = [&](double size) {
    const double size_m_x = size - x_bar;
    double f =
        dn * (std::log(size_m_x) - std::log(size)) +
        dn * cppunivariate::special::digamma(size + 1.0);
    double df =
        dn * x_bar / (size * size_m_x) + dn * cppunivariate::special::trigamma(size + 1.0);
    for (std::size_t i = 0; i < uniq.size(); ++i) {
      const double arg = size - static_cast<double>(uniq[i]) + 1.0;
      f -= counts[i] * cppunivariate::special::digamma(arg);
      df -= counts[i] * cppunivariate::special::trigamma(arg);
    }
    return f / df;
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

  if (x_bar / var_pop <= 1.0) {
    return {kNaN, kNaN, kNaN, 0, BinomStatus::Underdispersed};
  }

  const double size0 = static_cast<double>(max_x);
  std::vector<double> size_cands{size0};
  int iterations = 0;
  if (size0 != 1.0 && f_over_df(size0) > 0.0) {
    double size = size0;
    for (int i = 0; i < maxit; ++i) {
      const double step = f_over_df(size);
      const double size_next = size - step;
      iterations = i + 1;
      if (std::abs(step) < tol || !std::isfinite(size_next)) {
        size = size_next;
        break;
      }
      size = size_next;
    }
    size_cands.push_back(std::floor(size));
    size_cands.push_back(std::ceil(size));
  }

  double best_ll = -std::numeric_limits<double>::infinity();
  double best_size = size0;
  for (auto cand : size_cands) {
    if (cand <= 0.0 || (cand - x_bar) < 0.0) continue;
    const double ll = log_likelihood(cand);
    if (ll > best_ll || !std::isfinite(best_ll)) {
      best_ll = ll;
      best_size = cand;
    }
  }
  const double prob = x_bar / best_size;
  return {best_size, prob, best_ll, iterations, BinomStatus::Ok};
}

}  // namespace cppunivariate
