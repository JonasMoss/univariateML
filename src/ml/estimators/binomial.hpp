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

#include "../core/span.hpp"
#include "../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <map>
#include <vector>

namespace mlcore {

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

// Negative log-likelihood of Binomial(size, prob) treating `size` as fixed:
//   nll(p) = -sum(x)*log(p) - sum(size - x)*log(1 - p) + const.
// Hessian (1-D in p):
//   H_pp = (n*size - sum(x)) / (1-p)^2 + sum(x) / p^2
// At MLE p_hat = mean(x)/size, sum(x) = n*size*p_hat, so
//   H_pp = n*size*(1-p_hat)/(1-p_hat)^2 + n*size*p_hat/p_hat^2
//        = n*size/(1-p_hat) + n*size/p_hat = n*size / (p_hat*(1-p_hat))
// (the closed-form Fisher info * n).
//
// `size` is *not* a regular Wald parameter when it's also estimated via
// integer search — the asymptotics for binomial-n are non-standard
// (Olkin/Petkau/Zidek 1981). Phase 4 will flag it via regular_params.
// This Cost is 1-D in p with size as a constructor field; the size-row/col
// of the inference vcov gets NaN'd by the Phase 4 regularity machinery.
struct BinomCost {
  ispan x;
  double size;  // known or fitted; treated as a constant by this Cost
  int dim() const { return 1; }
  double operator()(const double *theta) const {
    const double p = theta[0];
    if (!(p > 0.0 && p < 1.0)) return std::numeric_limits<double>::infinity();
    double S_x = 0.0;
    for (auto v : x) S_x += static_cast<double>(v);
    const double n = static_cast<double>(x.size());
    return -S_x * std::log(p) - (n * size - S_x) * std::log(1.0 - p);
  }
  void gradient(const double *theta, double *g) const {
    const double p = theta[0];
    double S_x = 0.0;
    for (auto v : x) S_x += static_cast<double>(v);
    const double n = static_cast<double>(x.size());
    g[0] = -S_x / p + (n * size - S_x) / (1.0 - p);
  }
  void hessian(const double *theta, double *H) const {
    const double p = theta[0];
    double S_x = 0.0;
    for (auto v : x) S_x += static_cast<double>(v);
    const double n = static_cast<double>(x.size());
    const double q = 1.0 - p;
    H[0] = S_x / (p * p) + (n * size - S_x) / (q * q);
  }
};

inline double binom_lchoose(double n, double k) {
  return mlcore::special::lgamma(n + 1.0) - mlcore::special::lgamma(k + 1.0) -
         mlcore::special::lgamma(n - k + 1.0);
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
        dn * mlcore::special::digamma(size + 1.0);
    double df =
        dn * x_bar / (size * size_m_x) + dn * mlcore::special::trigamma(size + 1.0);
    for (std::size_t i = 0; i < uniq.size(); ++i) {
      const double arg = size - static_cast<double>(uniq[i]) + 1.0;
      f -= counts[i] * mlcore::special::digamma(arg);
      df -= counts[i] * mlcore::special::trigamma(arg);
    }
    return f / df;
  };

  // User-supplied size.
  if (std::isfinite(size_or_nan)) {
    if (size_or_nan < static_cast<double>(max_x)) {
      return {kNaN, kNaN, kNaN, 0, BinomStatus::SizeTooSmall};
    }
    const double prob = x_bar / size_or_nan;
    return {size_or_nan, prob, log_likelihood(size_or_nan), 0,
            BinomStatus::Ok};
  }

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
    if (cand <= 0.0 || (cand - x_bar) <= 0.0) continue;
    const double ll = log_likelihood(cand);
    if (ll > best_ll || !std::isfinite(best_ll)) {
      best_ll = ll;
      best_size = cand;
    }
  }
  const double prob = x_bar / best_size;
  return {best_size, prob, best_ll, iterations, BinomStatus::Ok};
}

}  // namespace mlcore
