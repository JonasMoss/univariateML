// Negative Binomial MLE. Newton on size (1D, hybrid-step convergence)
// using the unique-counts trick to avoid recomputing digamma at duplicate
// observations. A finite MLE exists iff the population-form variance
// exceeds the mean (Aragon, Eberly and Eberly 1992); otherwise, including
// equality, the profile likelihood increases to the Poisson limit and the
// result is flagged Underdispersed (caller routes to mlpois). When Newton
// leaves the domain or does not converge, the unique root of the profile
// score is bracketed on a log scale and bisected.

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

enum class NbinomStatus { Ok, Underdispersed };

struct NbinomResult {
  double size;
  double prob;
  double logLik;
  int iterations;
  bool underdispersed;
  NbinomStatus status = NbinomStatus::Ok;
};

inline NbinomResult nbinom_mle(ispan x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, false};

  double sum_x = 0.0, sum_x_sq = 0.0;
  for (auto v : x) {
    sum_x += static_cast<double>(v);
    sum_x_sq += static_cast<double>(v) * static_cast<double>(v);
  }
  const double dn = static_cast<double>(n);
  const double x_bar = sum_x / dn;
  if (x_bar == 0.0) return {1.0, 1.0, 0.0, 0, false};

  const double var_hat = sum_x_sq / dn - x_bar * x_bar;
  // At var_hat == x_bar the profile score is negative for all finite sizes
  // too: the supremum is the Poisson limit, not a finite size.
  if (var_hat <= x_bar) {
    return {kNaN, kNaN, kNaN, 0, true, NbinomStatus::Underdispersed};
  }

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
    const double prob = size / (x_bar + size);
    double ll = 0.0;
    for (std::size_t i = 0; i < uniq.size(); ++i) {
      const double xi = static_cast<double>(uniq[i]);
      ll += counts[i] * (cppunivariate::special::lgamma(xi + size) -
                         cppunivariate::special::lgamma(xi + 1.0));
    }
    ll -= dn * cppunivariate::special::lgamma(size);
    ll += dn * size * std::log(prob);
    ll += dn * std::log(1.0 - prob) * x_bar;
    return ll;
  };

  auto f_over_df = [&](double size) {
    const double prob = size / (x_bar + size);
    const double dprob = x_bar / (size * size + x_bar * size);
    double f = 0.0, df = 0.0;
    for (std::size_t i = 0; i < uniq.size(); ++i) {
      const double arg = size + static_cast<double>(uniq[i]);
      f += counts[i] * cppunivariate::special::digamma(arg);
      df += counts[i] * cppunivariate::special::trigamma(arg);
    }
    f -= dn * cppunivariate::special::digamma(size);
    df -= dn * cppunivariate::special::trigamma(size);
    f += dn * std::log(prob);
    df += dn * dprob;
    return f / df;
  };

  // Profile score in size (prob at its maximiser size / (x_bar + size)):
  // positive below the MLE and negative above it.
  auto score = [&](double size) {
    double f = 0.0;
    for (std::size_t i = 0; i < uniq.size(); ++i) {
      f += counts[i] * cppunivariate::special::digamma(size + static_cast<double>(uniq[i]));
    }
    return f - dn * cppunivariate::special::digamma(size) - dn * std::log1p(x_bar / size);
  };

  const double size0 = (x_bar * x_bar) / (var_hat - x_bar);
  double size = size0;
  int iter = 1;
  bool newton_ok = std::isfinite(size) && size > 0.0;
  for (; newton_ok && iter <= maxit; ++iter) {
    const double step = f_over_df(size);
    size -= step;
    if (size <= 0.0 || !std::isfinite(size)) {
      newton_ok = false;
      break;
    }
    if (std::abs(step) < (std::abs(size) * tol + tol)) break;
  }
  if (newton_ok && iter <= maxit) {
    const double prob = size / (x_bar + size);
    return {size, prob, log_likelihood(size), iter, false};
  }

  // Fallback: bracket the root in log(size) and bisect. Beyond kSizeMax the
  // fitted law equals the Poisson to double precision; a score that is still
  // nonnegative there means the variance excess is below rounding level.
  constexpr double kSizeMax = 1e15;
  double lo = (std::isfinite(size0) && size0 > 0.0) ? std::min(size0, kSizeMax) : 1.0;
  double hi = lo;
  int evals = 0;
  while (score(lo) <= 0.0 && lo > 1e-300) {
    lo *= 0.5;
    ++evals;
  }
  while (score(hi) >= 0.0) {
    if (hi >= kSizeMax * std::max(1.0, x_bar)) {
      return {kNaN, kNaN, kNaN, iter + evals, true, NbinomStatus::Underdispersed};
    }
    hi *= 2.0;
    ++evals;
  }
  if (!(score(lo) > 0.0)) return {kNaN, kNaN, kNaN, iter + evals, false};
  for (int k = 0; k < 200 && hi / lo - 1.0 > 4.0 * std::numeric_limits<double>::epsilon(); ++k) {
    const double mid = std::sqrt(lo) * std::sqrt(hi);
    if (score(mid) > 0.0) lo = mid; else hi = mid;
    ++evals;
  }
  size = std::sqrt(lo) * std::sqrt(hi);
  const double prob = size / (x_bar + size);
  return {size, prob, log_likelihood(size), iter + evals, false};
}

}  // namespace cppunivariate
