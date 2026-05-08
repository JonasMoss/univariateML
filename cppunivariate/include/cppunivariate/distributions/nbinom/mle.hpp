// Negative Binomial MLE. Newton on size (1D, hybrid-step convergence)
// using the unique-counts trick to avoid recomputing digamma at duplicate
// observations. Underdispersion (var < mean) flags via the result struct;
// caller routes to mlpois.

#pragma once

#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <map>
#include <vector>

namespace cppunivariate {

struct NbinomResult {
  double size;
  double prob;
  double logLik;
  int iterations;
  bool underdispersed;
};

// Negative log-likelihood of NegativeBinomial(size=r, prob=p):
//   nll(r, p) = -sum log f, where
//   log f = lgamma(x+r) - lgamma(r) - lgamma(x+1) + r log(p) + x log(1-p).
// Per-obs Hessian (sympy):
//   H_rr/obs = trigamma(r) - trigamma(r+x)
//   H_pp/obs = r/p^2 + x/(1-p)^2
//   H_rp/obs = -1/p
struct NbinomCost {
  ispan x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double r = theta[0];
    const double p = theta[1];
    if (!(r > 0.0 && p > 0.0 && p < 1.0))
      return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double sum = 0.0;
    for (auto v : x) {
      const double xd = static_cast<double>(v);
      sum += -cppunivariate::special::lgamma(xd + r) +
             cppunivariate::special::lgamma(r) +
             cppunivariate::special::lgamma(xd + 1.0) - r * std::log(p) -
             xd * std::log(1.0 - p);
    }
    return sum;
    (void)n;
  }
  void gradient(const double *theta, double *g) const {
    const double r = theta[0];
    const double p = theta[1];
    const double n = static_cast<double>(x.size());
    double S_x = 0.0, S_dig_xr = 0.0;
    for (auto v : x) {
      const double xd = static_cast<double>(v);
      S_x += xd;
      S_dig_xr += cppunivariate::special::digamma(xd + r);
    }
    g[0] = -S_dig_xr + n * cppunivariate::special::digamma(r) - n * std::log(p);
    g[1] = -n * r / p + S_x / (1.0 - p);
  }
  void hessian(const double *theta, double *H) const {
    const double r = theta[0];
    const double p = theta[1];
    const double n = static_cast<double>(x.size());
    double S_x = 0.0, S_trg_xr = 0.0;
    for (auto v : x) {
      const double xd = static_cast<double>(v);
      S_x += xd;
      S_trg_xr += cppunivariate::special::trigamma(xd + r);
    }
    H[0] = n * cppunivariate::special::trigamma(r) - S_trg_xr;
    H[1] = -n / p;
    H[2] = -n / p;
    H[3] = n * r / (p * p) + S_x / ((1.0 - p) * (1.0 - p));
  }
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
  if (x_bar == 0.0) return {1.0, 0.5, 0.0, 0, false};

  const double var_hat = sum_x_sq / dn - x_bar * x_bar;
  if (var_hat < x_bar) {
    return {kNaN, kNaN, kNaN, 0, true};
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

  double size = (x_bar * x_bar) / (var_hat - x_bar);
  int iter = 1;
  for (; iter <= maxit; ++iter) {
    const double step = f_over_df(size);
    size -= step;
    if (size <= 0.0 || !std::isfinite(size)) {
      return {kNaN, kNaN, kNaN, iter, false};
    }
    if (std::abs(step) < (std::abs(size) * tol + tol)) break;
  }
  const double prob = size / (x_bar + size);
  return {size, prob, log_likelihood(size), iter, false};
}

}  // namespace cppunivariate
