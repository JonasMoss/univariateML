// Beta-binomial distribution: scalar d/p/q/r.
// Parameters: size in {0, 1, ...}, alpha > 0, beta > 0; support {0, ..., size}.
// The log pmf is a sum of log rising factorials in p = alpha/(alpha+beta),
// s = 1/(alpha+beta) (see mle.hpp), so huge shapes keep full accuracy.
// p and q sum the pmf over the shorter side, built by the ratio recurrence
// f(x+1)/f(x) = (N-x)/(x+1) * (alpha+x)/(beta+N-x-1). O(size) per call.

#pragma once

#include "../../special/special.hpp"
#include "../beta/dpqr.hpp"
#include "../binom/dpqr.hpp"

#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
namespace distributions {

inline bool bbinom_valid(double size, double alpha, double beta) {
  return std::isfinite(size) && size >= 0 && size == std::floor(size) && size <= 1e7 &&
         std::isfinite(alpha) && alpha > 0 && std::isfinite(beta) && beta > 0;
}

// log f(x) for integer 0 <= x <= size.
inline double bbinom_log_pmf(int x, int size, double alpha, double beta) {
  const double scale = std::max(alpha, beta);
  const double total = alpha / scale + beta / scale;
  const double p = (alpha / scale) / total, q = (beta / scale) / total;
  const double s = (1 / scale) / total;
  double ll = cppunivariate::special::lgamma(size + 1.0) -
              cppunivariate::special::lgamma(x + 1.0) -
              cppunivariate::special::lgamma(size - x + 1.0);
  if (!std::isfinite(s)) {
    // Subnormal shapes: 1/t overflows, while log rising factorials do not.
    for (int j = 0; j < x; ++j) ll += std::log(alpha + j);
    for (int j = 0; j < size - x; ++j) ll += std::log(beta + j);
    for (int j = 0; j < size; ++j) ll -= std::log(alpha + beta + j);
    return ll;
  }
  for (int j = 0; j < x; ++j) ll += std::log(p + j * s);
  for (int j = 0; j < size - x; ++j) ll += std::log(q + j * s);
  for (int j = 1; j < size; ++j) ll -= std::log1p(j * s);
  return ll;
}

// log f(0), ..., log f(size).
inline std::vector<double> bbinom_log_pmf_all(int size, double alpha, double beta) {
  std::vector<double> lf(static_cast<std::size_t>(size) + 1);
  lf[0] = bbinom_log_pmf(0, size, alpha, beta);
  for (int x = 0; x < size; ++x) {
    lf[x + 1] = lf[x] + std::log(static_cast<double>(size - x) / (x + 1.0)) +
                std::log((alpha + x) / (beta + (size - x - 1.0)));
  }
  return lf;
}

inline double bbinom_log_sum(const std::vector<double> &lf, int from, int to) {
  if (from > to) return -std::numeric_limits<double>::infinity();
  const double m = *std::max_element(lf.begin() + from, lf.begin() + to + 1);
  double acc = 0;
  for (int x = from; x <= to; ++x) acc += std::exp(lf[x] - m);
  return m + std::log(acc);
}

inline double dbbinom(double x, double size, double alpha, double beta, bool log) {
  if (!bbinom_valid(size, alpha, beta) || std::isnan(x))
    return std::numeric_limits<double>::quiet_NaN();
  if (x < 0 || x > size || x != std::floor(x))
    return log ? -std::numeric_limits<double>::infinity() : 0.0;
  const double ll = bbinom_log_pmf(static_cast<int>(x), static_cast<int>(size), alpha, beta);
  return log ? ll : std::exp(ll);
}

inline double pbbinom(double q, double size, double alpha, double beta, bool lower_tail,
                      bool log_p) {
  if (!bbinom_valid(size, alpha, beta) || std::isnan(q))
    return std::numeric_limits<double>::quiet_NaN();
  const int N = static_cast<int>(size);
  double lower_log;  // log P(X <= q)
  double upper_log;  // log P(X > q)
  if (q < 0) {
    lower_log = -std::numeric_limits<double>::infinity();
    upper_log = 0;
  } else if (q >= size) {
    lower_log = 0;
    upper_log = -std::numeric_limits<double>::infinity();
  } else {
    const int k = static_cast<int>(std::floor(q));
    const std::vector<double> lf = bbinom_log_pmf_all(N, alpha, beta);
    lower_log = bbinom_log_sum(lf, 0, k);
    upper_log = bbinom_log_sum(lf, k + 1, N);
  }
  const double l = lower_tail ? lower_log : upper_log;
  return log_p ? l : std::exp(l);
}

// Smallest x with P(X <= x) >= p (lower tail) or P(X > x) <= p (upper tail).
inline double qbbinom(double p, double size, double alpha, double beta, bool lower_tail,
                      bool log_p) {
  if (!bbinom_valid(size, alpha, beta) || std::isnan(p) ||
      (log_p ? p > 0 : (p < 0 || p > 1)))
    return std::numeric_limits<double>::quiet_NaN();
  const int N = static_cast<int>(size);
  if (p == (log_p ? -std::numeric_limits<double>::infinity() : 0.0))
    return lower_tail ? 0 : N;
  if (p == (log_p ? 0.0 : 1.0)) return lower_tail ? N : 0;
  const double target = log_p ? p : std::log(p);
  const std::vector<double> lf = bbinom_log_pmf_all(N, alpha, beta);
  int lo = -1, hi = N;
  while (hi - lo > 1) {
    const int mid = lo + (hi - lo) / 2;
    const double tail = lower_tail ? bbinom_log_sum(lf, 0, mid) : bbinom_log_sum(lf, mid + 1, N);
    if (std::isnan(tail)) return std::numeric_limits<double>::quiet_NaN();
    const bool hit = lower_tail ? tail >= target : tail <= target;
    if (hit) hi = mid; else lo = mid;
  }
  return hi;
}

template <typename Rng>
inline double rbbinom(Rng &rng, double size, double alpha, double beta) {
  return rbinom(rng, size, rbeta(rng, alpha, beta));
}

}  // namespace distributions
}  // namespace cppunivariate
