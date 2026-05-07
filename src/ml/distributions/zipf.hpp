// Zipf distribution on {1, ..., N} with shape parameter s > 0.
//
// PMF:    p(k) = k^{-s} / H_N(s),    where H_N(s) = sum_{j=1}^N j^{-s}
//
// Scalar dzipf/pzipf each cost O(N) (they normalise via H_N). For bulk
// sampling, prefer zipf_cdf + rzipf_from_cdf (build the cumulative table
// once, then sample many times in O(log N) each); rzipf is a convenience
// one-shot (O(N) per call).

#pragma once

#include "../special/constants.hpp"
#include <algorithm>
#include <cmath>
#include <vector>

namespace mlcore {
namespace distributions {

using mlcore::special::kNaN;
using mlcore::special::kNegInf;

namespace detail {
inline double zipf_norm(int N, double s) {
  double total = 0.0;
  for (int k = 1; k <= N; ++k) {
    total += std::pow(static_cast<double>(k), -s);
  }
  return total;
}
} // namespace detail

inline double dzipf(double x, double N, double s, bool log) {
  if (!(N > 0.0 && s > 0.0) || !std::isfinite(x) || N != std::floor(N)) {
    return kNaN;
  }
  if (x < 1.0 || x > N || x != std::floor(x)) {
    return log ? kNegInf : 0.0;
  }
  const int Nin = static_cast<int>(N);
  const double log_norm = std::log(detail::zipf_norm(Nin, s));
  const double y = -s * std::log(x) - log_norm;
  return log ? y : std::exp(y);
}

inline double pzipf(double q, double N, double s, bool lower_tail,
                    bool log_p) {
  if (!(N > 0.0 && s > 0.0) || !std::isfinite(q) || N != std::floor(N)) {
    return kNaN;
  }
  double prob;
  if (q < 1.0) {
    prob = lower_tail ? 0.0 : 1.0;
    return log_p ? std::log(prob) : prob;
  }
  if (q >= N) {
    prob = lower_tail ? 1.0 : 0.0;
    return log_p ? std::log(prob) : prob;
  }
  const int Nin = static_cast<int>(N);
  const int qin = static_cast<int>(std::floor(q));
  double num = 0.0, den = 0.0;
  for (int k = 1; k <= Nin; ++k) {
    const double w = std::pow(static_cast<double>(k), -s);
    den += w;
    if (k <= qin) {
      num += w;
    }
  }
  prob = num / den;
  if (!lower_tail) {
    prob = 1.0 - prob;
  }
  return log_p ? std::log(prob) : prob;
}

inline double qzipf(double p, double N, double s, bool lower_tail,
                    bool log_p) {
  if (!(N > 0.0 && s > 0.0) || N != std::floor(N) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  if (p <= 0.0) {
    return 1.0;
  }
  if (p >= 1.0) {
    return N;
  }
  const int Nin = static_cast<int>(N);
  const double den = detail::zipf_norm(Nin, s);
  double cum = 0.0;
  for (int k = 1; k <= Nin; ++k) {
    cum += std::pow(static_cast<double>(k), -s) / den;
    if (cum >= p) {
      return static_cast<double>(k);
    }
  }
  return static_cast<double>(Nin);
}

// Build the cumulative distribution table for Zipf(N, s); used by the
// efficient bulk-sampling path.
inline std::vector<double> zipf_cdf(int N, double s) {
  std::vector<double> cum(static_cast<std::size_t>(N));
  const double total = detail::zipf_norm(N, s);
  double running = 0.0;
  for (int k = 1; k <= N; ++k) {
    running += std::pow(static_cast<double>(k), -s) / total;
    cum[static_cast<std::size_t>(k - 1)] = running;
  }
  return cum;
}

// Sample using a precomputed CDF (from zipf_cdf). O(log N) per call.
template <typename Rng>
inline int rzipf_from_cdf(Rng &rng, const std::vector<double> &cum) {
  const double u = rng.runif(0.0, 1.0);
  return static_cast<int>(std::lower_bound(cum.begin(), cum.end(), u) -
                          cum.begin()) +
         1;
}

// One-shot sampler: builds the CDF and returns one sample. O(N) per call;
// for many samples prefer zipf_cdf + rzipf_from_cdf.
template <typename Rng>
inline double rzipf(Rng &rng, int N, double s) {
  const auto cum = zipf_cdf(N, s);
  return static_cast<double>(rzipf_from_cdf(rng, cum));
}

} // namespace distributions
} // namespace mlcore
