// Zipf MLE. N is fixed at max(x); shape is found by 1D Newton on the
// score with hybrid (relative + absolute) convergence. The all-ones
// degeneracy and shape-negative cases are surfaced via the status enum.

#pragma once

#include "../../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class ZipfStatus { Ok = 0, AllOnes, ShapeNegative, IterationFailure };

struct ZipfResult {
  double N;
  double shape;
  double logLik;
  int iterations;
  ZipfStatus status;
};

inline ZipfResult zipf_mle(ispan x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, ZipfStatus::IterationFailure};

  int N = x[0];
  for (auto v : x) if (v > N) N = v;
  const double dN = static_cast<double>(N);

  std::vector<double> seq(N), log_seq(N), log_seq2(N), div_seq(N);
  for (int i = 0; i < N; ++i) {
    seq[i] = static_cast<double>(i + 1);
    log_seq[i] = std::log(seq[i]);
    log_seq2[i] = log_seq[i] * log_seq[i];
    div_seq[i] = 1.0 / seq[i];
  }

  double sum_lx = 0.0;
  for (auto v : x) sum_lx += std::log(static_cast<double>(v));
  if (sum_lx == 0.0) {
    return {dN, 1.0, 0.0, 0, ZipfStatus::AllOnes};
  }

  const double dn = static_cast<double>(n);
  auto f_over_df = [&](double shape0) {
    double sum_shape = 0.0, sum_log_hs = 0.0, sum_log2_hs = 0.0;
    for (int i = 0; i < N; ++i) {
      const double pow_i = std::pow(div_seq[i], shape0);
      sum_shape += pow_i;
      sum_log_hs += pow_i * log_seq[i];
      sum_log2_hs += pow_i * log_seq2[i];
    }
    const double top = sum_log_hs / sum_shape;
    const double bottom =
        -dn * sum_log2_hs / sum_shape + dn * top * top;
    return (dn * top - sum_lx) / bottom;
  };

  double shape = 1.0;
  int iter = 1;
  for (; iter <= maxit; ++iter) {
    const double step = f_over_df(shape);
    shape -= step;
    if (shape < 0.0 || !std::isfinite(shape)) break;
    if (std::abs(step) < (std::abs(shape) * tol + tol)) break;
  }
  if (shape < 0.0) {
    return {dN, kNaN, kNaN, iter, ZipfStatus::ShapeNegative};
  }

  double sum_pow = 0.0;
  for (int i = 0; i < N; ++i) sum_pow += std::pow(div_seq[i], shape);
  const double logLik = -shape * sum_lx - dn * std::log(sum_pow);
  return {dN, shape, logLik, iter, ZipfStatus::Ok};
}

}  // namespace cppunivariate
