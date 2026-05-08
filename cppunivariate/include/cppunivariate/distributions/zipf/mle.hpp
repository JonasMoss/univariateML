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

// Negative log-likelihood of Zipf(N, shape=s), with N fixed at max(x):
//   pmf: f(x; N, s) = x^{-s} / H_N(s),  H_N(s) = sum_{k=1..N} k^{-s}
//   log f = -s*log(x) - log(H_N(s))
// Hessian wrt s only (N is the integer support boundary, fitted by N_hat
// = max(x) — non-regular). Per-obs:
//   H_ss/obs = H_N''/H_N - (H_N'/H_N)^2 = Var_Zipf(log(K))
// where K is a Zipf(N, s) random variable. Always positive (it's the
// per-observation Fisher information for s).
//
// Cost is 1-D in s with N as a constructor field, mirroring BinomCost's
// treatment of `size`. Phase 4 will surface a 2x2 vcov with NaN on the
// N row/col via regular_params() = {0, 1} on the wrapper side.
struct ZipfCost {
  ispan x;
  double N;  // fixed; passed in by the wrapper
  int dim() const { return 1; }
  std::vector<int> regular_params() const { return {1}; }
  double operator()(const double *theta) const {
    const double s = theta[0];
    if (!(s > 0.0)) return std::numeric_limits<double>::infinity();
    double H = 0.0;
    const int Nint = static_cast<int>(N);
    for (int k = 1; k <= Nint; ++k) H += std::pow(static_cast<double>(k), -s);
    double S_lx = 0.0;
    for (auto v : x) S_lx += std::log(static_cast<double>(v));
    const double n = static_cast<double>(x.size());
    return s * S_lx + n * std::log(H);
  }
  void gradient(const double *theta, double *g) const {
    const double s = theta[0];
    double H = 0.0, Hp = 0.0;
    const int Nint = static_cast<int>(N);
    for (int k = 1; k <= Nint; ++k) {
      const double kd = static_cast<double>(k);
      const double kns = std::pow(kd, -s);
      H += kns;
      Hp += -kns * std::log(kd);
    }
    double S_lx = 0.0;
    for (auto v : x) S_lx += std::log(static_cast<double>(v));
    const double n = static_cast<double>(x.size());
    g[0] = S_lx + n * Hp / H;
  }
  void hessian(const double *theta, double *H_out) const {
    const double s = theta[0];
    double H = 0.0, Hp = 0.0, Hpp = 0.0;
    const int Nint = static_cast<int>(N);
    for (int k = 1; k <= Nint; ++k) {
      const double kd = static_cast<double>(k);
      const double kns = std::pow(kd, -s);
      const double lk = std::log(kd);
      H += kns;
      Hp += -kns * lk;
      Hpp += kns * lk * lk;
    }
    const double Hp_over_H = Hp / H;
    const double n = static_cast<double>(x.size());
    H_out[0] = n * (Hpp / H - Hp_over_H * Hp_over_H);
  }
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
