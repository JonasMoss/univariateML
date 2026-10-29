// Zipf MLE. N is fixed at max(x); shape is found by 1D Newton on the
// score with hybrid (relative + absolute) convergence. The all-ones
// degeneracy and shape-negative cases are surfaced via the status enum.

#pragma once

#include "../../core/span.hpp"

#include <cmath>
#include <algorithm>
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

namespace zipf_detail {
struct HarmonicMoments { double h = 0.0, l = 0.0, l2 = 0.0; };

// Sum k^-s (log k)^j, j=0,1,2. Small supports are summed exactly.
// Large supports use Euler--Maclaurin after 1024 terms. Integrate in log(k)
// coordinates with 8-point Gauss--Legendre panels of width at most 1/2;
// the first omitted endpoint correction is O(s^5 * 1024^(-s-5)).
inline HarmonicMoments moments(int N, double s) {
  HarmonicMoments out;
  const int K = std::min(N, 1024);
  for (int k = 1; k <= K; ++k) {
    const double t = std::log(static_cast<double>(k));
    const double w = std::exp(-s*t);
    out.h += w; out.l += w*t; out.l2 += w*t*t;
  }
  if (N <= K) return out;
  const double lower = std::log(static_cast<double>(K));
  const double upper = std::log(static_cast<double>(N));
  const int panels = static_cast<int>(std::ceil(2.0*(upper-lower)));
  const double width = (upper-lower)/panels;
  const double nodes[4] = {0.1834346424956498,0.5255324099163290,0.7966664774136267,0.9602898564975363};
  const double weights[4] = {0.3626837833783620,0.3137066458778873,0.2223810344533745,0.1012285362903763};
  for (int i = 0; i < panels; ++i) {
    const double mid = lower+(i+0.5)*width;
    for (int j = 0; j < 4; ++j) for (double sign : {-1.0,1.0}) {
      const double t = mid+sign*nodes[j]*width/2.0;
      const double w = weights[j]*width/2.0*std::exp((1.0-s)*t);
      out.h += w; out.l += w*t; out.l2 += w*t*t;
    }
  }
  const auto endpoint = [&](double t, double sign) {
    const auto term = [&](int r, double c, double dc, double ddc) {
      const double w = sign*std::exp(-(s+r)*t);
      out.h += w*c;
      out.l += w*(c*t-dc);
      out.l2 += w*(c*t*t-2.0*dc*t+ddc);
    };
    term(0,0.5,0.0,0.0);
    term(1,-s/12.0,-1.0/12.0,0.0);
    term(3,s*(s+1.0)*(s+2.0)/720.0,
         (3.0*s*s+6.0*s+2.0)/720.0,(6.0*s+6.0)/720.0);
  };
  endpoint(upper,1.0); endpoint(lower,-1.0);
  return out;
}
} // namespace zipf_detail

inline ZipfResult zipf_mle(ispan x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, ZipfStatus::IterationFailure};

  int N = x[0];
  for (auto v : x) if (v > N) N = v;
  const double dN = static_cast<double>(N);

  double sum_lx = 0.0;
  for (auto v : x) sum_lx += std::log(static_cast<double>(v));
  if (sum_lx == 0.0) {
    return {dN, 1.0, 0.0, 0, ZipfStatus::AllOnes};
  }

  const double dn = static_cast<double>(n);
  // Strictly concave profile: a nonpositive score at shape zero selects
  // the attained uniform boundary. Check before doing any harmonic work.
  if (sum_lx / dn >= std::lgamma(dN+1.0) / dN)
    return {dN,0.0,-dn*std::log(dN),0,ZipfStatus::Ok};
  auto f_over_df = [&](double shape0) {
    const auto h = zipf_detail::moments(N, shape0);
    const double sum_shape = h.h, sum_log_hs = h.l, sum_log2_hs = h.l2;
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
  if (!(shape > 0.0) || !std::isfinite(shape) || iter > maxit) {
    const auto score = [&](double s) {
      const auto h = zipf_detail::moments(N,s);
      return h.l/h.h - sum_lx/dn;
    };
    double lo = 0.0, hi = 1.0;
    for (int j = 0; j < 20 && score(hi) > 0.0; ++j) hi *= 2.0;
    for (int j = 0; j < 100; ++j) {
      const double mid = (lo+hi)/2.0;
      if (score(mid)>0.0) lo=mid; else hi=mid;
    }
    shape = (lo+hi)/2.0;
  }

  const double sum_pow = zipf_detail::moments(N, shape).h;
  const double logLik = -shape * sum_lx - dn * std::log(sum_pow);
  return {dN, shape, logLik, iter, ZipfStatus::Ok};
}

}  // namespace cppunivariate
