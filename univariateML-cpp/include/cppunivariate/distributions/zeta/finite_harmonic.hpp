// Zipf MLE. N is fixed at max(x); shape is found by 1D Newton on the
// score with hybrid (relative + absolute) convergence. The all-ones
// degeneracy and shape-negative cases are surfaced via the status enum.

#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/discrete_stop.hpp"

#include "../../core/span.hpp"

#include <cmath>
#include <algorithm>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

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

} // namespace cppunivariate
