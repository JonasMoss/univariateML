// Port of instructions/nmath/pnorm.c. Cody's near-minimax rational
// approximations for the normal CDF.

#include "nmath/dpq.hpp"
#include "nmath/nmath.hpp"
#include "nmath/rmath_macros.hpp"

#include <cfloat>
#include <cmath>

namespace mlcore {
namespace nmath {

double pnorm5(double x, double mu, double sigma, int lower_tail, int log_p) {
  double p, cp;

  if (ISNAN(x) || ISNAN(mu) || ISNAN(sigma)) return x + mu + sigma;
  if (!R_FINITE(x) && mu == x) return ML_NAN;  // x-mu is NaN
  if (sigma <= 0) {
    if (sigma < 0) ML_WARN_return_NAN;
    // sigma == 0
    return (x < mu) ? R_DT_0 : R_DT_1;
  }
  p = (x - mu) / sigma;
  if (!R_FINITE(p)) return (x < mu) ? R_DT_0 : R_DT_1;
  x = p;

  pnorm_both(x, &p, &cp, (lower_tail ? 0 : 1), log_p);
  return (lower_tail ? p : cp);
}

void pnorm_both(double x, double *cum, double *ccum, int i_tail, int log_p) {
  // i_tail in {0,1,2} == {lower, upper, both}
  static const double a[5] = {
      2.2352520354606839287, 161.02823106855587881, 1067.6894854603709582,
      18154.981253343561249, 0.065682337918207449113};
  static const double b[4] = {
      47.20258190468824187, 976.09855173777669322, 10260.932208618978205,
      45507.789335026729956};
  static const double c[9] = {
      0.39894151208813466764, 8.8831497943883759412,  93.506656132177855979,
      597.27027639480026226,  2494.5375852903726711,  6848.1904505362823326,
      11602.651437647350124,  9842.7148383839780218,  1.0765576773720192317e-8};
  static const double d[8] = {
      22.266688044328115691, 235.38790178262499861, 1519.377599407554805,
      6485.558298266760755,  18615.571640885098091, 34900.952721145977266,
      38912.003286093271411, 19685.429676859990727};
  static const double p[6] = {
      0.21589853405795699,    0.1274011611602473639,
      0.022235277870649807,   0.001421619193227893466,
      2.9112874951168792e-5,  0.02307344176494017303};
  static const double q[5] = {
      1.28426009614491121, 0.468238212480865118,    0.0659881378689285515,
      0.00378239633202758244, 7.29751555083966205e-5};

  double xden, xnum, temp, del, eps, xsq, y;
  int i, lower, upper;

  if (ISNAN(x)) {
    *cum = *ccum = x;
    return;
  }

  eps = DBL_EPSILON * 0.5;

  lower = i_tail != 1;
  upper = i_tail != 0;

  y = std::fabs(x);
  if (y <= 0.67448975) {  // qnorm(3/4) = .6744...
    if (y > eps) {
      xsq = x * x;
      xnum = a[4] * xsq;
      xden = xsq;
      for (i = 0; i < 3; ++i) {
        xnum = (xnum + a[i]) * xsq;
        xden = (xden + b[i]) * xsq;
      }
    } else {
      xnum = xden = 0.0;
    }

    temp = x * (xnum + a[3]) / (xden + b[3]);
    if (lower) *cum = 0.5 + temp;
    if (upper) *ccum = 0.5 - temp;
    if (log_p) {
      if (lower) *cum = std::log(*cum);
      if (upper) *ccum = std::log(*ccum);
    }
  } else if (y <= M_SQRT_32) {
    // pnorm for 0.674.. < |x| <= sqrt(32) ~= 5.657
    xnum = c[8] * y;
    xden = y;
    for (i = 0; i < 7; ++i) {
      xnum = (xnum + c[i]) * y;
      xden = (xden + d[i]) * y;
    }
    temp = (xnum + c[7]) / (xden + d[7]);

#define d_2(_x_) std::ldexp(_x_, -1)  // == _x_ / 2

#define do_del(X)                                                          \
  xsq = std::ldexp(std::trunc(std::ldexp(X, 4)), -4);                      \
  del = (X - xsq) * (X + xsq);                                             \
  if (log_p) {                                                             \
    *cum = (-xsq * d_2(xsq)) - d_2(del) + std::log(temp);                  \
    if ((lower && x > 0.) || (upper && x <= 0.))                           \
      *ccum = std::log1p(-std::exp(-xsq * d_2(xsq)) *                      \
                         std::exp(-d_2(del)) * temp);                      \
  } else {                                                                 \
    *cum = std::exp(-xsq * d_2(xsq)) * std::exp(-d_2(del)) * temp;         \
    *ccum = 1.0 - *cum;                                                    \
  }

#define swap_tail                                                          \
  if (x > 0.) {                                                            \
    temp = *cum;                                                           \
    if (lower) *cum = *ccum;                                               \
    *ccum = temp;                                                          \
  }

    do_del(y);
    swap_tail;
  } else if ((log_p && y < 1e170) ||
             (lower && -38.4674 < x && x < 8.2924) ||
             (upper && -8.2924 < x && x < 38.4674)) {
    // pnorm for x in (-37.5, -5.657) union (5.657, 37.5)
    xsq = 1.0 / (x * x);
    xnum = p[5] * xsq;
    xden = xsq;
    for (i = 0; i < 4; ++i) {
      xnum = (xnum + p[i]) * xsq;
      xden = (xden + q[i]) * xsq;
    }
    temp = xsq * (xnum + p[4]) / (xden + q[4]);
    temp = (M_1_SQRT_2PI - temp) / y;

    do_del(x);
    swap_tail;
  } else {
    // large |x| such that probs are 0 or 1
    if (x > 0) {
      *cum = R_D__1;
      *ccum = R_D__0;
    } else {
      *cum = R_D__0;
      *ccum = R_D__1;
    }
  }

#undef do_del
#undef swap_tail
#undef d_2
}

}  // namespace nmath
}  // namespace mlcore
