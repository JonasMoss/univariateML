// Port of instructions/nmath/qnorm.c. Wichura AS 241 with
// Mächler 2022 extension for the very-extreme tail (log_p = TRUE).

#include "nmath/dpq.hpp"
#include "nmath/nmath.hpp"
#include "nmath/rmath_macros.hpp"

#include <cmath>

namespace mlcore {
namespace nmath {

double qnorm5(double p, double mu, double sigma, int lower_tail, int log_p) {
  double p_, q, r, val;

  if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma)) return p + mu + sigma;
  R_Q_P01_boundaries(p, ML_NEGINF, ML_POSINF);

  if (sigma < 0) ML_WARN_return_NAN;
  if (sigma == 0) return mu;

  p_ = R_DT_qIv(p);  // real lower_tail prob. p
  q = p_ - 0.5;

  if (std::fabs(q) <= .425) {
    // |p~ - 0.5| <= .425
    r = .180625 - q * q;
    val = q *
          (((((((r * 2509.0809287301226727 + 33430.575583588128105) * r +
                67265.770927008700853) *
                   r +
               45921.953931549871457) *
                  r +
              13731.693765509461125) *
                 r +
             1971.5909503065514427) *
                r +
            133.14166789178437745) *
               r +
           3.387132872796366608) /
          (((((((r * 5226.495278852854561 + 28729.085735721942674) * r +
                39307.89580009271061) *
                   r +
               21213.794301586595867) *
                  r +
              5394.1960214247511077) *
                 r +
             687.1870074920579083) *
                r +
            42.313330701600911252) *
               r +
           1.);
  } else {
    // closer than 0.075 from {0,1} boundary
    double lp;
    if (log_p && ((lower_tail && q <= 0) || (!lower_tail && q > 0))) {
      lp = p;
    } else {
      lp = std::log((q > 0) ? R_DT_CIv(p) : p_);
    }
    r = std::sqrt(-lp);
    if (r <= 5.) {
      r += -1.6;
      val = (((((((r * 7.7454501427834140764e-4 + .0227238449892691845833) * r +
                  .24178072517745061177) *
                     r +
                 1.27045825245236838258) *
                    r +
                3.64784832476320460504) *
                   r +
               5.7694972214606914055) *
                  r +
              4.6303378461565452959) *
                 r +
             1.42343711074968357734) /
            (((((((r * 1.05075007164441684324e-9 + 5.475938084995344946e-4) *
                      r +
                  .0151986665636164571966) *
                     r +
                 .14810397642748007459) *
                    r +
                .68976733498510000455) *
                   r +
               1.6763848301838038494) *
                  r +
              2.05319162663775882187) *
                 r +
             1.);
    } else if (r <= 27) {
      r += -5.;
      val = (((((((r * 2.01033439929228813265e-7 + 2.71155556874348757815e-5) *
                      r +
                  .0012426609473880784386) *
                     r +
                 .026532189526576123093) *
                    r +
                .29656057182850489123) *
                   r +
               1.7848265399172913358) *
                  r +
              5.4637849111641143699) *
                 r +
             6.6579046435011037772) /
            (((((((r * 2.04426310338993978564e-15 + 1.4215117583164458887e-7) *
                      r +
                  1.8463183175100546818e-5) *
                     r +
                 7.868691311456132591e-4) *
                    r +
                .0148753612908506148525) *
                   r +
               .13692988092273580531) *
                  r +
              .59983220655588793769) *
                 r +
             1.);
    } else {
      // r > 27: p is *really* close to 0 or 1
      if (r >= 6.4e8) {
        val = r * M_SQRT2;
      } else {
        double s2 = -std::ldexp(lp, 1);
        double x2 = s2 - std::log(M_2PI * s2);
        if (r < 36000.) {
          x2 = s2 - std::log(M_2PI * x2) - 2. / (2. + x2);
          if (r < 840.) {
            x2 = s2 - std::log(M_2PI * x2) +
                 2 * std::log1p(-(1 - 1 / (4 + x2)) / (2. + x2));
            if (r < 109.) {
              x2 = s2 - std::log(M_2PI * x2) +
                   2 *
                       std::log1p(-(1 - (1 - 5 / (6 + x2)) / (4. + x2)) /
                                  (2. + x2));
              if (r < 55.) {
                x2 = s2 - std::log(M_2PI * x2) +
                     2 *
                         std::log1p(
                             -(1 - (1 - (5 - 9 / (8. + x2)) / (6. + x2)) /
                                       (4. + x2)) /
                             (2. + x2));
              }
            }
          }
        }
        val = std::sqrt(x2);
      }
    }
    if (q < 0.0) val = -val;
  }
  return mu + sigma * val;
}

}  // namespace nmath
}  // namespace mlcore
