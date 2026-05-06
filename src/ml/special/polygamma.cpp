// Polygamma — Bell Labs / Amos algorithm 610.
//
// Port of R's nmath/polygamma.c; see instructions/nmath/polygamma.c for the
// upstream. The signature and control-flow are kept verbatim so digamma /
// trigamma / tetragamma agree with Rmath at ULP level; goldens that previously
// flowed through Rf_psigamma should not need regeneration.
//
// Kept off the package's R-side build path: src/ml_exports.cpp and the dpqr
// cpps continue to use Rmath via LinkingTo. This file is wired into the
// standalone tests/cpp harness so it can build and run without an R install.

#include "polygamma.hpp"
#include "rmath_compat.hpp"

#include <cmath>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

namespace mlcore {
namespace special {

using mlcore::rmath::Rf_d1mach;
using mlcore::rmath::Rf_i1mach;
using mlcore::rmath::R_pow_di;
using mlcore::rmath::fmax2;
using mlcore::rmath::fmin2;
using mlcore::rmath::imin2;
constexpr int n_max = mlcore::rmath::n_max;

namespace {

// d_n(x) := (d/dx)^n cot(x). Closed forms for n in {0,..,5}; NaN beyond.
double d_n_cot(double x, int n) {
  if (n == 0) return std::cos(x) / std::sin(x);
  if (n == 1) return -1.0 / R_pow_di(std::sin(x), 2);
  if (n == 2) return 2.0 * std::cos(x) / R_pow_di(std::sin(x), 3);
  if (n == 3) {
    const double sin2 = R_pow_di(std::sin(x), 2);
    return -2.0 * (3.0 - 2.0 * sin2) / R_pow_di(sin2, 2);
  }
  if (n == 4) {
    const double co = std::cos(x);
    return 8.0 * co * (R_pow_di(co, 2) + 2.0) / R_pow_di(std::sin(x), 5);
  }
  if (n == 5) {
    const double co2 = R_pow_di(std::cos(x), 2);
    return -8.0 * (2.0 * R_pow_di(co2, 2) + 11.0 * co2 + 2.0) /
           R_pow_di(std::sin(x), 6);
  }
  return mlcore::rmath::ML_NAN_value();
}

}  // namespace

void dpsifn(double x, int n, int kode, int m, double *ans, int *nz,
            int *ierr) {
  static const double bvalues[] = {
      1.00000000000000000e+00,  -5.00000000000000000e-01,
      1.66666666666666667e-01,  -3.33333333333333333e-02,
      2.38095238095238095e-02,  -3.33333333333333333e-02,
      7.57575757575757576e-02,  -2.53113553113553114e-01,
      1.16666666666666667e+00,  -7.09215686274509804e+00,
      5.49711779448621554e+01,  -5.29124242424242424e+02,
      6.19212318840579710e+03,  -8.65802531135531136e+04,
      1.42551716666666667e+06,  -2.72982310678160920e+07,
      6.01580873900642368e+08,  -1.51163157670921569e+10,
      4.29614643061166667e+11,  -1.37116552050883328e+13,
      4.88332318973593167e+14,  -1.92965793419400681e+16};

  int i, j, k, nn, np, nx, fn;
  double arg, den, eps, fx, rxsq, s, t, ta, tk, tol, tols, tss, tst, tt, t1,
      t2, xdmln, xdmy, xinc, xm, xmin, xq;
  double trm[23], trmr[n_max + 1];

  *ierr = 0;
  *nz = 0;
  if (n < 0 || kode < 1 || kode > 2 || m < 1) {
    *ierr = 1;
    return;
  }
  if (x <= 0.0) {
    if (x == std::round(x)) {
      // non-positive integer: +Inf or NaN depending on n
      for (j = 0; j < m; j++) {
        ans[j] = ((j + n) % 2) ? mlcore::rmath::ML_POSINF_value()
                                : mlcore::rmath::ML_NAN_value();
      }
      return;
    }
    dpsifn(1.0 - x, n, /*kode=*/1, m, ans, nz, ierr);
    if (n > 5) {
      // not yet implemented for x < 0 and n >= 6
      *ierr = 4;
      return;
    }
    x *= M_PI;  // pi * x
    t1 = t2 = s = 1.0;
    for (k = 0, j = k - n; j < m; k++, j++, s = -s) {
      t1 *= M_PI;
      if (k >= 2) t2 *= k;
      if (j >= 0) ans[j] = s * (ans[j] + t1 / t2 * d_n_cot(x, k));
    }
    return;
  }

  // x > 0
  double xln = std::log(x);
  if (kode == 1 && m == 1) {
    const double lrg = 1.0 / (2.0 * DBL_EPSILON);
    if (n == 0 && x * xln > lrg) {
      ans[0] = -xln;
      return;
    } else if (n >= 1 && x > n * lrg) {
      ans[0] = std::exp(-n * xln) / n;
      return;
    }
  }
  nx = imin2(-Rf_i1mach(15), Rf_i1mach(16));

  const double r1m5 = Rf_d1mach(5);
  const double r1m4 = Rf_d1mach(4) * 0.5;
  const double wdtol = fmax2(r1m4, 0.5e-18);

  double elim = 2.302 * (nx * r1m5 - 3.0);
  const double rln = fmin2(r1m5 * Rf_i1mach(14), 18.06);
  double fln = fmax2(rln, 3.0) - 3.0;
  const double yint = 3.50 + 0.40 * fln;
  const double slope = 0.21 + fln * (0.0006038 * fln + 0.008677);

  int mm = m;
  for (;;) {
    nn = n + mm - 1;
    fn = nn;
    t = (fn + 1) * xln;

    if (std::fabs(t) > elim) {
      if (t <= 0.0) {
        *ierr = 2;
        return;
      }
    } else {
      if (x < wdtol) {
        ans[0] = R_pow_di(x, -n - 1);
        if (mm != 1) {
          for (k = 1; k < mm; k++) ans[k] = ans[k - 1] / x;
        }
        if (n == 0 && kode == 2) ans[0] += xln;
        return;
      }

      xm = yint + slope * fn;
      int mx = static_cast<int>(xm) + 1;
      xmin = mx;
      if (n != 0) {
        xm = -2.302 * rln - fmin2(0.0, xln);
        arg = fmin2(0.0, xm / n);
        eps = std::exp(arg);
        xm = (std::fabs(arg) < 1.0e-3) ? -arg : 1.0 - eps;
        fln = x * xm / eps;
        xm = xmin - x;
        if (xm > 7.0 && fln < 15.0) break;
      }
      xdmy = x;
      xdmln = xln;
      xinc = 0.0;
      if (x < xmin) {
        nx = static_cast<int>(x);
        xinc = xmin - nx;
        xdmy = x + xinc;
        xdmln = std::log(xdmy);
      }

      // generate w(n+mm-1, x) by the asymptotic expansion
      t = fn * xdmln;
      t1 = xdmln + xdmln;
      t2 = t + xdmln;
      tk = fmax2(std::fabs(t), fmax2(std::fabs(t1), std::fabs(t2)));
      if (tk <= elim) goto L10;
    }
    (*nz)++;
    mm--;
    ans[mm] = 0.0;
    if (mm == 0) return;
  }
  nn = static_cast<int>(fln) + 1;
  np = n + 1;
  t1 = (n + 1) * xln;
  t = std::exp(-t1);
  s = t;
  den = x;
  for (i = 1; i <= nn; i++) {
    den += 1.0;
    trm[i] = std::pow(den, static_cast<double>(-np));
    s += trm[i];
  }
  ans[0] = s;
  if (n == 0 && kode == 2) ans[0] = s + xln;

  if (mm != 1) {
    tol = wdtol / 5.0;
    for (j = 1; j < mm; j++) {
      t /= x;
      s = t;
      tols = t * tol;
      den = x;
      for (i = 1; i <= nn; i++) {
        den += 1.0;
        trm[i] /= den;
        s += trm[i];
        if (trm[i] < tols) break;
      }
      ans[j] = s;
    }
  }
  return;

L10:
  tss = std::exp(-t);
  tt = 0.5 / xdmy;
  t1 = tt;
  tst = wdtol * tt;
  if (nn != 0) t1 = tt + 1.0 / fn;
  rxsq = 1.0 / (xdmy * xdmy);
  ta = 0.5 * rxsq;
  t = (fn + 1) * ta;
  s = t * bvalues[2];
  if (std::fabs(s) >= tst) {
    tk = 2.0;
    for (k = 4; k <= 22; k++) {
      t = t * ((tk + fn + 1) / (tk + 1.0)) * ((tk + fn) / (tk + 2.0)) * rxsq;
      trm[k] = t * bvalues[k - 1];
      if (std::fabs(trm[k]) < tst) break;
      s += trm[k];
      tk += 2.0;
    }
  }
  s = (s + t1) * tss;
  if (xinc != 0.0) {
    nx = static_cast<int>(xinc);
    np = nn + 1;
    if (nx > n_max) {
      *ierr = 3;
      return;
    } else {
      if (nn == 0) goto L20;
      xm = xinc - 1.0;
      fx = x + xm;
      for (i = 1; i <= nx; i++) {
        trmr[i] = std::pow(fx, static_cast<double>(-np));
        s += trmr[i];
        xm -= 1.0;
        fx = x + xm;
      }
    }
  }
  ans[mm - 1] = s;
  if (fn == 0) goto L30;

  // generate lower derivatives, j < n+mm-1
  for (j = 2; j <= mm; j++) {
    fn--;
    tss *= xdmy;
    t1 = tt;
    if (fn != 0) t1 = tt + 1.0 / fn;
    t = (fn + 1) * ta;
    s = t * bvalues[2];
    if (std::fabs(s) >= tst) {
      tk = 4 + fn;
      for (k = 4; k <= 22; k++) {
        trm[k] = trm[k] * (fn + 1) / tk;
        if (std::fabs(trm[k]) < tst) break;
        s += trm[k];
        tk += 2.0;
      }
    }
    s = (s + t1) * tss;
    if (xinc != 0.0) {
      if (fn == 0) goto L20;
      xm = xinc - 1.0;
      fx = x + xm;
      for (i = 1; i <= nx; i++) {
        trmr[i] = trmr[i] * fx;
        s += trmr[i];
        xm -= 1.0;
        fx = x + xm;
      }
    }
    ans[mm - j] = s;
    if (fn == 0) goto L30;
  }
  return;

L20:
  for (i = 1; i <= nx; i++)
    s += 1.0 / (x + (nx - i));  // avoid disastrous cancellation, PR#13714

L30:
  if (kode != 2) {
    ans[0] = s - xdmln;
  } else if (xdmy != x) {
    xq = xdmy / x;
    ans[0] = s - std::log(xq);
  } else {
    ans[0] = s;
  }
  // suppress unused-variable warning when goto target lands here directly
  (void)tt;
  (void)tst;
  (void)t2;
  (void)nz;
}

double psigamma_deriv(double x, int n) {
  if (std::isnan(x)) return x;
  if (n > n_max) return mlcore::rmath::ML_NAN_value();
  double ans;
  int nz = 0, ierr = 0;
  dpsifn(x, n, /*kode=*/1, /*m=*/1, &ans, &nz, &ierr);
  if (ierr != 0) return mlcore::rmath::ML_NAN_value();
  // dpsifn returns A := (-1)^(n+1) / n! * psi^(n)(x); convert to psi^(n)(x).
  double out = -ans;
  for (int k = 1; k <= n; k++) out *= -k;
  return out;
}

}  // namespace special
}  // namespace mlcore
