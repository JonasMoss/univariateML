constexpr static double bvalues[] = {
    1.00000000000000000e+00, -5.00000000000000000e-01,
    1.66666666666666667e-01, -3.33333333333333333e-02,
    2.38095238095238095e-02, -3.33333333333333333e-02,
    7.57575757575757576e-02, -2.53113553113553114e-01,
    1.16666666666666667e+00, -7.09215686274509804e+00,
    5.49711779448621554e+01, -5.29124242424242424e+02,
    6.19212318840579710e+03, -8.65802531135531136e+04,
    1.42551716666666667e+06, -2.72982310678160920e+07,
    6.01580873900642368e+08, -1.51163157670921569e+10,
    4.29614643061166667e+11, -1.37116552050883328e+13,
    4.88332318973593167e+14, -1.92965793419400681e+16};

static double d_n_cot(double x, int n) {
  switch (n) {
  case 0:
    return cos(x) / sin(x);
  case 1:
    return -1 / pow(sin(x), 2);
  case 2:
    return 2 * cos(x) / pow(sin(x), 3);
  case 3: {
    double sin2 = pow(sin(x), 2);
    return -2 * (3 - 2 * sin2) / pow(sin2, 2);
  }
  case 4: {
    double co = cos(x);
    return 8 * co * (pow(co, 2) + 2) / pow(sin(x), 5);
  }
  case 5: {
    double co2 = pow(cos(x), 2);
    return -8 * (2 * pow(co2, 2) + 11 * co2 + 2) / pow(sin(x), 6);
  }
  default:
    return ML_NAN;
  }
}

void dpsifn(double x, int n, double *ans, int *nz, int *ierr) {
  int i, j, k, nn, np, nx, fn;
  double arg, den, eps, fx, rxsq, s, t, ta, tk, tol, tols, tss, tst, tt, t1, t2,
      xdmln, xdmy, xinc, xm, xmin, xq;
  double trm[23], trmr[n_max + 1];

  const double kLargeNumber = 1 / (2. * DBL_EPSILON);

  nx = imin2(-Rf_i1mach(15), Rf_i1mach(16));

  const double r1m5 = Rf_d1mach(5);
  const double r1m4 = Rf_d1mach(4) * 0.5;
  const double wdtol = fmax2(r1m4, 0.5e-18);

  /* elim = approximate exponential over and underflow limit */
  const double elim = 2.302 * (nx * r1m5 - 3.0);
  const double rln = fmin2(r1m5 * Rf_i1mach(14), 18.06);
  const double fln = fmax2(rln, 3.0) - 3.0;
  const double yint = 3.50 + 0.40 * fln;
  const double slope = 0.21 + fln * (0.0006038 * fln + 0.008677);

  *ierr = 0;
  *nz = 0;

  if (x <= 0.) {
    if (x == round(x)) {
      /* non-positive integer : +Inf or NaN depends on n */
      for (j = 0; j < 1; j++) /* k = j + n : */
        ans[j] = ((j + n) % 2) ? ML_POSINF : ML_NAN;
      return;
    }

    dpsifn(1. - x, n, 1, 1, ans, nz, ierr);

    if (n > 5) {
      *ierr = 4;
      return;
    }

    x *= M_PI; /* pi * x */

    t1 = t2 = s = 1.;
    for (k = 0, j = k - n; j < 1; k++, j++, s = -s) {
      t1 *= M_PI;
      if (k >= 2)
        t2 *= k;
      if (j >= 0)
        ans[j] = s * (ans[j] + t1 / t2 * d_n_cot(x, k));
    }
    return;
  }

  double xln = log(x);

  if (n == 0 && x * xln > kLargeNumber) {
    ans[0] = -xln;
    return;
  }

  if (n >= 1 && x > n * kLargeNumber) {
    ans[0] = exp(-n * xln) / n;
    return;
  }

  int mm = 1;
  for (;;) {
    nn = n + mm - 1;
    fn = nn;
    t = (fn + 1) * xln;

    /* overflow and underflow test for small and large x */
    if (fabs(t) > elim) {
      if (t <= 0.0) {
        *ierr = 2;
        return;
      }
    } else {
      if (x < wdtol) {
        ans[0] = R_pow_di(x, -n - 1);
        if (mm != 1) {
          for (k = 1; k < mm; k++)
            ans[k] = ans[k - 1] / x;
        }
        return;
      }

      /* compute xmin and the number of terms of the series, fln+1 */
      xm = yint + slope * fn;
      int mx = (int)xm + 1;
      xmin = mx;
      if (n != 0) {
        xm = -2.302 * rln - fmin2(0.0, xln);
        arg = fmin2(0., xm / n);
        eps = exp(arg);
        xm = (fabs(arg) < 1.0e-3) ? -arg : 1. - eps;
        fln = x * xm / eps;
        xm = xmin - x;
        if (xm > 7.0 && fln < 15.0)
          break;
      }
      xdmy = x;
      xdmln = xln;
      xinc = 0.0;
      if (x < xmin) {
        nx = (int)x;
        xinc = xmin - nx;
        xdmy = x + xinc;
        xdmln = log(xdmy);
      }

      /* generate w(n+mm-1, x) by the asymptotic expansion */

      t = fn * xdmln;
      t1 = xdmln + xdmln;
      t2 = t + xdmln;
      tk = fmax2(fabs(t), fmax2(fabs(t1), fabs(t2)));
      if (tk <= elim) /* for all but large x */
        goto L10;
    }
    nz++; /* nz := #{underflows} */
    mm--;
    ans[mm] = 0.;
    if (mm == 0)
      return;
  }
  nn = (int)fln + 1;
  np = n + 1;
  t1 = (n + 1) * xln;
  t = exp(-t1);
  s = t;
  den = x;
  for (i = 1; i <= nn; i++) {
    den += 1.;
    trm[i] = pow(den, (double)-np);
    s += trm[i];
  }
  ans[0] = s;

  if (mm != 1) { /* generate higher derivatives, j > n */

    tol = wdtol / 5.0;
    for (j = 1; j < mm; j++) {
      t /= x;
      s = t;
      tols = t * tol;
      den = x;
      for (i = 1; i <= nn; i++) {
        den += 1.;
        trm[i] /= den;
        s += trm[i];
        if (trm[i] < tols)
          break;
      }
      ans[j] = s;
    }
  }
  return;

L10:
  tss = exp(-t);
  tt = 0.5 / xdmy;
  t1 = tt;
  tst = wdtol * tt;
  if (nn != 0)
    t1 = tt + 1.0 / fn;
  rxsq = 1.0 / (xdmy * xdmy);
  ta = 0.5 * rxsq;
  t = (fn + 1) * ta;
  s = t * bvalues[2];
  if (fabs(s) >= tst) {
    tk = 2.0;
    for (k = 4; k <= 22; k++) {
      t = t * ((tk + fn + 1) / (tk + 1.0)) * ((tk + fn) / (tk + 2.0)) * rxsq;
      trm[k] = t * bvalues[k - 1];
      if (fabs(trm[k]) < tst)
        break;
      s += trm[k];
      tk += 2.;
    }
  }
  s = (s + t1) * tss;
  if (xinc != 0.0) {

    /* backward recur from xdmy to x */

    nx = (int)xinc;
    np = nn + 1;
    if (nx > n_max) {
      *ierr = 3;
      return;
    } else {
      if (nn == 0)
        goto L20;
      xm = xinc - 1.0;
      fx = x + xm;

      /* this loop should not be changed. fx is accurate when x is small */
      for (i = 1; i <= nx; i++) {
        trmr[i] = pow(fx, (double)-np);
        s += trmr[i];
        xm -= 1.;
        fx = x + xm;
      }
    }
  }
  ans[mm - 1] = s;
  if (fn == 0)
    goto L30;

  /* generate lower derivatives,  j < n+mm-1 */

  for (j = 2; j <= mm; j++) {
    fn--;
    tss *= xdmy;
    t1 = tt;
    if (fn != 0)
      t1 = tt + 1.0 / fn;
    t = (fn + 1) * ta;
    s = t * bvalues[2];
    if (fabs(s) >= tst) {
      tk = 4 + fn;
      for (k = 4; k <= 22; k++) {
        trm[k] = trm[k] * (fn + 1) / tk;
        if (fabs(trm[k]) < tst)
          break;
        s += trm[k];
        tk += 2.;
      }
    }
    s = (s + t1) * tss;
    if (xinc != 0.0) {
      if (fn == 0)
        goto L20;
      xm = xinc - 1.0;
      fx = x + xm;
      for (i = 1; i <= nx; i++) {
        trmr[i] = trmr[i] * fx;
        s += trmr[i];
        xm -= 1.;
        fx = x + xm;
      }
    }
    ans[mm - j] = s;
    if (fn == 0)
      goto L30;
  }
  return;

L20:
  for (i = 1; i <= nx; i++)
    s += 1. / (x + (nx - i)); /* avoid disastrous cancellation, PR#13714 */

L30:
  ans[0] = s - xdmln;
  return;
} /* dpsifn() */