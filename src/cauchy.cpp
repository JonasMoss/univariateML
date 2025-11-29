#include <Rcpp.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#include <algorithm>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::plugins(openmp)]]

#if defined(__GNUC__) || defined(__clang__)
# pragma GCC optimize("O3,unroll-loops,fast-math")
#endif

// [[Rcpp::export]]
List mlcauchy_rcpp(NumericVector x,
                   double tol   = 1e-8,
                   int    maxit = 100)
{
  int n = x.size();
  if (n == 0) stop("x must have positive length");

  // —————————————————————————————————————————————————————————————————————
  // 1) initial mu = median(x)
  //    • do it in-place on a clone to avoid two std::vector copies
  // —————————————————————————————————————————————————————————————————————
  NumericVector xc = clone(x);
  double* __restrict cptr = &xc[0];
  std::nth_element(cptr, cptr + n/2, cptr + n);
  double mu = cptr[n/2];

  // —————————————————————————————————————————————————————————————————————
  // 2) initial θ = log(MAD + ε)
  //    • overwrite the same buffer with |xi – mu|
  // —————————————————————————————————————————————————————————————————————
  for (int i = 0; i < n; ++i) {
    cptr[i] = std::fabs(cptr[i] - mu);
  }
  std::nth_element(cptr, cptr + n/2, cptr + n);
  double mad0  = cptr[n/2];
  double theta = std::log(mad0 + std::numeric_limits<double>::epsilon());

  // —————————————————————————————————————————————————————————————————————
  // 3) Newton–Raphson: fuse all sums, hoist constants, use restrict pointer
  // —————————————————————————————————————————————————————————————————————
  double dmu = 0.0, dth = 0.0;
  double* __restrict xp = &x[0];

  int iter;
  for (iter = 1; iter <= maxit; ++iter) {
    const double inv_s  = std::exp(-theta);
    const double inv_s2 = inv_s * inv_s;

    double sum1 = 0.0, sum2 = 0.0, sum3 = 0.0, sum4 = 0.0, sum5 = 0.0;

    // hint to vectorizer, unroll by 4
#pragma GCC ivdep
    for (int i = 0; i < n; i += 4) {
      for (int k = 0; k < 4 && i + k < n; ++k) {
        double z  = (xp[i+k] - mu) * inv_s;
        double z2 = z * z;
        double d1 = 1.0 + z2;
        double i1 = 1.0 / d1;
        double i2 = i1 * i1;

        sum1 += z * i1;        // ∂ℓ/∂μ
        sum2 += (z2 - 1) * i1;  // ∂ℓ/∂θ
        sum3 += (1 - z2) * i2;  // H_mm
        sum4 +=   z    * i2;    // H_mθ
        sum5 +=   z2   * i2;    // H_θθ
      }
    }

    // finalize score & Hessian
    double U_mu    = 2 * inv_s  * sum1;
    double U_th    =       1.0 * sum2;
    double H_mm    = 2 * inv_s2 * sum3;
    double H_mth   = 4 * inv_s  * sum4;
    double H_thth  = 4           * sum5;

    double det = H_mm * H_thth - H_mth * H_mth;
    dmu  = ( H_thth * U_mu   - H_mth * U_th) / det;
    dth  = (-H_mth   * U_mu   + H_mm   * U_th) / det;

    // convergence?
    if (std::max(std::fabs(dmu), std::fabs(dth)) < tol) {
      mu    += dmu;
      theta += dth;
      break;
    }
    mu    += dmu;
    theta += dth;
  }

  return List::create(
    _["estimates"]  = NumericVector::create(
      _["mu"]    = mu,
      _["scale"] = std::exp(theta)
    ),
    _["iterations"] = iter
  );
}

