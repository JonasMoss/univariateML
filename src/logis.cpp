#include <Rcpp.h>
#include <cmath>    // for std::exp, std::tanh, std::fabs

using namespace Rcpp;

// Tell GCC/Clang to crank up optimization, unroll loops, and enable fast-math.
// You can also add these in src/Makevars:   CXXFLAGS += -O3 -march=native -ffast-math
#if defined(__GNUC__) || defined(__clang__)
# pragma GCC optimize("O3,unroll-loops,fast-math")
#endif

// [[Rcpp::export]]
List mllogis_fast(NumericVector x,
                  double tol   = 1.490116e-08,
                  int    maxit = 100)
{
  int n = x.size();
  if (n == 0) stop("x must have positive length");

  // --------------------------------------------------------------------------------
  // 1) Initial guesses
  //    • median: nth_element in place (avoiding clone)
  //    • var/mean by one fast pass (Welford’s algorithm)
  // --------------------------------------------------------------------------------

  // — median
  // std::nth_element(x.begin(), x.begin() + n/2, x.end());
  // double mu = x[n/2];


  // — mean and (sample) variance in one pass
  double m = 0.0, M2 = 0.0;
  for (int i = 0; i < n; ++i) {
    double v = x[i] - m;
    m += v / (i + 1);
    M2 += v * (x[i] - m);
  }

  double mu = m;
  double sample_var = (n > 1 ? M2 / (n - 1) : 0.0);
  double theta = std::log(std::sqrt(sample_var) * std::sqrt(3.0) / M_PI);

  // --------------------------------------------------------------------------------
  // 2) Newton–Raphson, with a single fused pass per iteration
  //    • remove cosh()/1/(cosh^2) by using sech^2 = 1 – tanh^2
  //    • hoist constants out of the inner loop
  //    • use raw pointer + restrict to help auto-vectorization
  // --------------------------------------------------------------------------------

  double dmu = 0.0, dtheta = 0.0;
  double* __restrict xp = &x[0];

  for (int iter = 0; iter < maxit; ++iter) {
    // hoist these
    const double inv_s    = std::exp(-theta);
    const double inv_s2   = inv_s * inv_s;
    const double half_inv = 0.5 * inv_s;

    // accumulators
    double U1        = 0.0;
    double U2        = 0.0;
    double sum_c2    = 0.0;
    double sum_tz    = 0.0;
    double sum_z2c2  = 0.0;

    // no OpenMP, single-threaded but vector-friendly
    // unroll by 4 for a small extra speed bump
    int i = 0, n4 = (n & ~3);
    for (; i < n4; i += 4) {
      // manually unrolled chunk
      for (int k = 0; k < 4; ++k) {
        double xi = xp[i + k];
        double z  = (xi - mu) * inv_s;
        double h  = 0.5 * z;
        double th = std::tanh(h);
        double c2 = 1.0 - th * th;            // = sech^2(h)

        U1       += th;
        U2       += z * th;
        sum_c2   += c2;
        sum_tz   += th + 0.5 * z * c2;
        sum_z2c2 += z * z * c2;
      }
    }
    // finish the tail
    for (; i < n; ++i) {
      double xi = xp[i];
      double z  = (xi - mu) * inv_s;
      double h  = 0.5 * z;
      double th = std::tanh(h);
      double c2 = 1.0 - th * th;

      U1       += th;
      U2       += z * th;
      sum_c2   += c2;
      sum_tz   += th + 0.5 * z * c2;
      sum_z2c2 += z * z * c2;
    }

    // finish up scores & information
    U1 *= inv_s;
    U2 -= n;
    double J11  = 0.5 * sum_c2 * inv_s2;
    double J12  = inv_s * sum_tz;
    double J22  = (U2 + n) + 0.5 * sum_z2c2;

    // solve 2×2 system
    double det  = J11 * J22 - J12 * J12;
    dmu         = ( J22 * U1 - J12 * U2) / det;
    dtheta      = (-J12 * U1 + J11 * U2) / det;

    // convergence check
    if (std::max(std::fabs(dmu), std::fabs(dtheta)) < tol) {
      mu    += dmu;
      theta += dtheta;
      iter; // for correct iteration count below
      return List::create(
        _["estimates"]  = NumericVector::create(
          _["mu"]    = mu,
          _["scale"] = std::exp(theta)
        ),
        _["iterations"] = iter + 1
      );
    }

    mu    += dmu;
    theta += dtheta;
  }

  // if we fall out, we hit maxit
  return List::create(
    _["estimates"]  = NumericVector::create(
      _["mu"]    = mu,
      _["scale"] = std::exp(theta)
    ),
    _["iterations"] = maxit
  );
}
