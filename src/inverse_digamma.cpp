// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]
#include <RcppArmadillo.h>
#include <Rmath.h>         // for Rf_digamma, Rf_psigamma
#include <cmath>
#ifdef _OPENMP
#include <omp.h>
#endif

static const double GAMMA_C    = 0.57721566490153286060;
static const double ZETA3_C    = 1.2020569031595942854;
static const double PI_C       = M_PI;
static const double PI_SQ_C    = PI_C * PI_C;
static const double PSI1_C     = Rf_digamma(1.0);    // ≈ -0.577
static const double PSI1_1_C   = Rf_psigamma(1.0,1); // ≈ 1.645 (π²/6)

inline double inverse_digamma_scalar(double x) {
  double y, r;
  if (x + GAMMA_C < -30.0) {
    double t     = x + GAMMA_C;
    double t_inv = 1.0/t, t3 = t_inv*t_inv*t_inv;
    y = -t_inv + (PI_SQ_C/6.0)*t3 + (ZETA3_C/2.0)*t3*t_inv;
  } else {
    y = 1.0/std::log1p(std::exp(-x));
    for (int i=0; i<2; ++i) {
      r = Rf_digamma(y) - x;
      y -= r / Rf_psigamma(y,1);
    }
  }
  for (int k = 0; k < 2; ++k) {
    r = Rf_digamma(y) - x;
    if (std::fabs(r) < 1e-14) break;
    y -= r / Rf_psigamma(y,1);
  }
  return y;
}

// [[Rcpp::export]]
inline double logmom1_c(double mu, double sigma2,
                        const double tol = 1.220703e-4,
                        const int max_iter = 100) {
  double c = PI_C/std::sqrt(3.0*sigma2), step=0.0;
  for(int iter=0; iter<max_iter; ++iter){
    double k     = inverse_digamma_scalar(PSI1_C - c*mu);
    double psi1k = Rf_psigamma(k,1);
    double psi2k = Rf_psigamma(k,2);

    double c_inv = 1.0/c, c2 = c_inv*c_inv, c3 = c2*c_inv;
    double f     = c2*(PSI1_1_C + psi1k) - sigma2;
    double dk_dc = -mu/psi1k;
    double df    = -2.0*c3*(PSI1_1_C + psi1k) + c2*psi2k*dk_dc;

    step = f/df;
    c   -= step;
    if (std::fabs(step) < tol) break;
  }
  return c;
}


// [[Rcpp::export]]
Rcpp::NumericVector mlburr_cpp(const arma::vec& x,
                                            double tol = 1.220703e-4,
                                            int max_iter = 100) {
  const int    n     = x.n_elem;
  const double nd    = double(n);
  arma::vec     log_x = arma::log(x);
  const double  sum_l = arma::sum(log_x);

  const double* __restrict xp   = x.memptr();
  const double* __restrict lxp  = log_x.memptr();

  double m1 = sum_l/nd;
  double m2 = (arma::dot(log_x, log_x)/nd) - m1*m1;

  double c = (m1 * m1 > m2 && m1 > 0) ? PI_C / sqrt(3 * m2) : logmom1_c(m1, m2, tol, max_iter);

  double step = 0.0;

  for(int iter=0; iter<max_iter; ++iter){
    double g=0.0, dg=0.0, d2g1=0.0, d2g2=0.0;
#pragma omp simd reduction(+:g,dg,d2g1,d2g2)
    for(int i=0; i<n; ++i){
      double lx = lxp[i];
      double xc = std::exp(c * lx);
      double ip = xc + 1.0;
      double t  = xc / ip;
      g    += std::log1p(xc);
      dg   += lx * t;
      d2g1 += lx*lx * t;
      d2g2 += (lx * t)*(lx * t);
    }
    double d2g = d2g1 - d2g2;
    double g2  = g*g, c2 = c*c;
    double f   = nd/c - nd*dg/g + sum_l - dg;
    double df  = -nd/c2 - nd*(g*d2g - dg*dg)/g2 - d2g;

    step = f/df;
    c   -= step;
    if (std::fabs(step) < tol) break;
  }

  arma::vec xc_final = arma::exp(c*log_x);
  double g_final     = arma::sum(arma::log1p(xc_final));
  double k           = nd / g_final;

  Rcpp::NumericVector out = Rcpp::NumericVector::create(k, c);
  out.names() = Rcpp::CharacterVector::create("shape1","shape2");
  return out;
}
