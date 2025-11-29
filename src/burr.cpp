// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]
#include <RcppArmadillo.h>
#include <Rmath.h>         // for Rf_digamma, Rf_psigamma
#include <cmath>
#ifdef _OPENMP
#include <omp.h>
#endif

#include <limits>

// ——— compile-time constants ———
constexpr double NEULER_CST = -0.5772156649015328606;      // = digamma(1)
constexpr double BASEL_CST  = M_PI*M_PI / 6.0;            // = trigamma(1)
constexpr double PI_C       = M_PI;

// ——— inline logistic helpers ———
static inline double logit(double p) noexcept {
  return std::log(p / (1.0 - p));
}
static inline double logistic(double x) noexcept {
  double e = std::exp(-x);
  return 1.0 / (1.0 + e);
}
static inline double logistic_pdf(double x) noexcept {
  double L = logistic(x);
  return L * (1.0 - L);
}
static inline double qlogis(double p, double loc, double scale) noexcept {
  return loc + scale * logit(p);
}
static inline double plogis(double x, double loc, double scale) noexcept {
  return 1.0 / (1.0 + std::exp(-(x - loc) / scale));
}

// [[Rcpp::export]]
Rcpp::NumericVector f_lower_inv_rcpp2(double cv) {
  // starting-value constants
  const double loc = -0.5495872, scl = 0.6103216;
  const double cv_lo = 0.1, cv_hi = 0.95;
  const double eps   = 1e-15;

  // — start k—
  double k;
  if (cv < cv_lo) {
    k = 1.0 - std::sqrt(2.0 * cv / BASEL_CST);
  } else if (cv > cv_hi) {
    k = std::sqrt((1.0 - cv) / (cv * BASEL_CST));
  } else {
    double y = std::min(1.0 - eps, std::max(eps, 1.0 - cv));
    double x0_log = qlogis(y, loc, scl);
    k = plogis(x0_log, loc, scl);
  }
  k = std::min(1.0 - eps, std::max(eps, k));

  // parameterize in x = logit(k)
  double x0 = logit(k);

  const double reltol = std::sqrt(std::numeric_limits<double>::epsilon());
  const int    maxit  = 100;
  int          iter   = 0;

  for (; iter < maxit; ++iter) {
    // k, polygamma
    double kk    = logistic(x0);
    double di    = Rf_psigamma(kk, 0);
    double tri   = Rf_psigamma(kk, 1);
    double tetra = Rf_psigamma(kk, 2);

    // f(x)
    double pd    = di - NEULER_CST;
    double bpt   = BASEL_CST + tri;
    double f     = (pd*pd)/bpt - cv;

    // f'(x) = f'_k * d k/d x
    double fpk   = ((2.0*pd*tri*bpt) - (pd*pd*tetra)) / (bpt*bpt);
    double df    = fpk * logistic_pdf(x0);

    // Newton step
    double step  = f/df;
    double x1    = x0 - step;
    if (std::abs(x1 - x0) < (std::abs(x0)*reltol + 1e-3*reltol)) {
      x0 = x1;
      break;
    }
    x0 = x1;
  }

  double k_out = logistic(x0);
  return Rcpp::NumericVector::create(
    Rcpp::_["k"]    = k_out,
    Rcpp::_["iter"] = iter+1
  );
}

// [[Rcpp::export]]
Rcpp::NumericVector f_lower_inv_rcpp(double cv) {
  // starting‐value constants
  const double loc   = -0.5495872,
    scl   =  0.6103216,
    cv_lo =  0.1,
    cv_hi =  0.95,
    eps   =  1e-15;
  // 1) crude start for k in (0,1)
  double k0;
  if (cv < cv_lo) {
    k0 = 1.0 - std::sqrt(2.0 * cv / BASEL_CST);
  } else if (cv > cv_hi) {
    k0 = std::sqrt((1.0 - cv) / (cv * BASEL_CST));
  } else {
    double y    = std::min(1.0 - eps, std::max(eps, 1.0 - cv));
    double xlog = qlogis(y, loc, scl);
    k0          = plogis(xlog, loc, scl);
  }
  // clamp into (eps, 1-eps)
  k0 = std::max(eps, std::min(1.0 - eps, k0));

  // 2) re‐parameterize: p = log(k)
  double p    = std::log(k0);
  const double reltol = std::sqrt(std::numeric_limits<double>::epsilon());
  const int    maxit  = 100;
  int          iter   = 0;

  // 3) Newton–Raphson in p
  for (; iter < maxit; ++iter) {
    double k     = std::exp(p);
    double di    = Rf_psigamma(k, 0);
    double tri   = Rf_psigamma(k, 1);
    double tetra = Rf_psigamma(k, 2);

    double pd   = di - NEULER_CST;          // ψ(k) − γ
    double bpt  = BASEL_CST + tri;          // π²/6 + ψ′(k)
    double f    = (pd*pd)/bpt - cv;         // target

    // df/dk
    double fpk  = ((2.0*pd*tri*bpt) - (pd*pd*tetra)) / (bpt*bpt);
    // dp‐derivative: df/dp = df/dk * dk/dp = fpk * k
    double dfp  = fpk * k;

    double step = f / dfp;
    double p1   = p - step;
    if (std::abs(p1 - p) < (std::abs(p)*reltol + 1e-3*reltol)) {
      p = p1;
      break;
    }
    p = p1;
  }

  double k_out = std::exp(p);
  return Rcpp::NumericVector::create(
    Rcpp::_["k"]    = k_out,
    Rcpp::_["iter"] = iter + 1
  );
}


// [[Rcpp::export]]
Rcpp::NumericVector f_upper_inv_rcpp(double cv) {
  const double thr  = 0.2;
  const double eps  = 1e-9;

  // — start k—
  double k0;
  if (cv < thr) {
    k0 = 1.0 + std::sqrt(2.0 * cv / BASEL_CST);
  } else {
    k0 = std::exp(std::sqrt(cv * BASEL_CST));
  }
  k0 = std::max(1.0 + eps, k0);

  // parameterize in x = log(k - 1)
  double x0 = std::log(k0 - 1.0);

  const double reltol = std::sqrt(std::numeric_limits<double>::epsilon());
  const int    maxit  = 100;
  int          iter   = 0;

  for (; iter < maxit; ++iter) {
    double kk    = std::exp(x0) + 1.0;
    double di    = Rf_psigamma(kk, 0);
    double tri   = Rf_psigamma(kk, 1);
    double tetra = Rf_psigamma(kk, 2);

    double pd    = di - NEULER_CST;
    double bpt   = BASEL_CST + tri;
    double f     = (pd*pd)/bpt - cv;

    double fpk   = ((2.0*pd*tri*bpt) - (pd*pd*tetra)) / (bpt*bpt);
    double df    = fpk * std::exp(x0);

    double step  = f/df;
    double x1    = x0 - step;
    if (std::abs(x1 - x0) < (std::abs(x0)*reltol + 1e-3*reltol)) {
      x0 = x1;
      break;
    }
    x0 = x1;
  }

  double k_out = std::exp(x0) + 1.0;
  return Rcpp::NumericVector::create(
    Rcpp::_["k"]    = k_out,
    Rcpp::_["iter"] = iter+1
  );
}

// [[Rcpp::export]]
Rcpp::NumericVector burr_lmom2(double mu, double sigma2) {
  double k_hat = NA_REAL, c_hat = NA_REAL;
  int    iter  = 0;

  if (sigma2 > 0 && std::abs(mu) >= std::numeric_limits<double>::epsilon()) {
    double cv = mu*mu / sigma2;
    if (mu > 0) {
      auto res = f_lower_inv_rcpp(cv);
      k_hat = res["k"]; iter = res["iter"];
    } else {
      auto res = f_upper_inv_rcpp(cv);
      k_hat = res["k"]; iter = res["iter"];
    }
    double t1 = Rf_psigamma(k_hat, 1);
    double csq = (BASEL_CST + t1) / sigma2;
    if (csq > 0) c_hat = std::sqrt(csq);
  } else if (std::abs(mu) < std::numeric_limits<double>::epsilon()) {
    k_hat = 1.0;
    double t1 = Rf_psigamma(1.0,1);
    double csq = (BASEL_CST + t1) / sigma2;
    if (csq > 0) c_hat = std::sqrt(csq);
  }

  return Rcpp::NumericVector::create(
    Rcpp::_["k"]    = k_hat,
    Rcpp::_["c"]    = c_hat,
    Rcpp::_["iter"] = iter
  );
}

// [[Rcpp::export]]
Rcpp::NumericVector burr_lmom(double mu, double sigma2) {
  // outputs
  double k_hat = NA_REAL, c_hat = NA_REAL;
  int    iter  = 0;

  // only valid if Var>0 and |mu|>0
  if (sigma2 > 0.0 && std::abs(mu) >= std::numeric_limits<double>::epsilon()) {
    double cv = (mu*mu) / sigma2;

    // invert coefficient of variation -> k
    if (mu > 0.0) {
      auto res = f_lower_inv_rcpp(cv);
      k_hat = res["k"];  iter = res["iter"];
    } else {
      auto res = f_upper_inv_rcpp(cv);
      k_hat = res["k"];  iter = res["iter"];
    }

    // trigamma at k
    double psi1_k = Rf_psigamma(k_hat, 1);
    double numer  = BASEL_CST + psi1_k;   // = ψ₁(1) + ψ₁(k)

    // compute log(c^2) = log(numer) - log(sigma2)  ⇒  log(c) = .5 * (…)
    if (numer > 0.0) {
      double logcsq = std::log(numer) - std::log(sigma2);
      double logc   = 0.5 * logcsq;
      c_hat = std::exp(logc);
    }
    // otherwise leave c_hat = NA
  }
  // special “mu≈0” fallback could go here if desired

  return Rcpp::NumericVector::create(
    Rcpp::_["k"]    = k_hat,
    Rcpp::_["c"]    = c_hat,
    Rcpp::_["iter"] = iter
  );
}
// [[Rcpp::export]]
Rcpp::NumericVector mlburr_rcpp2(const arma::vec& x,
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

  double c = burr_lmom(m1, m2)[1];

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

// [[Rcpp::export]]
Rcpp::NumericVector mlburr_rcpp(const arma::vec& x,
                                Rcpp::Nullable<Rcpp::NumericVector> init = R_NilValue,
                                double tol = 1.220703e-4,
                                int max_iter = 100)
{
  const int    n     = x.n_elem;
  const double nd    = double(n);

  // precompute log‐data
  arma::vec     log_x = arma::log(x);
  const double* lxp   = log_x.memptr();
  double        sum_l = arma::sum(log_x);

  // 1) Initialize logc
  double logc;
  if (init.isNotNull()) {
    // user provided a starting c
    Rcpp::NumericVector iv(init);
    if (iv.size() < 1 || Rcpp::NumericVector::is_na(iv[0]) || iv[0] <= 0) {
      Rcpp::stop("`init` must be a length-1 numeric vector with a positive c");
    }
    logc = std::log(iv[0]);
  }
  else {
    // fall back to log‐moment estimator
    double m1 = sum_l/nd;
    double m2 = (arma::dot(log_x, log_x)/nd) - m1*m1;
    double c0 = burr_lmom(m1, m2)["c"];
    if (Rcpp::NumericVector::is_na(c0) || c0 <= 0) {
      Rcpp::stop("Failed to get a valid initial c from burr_lmom()");
    }
    logc = std::log(c0);
  }

  // 2) Newton–Raphson on p = log(c)
  double step_p = 0.0;
  int    iter   = 0;
  for (; iter < max_iter; ++iter) {
    double c = std::exp(logc);

    // accumulators
    double g=0.0, dg=0.0, d2g1=0.0, d2g2=0.0;
#pragma omp simd reduction(+:g,dg,d2g1,d2g2)
    for(int i=0; i<n; ++i){
      double lx = lxp[i];
      double xc = std::exp(c * lx);
      double t  = xc / (xc + 1.0);
      g    += std::log1p(xc);
      dg   += lx * t;
      d2g1 += lx*lx * t;
      d2g2 += (lx * t)*(lx * t);
    }
    double d2g  = d2g1 - d2g2;
    double g2   = g * g;

    // score & Hessian wrt c
    double f_c  = nd/c - nd*dg/g + sum_l - dg;
    double df_c = -nd/(c*c)
      - nd*(g*d2g - dg*dg)/g2
    - d2g;

    // translate to p = log(c)
    double f_p  = c * f_c;
    double d2_p = c * (f_c + c * df_c);

    step_p = f_p / d2_p;
    logc  -= step_p;
    if (std::fabs(step_p) < tol) break;
  }

  // 3) Final estimates
  double c = std::exp(logc);
  arma::vec xc_final = arma::exp(c * log_x);
  double g_final     = arma::sum(arma::log1p(xc_final));
  double k           = nd / g_final;

  return Rcpp::NumericVector::create(
    Rcpp::_["shape1"] = k,
    Rcpp::_["shape2"] = c,
    Rcpp::_["iter"]   = iter+1
  );
}
