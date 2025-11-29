// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]

#include <RcppArmadillo.h>
#include <cmath>
#ifdef _OPENMP
#include <omp.h>
#endif

#include <Rcpp.h>
#ifdef _OPENMP
#include <omp.h>
// [[Rcpp::plugins(openmp)]]
#endif

static constexpr int OMP_THRESHOLD = 10000;

// [[Rcpp::export]]
Rcpp::NumericVector ml_gumbel_halley_rcpp(const Rcpp::NumericVector& x) {
  const int n = x.size();
  const double* __restrict__ xp = REAL(x);

  // 1) mean & variance in one pass
  double sum_x = 0.0, sum_x2 = 0.0;
#pragma omp simd reduction(+:sum_x,sum_x2)
  for (int i = 0; i < n; ++i) {
    double xi = xp[i];
    sum_x  += xi;
    sum_x2 += xi * xi;
  }
  const double xbar = sum_x / n;
  const double var0 = sum_x2 / n - xbar * xbar;

  // 2) initial σ via moment estimator
  double sigma = std::sqrt(var0) * 2.45 / M_PI;
  double theta = std::log(sigma);

  // 3) Halley iterations on θ = log(σ)
  const int    maxit = 100;
  const double reltol = std::sqrt(std::numeric_limits<double>::epsilon());
  int          iter = 0;

  for (; iter < maxit; ++iter) {
    const double s0   = std::exp(theta);
    const double invs = 1.0 / s0;
    const double negs = -invs;

    double p0 = 0.0, p1 = 0.0, p2 = 0.0, p3 = 0.0;
#pragma omp simd reduction(+:p0,p1,p2,p3)
    for (int i = 0; i < n; ++i) {
      double v = xp[i] - xbar;
      double e = std::exp(v * negs);
      p0 += e;
      p1 += v * e;
      double v2 = v * v;
      p2 += v2 * e;
      p3 += v2 * v * e;
    }

    p1 /= p0;
    double raw2 = p2 / p0;
    double m2   = raw2 - p1 * p1;
    double raw3 = p3 / p0;
    double m3   = raw3 - 3.0 * p1 * raw2 + 2.0 * p1 * p1 * p1;

    // f, f′, f″ for Halley update
    double f   = s0 + p1;
    double fp  = 1.0 + m2 * (invs * invs);
    double fpp =  m3 * std::pow(invs,4)
      - 2.0 * m2 * (invs * invs * invs);

    // Halley step
    double s_new   = s0 - (2.0 * f * fp) / (2.0 * fp * fp - f * fpp);
    double theta_n = std::log(s_new);

    if (std::abs(theta_n - theta) < std::abs(theta) * reltol) {
      theta = theta_n;
      break;
    }
    theta = theta_n;
  }

  // 4) finalize σ and compute μ in one more pass
  sigma = std::exp(theta);
  const double neg1f = -1.0 / sigma;
  double sum_e = 0.0;
#pragma omp simd reduction(+:sum_e)
  for (int i = 0; i < n; ++i) {
    sum_e += std::exp((xp[i] - xbar) * neg1f);
  }
  double mu = -sigma * std::log(sum_e / n) + xbar;

  // 5) return
  Rcpp::NumericVector out(3);
  out[0] = mu;
  out[1] = sigma;
  out[2] = iter + 1;
  out.names() = Rcpp::CharacterVector::create("mu", "sigma", "iterations");
  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector ml_gumbel_newt_rcpp(const Rcpp::NumericVector& x) {
  const int n = x.size();
  const double* xp = x.begin();

  // 1) Compute mean and variance in one fused pass
  double sum_x  = 0.0;
  double sum_x2 = 0.0;
#pragma omp simd reduction(+:sum_x,sum_x2)
  for (int i = 0; i < n; ++i) {
    double xi = xp[i];
    sum_x  += xi;
    sum_x2 += xi * xi;
  }
  const double xbar   = sum_x / n;
  // Var = E[(x - μ)^2] = E[x^2] - μ^2
  const double var0   = sum_x2 / n - xbar * xbar;
  // Initial σ via moment estimator, with your same scaling
  double sigma = std::sqrt(var0) * 2.45 / M_PI;

  // 2) Newton–Raphson on θ = log(σ)
  double theta = std::log(sigma);
  const int    maxit = 100;
  const double reltol = std::sqrt(std::numeric_limits<double>::epsilon());
  int          iter = 0;

  for (iter = 1; iter <= maxit; ++iter) {
    const double s0   = std::exp(theta);
    const double invs = 1.0 / s0;
    const double m1   = -invs;

    double psi0 = 0.0, psi1 = 0.0, psi2 = 0.0;
#pragma omp simd reduction(+:psi0,psi1,psi2)
    for (int i = 0; i < n; ++i) {
      const double v = xp[i] - xbar;
      const double e = std::exp(v * m1);
      psi0 += e;
      psi1 += v * e;
      psi2 += v * v * e;
    }
    psi1 /= psi0;
    psi2  = psi2/psi0 - psi1*psi1;

    // f = s0 + ψ₁,   f' = 1 + ψ₂ / s0^2
    const double f   = s0 + psi1;
    const double df  = 1.0 + psi2 * (invs*invs);
    const double dth = f / (df * s0);

    theta -= dth;
    if (std::abs(dth) < std::abs(theta)*reltol) break;
  }

  // 3) Final σ, μ
  sigma = std::exp(theta);
  const double invsF = -1.0 / sigma;
  double sum_e = 0.0;
#pragma omp simd reduction(+:sum_e)
  for (int i = 0; i < n; ++i) {
    sum_e += std::exp((xp[i] - xbar) * invsF);
  }
  const double mu = -sigma * std::log(sum_e / n) + xbar;

  // Return as a length-3 named numeric vector
  Rcpp::NumericVector out(3);
  out[0] = mu;
  out[1] = sigma;
  out[2] = iter;
  out.names() = Rcpp::CharacterVector::create("mu", "sigma", "iterations");
  return out;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]
#include <RcppArmadillo.h>
#include <cmath>

#ifdef _OPENMP
#include <omp.h>
#endif

// common first step: mean & variance
static inline void mean_var(const double* __restrict__ x, int n,
                            double& xbar, double& var0) {
  double sum = 0.0, sum2 = 0.0;
#pragma omp parallel for reduction(+:sum,sum2) schedule(static)
  for (int i = 0; i < n; ++i) {
    double xi = x[i];
    sum  += xi;
    sum2 += xi*xi;
  }
  xbar = sum / n;
  var0 = sum2 / n - xbar*xbar;
}
// [[Rcpp::export]]
Rcpp::NumericVector ml_gumbel_halley2_rcpp(const Rcpp::NumericVector& x) {
  const int    n     = x.size();
  const double*__restrict__ xp = REAL(x);

  double xbar, var0;
  mean_var(xp, n, xbar, var0);

  double theta = std::log(std::sqrt(var0) * 2.45 / M_PI);
  const int    maxit = 100;
  const double tol   = std::sqrt(std::numeric_limits<double>::epsilon());
  int          iter  = 0;
  double last_p0     = 0.0;

  for (; iter < maxit; ++iter) {
    const double s0    = std::exp(theta);
    const double invs  = 1.0 / s0;
    const double invs2 = invs*invs;
    const double invs3 = invs2*invs;
    const double invs4 = invs2*invs2;

    double p0 = 0.0, p1 = 0.0, p2 = 0.0, p3 = 0.0;
#pragma omp parallel for if(n >= OMP_THRESHOLD) reduction(+:p0,p1,p2,p3) schedule(static)
    for (int i = 0; i < n; ++i) {
      double v = xp[i] - xbar;
      double e = std::exp(-v * invs);
      p0 += e; p1 += v*e; p2 += v*v*e; p3 += v*v*v*e;
    }

    last_p0 = p0;
    p1     /= p0;
    double raw2 = p2 / p0;
    double m2   = raw2 - p1*p1;
    double raw3 = p3 / p0;
    double m3   = raw3 - 3.0*p1*raw2 + 2.0*p1*p1*p1;

    double f   = s0 + p1;
    double fp  = 1.0 + m2 * invs2;
    double fpp = m3 * invs4 - 2.0*m2 * invs3;

    double s_new   = s0 - (2.0*f*fp) / (2.0*fp*fp - f*fpp);
    double theta_n = std::log(s_new);

    if (std::abs(theta_n - theta) < tol * std::abs(theta)) {
      theta = theta_n;
      break;
    }
    theta = theta_n;
  }

  const double sigma = std::exp(theta);
  double mu = -sigma * std::log(last_p0 / n) + xbar;

  return Rcpp::NumericVector::create(
    Rcpp::_["mu"]=mu,
    Rcpp::_["sigma"]=sigma,
    Rcpp::_["iterations"]=iter+1
  );
}

// [[Rcpp::export]]
Rcpp::NumericVector ml_gumbel_newt2_rcpp(const Rcpp::NumericVector& x) {
  const int    n     = x.size();
  const double*__restrict__ xp = REAL(x);

  double xbar, var0;
  mean_var(xp, n, xbar, var0);

  double theta = std::log(std::sqrt(var0) * 2.45 / M_PI);
  const int    maxit = 100;
  const double tol   = std::sqrt(std::numeric_limits<double>::epsilon());
  int          iter;
  double last_p0 = 0.0;

  for (iter = 1; iter <= maxit; ++iter) {
    const double s0    = std::exp(theta);
    const double invs  = 1.0 / s0;
    const double invs2 = invs*invs;

    double p0 = 0.0, p1 = 0.0, p2 = 0.0;
#pragma omp parallel for if(n >= OMP_THRESHOLD) reduction(+:p0,p1,p2) schedule(static)
    for (int i = 0; i < n; ++i) {
      double v = xp[i] - xbar;
      double e = std::exp(-v * invs);
      p0 += e; p1 += v*e; p2 += v*v*e;
    }

    last_p0 = p0;
    p1    /= p0;
    double var_e = p2/p0 - p1*p1;

    double f  = s0 + p1;
    double df = 1.0 + var_e * invs2;
    double dθ = f / (df * s0);
    theta   -= dθ;

    if (std::abs(dθ) < tol * std::abs(theta)) break;
  }

  const double sigma = std::exp(theta);
  double mu = -sigma * std::log(last_p0 / n) + xbar;

  return Rcpp::NumericVector::create(
    Rcpp::_["mu"]=mu,
    Rcpp::_["sigma"]=sigma,
    Rcpp::_["iterations"]=iter
  );
}


// [[Rcpp::export]]
Rcpp::NumericVector ml_gumbel_halley_fast(const Rcpp::NumericVector& x) {
    const int n = x.size();
    const double* __restrict__ xp = REAL(x);

    // 1) mean & variance
    double sum_x = 0.0, sum_x2 = 0.0;
    for (int i = 0; i < n; ++i) {
      double xi = xp[i];
      sum_x  += xi;
      sum_x2 += xi * xi;
    }
    const double xbar = sum_x / n;
    const double var0 = sum_x2 / n - xbar * xbar;

    // 2) initial σ via moments
    double sigma = std::sqrt(var0) * 2.45 / M_PI;
    double theta = std::log(sigma);

    // 3) Halley iterations
    const double reltol = std::sqrt(std::numeric_limits<double>::epsilon());
    int iter = 0;
    for (; iter < 100; ++iter) {
      const double s0    = std::exp(theta);
      const double invs  = 1.0 / s0;
      const double invs2 = invs * invs;
      const double invs3 = invs2 * invs;
      const double invs4 = invs2 * invs2;

      double p0 = 0.0, p1 = 0.0, p2 = 0.0, p3 = 0.0;
      for (int i = 0; i < n; ++i) {
        double v = xp[i] - xbar;
        double e = std::exp(-v * invs);
        p0 += e;
        p1 += v * e;
        p2 += v * v * e;
        p3 += v * v * v * e;
      }
      p1 /= p0;
      const double raw2 = p2 / p0;
      const double m2   = raw2 - p1 * p1;
      const double raw3 = p3 / p0;
      const double m3   = raw3 - 3.0 * p1 * raw2 + 2.0 * p1 * p1 * p1;

      // Halley update f, f′, f″
      const double f   = s0 + p1;
      const double fp  = 1.0 + m2 * invs2;
      const double fpp = m3 * invs4 - 2.0 * m2 * invs3;
      const double s_new = s0 - (2.0 * f * fp) / (2.0 * fp * fp - f * fpp);
      double theta_n = std::log(s_new);

      if (std::abs(theta_n - theta) < std::abs(theta) * reltol) {
        theta = theta_n;
        ++iter;
        break;
      }
      theta = theta_n;
    }

    // 4) finalize μ
    sigma = std::exp(theta);
    const double invsF = 1.0 / sigma;
    double sum_e = 0.0;
    for (int i = 0; i < n; ++i) {
      sum_e += std::exp(-(xp[i] - xbar) * invsF);
    }
    const double mu = -sigma * std::log(sum_e / n) + xbar;

    return Rcpp::NumericVector::create(
      Rcpp::Named("mu")         = mu,
      Rcpp::Named("sigma")      = sigma,
      Rcpp::Named("iterations") = iter
    );
  }

// Newton–Raphson on θ = log σ
// [[Rcpp::export]]
Rcpp::NumericVector ml_gumbel_newt_fast(const Rcpp::NumericVector& x) {
  const int n = x.size();
  const double* __restrict__ xp = REAL(x);

  // 1) mean & variance
  double sum_x = 0.0, sum_x2 = 0.0;
  for (int i = 0; i < n; ++i) {
    double xi = xp[i];
    sum_x  += xi;
    sum_x2 += xi * xi;
  }
  const double xbar = sum_x / n;
  const double var0 = sum_x2 / n - xbar * xbar;

  // 2) initial σ via moments
  double sigma = std::sqrt(var0) * 2.45 / M_PI;
  double theta = std::log(sigma);

  // Newton loop
  const double reltol = std::sqrt(std::numeric_limits<double>::epsilon());
  int iter = 0;
  for (iter = 1; iter <= 100; ++iter) {
    const double s0    = std::exp(theta);
    const double invs  = 1.0 / s0;
    const double invs2 = invs * invs;

    double psi0 = 0.0, psi1 = 0.0, psi2 = 0.0;
    for (int i = 0; i < n; ++i) {
      double v = xp[i] - xbar;
      double e = std::exp(-v * invs);
      psi0 += e;
      psi1 += v * e;
      psi2 += v * v * e;
    }
    psi1 /= psi0;
    psi2  = (psi2 / psi0) - psi1 * psi1;

    // f, f′
    const double f   = s0 + psi1;
    const double df  = 1.0 + psi2 * invs2;
    const double dth = f / (df * s0);

    theta -= dth;
    if (std::abs(dth) < std::abs(theta) * reltol) break;
  }

  // 3) finalize μ
  sigma = std::exp(theta);
  const double invsF = 1.0 / sigma;
  double sum_e = 0.0;
  for (int i = 0; i < n; ++i) {
    sum_e += std::exp(-(xp[i] - xbar) * invsF);
  }
  const double mu = -sigma * std::log(sum_e / n) + xbar;

  return Rcpp::NumericVector::create(
    Rcpp::Named("mu")         = mu,
    Rcpp::Named("sigma")      = sigma,
    Rcpp::Named("iterations") = iter
  );
}