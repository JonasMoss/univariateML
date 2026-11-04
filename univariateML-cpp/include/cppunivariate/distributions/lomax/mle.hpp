// Lomax profile search on the dimensionless bounded existence interval.

#pragma once
#include "../../core/precision.hpp"

#include "../../core/reductions.hpp"
#include "../../core/span.hpp"

#include <cmath>
#include <algorithm>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class LomaxStatus {
  Ok = 0,
  ExponentialLimit,
  InvalidStart,
  IterationFailure
};

enum class LomaxCertificateFailure { None, Budget, Arithmetic };

struct LomaxResult {
  double lambda;
  double kappa;
  double logLik;
  int iterations;
  LomaxStatus status;
  LomaxCertificateFailure certificate_failure = LomaxCertificateFailure::None;
};

namespace lomax_detail {
// Profile log-likelihood in lambda (kappa at its closed-form maximiser),
// up to a constant: n log(lambda) - n log(S) - S, S = sum log1p(lambda x).
inline double profile_loglik(span x, double lambda) {
  double S = 0.0;
  for (auto v : x) S += std::log1p(lambda * v);
  const double n = static_cast<double>(x.size());
  return n * std::log(lambda) - n * std::log(S) - S;
}
}  // namespace lomax_detail

// lambda0: optional start for lambda; NaN (the default) or a non-positive
// value means the method-of-moments start.
inline LomaxResult lomax_newton_candidate(
    span x, double tol, int maxit,
    double lambda0 = std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0 || maxit<1) return {0.0, 0.0, kNaN, 0, LomaxStatus::InvalidStart};

  CPPUNIV_EXTENDED_REAL total = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (auto v : x) total += v;
  const double unit = static_cast<double>(total / static_cast<CPPUNIV_EXTENDED_REAL>(n));
  if (unit > 0.0 && std::isfinite(unit) && (unit < 1e-3 || unit > 1e3)) {
    std::vector<double> z(n);
    for (std::size_t i = 0; i < n; ++i) z[i] = x[i] / unit;
    const double start = std::isfinite(lambda0) && lambda0 > 0.0 ? lambda0 * unit : lambda0;
    const auto r = lomax_newton_candidate(span{z.data(), n}, tol, maxit, start);
    return {r.lambda/unit, r.kappa, r.logLik-static_cast<double>(n)*std::log(unit), r.iterations, r.status};
  }

  const double m = cppunivariate::mean(x);
  double sum_xsq = 0.0;
  for (auto v : x) sum_xsq += v * v;
  const double s = sum_xsq / static_cast<double>(n);
  const double m2 = m * m;

  const double alpha0 = 2.0 * (s - m2) / (s - 2.0 * m2);
  double lambda = (std::isfinite(lambda0) && lambda0 > 0.0)
                      ? lambda0
                      : 1.0 / (m * (alpha0 - 1.0));
  if (std::isfinite(lambda0) && lambda0>0.0 &&
      (lambda0*m < 1e-8 || lambda0*m > 1e8))
    return lomax_newton_candidate(x,tol,maxit); // A starting hint outside resolvable score arithmetic.
  if (lambda <= 0.0 || !std::isfinite(lambda)) {
    return {kNaN, kNaN, kNaN, 0, LomaxStatus::InvalidStart};
  }

  int iter = 1;
  for (; iter <= maxit; ++iter) {
    double S = 0.0, S1 = 0.0, S2 = 0.0;
    for (auto v : x) {
      const double xb = 1.0 + lambda * v;
      S += lambda*v < 1e-8 ? std::log1p(lambda*v) : std::log(xb);
      const double xd = v / xb;
      S1 += xd;
      S2 += xd * xd;
    }
    const double dn = static_cast<double>(n);
    S /= dn;
    S1 /= dn;
    S2 /= dn;

    const double psi = S1 / S;
    const double f = 1.0 / lambda - psi - S1;
    const double df = psi * psi + S2 / S + S2 - 1.0 / (lambda * lambda);

    // f is the profile score (per observation) and df its derivative.
    // Newton is an ascent step only where the profile is concave (df < 0);
    // elsewhere (possible from a user-supplied lambda0 far from the MLE)
    // move geometrically uphill. Every step is halved until the profile
    // log-likelihood does not decrease, and convergence is only declared
    // on a full Newton step, so a shrunken step cannot fake convergence.
    // From the moment start the plain Newton step is normally accepted.
    const bool newton = df < 0.0;
    double step = newton ? f / df : (f > 0.0 ? -lambda : 0.5 * lambda);
    if (!std::isfinite(step)) {
      return {kNaN, kNaN, kNaN, iter, LomaxStatus::IterationFailure};
    }
    if (lambda - step <= 0.0) step = 0.5 * lambda;
    const double lp_old = lomax_detail::profile_loglik(x, lambda);
    int halvings = 0;
    for (; halvings < 60; ++halvings) {
      const double lp_new = lomax_detail::profile_loglik(x, lambda - step);
      if (lp_new >= lp_old - 1e-12 * (std::abs(lp_old) + 1.0)) break;
      step *= 0.5;
    }
    if (halvings == 60)
      return {kNaN,kNaN,kNaN,iter,LomaxStatus::IterationFailure};
    const double lambda_new = lambda - step;
    if (newton && halvings == 0 &&
        std::abs(step) < (std::abs(lambda) * tol + tol)) {
      lambda = lambda_new;
      break;
    }
    lambda = lambda_new;
  }

  double sum_log1p = 0.0;
  for (auto v : x) sum_log1p += std::log1p(lambda * v);
  const double dn = static_cast<double>(n);
  const double kappa = dn / sum_log1p;
  const double logLik =
      dn * (std::log(kappa) + std::log(lambda) - 1.0 - 1.0 / kappa);
  if (iter>maxit) {
    double prob_sum=0;
    for (double v:x) {
      const double z=std::log(lambda)+std::log(v);
      prob_sum+=z>=0 ? 1/(1+std::exp(-z)) : std::exp(z)/(1+std::exp(z));
    }
    const double score=dn-(kappa+1)*prob_sum;
    if (!(std::isfinite(score) && std::abs(score)<1e-3))
      return {lambda,kappa,logLik,iter,LomaxStatus::IterationFailure};
  }
  return {lambda, kappa, logLik, iter, LomaxStatus::Ok};
}

// The global certificate is separate from Newton proposal generation.
} // namespace cppunivariate
#include "certificate.hpp"
namespace cppunivariate {
inline LomaxResult lomax_mle(span x, double tol, int maxit,
    double lambda0 = std::numeric_limits<double>::quiet_NaN()) {
  return lomax_cert_detail::fit(x,tol,maxit,lambda0);
}

inline LomaxResult lomax_mle(span x,double tol,int maxit,double lambda0,double lambda,double kappa) {
  if(std::isnan(lambda) && std::isnan(kappa))return lomax_mle(x,tol,maxit,lambda0);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.size()==0 || !(tol>0 && std::isfinite(tol)) || maxit<=0 || (!std::isnan(lambda) && !(lambda>0 && std::isfinite(lambda))) ||
     (!std::isnan(kappa) && !(kappa>0 && std::isfinite(kappa))))return {nan,nan,nan,0,LomaxStatus::InvalidStart};
  for(double v:x)if(!(v>0 && std::isfinite(v)))return {nan,nan,nan,0,LomaxStatus::InvalidStart};
  const double n=static_cast<double>(x.size());int iterations=0;
  if(std::isnan(lambda)) {
    // Strictly decreasing total log-rate score, computed with log weights.
    // An unrepresentable root is a numerical failure, never a capped estimate.
    const double lower=std::log(std::numeric_limits<double>::denorm_min()),upper=std::log(std::numeric_limits<double>::max());
    auto score=[&](double t){double g=n;for(double v:x){const double z=t+std::log(v);const double logp=-std::max(-z,0.0)-std::log1p(std::exp(-std::abs(z)));g-=std::exp(std::log1p(kappa)+logp);}return g;};
    double lo=std::max(lower,-std::log(x[0])-std::log(kappa)-2),hi=std::min(upper,lo+4),step=32;
    lo=std::min(lo,upper);
    while(score(lo)<=0 && lo>lower){lo=std::max(lower,lo-step);step*=2;}
    step=32;while(score(hi)>=0 && hi<upper){hi=std::min(upper,hi+step);step*=2;}
    if(!(score(lo)>0 && score(hi)<0))return {nan,kappa,nan,iterations,LomaxStatus::IterationFailure};
    for(int j=0;j<maxit && hi-lo>1e-12;++j){const double mid=lo/2+hi/2;if(score(mid)>0)lo=mid;else hi=mid;++iterations;}
    lambda=std::exp(lo/2+hi/2);
    if(!(lambda>0 && std::isfinite(lambda) && std::abs(score(std::log(lambda)))<1e-3))return {nan,kappa,nan,iterations,LomaxStatus::IterationFailure};
  }
  double s=0;for(double v:x){const double z=std::log(lambda)+std::log(v);s+=std::max(z,0.0)+std::log1p(std::exp(-std::abs(z)));}
  if(std::isnan(kappa))kappa=n/s;
  const double ll=n*(std::log(lambda)+std::log(kappa))-(kappa+1)*s;
  if(!(lambda>0 && std::isfinite(lambda) && kappa>0 && std::isfinite(kappa) && std::isfinite(ll)))
    return {lambda,kappa,ll,iterations,LomaxStatus::IterationFailure};
  return {lambda,kappa,ll,iterations,LomaxStatus::Ok};
}

}  // namespace cppunivariate
