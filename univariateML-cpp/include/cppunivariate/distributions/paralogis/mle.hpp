// Paralogistic MLE on (shape, scale).
//
// The previous R-side fit was 1-D: it Newton-iterated on shape with scale
// silently fixed at 1, which only happens to be correct on data that was
// sampled with the actuar default. This implementation fits both
// parameters jointly via BFGS on the log-reparameterised cost
// (phi = (log shape, log scale)), then certifies the proposed maximizer
// by root enclosure and compact-domain exclusion (certificate.hpp).
//
// Density (actuar::dparalogis):
//   f(x; alpha, theta) = alpha^2 * (x/theta)^alpha
//                        / [x * (1 + (x/theta)^alpha)^(alpha + 1)]
//
// Log-likelihood at (alpha, theta):
//   l = n * 2*log(alpha)
//       + alpha * sum(log(x_i / theta))
//       - sum(log x_i)
//       - (alpha + 1) * sum(log1p((x_i / theta)^alpha))
//
// Start values: theta0 = median(x),
//               alpha0 = max(1.5, 2 / sd(log(x / theta0))).

#pragma once
#include "../../core/work_budget.hpp"
#include "../burr/mle.hpp"
#include "certificate.hpp"
#include "accuracy.hpp"

#include "../../core/bfgs.hpp"
#include "../../core/finite_diff.hpp"
#include "../../core/reparam.hpp"
#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class ParalogisStatus { Ok = 0, BfgsFailure, InvalidStart, Unbounded, NumericalFailure, CertificateBudget };

struct ParalogisResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
  ParalogisStatus status;
  StopDetail stop_detail = {};
  bool certified=true;
};

// Negative log-likelihood as a BFGS Cost, parameterised in phi-space:
//   phi[0] = u = log(shape), phi[1] = v = log(scale).
//
// Per-observation: with z_i = log(x_i) - v, t_i = alpha * z_i,
//                  p_i = sigmoid(t_i), q_i = p_i (1 - p_i),
//                  s_i = log1p(exp(t_i)),
//   nll = -2n*u + sum_i [ -alpha*z_i + (alpha+1)*s_i ] + const.
// Hessian (in (u, v) space):
//   H_uu = -sum t + (3a+1) sum (p t) + (a+1) sum (q t^2) + a * sum s
//   H_uv = n*a - a*(2a+1) sum p - a*(a+1) sum (q t)
//   H_vv = a^2 (a+1) sum q
// Sympy-verified against numerical derivatives at random points.
struct ParalogisCost {
  span x;
  double divisor = 1.0;
  int dim() const { return 2; }
  double operator()(const double *phi) const {
    const double alpha = cppunivariate::from_log(phi[0]);
    const double theta = cppunivariate::from_log(phi[1]);
    if (!std::isfinite(alpha) || !std::isfinite(theta) || alpha <= 0.0 ||
        theta <= 0.0) {
      return std::numeric_limits<double>::infinity();
    }
    const std::size_t n = x.size();
    double sum_log_xt = 0.0;  // sum log(x_i / theta)
    double sum_log_x = 0.0;
    double sum_log1p_r = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
      const double lx = std::log(x[i]);
      sum_log_x += lx;
      const double log_z = lx - phi[1];  // log(x_i / theta)
      sum_log_xt += log_z;
      const double t = alpha * log_z;
      sum_log1p_r += t>500.0 ? t+std::log1p(std::exp(-t)) : std::log1p(std::exp(t));
    }
    const double dn = static_cast<double>(n);
    const double logLik = dn * 2.0 * std::log(alpha) + alpha * sum_log_xt -
                          sum_log_x - (alpha + 1.0) * sum_log1p_r;
    return -logLik/divisor;
  }
  void hessian(const double *phi, double *H) const {
    const double alpha = cppunivariate::from_log(phi[0]);
    const double theta = cppunivariate::from_log(phi[1]);
    if (!std::isfinite(alpha) || !std::isfinite(theta) || alpha <= 0.0 ||
        theta <= 0.0) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      H[0] = H[1] = H[2] = H[3] = nan;
      return;
    }
    const double dn = static_cast<double>(x.size());
    double sum_t = 0.0, sum_p = 0.0, sum_q = 0.0;
    double sum_pt = 0.0, sum_qt = 0.0, sum_qt2 = 0.0, sum_s = 0.0;
    for (auto v_x : x) {
      const double t = alpha * (std::log(v_x) - phi[1]);
      // Numerically stable sigmoid / softplus / sigmoid-derivative.
      double p, q, s;
      if (t >= 0.0) {
        const double e_neg = std::exp(-t);
        const double one_plus = 1.0 + e_neg;
        p = 1.0 / one_plus;
        q = e_neg / (one_plus * one_plus);
        s = t + std::log1p(e_neg);
      } else {
        const double e_pos = std::exp(t);
        const double one_plus = 1.0 + e_pos;
        p = e_pos / one_plus;
        q = e_pos / (one_plus * one_plus);
        s = std::log1p(e_pos);
      }
      sum_t += t;
      sum_p += p;
      sum_q += q;
      sum_pt += p * t;
      sum_qt += q * t;
      sum_qt2 += q * t * t;
      sum_s += s;
    }
    const double a1 = alpha + 1.0;
    H[0] = -sum_t + (3.0 * alpha + 1.0) * sum_pt + a1 * sum_qt2 + alpha * sum_s;
    H[1] = dn * alpha - alpha * (2.0 * alpha + 1.0) * sum_p - alpha * a1 * sum_qt;
    H[2] = H[1];
    H[3] = alpha * alpha * a1 * sum_q;
    for(int i=0;i<4;++i) H[i]/=divisor;
  }
};

// Historical one-dimensional shape score, with z=log(x/scale).
// Safeguarded Newton keeps the shape positive and brackets its score root.
inline ParalogisResult paralogis_shape_proposal(
    span x, double scale, double tol = 1e-10, int maxit = 100,
    double shape0 = 1.0) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (x.size() == 0 || !(scale > 0) || !std::isfinite(scale))
    return {nan,scale,nan,0,ParalogisStatus::InvalidStart};
  for (double v : x)
    if (!(v > 0) || !std::isfinite(v))
      return {nan,scale,nan,0,ParalogisStatus::InvalidStart};
  auto evaluate = [&](double a, double &score, double &hessian) {
    score = 2.0*x.size()/a;
    hessian = -2.0*x.size()/(a*a);
    double ll = 2.0*x.size()*std::log(a);
    for (double v : x) {
      const double z = std::log(v)-std::log(scale), t = a*z;
      const double e = std::exp(-std::abs(t));
      const double p = t >= 0 ? 1/(1+e) : e/(1+e);
      const double soft = std::max(t,0.0)+std::log1p(e);
      score += z-soft-(a+1)*z*p;
      hessian -= 2*z*p+(a+1)*z*z*e/((1+e)*(1+e));
      ll += t-std::log(v)-(a+1)*soft;
    }
    return ll;
  };
  double lo = 0, hi = 1, score, hessian;
  evaluate(hi,score,hessian);
  for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (score > 0 && hi < 1e150); ++bracket_pass) {
    hi *= 2;
    evaluate(hi,score,hessian);
  }
  if(!(score<=0))return {NAN,scale,NAN,0,ParalogisStatus::BfgsFailure};
  double a = shape0 > lo && shape0 < hi ? shape0 : hi/2;
  for (int iter=1; iter<=maxit; ++iter) {
    const double ll = evaluate(a,score,hessian);
    if (std::abs(score*a) < std::min(tol,1e-10)*x.size())
      return {a,scale,ll,iter,ParalogisStatus::Ok};
    if (score > 0) lo=a; else hi=a;
    const double next = a-score/hessian;
    a = next > lo && next < hi ? next : (lo+hi)/2;
  }
  const double ll = evaluate(a,score,hessian);
  return {a,scale,ll,maxit,ParalogisStatus::BfgsFailure};
}

// shape0: optional start for the shape; NaN (the default) or a non-positive
// value means the automatic start below.
inline ParalogisResult paralogis_mle_impl(
    span x, double tol, int maxit,
    double shape0, bool standardized) {
  maxit = static_cast<int>(solver_budget(maxit));
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n < 2) return {kNaN, kNaN, kNaN, 0, ParalogisStatus::InvalidStart};
  for (auto v : x) {
    if (!std::isfinite(v) || v <= 0.0) {
      return {kNaN, kNaN, kNaN, 0, ParalogisStatus::InvalidStart};
    }
  }

  if (std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))
    return {kNaN,kNaN,kNaN,0,ParalogisStatus::InvalidStart};

  // Median start for theta.
  std::vector<double> sorted(x.begin(), x.end());
  std::sort(sorted.begin(), sorted.end());
  const double theta0 = (n % 2 == 0)
                            ? 0.5 * (sorted[n / 2 - 1] + sorted[n / 2])
                            : sorted[n / 2];
  if (theta0 <= 0.0) {
    return {kNaN, kNaN, kNaN, 0, ParalogisStatus::InvalidStart};
  }

  // Optimise a dimensionless scale: FD increments and BFGS stopping rules
  // must not depend on the units of x or an additive likelihood constant.
  if (!standardized) {
    std::vector<double> z(n);
    for (std::size_t i = 0; i < n; ++i) {
      z[i] = x[i] / theta0;
      if(!(z[i]>0)||!std::isfinite(z[i]))
        return {kNaN,kNaN,kNaN,0,ParalogisStatus::NumericalFailure};
    }
    auto r = paralogis_mle_impl(span{z.data(), n}, tol, maxit, shape0, true);
    r.scale *= theta0;
    r.logLik -= static_cast<double>(n) * std::log(theta0);
    return r;
  }

  double mean_lz = 0.0;
  for (auto v : x) mean_lz += ((std::isfinite(v/theta0) && v/theta0>0.0) ? std::log(v/theta0) : std::log(v)-std::log(theta0));
  mean_lz /= static_cast<double>(n);
  double var_lz = 0.0;
  for (auto v : x) {
    const double d = ((std::isfinite(v/theta0) && v/theta0>0.0) ? std::log(v/theta0) : std::log(v)-std::log(theta0)) - mean_lz;
    var_lz += d * d;
  }
  var_lz /= static_cast<double>(n - 1);
  const double sd_lz = std::sqrt(var_lz);
  double alpha0 = (sd_lz > 0.0) ? 2.0 / sd_lz : 1.5;
  if (alpha0 < 1.5 && sd_lz < 10.0) alpha0 = 1.5;
  if (std::isfinite(shape0) && shape0 > 0.0) alpha0 = shape0;

  const double objective_divisor = n>=100000 ? static_cast<double>(n) : 1.0;
  ParalogisCost cost{x,objective_divisor};
  finite_diff_cost<ParalogisCost> fd{cost};
  const double phi0[2] = {cppunivariate::to_log(alpha0), cppunivariate::to_log(theta0)};

  BfgsControl ctl;
  ctl.grad_tol = tol;
  ctl.maxit = maxit;
  auto r = bfgs_minimize(fd, phi0, ctl);
  r.f *= objective_divisor;
  r.grad_norm *= objective_divisor;

  const double shape = cppunivariate::from_log(r.theta[0]);
  const double scale = cppunivariate::from_log(r.theta[1]);
  const double logLik = -r.f;
  if (r.status != BfgsStatus::Ok && !(std::isfinite(r.grad_norm) && r.grad_norm < 1e-3)) {
    return {shape, scale, logLik, r.iterations, ParalogisStatus::BfgsFailure};
  }
  return {shape, scale, logLik, r.iterations, ParalogisStatus::Ok};
}

namespace paralogis_detail {
// Attach comparisons to a search nomination; public returns refine it below.
inline ParalogisResult uncertified(ParalogisResult searched,
    const paralogis_cert_detail::Result& certificate,span x) {
  searched.certified=false;
  searched.stop_detail=paralogis_cert_detail::witness(x,searched.shape,searched.scale);
  if(searched.stop_detail.candidates.empty() || !std::isfinite(searched.logLik)) {
    searched.status=ParalogisStatus::NumericalFailure;return searched;
  }
  const auto physical=searched.stop_detail.candidates[0];
  if(searched.logLik<physical.loglik_lower || searched.logLik>physical.loglik_upper)
    searched.status=ParalogisStatus::NumericalFailure;
  const auto competing=paralogis_cert_detail::witness(x,certificate.shape,certificate.scale);
  for(const auto& candidate:competing.candidates) {
    searched.stop_detail.candidates.push_back(candidate);
    searched.stop_detail.loglik_lower=std::max(searched.stop_detail.loglik_lower,candidate.loglik_lower);
    if(candidate.loglik_lower>physical.loglik_upper)searched.status=ParalogisStatus::NumericalFailure;
  }
  return searched;
}
inline ParalogisResult accurate_uncertified(ParalogisResult searched,
    const paralogis_cert_detail::Result& certificate,span x,double tol,int maxit,bool fixed_scale) {
  searched=uncertified(searched,certificate,x);
  if(searched.status!=ParalogisStatus::Ok)return searched;
  if(!paralogis_local_accuracy(x,searched.shape,searched.scale,searched.logLik,tol,maxit,fixed_scale)){searched.status=ParalogisStatus::NumericalFailure;return searched;}
  searched.stop_detail=paralogis_cert_detail::witness(x,searched.shape,searched.scale);
  return searched;
}
}

inline ParalogisResult paralogis_mle(span x, double tol, int maxit,
    double shape0 = std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  for(double v:x) if(!(v>0)||!std::isfinite(v))
    return {NAN,NAN,NAN,0,ParalogisStatus::InvalidStart};
  if(!x.empty() && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];})) {
    const double nan=std::numeric_limits<double>::quiet_NaN();
    StopDetail detail;detail.loglik_lower=detail.loglik_upper=INFINITY;
    return {nan,nan,INFINITY,0,ParalogisStatus::Unbounded,detail};
  }
  auto r = paralogis_mle_impl(x, tol, maxit, shape0, false);
  r.stop_detail=paralogis_cert_detail::witness(x,r.shape,r.scale);
  if (r.status != ParalogisStatus::Ok) return r;
  const auto certified = paralogis_cert_detail::certify(x,r.shape,r.scale,false,certificate_budget(maxit),maxit);
  if (!certified.ok) return paralogis_detail::accurate_uncertified(r,certified,x,tol,maxit,false);
  r.shape=certified.shape; r.scale=certified.scale; r.logLik=certified.logLik;
  return r;
}

// Rescue proposals only: a dyadic scan can miss roots. Global acceptance
// still requires the complete compact-domain certificate below.
inline double paralogis_competing_shape(span x,double scale,double initial,int maxit=100) {
  maxit = static_cast<int>(solver_budget(maxit));
  std::vector<double> y;
  double distance=0;
  for(double v:x) {y.push_back(std::log(v)-std::log(scale));
    distance=std::max(distance,std::abs(y.back()));}
  if(!(distance>0))return initial;
  auto evaluate=[&](double a,double& score) {
    score=2*x.size()/a;
    double value=2*x.size()*std::log(a);
    for(double v:y) {
      const double z=a*v;
      const double p=burr_cert_detail::sigmoid(z),s=burr_cert_detail::softplus(z);
      value+=z-(a+1)*s;score+=v-s-(a+1)*v*p;
    }
    return value;
  };
  double score=0,best=evaluate(initial,score),winner=initial;
  double left=std::min(1.,initial),gl=0;
  evaluate(left,gl);
  for(int i=0;i<maxit;++i) {
    const double right=2*left;
    if(!std::isfinite(right))break;
    double gr=0;const double value=evaluate(right,gr);
    if(std::isfinite(value)&&value>best){best=value;winner=right;}
    if(gl>0&&gr<0) {
      double lo=left,hi=right;
      for(int j=0;j<maxit;++j) {
        const double mid=lo+(hi-lo)/2;
        if(!(mid>lo&&mid<hi))break;
        double g=0;evaluate(mid,g);if(g>0)lo=mid;else hi=mid;
      }
      const double root=lo+(hi-lo)/2;
      double g=0;const double v=evaluate(root,g);
      if(std::isfinite(v)&&v>best){best=v;winner=root;}
    }
    // This cutoff merely ends proposal generation. The certificate
    // independently re-establishes both tails with padded comparisons.
    if(right>std::max(1.,2*x.size()/distance) &&
        2*x.size()*std::log(right)-distance*right<best)break;
    left=right;gl=gr;
  }
  return winner;
}

inline ParalogisResult paralogis_mle_fixed_scale(
    span x, double scale, double tol=1e-10, int maxit=100,
    double shape0=1.0) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto r=paralogis_shape_proposal(x,scale,tol,maxit,shape0);
  r.stop_detail=paralogis_cert_detail::witness(x,r.shape,r.scale);
  if(r.status!=ParalogisStatus::Ok) return r;
  auto certified=paralogis_cert_detail::certify(x,r.shape,scale,true,certificate_budget(maxit),maxit);
  if(!certified.ok) {
    const double alternative=paralogis_competing_shape(x,scale,r.shape,maxit);
    if(alternative!=r.shape)
      certified=paralogis_cert_detail::certify(x,alternative,scale,true,certificate_budget(maxit),maxit);
  }
  if(!certified.ok) return paralogis_detail::accurate_uncertified(r,certified,x,tol,maxit,true);
  r.shape=certified.shape;r.logLik=certified.logLik;
  return r;
}

}  // namespace cppunivariate

namespace cppunivariate {

inline ParalogisResult paralogis_mle_holds(span x,double shape,double scale=std::numeric_limits<double>::quiet_NaN(), double tol=1e-9, int maxit=1000){
  maxit = static_cast<int>(solver_budget(maxit));
 if(std::isnan(shape)) return std::isnan(scale)?paralogis_mle(x,tol,maxit):paralogis_mle_fixed_scale(x,scale,tol,maxit);
 auto r=burr_mle_holds(x,shape,shape,scale,tol,maxit);
 return {shape,r.scale,r.logLik,r.iterations,r.status==BurrStatus::Ok?ParalogisStatus::Ok:r.status==BurrStatus::IterationFailure?ParalogisStatus::BfgsFailure:ParalogisStatus::InvalidStart};
}

}
