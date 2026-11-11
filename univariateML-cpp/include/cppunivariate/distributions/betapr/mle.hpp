// Beta-prime fitters: certified conditional profiles and all-free cover.
#pragma once
#include "joint_bounds.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/face_compare.hpp"
#include "../../core/precision.hpp"
#include "../../core/multistart.hpp"
#include "../../core/bfgs.hpp"
#include "../../core/span.hpp"
#include "../../special/special.hpp"
#include "../gamma/mle.hpp"
#include "../beta/mle.hpp"
#include "centered.hpp"
#include <vector>
#include <algorithm>
#include "../invgamma/mle.hpp"
#include "../lomax/mle.hpp"
#include "../burr/lomax_face.hpp"
#include "../invpareto/mle.hpp"
#include <cmath>
#include <cstddef>
#include <limits>
namespace cppunivariate {
enum class BetaPrimeStatus { Ok, BfgsFailure, InvalidStart,
                             GammaBoundary, InvGammaBoundary, ExpBoundary, InvExpBoundary };
struct BetaPrimeResult {
  double alpha, beta, scale, logLik;
  int iterations;
  BetaPrimeStatus status;
  StopDetail stop_detail{};
  LomaxCertificateFailure certificate_failure=LomaxCertificateFailure::None;
  BetaPrimeJointBounds joint_exterior{};
  bool certified=true;
  bool search_failed=false;
};
// Negative log-likelihood in the two shapes at a supplied scale.
// Default scale 1 also preserves the legacy validation-harness seam.
// Hessian (closed form, data-independent — same shape as Beta on the
// transformed variable):
//   H_aa = n*(trigamma(a) - trigamma(a+b))
//   H_bb = n*(trigamma(b) - trigamma(a+b))
//   H_ab = -n*trigamma(a+b)
struct BetaPrimeCost {
  span x;
  double scale = 1.0;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double a = theta[0];
    const double b = theta[1];
    if (!(a > 0.0 && b > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1px = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      S_lx += std::log(v / scale);
      S_l1px += std::log1p(v / scale);
    }
    return n * std::log(scale) - (a - 1.0) * S_lx + (a + b) * S_l1px +
           n * (cppunivariate::special::lgamma(a) + cppunivariate::special::lgamma(b) -
                cppunivariate::special::lgamma(a + b));
  }
  void gradient(const double *theta, double *g) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1px = 0.0;
    for (auto v : x) { S_lx += std::log(v / scale); S_l1px += std::log1p(v / scale); }
    const double psi_ab = cppunivariate::special::digamma(a + b);
    g[0] = -S_lx + S_l1px + n * (cppunivariate::special::digamma(a) - psi_ab);
    g[1] = S_l1px + n * (cppunivariate::special::digamma(b) - psi_ab);
  }
  void hessian(const double *theta, double *H) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    const double psi1_ab = cppunivariate::special::trigamma(a + b);
    H[0] = n * (cppunivariate::special::trigamma(a) - psi1_ab);
    H[1] = -n * psi1_ab;
    H[2] = -n * psi1_ab;
    H[3] = n * (cppunivariate::special::trigamma(b) - psi1_ab);
  }
};


// Log coordinates; the fixed-scale shape cost is also the production objective.
struct BetaPrimeLogCost {
  span x;
  mutable std::int64_t evaluations=0;
  std::int64_t evaluation_limit=std::numeric_limits<std::int64_t>::max();
  mutable bool exhausted=false;
  bool tick() const {
    if(evaluations>=evaluation_limit){exhausted=true;return false;}
    ++evaluations;return true;
  }
  int dim() const { return 3; }
  double operator()(const double *phi) const {
    if(!tick())return INFINITY;
    const double a = std::exp(phi[0]), b = std::exp(phi[1]);
    const double scale = std::exp(phi[2]);
    // Avoid cancellation at numerically indistinguishable infinite-shape faces.
    if (!(a > 0 && b > 0 && scale > 0) || !std::isfinite(scale) ||
        a > 1e6 || b > 1e6) return INFINITY;
    const double theta[2] = {a, b};
    return BetaPrimeCost{x, scale}(theta);
  }
  void gradient(const double *phi, double *g) const {
    if(!tick()){g[0]=g[1]=g[2]=INFINITY;return;}
    const double a = std::exp(phi[0]), b = std::exp(phi[1]);
    const double scale = std::exp(phi[2]);
    const double theta[2] = {a, b};
    BetaPrimeCost{x, scale}.gradient(theta, g);
    g[0] *= a;
    g[1] *= b;
    g[2] = 0;
    for (double v : x) g[2] += a - (a + b) * (v / (scale + v));
  }
};

// Fixed scale: the parameter-independent transformation Y=X/(s+X)
// gives the ordinary beta likelihood. Nonconstant interior observations
// have a unique finite beta MLE; no gamma or inverse-gamma face remains.
inline BetaPrimeResult betapr_mle_fixed_scale(
    span x, double scale, double tol = 1e-6, int maxit = 1000) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (!(scale > 0) || !std::isfinite(scale) || x.size() < 2)
    return {nan,nan,scale,nan,0,BetaPrimeStatus::InvalidStart};
  if (!(tol>0) || !std::isfinite(tol) || maxit<=0)
    return {nan,nan,scale,nan,0,BetaPrimeStatus::InvalidStart};
  std::vector<double> y;
  bool different = false;
  double jacobian=0, relative_spread=0;
  const bool flip=x[0]>scale;
  for (double v : x) {
    if (!(v > 0) || !std::isfinite(v))
      return {nan,nan,scale,nan,0,BetaPrimeStatus::InvalidStart};
    different = different || v != x[0];
    relative_spread=std::max(relative_spread,std::abs((v-x[0])/x[0]));
    // Transform the smaller tail; either orientation has the same Jacobian.
    const double z=flip?scale/v:v/scale;
    y.push_back(z/(1+z));
    jacobian+=std::log(scale)-2*(std::max(std::log(scale),std::log(v))+
        std::log1p(std::min(scale,v)/std::max(scale,v)));
  }
  if (!different)
    return {nan,nan,scale,nan,0,BetaPrimeStatus::InvalidStart};
  const bool centered=relative_spread<=1e-3;
  bool arithmetic_failure=false;
  auto r=centered?betapr_detail::centered_shapes(x,scale,tol,maxit,arithmetic_failure):
      beta_mle(span{y.data(),y.size()},std::min(tol,1e-12),maxit);
  if(flip && !centered)std::swap(r.alpha,r.beta);
  const double ll=r.logLik+jacobian;
  BetaPrimeResult result{r.alpha,r.beta,scale,ll,r.iterations,
      r.status==BetaStatus::Ok && std::isfinite(ll) ? BetaPrimeStatus::Ok : BetaPrimeStatus::BfgsFailure};
  if(arithmetic_failure || (r.status==BetaStatus::Ok && !std::isfinite(ll)))
    result.certificate_failure=LomaxCertificateFailure::Arithmetic;
  return result;
}

inline BetaPrimeResult betapr_mle(span x, double tol = 1e-6, int maxit = 2000,
    double shape1 = 0.0, double shape2 = 0.0, double scale = 0.0,
    double reltol = 1e-13, double abstol = 1e-10);
} // namespace cppunivariate
#include "certificate.hpp"
namespace cppunivariate {
// Hold-aware profiles. The conditional interface is also used by GB2.
namespace betapr_joint_cert { inline double log_beta(double p,double q); }
inline BetaPrimeResult betapr_mle_holds(span x,double a,double b,double s,double tol=1e-6,int maxit=1000);
namespace betapr_detail {
inline double shape_root(double other, double target,int maxit=1000) {
  maxit = static_cast<int>(solver_budget(maxit));
  // An underflowed statistic cannot determine a relative shape root. Keep
  // at least 128 subnormal quanta inside the requested score resolution.
  if(!(target>128*std::numeric_limits<double>::denorm_min()/1e-10) || !std::isfinite(target))return NAN;
  double lo=-40,hi=std::log(1e8);
  for(int j=0;j<maxit;++j) {
    const double t=(lo+hi)/2,v=std::exp(t);
    const double score=beta_detail::psi_increment(v,other)-target;
    if(score>0) lo=t;else hi=t;
  }
  if(hi-lo>1e-13)return NAN;
  const double value=std::exp((lo+hi)/2);
  if(value<20 && other<.01*value &&
      other/(value+other)<128*std::numeric_limits<double>::denorm_min()/1e-10)return NAN;
  return value;
}
}
inline BetaPrimeResult betapr_mle_holds_certificate(span x, double held_a, double held_b,
    double held_s, double tol=1e-6,int maxit=1000) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const bool fa=!std::isnan(held_a),fb=!std::isnan(held_b),fs=!std::isnan(held_s);
  BetaPrimeResult invalid{nan,nan,nan,nan,0,BetaPrimeStatus::InvalidStart};
  if(x.empty() || !(tol>0) || !std::isfinite(tol) || maxit<=0 ||
    (fa&&(!(held_a>0)||!std::isfinite(held_a))) ||
    (fb&&(!(held_b>0)||!std::isfinite(held_b))) ||
    (fs&&(!(held_s>0)||!std::isfinite(held_s)))) return invalid;
  if(held_a==1) {
    auto r=lomax_mle(x,tol,maxit,NAN,fs?1/held_s:NAN,held_b);
    if(r.status==LomaxStatus::ExponentialLimit && !fallback_lomax_face_proved(x,maxit)) {
      r.status=LomaxStatus::IterationFailure;
      fallback_retain_unclosed_face(r.stop_detail,StopModel::Exp);
    }
    auto d=r.stop_detail;
    for(auto& c:d.candidates)if(c.model==StopModel::None && c.coef.size()==2)c.coef={1,c.coef[1],1/c.coef[0]};
    return {1,r.kappa,fs?held_s:1/r.lambda,r.logLik,r.iterations,
      r.status==LomaxStatus::Ok?BetaPrimeStatus::Ok:
      r.status==LomaxStatus::ExponentialLimit?BetaPrimeStatus::ExpBoundary:
      r.status==LomaxStatus::InvalidStart?BetaPrimeStatus::InvalidStart:BetaPrimeStatus::BfgsFailure,d,r.certificate_failure};
  }
  if(held_b==1) {
    auto r=invpareto_mle(x,tol,maxit,NAN,held_a,held_s);
    if(r.status==LomaxStatus::ExponentialLimit){
      std::vector<double> reciprocal;for(double v:x)reciprocal.push_back(1/v);
      if(!fallback_lomax_face_proved(span{reciprocal.data(),reciprocal.size()},maxit)) {
        r.status=LomaxStatus::IterationFailure;
        fallback_retain_unclosed_face(r.stop_detail,StopModel::InvExp);
      }
    }
    auto d=r.stop_detail;
    for(auto& c:d.candidates)if(c.model==StopModel::None && c.coef.size()==2)c.coef={c.coef[0],1,c.coef[1]};
    return {r.shape,1,r.scale,r.logLik,r.iterations,
      r.status==LomaxStatus::Ok?BetaPrimeStatus::Ok:
      r.status==LomaxStatus::ExponentialLimit?BetaPrimeStatus::InvExpBoundary:
      r.status==LomaxStatus::InvalidStart?BetaPrimeStatus::InvalidStart:BetaPrimeStatus::BfgsFailure,d,r.certificate_failure};
  }
  double lm=0;
  for(double v:x) {
    if(!(v>0)||!std::isfinite(v))return invalid;
    lm+=std::log(v)/x.size();
  }
  if(!fa&&!fb) return fs?betapr_mle_fixed_scale(x,held_s,tol,maxit):betapr_mle(x,tol,maxit);
  const double n=static_cast<double>(x.size());
  const auto profile=[&](double ls) {
    double ta=0,tb=0;
    for(double v:x) {
      const double z=std::log(v)-ls;
      tb+=(std::max(z,0.0)+std::log1p(std::exp(-std::abs(z))))/n;
      ta+=(std::max(-z,0.0)+std::log1p(std::exp(-std::abs(z))))/n;
    }
    const double a=fa?held_a:betapr_detail::shape_root(held_b,ta,maxit);
    const double b=fb?held_b:betapr_detail::shape_root(held_a,tb,maxit);
    // Stable density through log(Y) and log(1-Y); no x/scale overflow.
    const CPPUNIV_EXTENDED_REAL la=a,lb=b;
    const CPPUNIV_EXTENDED_REAL logbeta=betapr_joint_cert::log_beta(a,b);
    const double ll=static_cast<double>(n*(-logbeta-la*ta-lb*tb-lm));
    const bool bad_a=!fa&&(a>=.999e8 || a<=std::exp(-39.99) ||
      std::abs(special::digamma(a+b)-special::digamma(a)-ta)>1e-7*std::max(1.0,ta));
    const bool bad_b=!fb&&(b>=.999e8 || b<=std::exp(-39.99) ||
      std::abs(special::digamma(a+b)-special::digamma(b)-tb)>1e-7*std::max(1.0,tb));
    BetaPrimeResult out{a,b,fs?held_s:std::exp(ls),ll,0,
      (bad_a||bad_b||!std::isfinite(ll))
      ?BetaPrimeStatus::BfgsFailure:BetaPrimeStatus::Ok};
    const double resolution=128*std::numeric_limits<double>::denorm_min()/1e-10;
    if((!fa && !(ta>resolution)) || (!fb && !(tb>resolution)))out.search_failed=true;
    return out;
  };
  if(fs) return profile(std::log(held_s));
  if(fa&&fb) {
    // Strictly decreasing log-scale score, with signs +nb and -na.
    double lo=lm-2,hi=lm+2;
    const auto score=[&](double ls) {
      double v=-n*held_a;
      for(double z:x) {
        const double t=std::log(z)-ls;
        const double q=t>=0?1/(1+std::exp(-t)):std::exp(t)/(1+std::exp(t));
        v+=(held_a+held_b)*q;
      }
      return v;
    };
    for(int j=0;j<maxit&&score(lo)<=0;++j)lo-=2;
    for(int j=0;j<maxit&&score(hi)>=0;++j)hi+=2;
    for(int j=0;j<maxit;++j) {const double mid=(lo+hi)/2;if(score(mid)>0)lo=mid;else hi=mid;}
    if(hi-lo>1e-10)return {nan,nan,nan,nan,maxit,BetaPrimeStatus::BfgsFailure};
    return profile((lo+hi)/2);
  }
  if(fa)return betapr_cert_detail::fit(x,held_a,tol,maxit);
  std::vector<double> reciprocal;betapr_cert_detail::Sum jac_sum,jac_magnitude;
  for(double v:x){
    if(!(1/v>0)||!std::isfinite(1/v)) {
      invalid.status=BetaPrimeStatus::BfgsFailure;
      invalid.certificate_failure=LomaxCertificateFailure::Arithmetic;
      invalid.search_failed=true;return invalid;
    }
    reciprocal.push_back(1/v);
    const double term=-2*std::log(v);jac_sum.add(term);jac_magnitude.add(std::abs(term));
  }
  const double jac=jac_sum.value;
  const double jac_error=64*std::numeric_limits<double>::epsilon()*(1+jac_magnitude.value);
  auto r=betapr_cert_detail::fit(span{reciprocal.data(),reciprocal.size()},held_b,tol,maxit);
  std::swap(r.alpha,r.beta);const double original_scale=r.scale;
  r.scale=1/r.scale;r.logLik+=jac;
  if(r.status==BetaPrimeStatus::Ok && (!(r.scale>0)||!std::isfinite(r.scale)||
      std::abs((1/r.scale)/original_scale-1)>1e-6)) {
    r.status=BetaPrimeStatus::BfgsFailure;r.certificate_failure=LomaxCertificateFailure::Arithmetic;
    r.search_failed=true;
  }
  if(r.status==BetaPrimeStatus::GammaBoundary)r.status=BetaPrimeStatus::InvGammaBoundary;
  auto& d=r.stop_detail;
  d.limit_model=StopModel::InvGamma;
  if(d.limit_coef.size()==2)d.limit_coef[1]=1/d.limit_coef[1];
  d.limit_loglik+=jac;d.loglik_lower+=jac-jac_error;d.loglik_upper+=jac+jac_error;
  d.segment_loglik_upper+=jac+jac_error;
  for(auto& c:d.candidates){
    if(c.model==StopModel::Gamma){c.model=StopModel::InvGamma;if(c.coef.size()==2)c.coef[1]=1/c.coef[1];}
    c.loglik_lower+=jac-jac_error;c.loglik_upper+=jac+jac_error;
    if(c.coef.size()==3){std::swap(c.coef[0],c.coef[1]);c.coef[2]=1/c.coef[2];}
  }
  d.candidates.erase(std::remove_if(d.candidates.begin(),d.candidates.end(),
    [](const FitCandidate& c){return !valid_stop_coefficients(c.model,c.coef);}),d.candidates.end());
  if(!valid_stop_coefficients(d.limit_model,d.limit_coef)) {
    d.limit_coef.clear();d.limit_loglik=NAN;d.limit_model=StopModel::None;
    if(r.status==BetaPrimeStatus::InvGammaBoundary){r.status=BetaPrimeStatus::BfgsFailure;r.certificate_failure=LomaxCertificateFailure::Arithmetic;}
  }
  return r;
}

} // namespace cppunivariate

namespace cppunivariate { namespace betapr_detail {
inline BetaPrimeResult fallback(BetaPrimeResult certificate,span x,double a,double b,double s,
    double tol,int maxit,double starta,double startb,double starts);
}}
#include "joint_certificate.hpp"

#include "fallback.hpp"
namespace cppunivariate {
inline BetaPrimeResult betapr_mle_holds(span x,double a,double b,double s,double tol,int maxit) {
  const auto r=betapr_mle_holds_certificate(x,a,b,s,tol,maxit);
  // The all-free delegate already applied its fallback; fixed-scale beta
  // failures are failures of that search, without an outer certificate.
  if(std::isnan(a) && std::isnan(b))return r;
  return betapr_detail::fallback(r,x,a,b,s,tol,maxit);
}
}
