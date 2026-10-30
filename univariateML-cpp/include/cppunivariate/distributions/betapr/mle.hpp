// Three-parameter beta-prime MLE, with explicit likelihood comparisons to faces.
#pragma once
#include "../../core/precision.hpp"
#include "../../core/multistart.hpp"
#include "../../core/bfgs.hpp"
#include "../../core/span.hpp"
#include "../../special/special.hpp"
#include "../gamma/mle.hpp"
#include "../beta/mle.hpp"
#include <vector>
#include <algorithm>
#include "../invgamma/mle.hpp"
#include <cmath>
#include <cstddef>
#include <limits>
namespace cppunivariate {
enum class BetaPrimeStatus { Ok, BfgsFailure, InvalidStart,
                             GammaBoundary, InvGammaBoundary };
struct BetaPrimeResult {
  double alpha, beta, scale, logLik;
  int iterations;
  BetaPrimeStatus status;
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
  int dim() const { return 3; }
  double operator()(const double *phi) const {
    const double a = std::exp(phi[0]), b = std::exp(phi[1]);
    const double scale = std::exp(phi[2]);
    // Avoid cancellation at numerically indistinguishable infinite-shape faces.
    if (!(a > 0 && b > 0 && scale > 0) || !std::isfinite(scale) ||
        a > 1e6 || b > 1e6) return INFINITY;
    const double theta[2] = {a, b};
    return BetaPrimeCost{x, scale}(theta);
  }
  void gradient(const double *phi, double *g) const {
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
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (!(scale > 0) || !std::isfinite(scale) || x.size() < 2)
    return {nan,nan,scale,nan,0,BetaPrimeStatus::InvalidStart};
  std::vector<double> y;
  bool different = false;
  double lx = 0, l1p = 0;
  for (double v : x) {
    if (!(v > 0) || !std::isfinite(v))
      return {nan,nan,scale,nan,0,BetaPrimeStatus::InvalidStart};
    different = different || v != x[0];
    const double z = v / scale;
    y.push_back(z / (1 + z));
    lx += std::log(z);
    l1p += std::log1p(z);
  }
  if (!different)
    return {nan,nan,scale,nan,0,BetaPrimeStatus::InvalidStart};
  const auto r = beta_mle(span{y.data(), y.size()}, tol, maxit);
  const double ll = (r.alpha-1)*lx - (r.alpha+r.beta)*l1p -
      x.size()*(special::lbeta(r.alpha,r.beta)+std::log(scale));
  return {r.alpha,r.beta,scale,ll,r.iterations,
      std::isfinite(ll) ? BetaPrimeStatus::Ok : BetaPrimeStatus::BfgsFailure};
}

inline BetaPrimeResult betapr_mle_impl(span x, double tol = 1e-6, int maxit = 1000,
                                  double shape1 = 0.0, double shape2 = 0.0,
                                  double scale = 0.0, double reltol = 1e-13,
                                  double abstol = 1e-10, bool standardized = false) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (shape1 < 0 || shape2 < 0 || scale < 0 ||
      !std::isfinite(shape1) || !std::isfinite(shape2) || !std::isfinite(scale) ||
      x.size() < 2) return {nan,nan,nan,nan,0,BetaPrimeStatus::InvalidStart};
  if (std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
    return {nan,nan,nan,nan,0,BetaPrimeStatus::InvalidStart};
  double lm = 0.0;
  for (double v : x) {
    if (!(v > 0) || !std::isfinite(v))
      return {nan,nan,nan,nan,0,BetaPrimeStatus::InvalidStart};
    lm += std::log(v) / static_cast<double>(x.size());
  }
  if (!standardized) {
    const double unit = std::exp(lm);
    std::vector<double> z(x.size());
    for (std::size_t i = 0; i < x.size(); ++i) z[i] = x[i] / unit;
    auto r = betapr_mle_impl(span{z.data(), z.size()}, tol, maxit,
        shape1, shape2, scale / unit, reltol, abstol, true);
    r.scale *= unit;
    r.logLik -= static_cast<double>(x.size()) * std::log(unit);
    return r;
  }
  double lv = 0;
  for (double v : x) lv += (std::log(v)-lm)*(std::log(v)-lm) / x.size();
  if (!(lv > 0) || !std::isfinite(lv))
    return {nan,nan,nan,nan,0,BetaPrimeStatus::InvalidStart};
  const double n = static_cast<double>(x.size());
  auto gamma = gamma_mle(x, 1e-10, 1000);
  auto inv = invgamma_mle(x, 1e-10, 1000);
  // The closure's faces: shape2 -> inf (gamma) and shape1 -> inf (inverse
  // gamma). Both shapes -> inf is not a lognormal: Var(log X) =
  // trigamma(shape1) + trigamma(shape2) -> 0, a point mass.
  BetaPrimeResult face{gamma.shape,INFINITY,INFINITY,
      gamma.logLik,0,BetaPrimeStatus::GammaBoundary};
  if (inv.logLik > face.logLik) face = {INFINITY,inv.shape,0,
      inv.logLik,0,BetaPrimeStatus::InvGammaBoundary};
  BetaPrimeLogCost cost{x};
  BfgsControl ctl;
  ctl.grad_tol = tol; ctl.maxit = maxit; ctl.reltol = reltol; ctl.step_tol = abstol;
  BfgsResult attempted{}; attempted.f=INFINITY;
  BfgsResult best;
  best.f = INFINITY;
  for (double a : {0.2, 1.0, 5.0, 20.0, 100.0}) {
    for (double b : {0.2, 1.0, 5.0, 20.0, 100.0}) {
      const double start[3] = {std::log(a),std::log(b),
          lm - special::digamma(a) + special::digamma(b)};
      auto r = bfgs_minimize(cost, start, ctl);
      consider_bfgs_candidate(best,r,1.0,attempted);
    }
  }
  if (shape1 > 0 || shape2 > 0 || scale > 0) {
    const double a = shape1 > 0 ? shape1 : 2.0;
    const double b = shape2 > 0 ? shape2 : 2.0;
    const double ls = scale > 0 ? std::log(scale) :
        lm - special::digamma(a) + special::digamma(b);
    const double start[3] = {std::log(a),std::log(b),ls};
    auto r = bfgs_minimize(cost, start, ctl);
    consider_bfgs_candidate(best,r,1.0,attempted);
  }
  if (face.logLik >= -attempted.f - 1e-8*n) return face;
  if (!std::isfinite(best.f)) {
    if(attempted.theta.size()!=3) return {nan,nan,nan,nan,0,BetaPrimeStatus::BfgsFailure};
    return {std::exp(attempted.theta[0]),std::exp(attempted.theta[1]),std::exp(attempted.theta[2]),
      -attempted.f,attempted.iterations,BetaPrimeStatus::BfgsFailure};
  }
  return {std::exp(best.theta[0]),std::exp(best.theta[1]),std::exp(best.theta[2]),
          -best.f,best.iterations,(best.status == BfgsStatus::Ok || (std::isfinite(best.grad_norm) && best.grad_norm < 1e-3)) ?
          BetaPrimeStatus::Ok : BetaPrimeStatus::BfgsFailure};
}
inline BetaPrimeResult betapr_mle(span x, double tol = 1e-6, int maxit = 1000,
    double shape1 = 0.0, double shape2 = 0.0, double scale = 0.0,
    double reltol = 1e-13, double abstol = 1e-10) {
  return betapr_mle_impl(x, tol, maxit, shape1, shape2, scale, reltol, abstol);
}
// Hold-aware profiles. The legacy all-free and fixed-scale paths above are
// retained verbatim so their golden fits do not change.
namespace betapr_detail {
inline double shape_root(double other, double target) {
  double lo=-40,hi=std::log(1e8);
  for(int j=0;j<120;++j) {
    const double t=(lo+hi)/2,v=std::exp(t);
    const double score=special::digamma(v+other)-special::digamma(v)-target;
    if(score>0) lo=t;else hi=t;
  }
  return std::exp((lo+hi)/2);
}
}
inline BetaPrimeResult betapr_mle_holds(span x, double held_a, double held_b,
    double held_s, double tol=1e-6,int maxit=1000) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const bool fa=!std::isnan(held_a),fb=!std::isnan(held_b),fs=!std::isnan(held_s);
  BetaPrimeResult invalid{nan,nan,nan,nan,0,BetaPrimeStatus::InvalidStart};
  if(x.empty() || !(tol>0) || !std::isfinite(tol) || maxit<=0 ||
    (fa&&(!(held_a>0)||!std::isfinite(held_a))) ||
    (fb&&(!(held_b>0)||!std::isfinite(held_b))) ||
    (fs&&(!(held_s>0)||!std::isfinite(held_s)))) return invalid;
  double lm=0,mean=0,invmean=0;
  for(double v:x) {
    if(!(v>0)||!std::isfinite(v))return invalid;
    lm+=std::log(v)/x.size();mean+=v/x.size();invmean+=(1/v)/x.size();
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
    const double a=fa?held_a:betapr_detail::shape_root(held_b,ta);
    const double b=fb?held_b:betapr_detail::shape_root(held_a,tb);
    // Stable density through log(Y) and log(1-Y); no x/scale overflow.
    const CPPUNIV_EXTENDED_REAL la=a,lb=b;
    const CPPUNIV_EXTENDED_REAL logbeta=std::lgamma(la)+std::lgamma(lb)-std::lgamma(la+lb);
    const double ll=static_cast<double>(n*(-logbeta-la*ta-lb*tb-lm));
    const bool bad_a=!fa&&(a>=.999e8 || a<=std::exp(-39.99) ||
      std::abs(special::digamma(a+b)-special::digamma(a)-ta)>1e-7*std::max(1.0,ta));
    const bool bad_b=!fb&&(b>=.999e8 || b<=std::exp(-39.99) ||
      std::abs(special::digamma(a+b)-special::digamma(b)-tb)>1e-7*std::max(1.0,tb));
    return BetaPrimeResult{a,b,fs?held_s:std::exp(ls),ll,0,
      (bad_a||bad_b||!std::isfinite(ll))
      ?BetaPrimeStatus::BfgsFailure:BetaPrimeStatus::Ok};
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
    for(int j=0;j<1024&&score(lo)<=0;++j)lo-=2;
    for(int j=0;j<1024&&score(hi)>=0;++j)hi+=2;
    for(int j=0;j<120;++j) {const double mid=(lo+hi)/2;if(score(mid)>0)lo=mid;else hi=mid;}
    return profile((lo+hi)/2);
  }
  // Only the face opposite the held shape survives. Optimize that face
  // retaining the held shape; comparing with an unrestricted face is wrong.
  double face_ll=0;
  if(fa) {
    const double rate=held_a/mean;
    face_ll=n*(held_a*std::log(rate)-special::lgamma(held_a)+(held_a-1)*lm-held_a);
  } else {
    const double scale=held_b/invmean;
    face_ll=n*(held_b*std::log(scale)-special::lgamma(held_b)-(held_b+1)*lm-held_b);
  }
  // With identical observations, profiling scale gives a monotone density
  // in the free shape: psi(a+b)-psi(b) > log((a+b)/b).
  // Thus its supremum is precisely the appropriate gamma face.
  if (std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))
    return {held_a,held_b,nan,face_ll,0,
      fa?BetaPrimeStatus::GammaBoundary:BetaPrimeStatus::InvGammaBoundary};
  BetaPrimeResult best=invalid;best.logLik=-INFINITY;
  const double lo=lm-24,hi=lm+24;
  constexpr int grids=384;
  std::vector<double> values(grids+1);
  const auto value=[&](double ls) {
    const auto r=profile(ls);
    if(r.status==BetaPrimeStatus::Ok&&std::isfinite(r.logLik)&&r.logLik>best.logLik)best=r;
    return r.logLik;
  };
  for(int j=0;j<=grids;++j)values[j]=value(lo+(hi-lo)*j/grids);
  int iterations=0;
  for(int j=1;j<grids;++j)if(values[j]>=values[j-1]&&values[j]>=values[j+1]) {
    double l=lo+(hi-lo)*(j-1)/grids,r=lo+(hi-lo)*(j+1)/grids;
    int it=0;
    for(;it<maxit&&r-l>1e-9;++it) {
      const double a=r-(r-l)*.6180339887498949,b=l+(r-l)*.6180339887498949;
      if(value(a)>value(b))r=b;else l=a;
    }
    iterations=std::max(iterations,it);
    if(it==maxit) {best.status=BetaPrimeStatus::BfgsFailure;return best;}
  }
  if(!std::isfinite(best.logLik)) {best.status=BetaPrimeStatus::BfgsFailure;return best;}
  if(face_ll>=best.logLik-1e-8*n) return {held_a,held_b,nan,face_ll,iterations,
    fa?BetaPrimeStatus::GammaBoundary:BetaPrimeStatus::InvGammaBoundary};
  if(!std::isfinite(best.logLik) || best.alpha>=.999e8 || best.beta>=.999e8 ||
      !(best.scale>std::exp(lo)&&best.scale<std::exp(hi))) best.status=BetaPrimeStatus::BfgsFailure;
  best.iterations=iterations;
  return best;
}

} // namespace cppunivariate
