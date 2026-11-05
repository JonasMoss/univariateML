// Three-parameter Burr XII MLE, with shape1 profiled exactly.
#pragma once
#include "../../core/stop_detail.hpp"
#include "cppunivariate/core/constants.hpp"
#include "../../core/multistart.hpp"
#include "../lomax/mle.hpp"
#include "fixed_scale.hpp"
#include "certificate.hpp"
#include "../../core/bfgs.hpp"
#include "../../core/finite_diff.hpp"
#include "../../core/span.hpp"
#include "../weibull/mle.hpp"
#include "../pareto/mle.hpp"
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
inline double burr_softplus(double z) {
  return std::max(z, 0.0) + std::log1p(std::exp(-std::abs(z)));
}
// Fixed-scale cost supplies an additional legacy two-shape starting fit.
struct BurrCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double k = theta[0];
    const double c = theta[1];
    if (!(k > 0.0 && c > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1py = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      const double lx = std::log(v);
      S_lx += lx;
      S_l1py += burr_softplus(c * lx);
    }
    return -n * (std::log(k) + std::log(c)) - (c - 1.0) * S_lx +
           (k + 1.0) * S_l1py;
  }
  void gradient(const double *theta, double *g) const {
    const double k = theta[0];
    const double c = theta[1];
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1py = 0.0, S_y_lx_over_1py = 0.0;
    for (auto v : x) {
      const double lx = std::log(v);
      const double z = c * lx;
      if (z > 350.0) {
        S_lx += lx;
        S_l1py += burr_softplus(z);
        S_y_lx_over_1py += lx / (1.0 + std::exp(-z));
        continue;
      }
      const double y = std::exp(z);
      S_lx += lx;
      S_l1py += std::log1p(y);
      S_y_lx_over_1py += y * lx / (1.0 + y);
    }
    g[0] = -n / k + S_l1py;
    g[1] = -n / c - S_lx + (k + 1.0) * S_y_lx_over_1py;
  }
  void hessian(const double *theta, double *H) const {
    const double k = theta[0];
    const double c = theta[1];
    const double n = static_cast<double>(x.size());
    double H_cc_acc = 0.0, H_kc_acc = 0.0;
    for (auto v : x) {
      const double lx = std::log(v);
      const double z = c * lx;
      if (z > 350.0) {
        const double e = std::exp(-z);
        H_cc_acc += (k + 1.0) * lx * lx * e / ((1.0 + e) * (1.0 + e));
        H_kc_acc += lx / (1.0 + e);
        continue;
      }
      const double y = std::exp(z);
      const double yp1 = 1.0 + y;
      H_cc_acc += (k + 1.0) * y * lx * lx / (yp1 * yp1);
      H_kc_acc += y * lx / yp1;
    }
    H[0] = n / (k * k);
    H[1] = H_kc_acc;
    H[2] = H_kc_acc;
    H[3] = n / (c * c) + H_cc_acc;
  }
};


enum class BurrStatus { Ok = 0, WeibullBoundary, ParetoBoundary, InvalidInit, IterationFailure, AllGeOne, Unbounded, CertificateTie, CertificateBudget, NumericalFailure };
struct BurrMLE {
  double shape1, shape2, scale, logLik;
  int iterations;
  BurrStatus status;
  StopDetail stop_detail{};
};
// theta = (log shape2, log scale relative to geometric mean).
// shape1 = n / sum softplus(shape2 * log(x/scale)).
// Writing z-softplus(z) = -softplus(-z) avoids tail cancellation.
struct BurrProfileCost {
  span logs;
  int dim() const { return 2; }
  double shape1(const double *theta) const {
    double total = 0.0;
    for (double l : logs) total += burr_softplus(std::exp(theta[0]) * (l-theta[1]));
    return static_cast<double>(logs.size()) / total;
  }
  double operator()(const double *theta) const {
    const double c = std::exp(theta[0]);
    const double k = shape1(theta);
    if (!(c > 0.0 && k > 0.0) || !std::isfinite(c) || !std::isfinite(k))
      return std::numeric_limits<double>::infinity();
    double value = 1.0 - std::log(k) - theta[0];
    const double n = static_cast<double>(logs.size());
    for (double l : logs) value += (l + burr_softplus(-c*(l-theta[1]))) / n;
    return value;
  }
};
// Compatibility path for dependent families; no global certificate.
inline BurrMLE burr_mle_legacy(span x, double tol, int maxit,
    double shape10 = std::numeric_limits<double>::quiet_NaN(),
    double shape20 = std::numeric_limits<double>::quiet_NaN(),
    double scale0 = std::numeric_limits<double>::quiet_NaN(), double reltol = 1e-12) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (x.size() < 2) return {nan,nan,nan,nan,0,BurrStatus::InvalidInit};
  std::vector<double> logs;
  double center = 0.0;
  for (double v : x) {
    if (!(v > 0.0) || !std::isfinite(v)) return {nan,nan,nan,nan,0,BurrStatus::InvalidInit};
    logs.push_back(std::log(v)); center += logs.back()/static_cast<double>(x.size());
  }
  double variance = 0.0;
  for (double &l : logs) { l -= center; variance += l*l/static_cast<double>(x.size()); }
  if (!(variance > 0.0)) return {nan,nan,nan,nan,0,BurrStatus::InvalidInit};
  for (double start : {shape10, shape20, scale0})
    if (!std::isnan(start) && (!(start > 0.0) || !std::isfinite(start)))
      return {nan,nan,nan,nan,0,BurrStatus::InvalidInit};
  BurrProfileCost cost{span{logs.data(),logs.size()}};
  finite_diff_cost<BurrProfileCost> fd{cost};
  BfgsControl ctl;
  ctl.grad_tol = tol; ctl.reltol = reltol; ctl.maxit = maxit;
  BfgsResult attempted{}; attempted.f=INFINITY;
  BfgsResult best;
  best.f = std::numeric_limits<double>::infinity();
  best.iterations = 0;
  const double unit = std::sqrt(variance);
  // Match log moments at a grid of shape1 values, including log-logistic.
  // Extra endpoint starts probe the two different boundary basins.
  for (double k : {0.01,0.05,0.2,0.5,1.0,2.0,10.0,100.0}) {
    const double c = std::sqrt((cppunivariate::pi*cppunivariate::pi/6.0 + special::trigamma(k))/variance);
    const double loc = (special::digamma(k) + 0.5772156649015328606)/c;
    const double start[2] = {std::log(c),loc};
    auto r = bfgs_minimize(fd,start,ctl);
    consider_bfgs_candidate(best,r,static_cast<double>(x.size()),attempted);
  }
  // Fit the old two-shape family after centering log data. Its scale is
  // the geometric mean, so the seed is invariant to measurement units.
  std::vector<double> normalized;
  bool representable = true;
  for (double l : logs) {
    normalized.push_back(std::exp(l));
    if (!(normalized.back() > 0.0) || !std::isfinite(normalized.back())) representable = false;
  }
  if (representable) {
    BurrCost fixed{span{normalized.data(),normalized.size()}};
    const double fixed_start[2] = {1.0,1.0/unit};
    auto seed = bfgs_minimize(fixed,fixed_start,ctl);
    if (std::isfinite(seed.f) && seed.theta[1] > 0.0) {
      const double start[2] = {std::log(seed.theta[1]),0.0};
      auto r = bfgs_minimize(fd,start,ctl);
      consider_bfgs_candidate(best,r,static_cast<double>(x.size()),attempted);
    }
  }
  if (!std::isnan(shape10) || !std::isnan(shape20) || !std::isnan(scale0)) {
    const double k = std::isnan(shape10) ? 1.0 : shape10;
    const double c = std::isnan(shape20) ? 1.0/unit : shape20;
    const double start[2] = {std::log(c), std::isnan(scale0) ?
      (special::digamma(k)+0.5772156649015328606)/c : std::log(scale0)-center};
    auto r = bfgs_minimize(fd,start,ctl);
    consider_bfgs_candidate(best,r,static_cast<double>(x.size()),attempted);
  }
  const double n = static_cast<double>(x.size());
  const double ll = -n*(attempted.f+center);
  const auto w = weibull_mle(x,1e-10,std::max(maxit,100));
  const auto p = pareto_mle(x);
  const double tie = 1e-8*n;
  if (w.logLik >= ll-tie && w.logLik >= p.logLik-tie)
    return {INFINITY,w.shape,w.scale,w.logLik,best.iterations,BurrStatus::WeibullBoundary};
  if (p.logLik >= ll-tie)
    return {0.0,INFINITY,p.min,p.logLik,best.iterations,BurrStatus::ParetoBoundary};
  if (!std::isfinite(best.f)) {
    if(attempted.theta.size()!=2) return {nan,nan,nan,nan,0,BurrStatus::IterationFailure};
    return {cost.shape1(attempted.theta.data()),std::exp(attempted.theta[0]),
      std::exp(center+attempted.theta[1]),ll,attempted.iterations,BurrStatus::IterationFailure};
  }
  return {cost.shape1(best.theta.data()),std::exp(best.theta[0]),
    std::exp(center+best.theta[1]),-n*(best.f+center),best.iterations,
    best.status == BfgsStatus::Ok ? BurrStatus::Ok : BurrStatus::IterationFailure};
}

// Convert only checked certificate outcomes. No local-score rescue is valid
// after a failed global certificate. Held paths below retain their algorithms.
inline BurrMLE burr_mle_certified(span x, double tol, int maxit,
    double shape10 = std::numeric_limits<double>::quiet_NaN(),
    double shape20 = std::numeric_limits<double>::quiet_NaN(),
    double scale0 = std::numeric_limits<double>::quiet_NaN(), double reltol = 1e-12,
    int certificate_budget = 4096) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(!(tol>0)||!std::isfinite(tol)||maxit<1||!(reltol>0)||!std::isfinite(reltol))
    return {nan,nan,nan,nan,0,BurrStatus::InvalidInit};
  for(double v:{shape10,shape20,scale0})
    if(!std::isnan(v)&&(!(v>0)||!std::isfinite(v)))
      return {nan,nan,nan,nan,0,BurrStatus::InvalidInit};
  if(maxit==1)return {nan,nan,nan,nan,0,BurrStatus::IterationFailure};
  const auto r=burr_cert_detail::fit(x,certificate_budget,std::min(maxit,120));
  BurrStatus status=BurrStatus::NumericalFailure;
  switch(r.outcome){
    case burr_cert_detail::Outcome::Finite:status=BurrStatus::Ok;break;
    case burr_cert_detail::Outcome::Weibull:status=BurrStatus::WeibullBoundary;break;
    case burr_cert_detail::Outcome::Pareto:status=BurrStatus::ParetoBoundary;break;
    case burr_cert_detail::Outcome::Tie:status=BurrStatus::CertificateTie;break;
    case burr_cert_detail::Outcome::Budget:status=BurrStatus::CertificateBudget;break;
    case burr_cert_detail::Outcome::Invalid:status=BurrStatus::InvalidInit;break;
    case burr_cert_detail::Outcome::Unbounded:status=BurrStatus::Unbounded;break;
    case burr_cert_detail::Outcome::Numerical:break;
  }
  BurrMLE out{r.k,r.c,r.scale,r.logLik,r.profiles,status};
  if(status!=BurrStatus::Ok)out.stop_detail=r.stop_detail;
  if(status==BurrStatus::WeibullBoundary || status==BurrStatus::ParetoBoundary) {
    const auto& face=r.stop_detail.candidates[status==BurrStatus::ParetoBoundary?0:1];
    out.stop_detail.limit_coef=face.coef;out.stop_detail.limit_loglik=r.logLik;
  }
  return out;
}
inline BurrMLE burr_mle(span x, double tol, int maxit,
    double shape10 = std::numeric_limits<double>::quiet_NaN(),
    double shape20 = std::numeric_limits<double>::quiet_NaN(),
    double scale0 = std::numeric_limits<double>::quiet_NaN(), double reltol = 1e-12) {
  return burr_mle_certified(x,tol,maxit,shape10,shape20,scale0,reltol);
}

// Fixed scale reduces exactly to the historical scale-1 two-shape fit.
inline BurrMLE burr_mle_fixed_scale(span x, double scale,
    double tol = 1.220703e-4, int maxit = 1000) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (!(scale > 0.0) || !std::isfinite(scale))
    return {nan,nan,scale,nan,0,BurrStatus::InvalidInit};
  // With both shapes free, constant data on this side of the held
  // scale concentrate as shape2 grows (shape1 is profiled). Test the
  // original observations exactly, before normalization or any search.
  bool constant = x.size() > 0;
  for (double v : x) {
    if (!(v > 0.0) || !std::isfinite(v))
      return {nan,nan,scale,nan,0,BurrStatus::InvalidInit};
    if (v != x[0]) constant = false;
  }
  if (constant && x[0] <= scale)
    return {nan,nan,scale,INFINITY,0,BurrStatus::Unbounded};
  std::vector<double> normalized;
  for (double v : x) normalized.push_back(v / scale);
  const auto r = burr_fixed_detail::burr_mle(span{normalized.data(),normalized.size()},tol,maxit);
  BurrStatus status = BurrStatus::Ok;
  if (r.status == burr_fixed_detail::BurrStatus::AllGeOne) status = BurrStatus::AllGeOne;
  if (r.status == burr_fixed_detail::BurrStatus::InvalidInit) status = BurrStatus::InvalidInit;
  if (r.status == burr_fixed_detail::BurrStatus::IterationFailure) status = BurrStatus::IterationFailure;
  BurrMLE out{r.shape1,r.shape2,scale,r.logLik-static_cast<double>(x.size())*std::log(scale),r.iterations,status};
  if(status==BurrStatus::AllGeOne) {
    const auto face=pareto_mle(x,NAN,scale);
    out.stop_detail.limit_coef={face.shape,face.min};out.stop_detail.limit_loglik=face.logLik;
  }
  return out;
}
} // namespace cppunivariate

namespace cppunivariate {

namespace burr_hold_detail {
// Monotone decreasing score on log coordinates. Bracket before bisection.
template<class Score> inline double root(Score score, int maxit=160) {
  double lo=-1,hi=1;
  for(int i=0;i<12 && score(lo)<0;++i)lo*=2;
  for(int i=0;i<12 && score(hi)>0;++i)hi*=2;
  if(!(score(lo)>=0 && score(hi)<=0))return std::numeric_limits<double>::quiet_NaN();
  for(int i=0;i<maxit;++i){double m=(lo+hi)/2;if(m==lo||m==hi)return m;
    const double value=score(m);if(value==0)return m;if(value>0)lo=m;else hi=m;}
  const double mid=(lo+hi)/2;
  return hi-lo <= 1e-12*(1+std::abs(mid)) ? mid : std::numeric_limits<double>::quiet_NaN();
}
inline double sigmoid(double z){return z>=0?1/(1+std::exp(-z)):std::exp(z)/(1+std::exp(z));}
}
// Every shape hold is fitted on logs. With shape1 fixed, profile location
// exactly, then solve the strictly concave location-scale likelihood in
// inverse scale and standardized location.
inline BurrMLE burr_mle_holds(span x,double a,double b,double s,double tol=1e-9,int maxit=1000) {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 BurrMLE bad{a,b,s,nan,0,BurrStatus::InvalidInit};
 if(x.size()==0||!(tol>0)||!std::isfinite(tol)||maxit<1)return bad;
 for(double v:{a,b,s})if(!std::isnan(v)&&(!(v>0)||!std::isfinite(v)))return bad;
 std::vector<double> l;double mean=0,mn=INFINITY,mx=-INFINITY;
 for(double v:x){if(!(v>0)||!std::isfinite(v))return bad;l.push_back(std::log(v));mean+=l.back()/x.size();mn=std::min(mn,l.back());mx=std::max(mx,l.back());}
 if(std::isnan(a)&&std::isnan(b))return std::isnan(s)?burr_mle(x,tol,maxit):burr_mle_fixed_scale(x,s,tol,maxit);
 const bool fa=std::isnan(a),fb=std::isnan(b),fs=std::isnan(s);
 if(fb&&((fs&&mn==mx)||(!fs&&mn==mx&&x[0]==s))){bad.status=BurrStatus::Unbounded;return bad;}
 const auto solve_root=[&](auto score){return burr_hold_detail::root(score,maxit);};
 const double n=static_cast<double>(x.size());
 // Fixed power is precisely a Lomax likelihood; its exponential face
 // can be tested without ever forming x^b by scaling at max(log x).
 if(fa&&fs){
   std::vector<double> y;for(double v:l)y.push_back(std::exp(b*(v-mx)));
   for(double v:y)if(!(v>0) || !std::isfinite(v)){bad.status=BurrStatus::NumericalFailure;return bad;}
   auto r=lomax_mle(span{y.data(),y.size()},tol,maxit);
   const double jac=n*std::log(b)+(b-1)*n*mean-b*n*mx;
   bad.stop_detail=r.stop_detail;
   for(auto& c:bad.stop_detail.candidates){
     if(c.model==StopModel::None && c.coef.size()==2)c.coef={c.coef[1],b,std::exp(mx-std::log(c.coef[0])/b)};
     else if(c.model==StopModel::Exp && c.coef.size()==1){c.model=StopModel::Weibull;c.coef={b,std::exp(mx-std::log(c.coef[0])/b)};}
     c.loglik_lower+=jac;c.loglik_upper+=jac;
   }
   bad.stop_detail.loglik_lower+=jac;bad.stop_detail.loglik_upper+=jac;
   bad.stop_detail.segment_loglik_upper+=jac;bad.stop_detail.limit_loglik+=jac;
   if(r.status==LomaxStatus::ExponentialLimit){
     bad.status=BurrStatus::WeibullBoundary;
     const double rate=r.stop_detail.limit_coef.empty()?NAN:r.stop_detail.limit_coef[0];
     bad.stop_detail.limit_coef={b,std::exp(mx-std::log(rate)/b)};
     bad.stop_detail.limit_loglik=r.stop_detail.limit_loglik+n*std::log(b)+(b-1)*n*mean-b*n*mx;
     return bad;
   }
   if(r.status!=LomaxStatus::Ok){bad.status=r.certificate_failure==LomaxCertificateFailure::Budget?BurrStatus::CertificateBudget:r.certificate_failure==LomaxCertificateFailure::Arithmetic?BurrStatus::NumericalFailure:BurrStatus::IterationFailure;return bad;}
   a=r.kappa;s=std::exp(mx-std::log(r.lambda)/b);
   if(!(a>0 && std::isfinite(a) && s>0 && std::isfinite(s) && std::isfinite(r.logLik+jac))){bad.status=BurrStatus::NumericalFailure;return bad;}
   return {a,b,s,r.logLik+jac,r.iterations,BurrStatus::Ok};
 } else {
   auto location=[&](double power){return solve_root([&](double c){double z=n;for(double v:l)z-=(a+1)*burr_hold_detail::sigmoid(power*(v-mean)-c);return -z;});};
   if(fb){
     const double root=solve_root([&](double z){double power=std::exp(z),c=fs?location(power):power*(std::log(s)-mean),g=n;for(double v:l){double w=v-mean,u=power*w-c;g+=power*w*(1-(a+1)*burr_hold_detail::sigmoid(u));}if(!fs){g=n;for(double v:l){double u=power*(v-std::log(s));g+=u*(1-(a+1)*burr_hold_detail::sigmoid(u));}}return g;});
     b=std::exp(root);
   }
   if(fs)s=std::exp(mean+location(b)/b);
   if(fa){double total=0;for(double v:l)total+=burr_softplus(b*(v-std::log(s)));a=n/total;}
 }
 double ll=n*(std::log(a)+std::log(b));
 for(double v:l){double u=b*(v-std::log(s));ll+=-v-burr_softplus(-u)-a*burr_softplus(u);}
 return {a,b,s,ll,0,std::isfinite(ll)&&a>0&&b>0&&s>0&&std::isfinite(s)?BurrStatus::Ok:BurrStatus::IterationFailure};
}

}
