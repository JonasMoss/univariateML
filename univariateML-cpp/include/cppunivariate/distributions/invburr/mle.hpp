// Inverse Burr is the reciprocal of a Burr with reciprocal scale.
#pragma once
#include "../../core/stop_detail.hpp"
#include "../burr/mle.hpp"
#include "../invweibull/mle.hpp"
#include "../power/mle.hpp"
namespace cppunivariate {
namespace burr_fixed_detail {
enum class InvBurrStatus { Ok = 0, AllLeOne, InvalidInit, IterationFailure };

struct InvBurrResult {
  double shape1;
  double shape2;
  double logLik;
  int iterations;
  InvBurrStatus status;
};

inline InvBurrResult invburr_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, InvBurrStatus::InvalidInit};

  bool all_le_one = true;
  for (auto v : x) {
    if (!(v > 0.0) || !std::isfinite(v) || !std::isfinite(1.0 / v))
      return {kNaN, kNaN, kNaN, 0, InvBurrStatus::InvalidInit};
    if (v > 1.0) all_le_one = false;
  }
  if (all_le_one)
    return {kNaN, kNaN, kNaN, 0, InvBurrStatus::AllLeOne};

  std::vector<double> invx(n);
  double sum_log_invx = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    invx[i] = 1.0 / x[i];
    sum_log_invx += std::log(invx[i]);
  }
  const double c0 = burr_mle_start(span{invx.data(), n}, true);
  BurrMLE res = burr_mle_core(span{invx.data(), n}, c0, tol, maxit);
  if (res.status != BurrStatus::Ok) {
    return {kNaN, kNaN, kNaN, res.iterations, InvBurrStatus::IterationFailure};
  }

  const double shape1 = res.shape1;
  const double shape2 = res.shape2;
  // Change-of-variables Jacobian; reuse the checked Burr likelihood.
  const double logLik = res.logLik + 2.0 * sum_log_invx;
  if (!std::isfinite(logLik))
    return {kNaN, kNaN, kNaN, res.iterations, InvBurrStatus::IterationFailure};
  return {shape1, shape2, logLik, res.iterations, InvBurrStatus::Ok};
}


} // namespace burr_fixed_detail

struct InvBurrCost {
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
    return -n * (std::log(k) + std::log(c)) - (k * c - 1.0) * S_lx +
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
    g[0] = -n / k - c * S_lx + S_l1py;
    g[1] = -n / c - k * S_lx + (k + 1.0) * S_y_lx_over_1py;
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
        H_kc_acc += -lx * e / (1.0 + e);
        continue;
      }
      const double y = std::exp(z);
      const double yp1 = 1.0 + y;
      H_cc_acc += (k + 1.0) * y * lx * lx / (yp1 * yp1);
      H_kc_acc += -lx / yp1;
    }
    H[0] = n / (k * k);
    H[1] = H_kc_acc;
    H[2] = H_kc_acc;
    H[3] = n / (c * c) + H_cc_acc;
  }
};


enum class InvBurrStatus { Ok = 0, InvWeibullBoundary, PowerBoundary, InvalidInit, IterationFailure, AllLeOne, Unbounded, CertificateTie, CertificateBudget, NumericalFailure };
struct InvBurrResult {
  double shape1, shape2, scale, logLik;
  int iterations;
  InvBurrStatus status;
  StopDetail stop_detail{};
};
// Reciprocal transport applies to diagnostics as well as successful fits.
inline StopDetail invburr_stop_detail(StopDetail detail,double jacobian,double endpoint=NAN) {
  for(auto& candidate:detail.candidates) {
    if(candidate.model==StopModel::Pareto) {
      candidate.model=StopModel::Power;
      candidate.coef={std::isfinite(endpoint)?endpoint:1/candidate.coef[1],candidate.coef[0]};
    } else if(candidate.model==StopModel::Weibull) {
      candidate.model=StopModel::InvWeibull;candidate.coef[1]=1/candidate.coef[1];
    } else if(candidate.coef.size()==3)candidate.coef[2]=1/candidate.coef[2];
    candidate.loglik_lower+=jacobian;candidate.loglik_upper+=jacobian;
  }
  detail.loglik_lower+=jacobian;detail.loglik_upper+=jacobian;
  detail.segment_loglik_upper+=jacobian;
  detail.limit_loglik+=jacobian;
  return detail;
}
inline InvBurrResult invburr_mle(span x, double tol, int maxit,
    double shape10 = std::numeric_limits<double>::quiet_NaN(),
    double shape20 = std::numeric_limits<double>::quiet_NaN(),
    double scale0 = std::numeric_limits<double>::quiet_NaN(), double reltol = 1e-12) {
  std::vector<double> reciprocal;
  double jacobian = 0.0, maximum = 0.0;
  for (double v : x) {
    if (!(v > 0.0) || !std::isfinite(v)) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      return {nan,nan,nan,nan,0,InvBurrStatus::InvalidInit};
    }
    if (!std::isfinite(1.0/v)) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      return {nan,nan,nan,nan,0,InvBurrStatus::NumericalFailure};
    }
    reciprocal.push_back(1.0/v); jacobian -= 2.0*std::log(v);
    maximum=std::max(maximum,v);
  }
  // Division can merge distinct binary64 observations. Such artificial
  // ties must not supply a constant-sample or tied-endpoint certificate.
  std::vector<std::size_t> order;
  for(std::size_t i=0;i<x.size();++i)order.push_back(i);
  std::sort(order.begin(),order.end(),[&](std::size_t i,std::size_t j){return x[i]<x[j];});
  for(std::size_t i=1;i<order.size();++i)
    if(x[order[i]]!=x[order[i-1]] && reciprocal[order[i]]==reciprocal[order[i-1]]) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      return {nan,nan,nan,nan,0,InvBurrStatus::NumericalFailure};
    }
  auto r = burr_mle_certified(span{reciprocal.data(),reciprocal.size()},tol,maxit,
    shape10,shape20,std::isnan(scale0) ? scale0 : 1.0/scale0,reltol);
  InvBurrStatus status = InvBurrStatus::Ok;
  if (r.status == BurrStatus::WeibullBoundary) status = InvBurrStatus::InvWeibullBoundary;
  if (r.status == BurrStatus::Unbounded) status = InvBurrStatus::Unbounded;
  if (r.status == BurrStatus::ParetoBoundary) status = InvBurrStatus::PowerBoundary;
  if (r.status == BurrStatus::InvalidInit) status = InvBurrStatus::InvalidInit;
  if (r.status == BurrStatus::IterationFailure) status = InvBurrStatus::IterationFailure;
  if (r.status == BurrStatus::CertificateTie) status = InvBurrStatus::CertificateTie;
  if (r.status == BurrStatus::CertificateBudget) status = InvBurrStatus::CertificateBudget;
  if (r.status == BurrStatus::NumericalFailure) status = InvBurrStatus::NumericalFailure;
  // A face carries its limiting scale, not a finite Burr coefficient.
  // Reciprocation transports the certificate and its checked likelihood.
  // Keep the power endpoint exactly at the original sample maximum;
  // a reciprocal round trip can otherwise move it by one ulp.
  const double scale = status==InvBurrStatus::PowerBoundary ? maximum : 1.0/r.scale;
  const double logLik = r.logLik+jacobian;
  if ((status == InvBurrStatus::Ok || status == InvBurrStatus::InvWeibullBoundary ||
       status == InvBurrStatus::PowerBoundary) &&
      (!(scale>0) || !std::isfinite(scale) || !std::isfinite(logLik))) {
    const double nan = std::numeric_limits<double>::quiet_NaN();
    return {nan,nan,nan,nan,r.iterations,InvBurrStatus::NumericalFailure};
  }
  InvBurrResult out{r.shape1,r.shape2,scale,logLik,r.iterations,status};
  out.stop_detail=invburr_stop_detail(r.stop_detail,jacobian,maximum);
  if(status==InvBurrStatus::PowerBoundary)out.stop_detail.limit_coef={maximum,r.stop_detail.limit_coef[0]};
  if(status==InvBurrStatus::InvWeibullBoundary)out.stop_detail.limit_coef={r.stop_detail.limit_coef[0],1/r.stop_detail.limit_coef[1]};
  return out;
}

inline InvBurrResult invburr_mle_holds(span x,double a,double b,double s,double tol,int maxit);
// Share the certified reciprocal path with the held-parameter dispatcher.
inline InvBurrResult invburr_mle_fixed_scale(span x, double scale,
    double tol = 1.220703e-4, int maxit = 1000) {
  if(!(scale>0)||!std::isfinite(scale))
    return {NAN,NAN,scale,NAN,0,InvBurrStatus::InvalidInit};
  return invburr_mle_holds(x,NAN,NAN,scale,tol,maxit);
}
} // namespace cppunivariate

namespace cppunivariate {

inline InvBurrResult invburr_mle_holds(span x,double a,double b,double s,double tol=1e-9,int maxit=1000){
 if(std::isnan(a)&&std::isnan(b)&&std::isnan(s))
   return invburr_mle(x,tol,maxit);
 std::vector<double> y;double jac=0;
 for(double v:x){
   if(!(v>0)||!std::isfinite(v))return {NAN,NAN,s,NAN,0,InvBurrStatus::InvalidInit};
   if(!std::isfinite(1/v)||!(1/v>0))return {NAN,NAN,s,NAN,0,InvBurrStatus::NumericalFailure};
   y.push_back(1/v);jac-=2*std::log(v);
 }
 // Do not certify artificial ties created by reciprocal rounding.
 std::vector<std::size_t> order;
 for(std::size_t i=0;i<x.size();++i)order.push_back(i);
 std::sort(order.begin(),order.end(),[&](std::size_t i,std::size_t j){return x[i]<x[j];});
 for(std::size_t i=1;i<order.size();++i)
   if(x[order[i]]!=x[order[i-1]]&&y[order[i]]==y[order[i-1]])
     return {NAN,NAN,s,NAN,0,InvBurrStatus::NumericalFailure};
 if(!std::isnan(s)&&(!(s>0)||!std::isfinite(s)))return {NAN,NAN,s,NAN,0,InvBurrStatus::InvalidInit};
 if(!std::isnan(s)&&(!(1/s>0)||!std::isfinite(1/s)))return {NAN,NAN,s,NAN,0,InvBurrStatus::NumericalFailure};
 if(!std::isnan(s)&&std::isnan(a)&&std::isnan(b))
   for(std::size_t i=0;i<x.size();++i)
     if(x[i]!=s&&y[i]==1/s)return {NAN,NAN,s,NAN,0,InvBurrStatus::NumericalFailure};
 auto r=burr_mle_holds(span{y.data(),y.size()},a,b,std::isnan(s)?s:1/s,tol,maxit);
 InvBurrStatus st=InvBurrStatus::Ok;
 if(r.status==BurrStatus::InvalidInit)st=InvBurrStatus::InvalidInit;
 if(r.status==BurrStatus::IterationFailure)st=InvBurrStatus::IterationFailure;
 if(r.status==BurrStatus::CertificateTie)st=InvBurrStatus::CertificateTie;
 if(r.status==BurrStatus::CertificateBudget)st=InvBurrStatus::CertificateBudget;
 if(r.status==BurrStatus::NumericalFailure)st=InvBurrStatus::NumericalFailure;
 if(r.status==BurrStatus::Unbounded)st=InvBurrStatus::Unbounded;
 if(r.status==BurrStatus::WeibullBoundary)st=InvBurrStatus::InvWeibullBoundary;
 if(r.status==BurrStatus::ParetoBoundary)st=InvBurrStatus::PowerBoundary;
 if(r.status==BurrStatus::AllGeOne)st=InvBurrStatus::AllLeOne;
 InvBurrResult out{r.shape1,r.shape2,std::isnan(s)?1/r.scale:s,r.logLik+jac,r.iterations,st};
 out.stop_detail=invburr_stop_detail(r.stop_detail,jac,std::isnan(s)?NAN:s);
 if(!std::isnan(s))for(auto& candidate:out.stop_detail.candidates)
   if(candidate.model==StopModel::None&&candidate.coef.size()==3)candidate.coef[2]=s;
 if(st==InvBurrStatus::InvWeibullBoundary && r.stop_detail.limit_coef.size()==2)
   out.stop_detail.limit_coef={r.stop_detail.limit_coef[0],1/r.stop_detail.limit_coef[1]};
 if((st==InvBurrStatus::PowerBoundary||st==InvBurrStatus::AllLeOne) && r.stop_detail.limit_coef.size()==2)
   out.stop_detail.limit_coef={std::isnan(s)?1/r.stop_detail.limit_coef[1]:s,r.stop_detail.limit_coef[0]};
 return out;
}

}
