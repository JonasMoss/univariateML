// Inverse Burr is the reciprocal of a Burr with reciprocal scale.
#pragma once
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


enum class InvBurrStatus { Ok = 0, InvWeibullBoundary, PowerBoundary, InvalidInit, IterationFailure, AllLeOne, Unbounded };
struct InvBurrResult {
  double shape1, shape2, scale, logLik;
  int iterations;
  InvBurrStatus status;
};
inline InvBurrResult invburr_mle(span x, double tol, int maxit,
    double shape10 = std::numeric_limits<double>::quiet_NaN(),
    double shape20 = std::numeric_limits<double>::quiet_NaN(),
    double scale0 = std::numeric_limits<double>::quiet_NaN(), double reltol = 1e-12) {
  std::vector<double> reciprocal;
  double jacobian = 0.0;
  for (double v : x) {
    if (!(v > 0.0) || !std::isfinite(v) || !std::isfinite(1.0/v)) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      return {nan,nan,nan,nan,0,InvBurrStatus::InvalidInit};
    }
    reciprocal.push_back(1.0/v); jacobian -= 2.0*std::log(v);
  }
  if (x.size() > 1 && std::isnan(shape20)) {
    // The old inverse two-shape fit is an optional start, never a constraint.
    double center = 0.0;
    for (double v : x) center += std::log(v)/static_cast<double>(x.size());
    std::vector<double> normalized;
    bool representable = true;
    for (double v : x) {
      normalized.push_back(std::exp(std::log(v)-center));
      if (!(normalized.back() > 0.0) || !std::isfinite(normalized.back())) representable = false;
    }
    if (representable) {
      InvBurrCost fixed{span{normalized.data(),normalized.size()}};
      BfgsControl ctl; ctl.maxit=maxit; ctl.grad_tol=tol; ctl.reltol=reltol;
      const double start[2] = {1.0,2.0};
      auto seed = bfgs_minimize(fixed,start,ctl);
      if (std::isfinite(seed.f) && seed.theta[1]>0.0) shape20=seed.theta[1];
    }
  }
  auto r = burr_mle(span{reciprocal.data(),reciprocal.size()},tol,maxit,
    shape10,shape20,std::isnan(scale0) ? scale0 : 1.0/scale0,reltol);
  InvBurrStatus status = InvBurrStatus::Ok;
  if (r.status == BurrStatus::WeibullBoundary) status = InvBurrStatus::InvWeibullBoundary;
  if (r.status == BurrStatus::Unbounded) status = InvBurrStatus::Unbounded;
  if (r.status == BurrStatus::ParetoBoundary) status = InvBurrStatus::PowerBoundary;
  if (r.status == BurrStatus::InvalidInit) status = InvBurrStatus::InvalidInit;
  if (r.status == BurrStatus::IterationFailure) status = InvBurrStatus::IterationFailure;
  if (status == InvBurrStatus::InvWeibullBoundary) {
    const auto face = invweibull_mle(x,1e-10,std::max(maxit,100));
    return {INFINITY,face.shape,face.scale,face.logLik,r.iterations,status};
  }
  if (status == InvBurrStatus::PowerBoundary) {
    const auto face = power_mle(x,0.0);
    return {0.0,INFINITY,face.alpha,face.logLik,r.iterations,status};
  }
  return {r.shape1,r.shape2,1.0/r.scale,r.logLik+jacobian,r.iterations,status};
}

// Fixed scale reduces exactly to the historical scale-1 two-shape fit.
inline InvBurrResult invburr_mle_fixed_scale(span x, double scale,
    double tol = 1.220703e-4, int maxit = 1000) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (!(scale > 0.0) || !std::isfinite(scale))
    return {nan,nan,scale,nan,0,InvBurrStatus::InvalidInit};
  // With both shapes free, constant data on this side of the held
  // scale concentrate as shape2 grows (shape1 is profiled). Test the
  // original observations exactly, before normalization or any search.
  bool constant = x.size() > 0;
  for (double v : x) {
    if (!(v > 0.0) || !std::isfinite(v))
      return {nan,nan,scale,nan,0,InvBurrStatus::InvalidInit};
    if (v != x[0]) constant = false;
  }
  if (constant && x[0] >= scale)
    return {nan,nan,scale,INFINITY,0,InvBurrStatus::Unbounded};
  std::vector<double> normalized;
  for (double v : x) normalized.push_back(v / scale);
  const auto r = burr_fixed_detail::invburr_mle(span{normalized.data(),normalized.size()},tol,maxit);
  InvBurrStatus status = InvBurrStatus::Ok;
  if (r.status == burr_fixed_detail::InvBurrStatus::AllLeOne) status = InvBurrStatus::AllLeOne;
  if (r.status == burr_fixed_detail::InvBurrStatus::InvalidInit) status = InvBurrStatus::InvalidInit;
  if (r.status == burr_fixed_detail::InvBurrStatus::IterationFailure) status = InvBurrStatus::IterationFailure;
  return {r.shape1,r.shape2,scale,r.logLik-static_cast<double>(x.size())*std::log(scale),r.iterations,status};
}
} // namespace cppunivariate

namespace cppunivariate {

inline InvBurrResult invburr_mle_holds(span x,double a,double b,double s,double tol=1e-9,int maxit=1000){
 std::vector<double> y;double jac=0;
 for(double v:x){y.push_back(1/v);jac-=2*std::log(v);}
 auto r=burr_mle_holds(span{y.data(),y.size()},a,b,std::isnan(s)?s:1/s,tol,maxit);
 InvBurrStatus st=InvBurrStatus::Ok;
 if(r.status==BurrStatus::InvalidInit)st=InvBurrStatus::InvalidInit;
 if(r.status==BurrStatus::IterationFailure)st=InvBurrStatus::IterationFailure;
 if(r.status==BurrStatus::Unbounded)st=InvBurrStatus::Unbounded;
 if(r.status==BurrStatus::WeibullBoundary)st=InvBurrStatus::InvWeibullBoundary;
 if(r.status==BurrStatus::ParetoBoundary)st=InvBurrStatus::PowerBoundary;
 if(r.status==BurrStatus::AllGeOne)st=InvBurrStatus::AllLeOne;
 return {r.shape1,r.shape2,std::isnan(s)?1/r.scale:s,r.logLik+jac,r.iterations,st};
}

}
