// Inverse transformed gamma MLE: X ~ invtrgamma(shape1, shape2, scale)
// iff 1/X ~ trgamma(shape1, shape2, 1/scale). The transformed gamma fit
// runs on log(1/x) = -log(x); the log likelihood differs by the Jacobian
// -2 sum(log(x)). Faces mirror: lognormal stays lognormal, the power face
// with endpoint 1/min(x) becomes Pareto with minimum min(x).
#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"

#include "../trgamma/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class InvTrGammaStatus {
  Ok,
  LognormalBoundary,
  ParetoBoundary,
  ScaleOutOfRange,
  InvalidData,
  InvalidFixed,
  IterationFailure
};

struct InvTrGammaResult {
  double shape1, shape2, scale;
  double log_scale;
  double logLik;
  int iterations;
  InvTrGammaStatus status;
 StopDetail stop_detail{};
 bool reconstruction_failure=false;
 bool transformation_failure=false; };

inline InvTrGammaStatus invtrgamma_status(TrGammaStatus s) {
  switch (s) {
    case TrGammaStatus::Ok: return InvTrGammaStatus::Ok;
    case TrGammaStatus::LognormalBoundary: return InvTrGammaStatus::LognormalBoundary;
    case TrGammaStatus::PowerBoundary: return InvTrGammaStatus::ParetoBoundary;
    case TrGammaStatus::ScaleOutOfRange: return InvTrGammaStatus::ScaleOutOfRange;
    case TrGammaStatus::InvalidData: return InvTrGammaStatus::InvalidData;
    case TrGammaStatus::InvalidFixed: return InvTrGammaStatus::InvalidFixed;
    case TrGammaStatus::IterationFailure: return InvTrGammaStatus::IterationFailure;
  }
  return InvTrGammaStatus::IterationFailure;
}

inline StopDetail invtrgamma_transport_detail(StopDetail detail,double jacobian) {
  const auto transport=[](StopModel& model,std::vector<double>& coef) {
    if(model==StopModel::None && coef.size()==3)coef[2]=1/coef[2];
    if(model==StopModel::Lnorm && coef.size()==2)coef[0]=-coef[0];
    if(model==StopModel::Power && coef.size()==2) {
      coef={coef[1],1/coef[0]};model=StopModel::Pareto;
    }
  };
  detail.loglik_lower+=jacobian;detail.loglik_upper+=jacobian;
  detail.segment_loglik_upper+=jacobian;detail.limit_loglik+=jacobian;
  transport(detail.limit_model,detail.limit_coef);
  for(auto& c:detail.candidates) {
    transport(c.model,c.coef);
    c.loglik_lower+=jacobian;c.loglik_upper+=jacobian;
  }
  return detail;
}

inline InvTrGammaResult invtrgamma_mle(span x, double tol = 1e-10, int maxit = 100,
                                       double shape1 = std::numeric_limits<double>::quiet_NaN(),
                                       double shape2 = std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  std::vector<double> y(x.size());
  double jacobian = 0.0;
  for (std::size_t i = 0; i < x.size(); ++i) {
    if (!(x[i] > 0.0) || !std::isfinite(x[i])) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      return {shape1, shape2, nan, nan, nan, 0, InvTrGammaStatus::InvalidData};
    }
    y[i] = -std::log(x[i]);
    jacobian += 2.0 * y[i];
  }
  if(trgamma_detail::valid_fit_request(tol,maxit,shape1,shape2) &&
     trgamma_detail::lost_log_distinctions(x,{y.data(),y.size()})) {
    const double nan=std::numeric_limits<double>::quiet_NaN();
    InvTrGammaResult out{shape1,shape2,nan,nan,nan,0,InvTrGammaStatus::IterationFailure};
    out.transformation_failure=true;out.stop_detail.loglik_upper=INFINITY;return out;
  }
  TrGammaResult r = trgamma_fit_log(span{y.data(), y.size()}, tol, maxit, shape1, shape2);
  const double log_scale = -r.log_scale;
  const TrGammaStatus status = trgamma_range_status(r.status, log_scale);
  InvTrGammaResult out={r.shape1,r.shape2,std::exp(log_scale),log_scale,r.logLik+jacobian,r.iterations,invtrgamma_status(status)};
  out.stop_detail=invtrgamma_transport_detail(r.stop_detail,jacobian);
  out.reconstruction_failure=r.reconstruction_failure;
  out.transformation_failure=r.transformation_failure;
  if(!x.empty() && (out.status==InvTrGammaStatus::ParetoBoundary ||
      (out.stop_detail.limit_model==StopModel::None && std::isfinite(r.stop_detail.limit_loglik)))) {
    // The reciprocal endpoint can overflow even though the original Pareto
    // endpoint is a proper represented value. Rebuild only that face payload.
    const auto faces=trgamma_faces_log({y.data(),y.size()});
    const double endpoint=*std::min_element(x.begin(),x.end());
    if(faces.valid && faces.power_logLik>faces.lognormal_logLik) {
      if(out.status==InvTrGammaStatus::ParetoBoundary) {
        const double lower=out.stop_detail.loglik_lower,upper=out.stop_detail.loglik_upper;
        out.stop_detail=limit_detail({faces.power_shape,endpoint},out.logLik,StopModel::Pareto);
        out.stop_detail.loglik_lower=lower;out.stop_detail.loglik_upper=upper;
      } else {
        out.stop_detail.limit_model=StopModel::Pareto;
        out.stop_detail.limit_coef={faces.power_shape,endpoint};
        out.stop_detail.limit_loglik=faces.power_logLik+jacobian;
      }
    }
  }
  if(out.stop_detail.comparison_tie && out.stop_detail.limit_model==StopModel::Pareto &&
     std::none_of(out.stop_detail.candidates.begin(),out.stop_detail.candidates.end(),[](const FitCandidate& c){return c.model==StopModel::Pareto;}))
    out.stop_detail.candidates.push_back({out.stop_detail.limit_coef,out.stop_detail.limit_loglik,out.stop_detail.limit_loglik,StopModel::Pareto});
  if(out.stop_detail.limit_model==StopModel::Pareto && !x.empty()) {
    const double endpoint=*std::min_element(x.begin(),x.end());
    if(out.stop_detail.limit_coef.size()==2)out.stop_detail.limit_coef[1]=endpoint;
    for(auto& c:out.stop_detail.candidates)if(c.model==StopModel::Pareto && c.coef.size()==2)c.coef[1]=endpoint;
    if(out.status==InvTrGammaStatus::ParetoBoundary){out.scale=endpoint;out.log_scale=std::log(endpoint);}
  }
  return trgamma_detail::check_represented({y.data(),y.size()},out,InvTrGammaStatus::Ok,InvTrGammaStatus::IterationFailure,true,jacobian);
}

}  // namespace cppunivariate

namespace cppunivariate {

inline InvTrGammaResult invtrgamma_mle_fixed_scale(span x,double scale,double a=std::numeric_limits<double>::quiet_NaN(),double b=std::numeric_limits<double>::quiet_NaN(),double tol=1e-10,int maxit=100) {
  std::vector<double> y;
  double jac=0;
  for(double v:x) {
    const double logv=v>0 && std::isfinite(v)?std::log(v):NAN;
    y.push_back(-logv);jac-=2*logv;
  }
  if(!(scale>0) || !std::isfinite(scale)) {
    const double nan=std::numeric_limits<double>::quiet_NaN();
    return {a,b,scale,nan,nan,0,InvTrGammaStatus::InvalidFixed};
  }
  if(trgamma_detail::valid_fit_request(tol,maxit,a,b) &&
     trgamma_detail::lost_log_distinctions(x,{y.data(),y.size()},scale,-std::log(scale))) {
    InvTrGammaResult out{a,b,scale,std::log(scale),NAN,0,InvTrGammaStatus::IterationFailure};
    out.transformation_failure=true;out.stop_detail.loglik_upper=INFINITY;return out;
  }
  auto r=trgamma_fixed_scale_log({y.data(),y.size()},-std::log(scale),a,b,tol,maxit);
  InvTrGammaResult out={r.shape1,r.shape2,scale,std::log(scale),r.logLik+jac,r.iterations,invtrgamma_status(r.status)};
  out.stop_detail=invtrgamma_transport_detail(r.stop_detail,jac);
  out.reconstruction_failure=r.reconstruction_failure;
  out.transformation_failure=r.transformation_failure;
  if(!std::isnan(b))out.shape2=b;
  // The held endpoint is already represented in original coordinates;
  // its reciprocal need not be a representable double.
  if(out.stop_detail.limit_model==StopModel::Pareto && out.stop_detail.limit_coef.size()==2)
    out.stop_detail.limit_coef[1]=scale;
  for(auto& c:out.stop_detail.candidates) {
    if(c.model==StopModel::None && c.coef.size()==3) {
      c.coef[2]=scale;if(!std::isnan(b))c.coef[1]=b;
    }
    if(c.model==StopModel::Pareto && c.coef.size()==2)c.coef[1]=scale;
  }
  return trgamma_detail::check_represented({y.data(),y.size()},out,InvTrGammaStatus::Ok,InvTrGammaStatus::IterationFailure,true,jac);
}

}
