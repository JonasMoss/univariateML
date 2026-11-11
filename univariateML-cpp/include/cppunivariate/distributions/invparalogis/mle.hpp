// Actuar scale is the reciprocal of the paralogistic scale on 1/x.
// The fixed-scale path preserves the supplied value exactly.
#pragma once
#include "../../core/work_budget.hpp"
#include "../paralogis/mle.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
struct InvParalogisResult { double shape, scale; double logLik; int iterations; ParalogisStatus status; StopDetail stop_detail = {}; bool certified=true; };
inline StopDetail invparalogis_stop_detail(StopDetail d,double jacobian) {
  for(auto& c:d.candidates){if(c.coef.size()==2)c.coef[1]=1/c.coef[1];
    c.loglik_lower+=jacobian;c.loglik_upper+=jacobian;}
  d.loglik_lower+=jacobian;d.loglik_upper+=jacobian;
  d.segment_loglik_upper+=jacobian;return d;
}
inline InvParalogisResult invparalogis_mle(span x, double tol, int maxit, double shape0 = std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  for (double v : x) {
    if (!(v > 0) || !std::isfinite(v)) {
      const double nan=std::numeric_limits<double>::quiet_NaN();
      return {nan,nan,nan,0,ParalogisStatus::InvalidStart};
    }
  }
  if(!x.empty()&&std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];})) {
    StopDetail d;d.loglik_lower=d.loglik_upper=INFINITY;
    return {NAN,NAN,INFINITY,0,ParalogisStatus::Unbounded,d};
  }
  for(double v:x)if(!std::isfinite(1/v)||!(1/v>0))
    return {NAN,NAN,NAN,0,ParalogisStatus::NumericalFailure};
  std::vector<double> y(x.size());
  double jacobian = 0;
  for (std::size_t i=0; i<x.size(); ++i) { y[i]=1.0/x[i]; jacobian -= 2*std::log(x[i]); }
  auto r = paralogis_mle(span{y.data(), y.size()}, tol, maxit, shape0);
  // Original constant samples were handled above. Reciprocation can merge
  // distinct adjacent doubles; that is loss of resolution, not nonexistence.
  if(r.status==ParalogisStatus::Unbounded)
    return {NAN,NAN,NAN,r.iterations,ParalogisStatus::NumericalFailure};
  return {r.shape, 1.0/r.scale, r.logLik+jacobian, r.iterations, r.status,invparalogis_stop_detail(r.stop_detail,jacobian),r.certified};
}
inline InvParalogisResult invparalogis_mle_fixed_scale(span x, double scale,
    double tol=1e-10, int maxit=100,
    double shape0=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  if (!(scale > 0) || !std::isfinite(scale)) {
    const double nan=std::numeric_limits<double>::quiet_NaN();
    return {nan,scale,nan,0,ParalogisStatus::InvalidStart};
  }
  for(double v:x)if(!(v>0)||!std::isfinite(v))
    return {NAN,scale,NAN,0,ParalogisStatus::InvalidStart};
  if(!std::isfinite(1/scale)||!(1/scale>0))
    return {NAN,scale,NAN,0,ParalogisStatus::NumericalFailure};
  for(double v:x)if(!std::isfinite(1/v)||!(1/v>0))
    return {NAN,scale,NAN,0,ParalogisStatus::NumericalFailure};
  std::vector<double> y(x.size());
  double jacobian=0;
  for (std::size_t i=0; i<x.size(); ++i) { y[i]=1/x[i]; jacobian-=2*std::log(x[i]); }
  auto r=paralogis_mle_fixed_scale(span{y.data(),y.size()},1/scale,tol,maxit,shape0);
  return {r.shape,scale,r.logLik+jacobian,r.iterations,r.status,invparalogis_stop_detail(r.stop_detail,jacobian),r.certified};
}
} // namespace cppunivariate

namespace cppunivariate {

inline InvParalogisResult invparalogis_mle_holds(span x,double shape,double scale=std::numeric_limits<double>::quiet_NaN(), double tol=1e-9, int maxit=1000){
  maxit = static_cast<int>(solver_budget(maxit));
 if(std::isnan(shape)) return std::isnan(scale)?invparalogis_mle(x,tol,maxit):invparalogis_mle_fixed_scale(x,scale,tol,maxit);
 std::vector<double> y;double jac=0;for(double v:x){y.push_back(1/v);jac-=2*std::log(v);}
 auto r=paralogis_mle_holds(span{y.data(),y.size()},shape,std::isnan(scale)?scale:1/scale,tol,maxit);
 return {shape,std::isnan(scale)?1/r.scale:scale,r.logLik+jac,r.iterations,r.status,invparalogis_stop_detail(r.stop_detail,jac),r.certified};
}

}
