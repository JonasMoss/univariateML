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
// Reciprocation of a representable scale need not remain representable.
inline InvParalogisResult invparalogis_checked(InvParalogisResult r) {
  r.stop_detail.candidates.erase(std::remove_if(r.stop_detail.candidates.begin(),r.stop_detail.candidates.end(),
    [](const FitCandidate& c){return c.model==StopModel::None &&
      (c.coef.size()!=2 || !(c.coef[0]>0 && c.coef[1]>0) ||
       !std::isfinite(c.coef[0]) || !std::isfinite(c.coef[1]));}),r.stop_detail.candidates.end());
  if(!(r.shape>0 && r.scale>0) || !std::isfinite(r.shape) || !std::isfinite(r.scale) || !std::isfinite(r.logLik)) {
    if(r.status==ParalogisStatus::Ok){r.status=ParalogisStatus::NumericalFailure;r.certified=false;}
    r.stop_detail.accuracy_stop=AccuracyStop::None;r.stop_detail.attainable_tol=NAN;r.stop_detail.conditioning_bound=NAN;
  }
  return r;
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
  double physical=1/r.scale,ll=r.logLik+jacobian;
  if(r.status==ParalogisStatus::Ok&&!r.certified) {
    tol=std::min(tol,1e-10);
    double shape=r.shape,s=physical,value=ll;
    if(!paralogis_local_accuracy(x,shape,s,value,tol,maxit,false,true,r.stop_detail)){r.status=ParalogisStatus::NumericalFailure;record_accuracy(r,tol,{r.shape,r.scale},{r.shape,r.scale});}
    else if(std::abs(shape/r.shape-1)>tol*.125||std::abs(s/physical-1)>tol*.125){r.shape=shape;physical=s;ll=value;}
  }
  if(r.status==ParalogisStatus::Ok&&!r.certified) {
    tol=std::min(tol,1e-10);
    const double actual=burr_local_loglik(x,r.shape,r.shape,physical,true);
    const double margin=1024*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(actual));
    if(!std::isfinite(actual))r.status=ParalogisStatus::NumericalFailure;
    else if(std::abs(actual-ll)>margin)ll=actual;
  }
  return invparalogis_checked({r.shape,physical,ll,r.iterations,r.status,invparalogis_stop_detail(r.stop_detail,jacobian),r.certified});
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
  double physical=scale,ll=r.logLik+jacobian;
  if(r.status==ParalogisStatus::Ok&&!r.certified) {
    tol=std::min(tol,1e-10);
    double shape=r.shape,s=physical,value=ll;
    if(!paralogis_local_accuracy(x,shape,s,value,tol,maxit,true,true,r.stop_detail)){r.status=ParalogisStatus::NumericalFailure;record_accuracy(r,tol,{r.shape,r.scale},{r.shape,0});}
    else if(std::abs(shape/r.shape-1)>tol*.125){r.shape=shape;ll=value;}
  }
  if(r.status==ParalogisStatus::Ok&&!r.certified) {
    tol=std::min(tol,1e-10);
    const double actual=burr_local_loglik(x,r.shape,r.shape,scale,true);
    const double margin=1024*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(actual));
    if(!std::isfinite(actual))r.status=ParalogisStatus::NumericalFailure;
    else if(std::abs(actual-ll)>margin)ll=actual;
  }
  return invparalogis_checked({r.shape,scale,ll,r.iterations,r.status,invparalogis_stop_detail(r.stop_detail,jacobian),r.certified});
}
} // namespace cppunivariate

namespace cppunivariate {

inline InvParalogisResult invparalogis_mle_holds(span x,double shape,double scale=std::numeric_limits<double>::quiet_NaN(), double tol=1e-9, int maxit=1000){
  maxit = static_cast<int>(solver_budget(maxit));
 if(std::isnan(shape)) return std::isnan(scale)?invparalogis_mle(x,tol,maxit):invparalogis_mle_fixed_scale(x,scale,tol,maxit);
 std::vector<double> y;double jac=0;for(double v:x){y.push_back(1/v);jac-=2*std::log(v);}
 auto r=paralogis_mle_holds(span{y.data(),y.size()},shape,std::isnan(scale)?scale:1/scale,tol,maxit);
 return invparalogis_checked({shape,std::isnan(scale)?1/r.scale:scale,r.logLik+jac,r.iterations,r.status,invparalogis_stop_detail(r.stop_detail,jac),r.certified});
}

}
