#pragma once
#include "../../core/discrete_stop.hpp"
#include "../../special/cmp.hpp"
#include "dpqr.hpp"

namespace cppunivariate {
// Compatibility family entry point; the shared public solver owns closure.
struct CmpDomainResult : CmpResult { StopDetail stop_detail{}; };
inline CmpDomainResult cmp_domain_mle(span x, const CmpFitControl& ctl = {}) {
  CmpDomainResult out; static_cast<CmpResult&>(out)=cmp_mle(x,ctl);
  if(out.status==CmpStatus::PointMassLimit) out.stop_detail=discrete_point(x[0]);
  if(out.status==CmpStatus::TwoPointLimit) {
    const bool fixed=std::isfinite(ctl.fixed_lambda);
    const double lo=fixed?0:*std::min_element(x.begin(),x.end());
    out.stop_detail=discrete_two_point(x,lo,lo+1,fixed?ctl.fixed_lambda/(1+ctl.fixed_lambda):NAN,fixed?std::log(ctl.fixed_lambda):NAN);
  }
  if(out.status==CmpStatus::GeometricLimit) {
    double m=0;for(double v:x)m+=v;m/=x.size();
    const double p=std::isnan(ctl.fixed_lambda)?1/(1+m):1-ctl.fixed_lambda;
    double ll=0;for(double v:x)ll+=std::log(p)+v*std::log1p(-p);
    out.stop_detail=discrete_face(StopModel::Geom,{p},ll);
  }
  return out;
}
}
