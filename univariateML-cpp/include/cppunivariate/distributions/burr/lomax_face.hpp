// Strict exponential-face comparison for the owned transformed families.
#pragma once
namespace cppunivariate {
inline bool fallback_lomax_face_proved(span x,int maxit) {
  if(x.empty())return false;
  double largest=0;bool constant=true;
  for(double v:x){if(!(v>0)||!std::isfinite(v))return false;largest=std::max(largest,v);constant=constant&&v==x[0];}
  if(constant)return true;
  lomax_cert_detail::Sum mean;
  const double n=static_cast<double>(x.size());
  for(double v:x)mean.add((v/largest)/n);
  lomax_cert_detail::Data d;
  d.error=512*std::numeric_limits<double>::epsilon()*(1+std::abs(std::log(largest))+std::abs(std::log(mean.value)))*(1+n*std::numeric_limits<double>::epsilon());
  lomax_cert_detail::Sum logs,seconds;
  for(double v:x){
    const double scaled=v/largest,y=scaled/mean.value;
    if(scaled<std::numeric_limits<double>::min()||!(y>0)||!std::isfinite(y))return false;
    const double ly=std::log(y);d.error+=256*std::numeric_limits<double>::epsilon()*std::abs(ly)/n;
    d.values.push_back({y,ly,1/n});logs.add(ly/n);seconds.add(y*y/n);
  }
  d.logubar=-logs.value;d.second=seconds.value;
  const double m=d.second*(1+8*d.error);
  if(!(m<2-16*d.error))return false;
  // A >= q/(1+q M2/2). Its resulting bound for h is strictly
  // decreasing below t=2/M2-1, where t=q M2/2.
  const double lo=std::log((2/m)*(2/m-1)*.5);
  const double hi=d.logubar+2*std::exp(d.logubar+8*d.error);
  if(!std::isfinite(lo)||!std::isfinite(hi)||!(lo<hi))return false;
  const auto a=lomax_cert_detail::evaluate(d,lo),b=lomax_cert_detail::evaluate(d,hi);
  std::priority_queue<lomax_cert_detail::Segment> queue;
  queue.push({a,b,lomax_cert_detail::upper(d,a,b)});
  std::int64_t work=2,limit=certificate_budget(maxit);
  while(!queue.empty()){
    const auto s=queue.top();queue.pop();
    if(s.upper<0)continue;
    if(work>=limit)return false;
    const double z=(s.a.z+s.b.z)/2;
    if(!(z>s.a.z&&z<s.b.z))return false;
    const auto p=lomax_cert_detail::evaluate(d,z);++work;
    if(!std::isfinite(p.h)||p.h>p.error)return false;
    queue.push({s.a,p,lomax_cert_detail::upper(d,s.a,p)});
    queue.push({p,s.b,lomax_cert_detail::upper(d,p,s.b)});
  }
  return true;
}
// A reachable face is a lower bound even when its global comparison is
// unclosed. Keep it as a witness, not as a selected limit-model payload.
inline void fallback_retain_unclosed_face(StopDetail& detail,StopModel model) {
  if(std::isfinite(detail.limit_loglik)) {
    detail.loglik_lower=std::isnan(detail.loglik_lower)?detail.limit_loglik:
      std::max(detail.loglik_lower,detail.limit_loglik);
    if(valid_stop_coefficients(model,detail.limit_coef))
      detail.candidates.push_back({detail.limit_coef,detail.limit_loglik,detail.limit_loglik,model});
  }
  detail.limit_coef.clear();detail.limit_loglik=NAN;detail.limit_model=StopModel::None;
}

}
