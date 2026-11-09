// Included after Gb2Result. A pointwise density bound selects an endpoint
// face for held opposite beta shape below one; see the family dossier.
#pragma once
namespace cppunivariate {
namespace gb2_endpoint_detail {
inline Gb2Result fit(span x,double q,double p,double b,double tol,bool inverse) {
  Gb2Result out{q,NAN,p,b,NAN,0,Gb2Status::NumericalFailure};
  const double lb=std::log(b),eps=std::numeric_limits<double>::epsilon();
  betapr_cert_detail::Sum distance,logs,magnitude,uncertainty;
  std::size_t ties=0;
  for(double v:x) {
    const double lv=std::log(v),w=inverse?lv-lb:lb-lv;
    if(v==b)++ties;
    else if(!(w>0)||!std::isfinite(w))return out;
    distance.add(w);logs.add(lv);magnitude.add(std::abs(lv));
    if(v!=b)uncertainty.add(128*eps*(1+std::abs(lv)+std::abs(lb)));
  }
  const double s=distance.value,err=uncertainty.value,n=static_cast<double>(x.size());
  if(!(s>err)||err/(s-err)>=std::min(tol,1e-9))return out;
  const double penalty=ties*(inverse?p:q)*std::log(2.);
  const double rate=n/s,ll=n*std::log(rate)-n-logs.value-penalty;
  const double error=128*eps*(1+magnitude.value+std::abs(penalty)+n*(1+std::abs(std::log(rate))))+rate*err;
  if(!(rate>0)||!std::isfinite(rate)||!std::isfinite(ll)||!std::isfinite(error))return out;
  out.status=inverse?Gb2Status::ParetoBoundary:Gb2Status::PowerBoundary;
  out.logLik=ll;
  out.stop_detail=limit_detail(inverse?std::vector<double>{rate,b}:std::vector<double>{b,rate},ll,inverse?StopModel::Pareto:StopModel::Power);
  out.stop_detail.loglik_lower=ll-error;out.stop_detail.loglik_upper=ll+error;
  out.stop_detail.candidates[0].loglik_lower=ll-error;
  out.stop_detail.candidates[0].loglik_upper=ll+error;
  return out;
}
} // namespace gb2_endpoint_detail
} // namespace cppunivariate
