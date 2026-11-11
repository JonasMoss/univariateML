// Included after Gb2Result. Exact held-power beta-prime transport; see README.
#pragma once
namespace cppunivariate {
namespace gb2_power_detail {
inline Gb2Result fit(span x,double q,double a,double p,double tol,int maxit) {
  Gb2Result out{q,a,p,NAN,NAN,0,Gb2Status::NumericalFailure};
  if(x.empty()){out.status=Gb2Status::InvalidData;return out;}
  double lo=INFINITY,hi=-INFINITY;
  for(double v:x) {
    if(!(v>0)||!std::isfinite(v)){out.status=Gb2Status::InvalidData;return out;}
    lo=std::min(lo,std::log(v));hi=std::max(hi,std::log(v));
  }
  const double center=lo/2+hi/2;
  if(!std::isfinite(a*center))return out;
  std::vector<double> y;
  betapr_cert_detail::Sum jac_sum,jac_magnitude;
  for(double v:x) {
    const double lx=std::log(v),ly=a*(lx-center),value=std::exp(ly);
    // Do not turn a representational collapse into a constant-sample theorem.
    if(!(value>0)||!std::isfinite(value)||
       (v!=x[0] && value==std::exp(a*(std::log(x[0])-center))))return out;
    y.push_back(value);
    const double term=std::log(a)-lx+ly;
    if(!std::isfinite(term))return out;
    jac_sum.add(term);jac_magnitude.add(std::abs(term));
  }
  if(!std::isfinite(jac_sum.value)||!std::isfinite(jac_magnitude.value))return out;
  std::vector<std::size_t> order;
  for(std::size_t i=0;i<x.size();++i)order.push_back(i);
  std::sort(order.begin(),order.end(),[&](std::size_t i,std::size_t j){return x[i]<x[j];});
  for(std::size_t i=1;i<order.size();++i)
    if(x[order[i]]!=x[order[i-1]] && y[order[i]]==y[order[i-1]])return out;
  auto r=betapr_mle_holds(span{y.data(),y.size()},p,q,NAN,tol,maxit);
  out.iterations=r.iterations;out.logLik=r.logLik+jac_sum.value;
  out.stop_detail=r.stop_detail;out.certified=r.certified;
  if(r.status==BetaPrimeStatus::Ok)out.status=Gb2Status::Ok;
  else if(r.status==BetaPrimeStatus::GammaBoundary)out.status=Gb2Status::TrGammaBoundary;
  else if(r.status==BetaPrimeStatus::InvGammaBoundary)out.status=Gb2Status::InvTrGammaBoundary;
  else if(r.certificate_failure==LomaxCertificateFailure::Budget)out.status=Gb2Status::CertificateBudget;
  else if(r.stop_detail.comparison_tie)out.status=Gb2Status::CertificateTie;
  else out.status=Gb2Status::SearchUnresolved;
  const double eps=std::numeric_limits<double>::epsilon();
  const double jac_error=128*eps*(1+jac_magnitude.value+x.size()*(1+std::abs(a*center)));
  if(!std::isfinite(jac_error)){out.status=Gb2Status::NumericalFailure;out.stop_detail={};return out;}
  const auto scale=[&](double s) -> double {
    if(!(s>0)||!std::isfinite(s))return NAN;
    const double ls=center+std::log(s)/a;
    const double v=std::exp(ls);
    if(!(v>0)||!std::isfinite(v)||(out.certified && 128*eps*(1+std::abs(center)+(1+std::abs(std::log(s)))/a)>1e-7))return NAN;
    return v;
  };
  // Each surviving segment in the parent emits its left endpoint, and its
  // shape and scale widths are <1e-10 relative. Recover a conservative scale
  // enclosure before raising to 1/a; the parent's 1e-6 alone would not suffice
  // for small held powers. Include the roundoff in its log-scale inner solves.
  double sl=INFINITY,su=-INFINITY;
  for(const auto& c:r.stop_detail.candidates)if(c.model==StopModel::None && c.coef.size()==3) {
    const double s=c.coef[2],e=2e-10+128*eps*(1+std::abs(std::log(s)));
    sl=std::min(sl,std::log(s)-e);su=std::max(su,std::log(s)+e);
  }
  if(out.status==Gb2Status::Ok && out.certified && (!(su>=sl)||(su-sl)/a>std::log1p(1e-6)))
    out.status=Gb2Status::NumericalFailure;
  out.shape1=std::isnan(q)?r.beta:q;out.shape3=std::isnan(p)?r.alpha:p;
  out.scale=scale(r.scale);
  auto& d=out.stop_detail;
  d.loglik_lower+=jac_sum.value-jac_error;d.loglik_upper+=jac_sum.value+jac_error;
  d.segment_loglik_upper+=jac_sum.value+jac_error;d.limit_loglik+=jac_sum.value;
  bool invalid=false;
  const auto convert=[&](StopModel& model,std::vector<double>& coef) {
    if(model==StopModel::None && coef.size()==3)coef={coef[1],a,coef[0],scale(coef[2])};
    else if((model==StopModel::Gamma||model==StopModel::InvGamma)&&coef.size()==2) {
      model=model==StopModel::Gamma?StopModel::Trgamma:StopModel::InvTrgamma;
      coef={coef[0],a,scale(coef[1])};
    } else {invalid=true;return;}
    if(!valid_stop_coefficients(model,coef))invalid=true;
  };
  for(auto& c:d.candidates) {
    convert(c.model,c.coef);
    c.loglik_lower+=jac_sum.value-jac_error;c.loglik_upper+=jac_sum.value+jac_error;
    if(!std::isfinite(c.loglik_lower)||!std::isfinite(c.loglik_upper))invalid=true;
  }
  if(!d.limit_coef.empty())convert(d.limit_model,d.limit_coef);
  // The reciprocal parent labels an unselected, empty face InvGamma.
  // It is not a selected GB2 limit and must not leak into binding metadata.
  if(d.limit_coef.empty())d.limit_model=StopModel::None;
  d.candidates.erase(std::remove_if(d.candidates.begin(),d.candidates.end(),
    [](const FitCandidate& c){return !valid_stop_coefficients(c.model,c.coef)||
      !std::isfinite(c.loglik_lower)||!std::isfinite(c.loglik_upper);}),d.candidates.end());
  if(!d.limit_coef.empty()&&(!valid_stop_coefficients(d.limit_model,d.limit_coef)||
      !std::isfinite(d.limit_loglik))) {
    invalid=true;
    d.limit_coef.clear();d.limit_model=StopModel::None;d.limit_loglik=NAN;
  }
  if((invalid && (out.status!=Gb2Status::Ok || out.certified)) || (out.status==Gb2Status::Ok && !std::isfinite(out.scale)) ||
     ((out.status==Gb2Status::Ok||out.status==Gb2Status::TrGammaBoundary||
       out.status==Gb2Status::InvTrGammaBoundary)&&!std::isfinite(out.logLik)))out.status=Gb2Status::NumericalFailure;
  return out;
}
} // namespace gb2_power_detail
} // namespace cppunivariate
