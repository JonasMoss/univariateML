#pragma once
// Included inside cppunivariate, after SgedResult and the GED public fitter.
namespace sged_free_shape_certificate {
inline bool balanced_uniform(span x,SgedResult& out) {
  // With free moments, every fixed positive skew reaches this uniform face.
  // For any log-concave density, integrating the chord of log f on [l,h]
  // gives 1 >= (h-l)*sqrt(f(l)*f(h)). Equal endpoint counts therefore
  // bound the whole likelihood by -n log(h-l), strictly for finite SGED.
  const auto mm=std::minmax_element(x.begin(),x.end());
  const double lo=*mm.first,hi=*mm.second;
  if(!(lo<hi))return false;
  std::size_t left=0,right=0;
  for(double v:x) {
    if(v==lo)++left;
    else if(v==hi)++right;
    else return false;
  }
  if(left!=right)return false;
  out={NAN,NAN,NAN,NAN,NAN,0,SgedStatus::CertificateFailure};
  out.stop_detail.loglik_upper=INFINITY;
  // Compute width via its half to avoid overflow for opposite large ends.
  // Underflow or inaccurate reconstruction is a numerical stop.
  const double radius=hi/2-lo/2;
  if(!(radius>=std::numeric_limits<double>::min()) || !std::isfinite(radius))return true;
  const double ll=-static_cast<double>(x.size())*(std::log(2.)+std::log(radius));
  if(!std::isfinite(ll))return true;
  const double pad=4096*std::numeric_limits<double>::epsilon()*(1+x.size()+std::abs(ll));
  out.status=SgedStatus::UniformBoundary;out.logLik=ll;out.certified=true;
  out.stop_detail=limit_detail({lo,hi},ll,StopModel::Unif);
  out.stop_detail.loglik_lower=ll-pad;out.stop_detail.loglik_upper=ll+pad;
  for(auto& c:out.stop_detail.candidates){c.loglik_lower=ll-pad;c.loglik_upper=ll+pad;}
  return true;
}
inline SgedResult symmetric(span x,double tol,int maxit,double mean,double sd) {
  // xi is HELD at exactly one. The public moment map is then the identity
  // for every nu, including held moments and all limiting uniform paths.
  // Do not apply this reduction to an estimated skew near one.
  const auto r=ged_mle(x,tol,maxit,mean,sd,NAN);
  SgedResult out{r.mu,r.sigma,r.nu,1,r.logLik,r.iterations,SgedStatus::CertificateFailure};
  out.restricted=r.restricted;
  out.stop_detail=r.stop_detail;
  for(auto& c:out.stop_detail.candidates)
    if(c.model==StopModel::None && c.coef.size()==3)c.coef.push_back(1);
  // Face coordinates belong to the face model, so uniform endpoints and
  // all bounds remain unchanged. An accepted parent search remains accepted
  // when its cover is unclosed; its flag carries the missing certificate.
  switch(r.status) {
    case GedStatus::Ok:
      out.status=SgedStatus::Ok;out.certified=r.certified;
      break;
    case GedStatus::UniformBoundary:
      if(r.certified){out.status=SgedStatus::UniformBoundary;out.certified=true;}
      else {
        out.stop_detail.limit_coef.clear();out.stop_detail.limit_loglik=NAN;
        out.stop_detail.limit_model=StopModel::None;
      }
      break;
    case GedStatus::InvalidStart:
    case GedStatus::InvalidShape: out.status=SgedStatus::InvalidStart;break;
    case GedStatus::Nonunique: out.stop_detail.comparison_tie=true;break;
    case GedStatus::BfgsFailure:
    case GedStatus::UniformUnresolved:
    case GedStatus::TransformFailure: break;
  }
  // Exact collapse is a statistical stop, independent of the delegate's
  // choice of invalid-start versus zero-scale numerical representation.
  if(std::isnan(sd) && (std::isnan(mean) || mean==x[0]) &&
      std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];})) {
    out.status=SgedStatus::Unbounded;out.certified=false;out.stop_detail={};
  }
  return out;
}
} // namespace sged_free_shape_certificate

#include "weighted_shape_certificate.hpp"
#include "mode_skew_certificate.hpp"
