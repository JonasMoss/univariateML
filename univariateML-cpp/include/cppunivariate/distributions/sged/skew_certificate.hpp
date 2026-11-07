#pragma once
// Included inside cppunivariate, after SgedCost.
namespace sged_skew_certificate {
inline SgedResult normal(span x,int maxit,double mean,double sd) {
  // The public normal entry point selects its joint polynomial certificate
  // when both moments are free, and its conditional certificate otherwise.
  const auto r=snorm_mle(x,1e-10,maxit,mean,sd,NAN);
  SgedResult out{r.mu,r.sigma,2,r.xi,r.logLik,r.iterations,SgedStatus::CertificateFailure};
  out.stop_detail=r.stop_detail;
  bool representable=true;
  const auto half_ged_scale=[](double scale) {
    const double converted=scale*std::sqrt(2.);
    if(!(scale>0 && converted>0 && std::isfinite(converted)) ||
        std::abs(std::log(converted)-std::log(scale)-.5*std::log(2.))>1e-10)return double(NAN);
    return converted;
  };
  for(auto& c:out.stop_detail.candidates) {
    if(c.model==StopModel::None && c.coef.size()==3)c.coef.insert(c.coef.begin()+2,2);
    if(c.model==StopModel::Hnorm || c.model==StopModel::ReflectedHnorm) {
      c.coef[1]=half_ged_scale(c.coef[1]);c.coef.push_back(2);
      if(!std::isfinite(c.coef[1]))representable=false;
      c.model=c.model==StopModel::Hnorm?StopModel::HalfGed:StopModel::ReflectedHalfGed;
    }
  }
  if(out.stop_detail.limit_model==StopModel::Hnorm || out.stop_detail.limit_model==StopModel::ReflectedHnorm) {
    out.stop_detail.limit_coef[1]=half_ged_scale(out.stop_detail.limit_coef[1]);out.stop_detail.limit_coef.push_back(2);
    if(!std::isfinite(out.stop_detail.limit_coef[1]))representable=false;
    out.stop_detail.limit_model=out.stop_detail.limit_model==StopModel::Hnorm?StopModel::HalfGed:StopModel::ReflectedHalfGed;
  }
  if(!representable) {
    out.stop_detail={};out.stop_detail.loglik_upper=INFINITY;
    return out; // No representable half-GED payload; retain numerical status.
  }
  if(r.status==SnormStatus::Ok){out.status=SgedStatus::Ok;out.certified=true;}
  if(r.status==SnormStatus::Unbounded)out.status=SgedStatus::Unbounded;
  if(r.status==SnormStatus::RightHalfNormalBoundary)out.status=SgedStatus::RightHalfGedBoundary;
  if(r.status==SnormStatus::LeftHalfNormalBoundary)out.status=SgedStatus::LeftHalfGedBoundary;
  return out;
}
inline SgedResult subunit(span x,int maxit,double nu) {
  SgedResult out{NAN,NAN,nu,NAN,NAN,0,SgedStatus::CertificateFailure};
  out.stop_detail.loglik_upper=INFINITY;
  std::vector<double> raw(x.begin(),x.end());std::sort(raw.begin(),raw.end());
  const double origin=raw.front(),unit=raw.back()-origin,n=static_cast<double>(x.size());
  if(unit==0){out.status=SgedStatus::Unbounded;return out;}
  if(!(unit>0 && std::isfinite(unit)))return out;
  std::vector<double> y;for(double v:raw)y.push_back((v-origin)/unit);
  for(std::size_t i=1;i<y.size();++i)if(y[i]==y[i-1] && raw[i]!=raw[i-1])return out;
  std::size_t knots=1;
  for(std::size_t i=1;i<y.size();++i)if(y[i]!=y[i-1])++knots;
  const bool fundable=knots<=static_cast<std::size_t>(certificate_budget(maxit));
  const double k=std::exp(std::lgamma(2/nu)-std::lgamma(1/nu));
  const double v=std::exp(std::lgamma(3/nu)-std::lgamma(1/nu));
  if(!(v>k*k) || !std::isfinite(v+k))return out;
  std::vector<FitCandidate> candidates;
  SgedResult best=out;double bestll=-INFINITY;
  for(std::size_t i=0;i<y.size();) {
    if(out.iterations>=certificate_budget(maxit)){out.stop_detail.budget_exhausted=true;out.stop_detail.candidates=candidates;return out;}
    ++out.iterations;
    const double c=y[i];sged_cert_detail::Sum left,right;
    for(double z:y){if(z<c)left.add(std::pow(c-z,nu));else right.add(std::pow(z-c,nu));}
    const double A=left.value,B=right.value;
    const double ar=std::pow(A,1/(nu+1)),br=std::pow(B,1/(nu+1)),S=ar+br;
    const double t=std::exp(std::log(nu*S/n)/nu),a=t*ar,b=t*br;
    const double ll=n*(std::log(nu)-std::lgamma(1/nu)-std::log(t*S)-std::log(unit)-1/nu);
    if(!(S>0 && t>0) || !std::isfinite(ll+a+b) ||
        (A>0 && !(a>0)) || (B>0 && !(b>0)))return out;
    const double pad=4096*std::numeric_limits<double>::epsilon()*(1+n+std::abs(ll))*(1+1/nu);
    FitCandidate witness;
    if(a==0 || b==0) {
      const auto model=a==0?StopModel::HalfGed:StopModel::ReflectedHalfGed;
      witness={{raw[i],unit*(a+b),nu},ll-pad,ll+pad,model};
      if(!valid_stop_coefficients(model,witness.coef) ||
          std::abs(std::log(witness.coef[1])-std::log(unit)-std::log(a+b))>1e-10)return out;
      sged_cert_detail::Sum penalty;
      for(double z:raw)if(z!=raw[i])
        penalty.add(std::exp(nu*(std::log(std::abs(z-raw[i]))-std::log(witness.coef[1]))));
      const double actual=n*(std::log(nu)-std::lgamma(1/nu)-std::log(witness.coef[1]))-penalty.value;
      if(!std::isfinite(actual) || std::abs(actual-ll)>1e-10*(n+std::abs(ll)))return out;
      witness.loglik_lower=std::min(ll,actual)-pad;witness.loglik_upper=std::max(ll,actual)+pad;
    } else {
      SgedResult r{raw[i]+unit*k*(b-a),unit*std::sqrt(v*(a*a-a*b+b*b)-k*k*(b-a)*(b-a)),nu,std::sqrt(b/a),ll,out.iterations,SgedStatus::Ok};
      const double ideal_sd=std::sqrt(v*(a*a-a*b+b*b)-k*k*(b-a)*(b-a));
      if(!(r.sigma>0) || !std::isfinite(r.sigma) ||
          std::abs(std::log(r.sigma)-std::log(unit)-std::log(ideal_sd))>1e-10)return out;
      const double roundoff=256*std::numeric_limits<double>::epsilon()*
          (std::abs(raw[i])+std::abs(unit*k*(b-a)));
      if(roundoff>1e-7*(1+std::abs(r.mu)))return out;
      const double phi[4]={r.mu,std::log(r.sigma),std::log(nu),std::log(r.xi)};
      const double actual=-SgedCost{x}(phi);
      // Cusp reconstruction can amplify coordinate rounding by power nu.
      // Require both public mode recovery and likelihood agreement.
      double shift=0;const double su=distributions::fs::sged_sigma(nu,r.xi,shift);
      const double recovered=r.mu-r.sigma*shift/su;
      if(!(r.sigma>0) || !std::isfinite(r.mu+r.sigma+r.xi+actual) ||
          std::abs(recovered-raw[i])>1e-10*(1+std::abs(raw[i])) ||
          std::abs(actual-ll)>1e-7*(n+std::abs(ll)))return out;
      witness={{r.mu,r.sigma,nu,r.xi},std::min(ll,actual)-pad,std::max(ll,actual)+pad};
      if(ll>bestll)best=r;
    }
    if(ll>bestll)bestll=ll;
    out.stop_detail.loglik_lower=std::isnan(out.stop_detail.loglik_lower)?witness.loglik_lower:
        std::max(out.stop_detail.loglik_lower,witness.loglik_lower);
    candidates.push_back(witness);
    // A known unfundable partition stops after its first O(n) face witness,
    // rather than spending O(n * budget) before an inevitable budget stop.
    if(!fundable) {
      out.stop_detail.budget_exhausted=true;out.stop_detail.candidates=candidates;return out;
    }
    const double knot=y[i];while(i<y.size() && y[i]==knot)++i;
  }
  double lower=-INFINITY,upper=-INFINITY;
  for(const auto& c:candidates){lower=std::max(lower,c.loglik_lower);upper=std::max(upper,c.loglik_upper);}
  candidates.erase(std::remove_if(candidates.begin(),candidates.end(),[&](const FitCandidate& c){return c.loglik_upper<lower;}),candidates.end());
  out.stop_detail.candidates=candidates;out.stop_detail.loglik_lower=lower;out.stop_detail.loglik_upper=upper;
  if(candidates.size()!=1){out.stop_detail.comparison_tie=true;return out;}
  const auto& winner=candidates.front();
  if(winner.model!=StopModel::None) {
    out.status=winner.model==StopModel::HalfGed?SgedStatus::RightHalfGedBoundary:SgedStatus::LeftHalfGedBoundary;
    out.logLik=winner.loglik_lower;out.stop_detail.limit_coef=winner.coef;
    out.stop_detail.limit_model=winner.model;out.stop_detail.limit_loglik=winner.loglik_lower;return out;
  }
  best.certified=true;best.iterations=out.iterations;return best;
}
} // namespace sged_skew_certificate
