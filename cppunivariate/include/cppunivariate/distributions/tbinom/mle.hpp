#pragma once
#include "../tpois/profile.hpp"
#include "../binom/dpqr.hpp"
namespace cppunivariate {
inline truncated_detail::Result tbinom_mle(ispan x, double a, double b, double size, double held=std::numeric_limits<double>::quiet_NaN()) {
  if(x.size()==0)return {0,0,0,truncated_detail::Status::InvalidInput};
  int xmin=x[0],xmax=x[0];for(int v:x){xmin=std::min(xmin,v);xmax=std::max(xmax,v);}
  if(std::isnan(a))a=double(xmin)-1;
  if(std::isnan(b))b=xmax;
  if(!std::isfinite(size)||size<0||size!=std::floor(size)) return {0,0,0,truncated_detail::Status::InvalidInput};
  if(!std::isnan(held)) {
    auto r=truncated_detail::fit(x,a,b,size);
    if(r.status==truncated_detail::Status::InvalidInput)return r;
    if(held<0 || !std::isfinite(held) || held>1)return {0,0,0,truncated_detail::Status::InvalidInput};
    const double lo=std::max(0.,std::floor(a)+1),hi=std::min(std::floor(b),size);
    if(held==0 || held==1) {
      const double point=held==0?0:size;
      if(point<lo||point>hi)return {0,0,0,truncated_detail::Status::InvalidInput};
      for(int v:x)if(v!=point)return {0,0,0,truncated_detail::Status::InvalidInput};
      return {held,0,0,truncated_detail::Status::Ok};
    }
    const double t=std::log(held)-std::log1p(-held);
    auto z=truncated_detail::mass(t,lo,hi,size);
    if(!z.ok)return {0,0,0,truncated_detail::Status::NumericalFailure};
    double ll=0;for(int v:x)ll+=truncated_detail::kernel(v,t,size)-z.logz;
    return {held,ll,0,truncated_detail::Status::Ok};
  }
  auto result=truncated_detail::fit(x,a,b,size);
  if(result.status==truncated_detail::Status::Unidentified)return {.5,0,0,truncated_detail::Status::Ok};
  return result;
}
struct TbinomDesignResult {
  double prob,a,b,size,logLik;
  int iterations;
  truncated_detail::Status status;
};
inline TbinomDesignResult tbinom_design_mle(ispan x,double a,double b,double size,
    double prob=std::numeric_limits<double>::quiet_NaN()) {
  const auto invalid=[&]() {return TbinomDesignResult{0,a,b,size,0,0,truncated_detail::Status::InvalidInput};};
  if(x.size()==0)return invalid();
  int xmin=x[0],xmax=x[0];for(int v:x){if(v<0)return invalid();xmin=std::min(xmin,v);xmax=std::max(xmax,v);}
  if(std::isnan(a))a=double(xmin)-1;
  if(std::isnan(b))b=xmax;
  const auto fit=[&](double k) {
    auto r=tbinom_mle(x,a,b,k,prob);
    return TbinomDesignResult{r.parameter,a,b,k,r.logLik,r.iterations,r.status};
  };
  if(!std::isnan(size))return fit(size);
  // The free-probability size compactification remains unproved in P0.
  if(std::isnan(prob))return invalid();
  const double lo=std::max(0.,std::floor(a)+1), upper=std::floor(b);
  if(lo>xmin || upper<xmax)return invalid();
  if(prob==0 || prob==1)return fit(xmax);
  if(!(prob>0 && prob<1) || !std::isfinite(prob))return invalid();
  // For constant data at a finite upper edge, size->infinity concentrates
  // there. It is attained iff a finite design can have singleton support.
  if(xmin==xmax && upper==xmax && lo<xmin) return {prob,a,b,0,0,0,truncated_detail::Status::UpperPointMass};
  auto best=fit(xmax);
  if(best.status!=truncated_detail::Status::Ok)return best;
  if(xmin==xmax && lo==xmax)return best;
  int below=0;for(int v:x)below+=v<upper;
  for(int j=0;j<1000000;++j) {
    const double k=double(xmax)+j;
    auto candidate=fit(k);
    if(candidate.status!=truncated_detail::Status::Ok)return candidate;
    if(candidate.logLik>best.logLik)best=candidate;
    if(std::isfinite(upper) && below>0 && k>=upper) {
      // Consecutive ratios below U are bounded by U*q/((k-U+1)*p).
      // Therefore P(X<U | L<=X<=U) <= r/(1-r), decreasing in k.
      const double ratio=upper*(1-prob)/((k-upper+1)*prob);
      if(ratio>0 && ratio<1 && below*std::log(ratio/(1-ratio))<best.logLik)return best;
    } else if(!std::isfinite(upper) && k>=xmax/prob) {
      // Binomial stochastic ordering bounds the numerator and denominator
      // throughout the remaining integer tail.
      const double log_num=distributions::pbinom(xmax,k,prob,true,true);
      const double log_den=distributions::pbinom(lo-1,k,prob,false,true);
      if(x.size()*(log_num-log_den)<best.logLik)return best;
    }
  }
  best.status=truncated_detail::Status::NumericalFailure;return best;
}

}
