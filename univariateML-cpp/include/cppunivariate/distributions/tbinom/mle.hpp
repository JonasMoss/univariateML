#pragma once
#include "../../core/discrete_stop.hpp"
#include "../tpois/profile.hpp"
#include "../binom/dpqr.hpp"
#include "../binom/mle.hpp"
#include <map>
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
// Extends truncated_detail::Status with the one new free-size-and-prob
// escape (the truncated-Poisson face); tpois/profile.hpp is shared with
// other families and is not touched here, so the new value lives in a
// tbinom-local enum instead of truncated_detail::Status itself.
enum class TbinomStatus {
  Ok, InvalidInput, LowerPointMass, UpperPointMass, Unidentified, NumericalFailure, TpoisLimit
};
namespace tbinom_detail {
#ifdef CPPUNIV_SIZE_SEARCH_DIAGNOSTICS
struct SizeSearchDiagnostic { bool certified; double cutoff; double work; };
inline thread_local SizeSearchDiagnostic size_search_diagnostic{false,0,0};
#endif
// Poisson-scale coordinates: eta = log(size * prob / (1-prob)).
// Removing size^x from the combinatorial factor leaves only log1p(-j/size)
// corrections. Unlike lgamma(size-x+1)-lgamma(size+1), this remains accurate
// in the limit size -> infinity. size < 0 denotes the exact Poisson face.
inline double kernel(double k, double eta, double size) {
  if(k>2000000)return std::numeric_limits<double>::quiet_NaN();
  double value=k*eta-special::lgamma(k+1);
  if(size>0)for(int j=1;j<k;++j)value+=std::log1p(-j/size);
  return value;
}
inline truncated_detail::Mass mass(double eta,double lo,double hi,double size) {
  const double lambda=std::exp(eta);
  double mode=size<0 ? lambda : lambda/(1+lambda/size)*(1+1/size);
  mode=std::floor(std::max(lo,std::min(hi,mode)));
  if(!std::isfinite(mode))return {0,0,false};
  double total=1,offset=0;
  for(int direction : {-1,1}) {
    double k=mode,w=1;
    int j=0;
    for(;j<2000000;++j) {
      if((direction<0 && k<=lo)||(direction>0 && k>=hi))break;
      const double correction=size<0 ? 1 : (direction>0 ? 1-k/size : 1-(k-1)/size);
      const double ratio=direction>0 ? lambda*correction/(k+1) : k/lambda/correction;
      w*=ratio;k+=direction;total+=w;offset+=(k-mode)*w;
      if(ratio<1 && w*ratio/(1-ratio)<1e-17*total)break;
    }
    if(j==2000000)return {0,0,false};
  }
  const double logz=kernel(mode,eta,size)+std::log(total);
  return {logz,mode+offset/total,std::isfinite(logz)};
}
inline truncated_detail::Result profile(ispan x,double lo,double upper,double size) {
  const double hi=size<0 ? upper : std::min(upper,size);
  double mean=0;for(int v:x)mean+=v;mean/=x.size();
  if(mean==hi && size==hi)return {1,0,0,truncated_detail::Status::Ok};
  if(mean==lo && lo==0)return {0,0,0,truncated_detail::Status::Ok};
  if(!(mean>lo && mean<hi))return {0,0,0,truncated_detail::Status::NumericalFailure};
  const double centre=size<0 ? std::log(mean) : std::log(mean)-std::log1p(-mean/size);
  double left=centre-1,right=centre+1,width=1;
  bool bracketed=false;
  for(int j=0;j<20;++j) {
    const auto l=mass(left,lo,hi,size),h=mass(right,lo,hi,size);
    if(!l.ok||!h.ok)return {0,0,0,truncated_detail::Status::NumericalFailure};
    if(l.mean<mean && h.mean>mean){bracketed=true;break;}
    width*=2;
    if(l.mean>=mean)left=centre-width;
    if(h.mean<=mean)right=centre+width;
  }
  if(!bracketed)return {0,0,0,truncated_detail::Status::NumericalFailure};
  for(int j=0;j<100;++j) {
    const double eta=left+(right-left)/2;
    const auto z=mass(eta,lo,hi,size);
    if(!z.ok)return {0,0,j,truncated_detail::Status::NumericalFailure};
    if(z.mean<mean)left=eta;else right=eta;
  }
  const double eta=left+(right-left)/2;
  const auto z=mass(eta,lo,hi,size);
  // Total free-coordinate score, not the mean residual alone (R6).
  if(!z.ok || std::abs((z.mean-mean)*x.size())>=1e-7)
    return {0,0,100,truncated_detail::Status::NumericalFailure};
  double ll=0;for(int v:x)ll+=kernel(v,eta,size)-z.logz;
  const double prob=size<0 ? std::exp(eta) : 1/(1+size*std::exp(-eta));
  if(!std::isfinite(ll)||!std::isfinite(prob)||!(prob>0) || (size>0 && !(prob<1)))
    return {0,0,100,truncated_detail::Status::NumericalFailure};
  return {prob,ll,100,truncated_detail::Status::Ok};
}
inline TbinomStatus convert(truncated_detail::Status s) {
  switch (s) {
    case truncated_detail::Status::Ok: return TbinomStatus::Ok;
    case truncated_detail::Status::InvalidInput: return TbinomStatus::InvalidInput;
    case truncated_detail::Status::LowerPointMass: return TbinomStatus::LowerPointMass;
    case truncated_detail::Status::UpperPointMass: return TbinomStatus::UpperPointMass;
    case truncated_detail::Status::Unidentified: return TbinomStatus::Unidentified;
    default: return TbinomStatus::NumericalFailure;
  }
}
}
struct TbinomDesignResult {
  double prob,a,b,size,logLik;
  int iterations;
  TbinomStatus status;
  StopDetail stop_detail{};
};
inline TbinomDesignResult tbinom_design_mle(ispan x,double a,double b,double size,
    double prob=std::numeric_limits<double>::quiet_NaN()) {
  auto stopped = [&]() -> TbinomDesignResult {
#ifdef CPPUNIV_SIZE_SEARCH_DIAGNOSTICS
  tbinom_detail::size_search_diagnostic={false,0,0};
#endif
  const auto invalid=[&]() {return TbinomDesignResult{0,a,b,size,0,0,TbinomStatus::InvalidInput};};
  if(x.size()==0)return invalid();
  int xmin=x[0],xmax=x[0];for(int v:x){if(v<0)return invalid();xmin=std::min(xmin,v);xmax=std::max(xmax,v);}
  if(std::isnan(a))a=double(xmin)-1;
  if(std::isnan(b))b=xmax;
  const auto fit=[&](double k) {
    auto r=tbinom_mle(x,a,b,k,prob);
    return TbinomDesignResult{r.parameter,a,b,k,r.logLik,r.iterations,tbinom_detail::convert(r.status)};
  };
  if(!std::isnan(size))return fit(size);
  if(!std::isnan(prob)) {
    const double lo=std::max(0.,std::floor(a)+1), upper=std::floor(b);
    if(lo>xmin || upper<xmax)return invalid();
    if(prob==0 || prob==1)return fit(xmax);
    if(!(prob>0 && prob<1) || !std::isfinite(prob))return invalid();
    // For constant data at a finite upper edge, size->infinity concentrates
    // there. It is attained iff a finite design can have singleton support.
    if(xmin==xmax && upper==xmax && lo<xmin) return {prob,a,b,0,0,0,TbinomStatus::UpperPointMass};
    auto best=fit(xmax);
    if(best.status!=TbinomStatus::Ok)return best;
    if(xmin==xmax && lo==xmax)return best;
    int below=0;for(int v:x)below+=v<upper;
    for(int j=0;j<1000000;++j) {
      const double k=double(xmax)+j;
      auto candidate=fit(k);
      if(candidate.status!=TbinomStatus::Ok)return candidate;
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
    best.status=TbinomStatus::NumericalFailure;return best;
  }
  // Numerical integer profile: dense initial scan, geometrically growing
  // bracket, and integer refinement of every observed peak. No unimodality
  // theorem is assumed. Stop only after approaching the exact Poisson face
  // for four successive expansions; budget exhaustion is a failure, never
  // a capped estimate. See the README for the numerical (not global) guarantee.
  const double lo=std::max(0.,std::floor(a)+1), upper=std::floor(b);
  if(std::isnan(a)||std::isnan(b)||a>=b||!std::isfinite(lo)||lo>xmin||upper<xmax)return invalid();
  if(xmin==xmax)return {1.0,a,b,static_cast<double>(xmax),0.0,0,TbinomStatus::Ok};
  if(lo==0 && upper==INFINITY) {
    // The untruncated subcase has DeRiggi's proved unimodal profile and
    // the binomial fitter's cancellation-free adjacent difference.
    const auto r=binom_mle(x,std::numeric_limits<double>::quiet_NaN(),1e-10,512);
    const auto status=r.status==BinomStatus::Ok ? TbinomStatus::Ok :
      (r.status==BinomStatus::Underdispersed ? TbinomStatus::TpoisLimit : TbinomStatus::NumericalFailure);
    TbinomDesignResult out{r.prob,a,b,r.size,r.logLik,r.iterations,status};
    if(status==TbinomStatus::TpoisLimit) out.stop_detail=discrete_face(StopModel::Tpois,{r.stop_detail.limit_coef[0],a,b},r.stop_detail.limit_loglik);
    return out;
  }
  std::map<double,TbinomDesignResult> evaluated;
  const auto evaluate=[&](double k) {
    const auto found=evaluated.find(k);
    if(found!=evaluated.end())return found->second;
    const auto r=tbinom_detail::profile(x,lo,upper,k);
    const TbinomDesignResult result{r.parameter,a,b,k,r.logLik,r.iterations,tbinom_detail::convert(r.status)};
    evaluated.emplace(k,result);
    return result;
  };
  auto best=evaluate(xmax);
  if(best.status!=TbinomStatus::Ok)return best;
  // A two-point conditioning window is a saturated Bernoulli family for
  // every feasible size; equality with its Poisson face is attained here.
  if(upper-lo==1)return best;
  const auto tp=tbinom_detail::profile(x,lo,upper,-1);
  if(tp.status!=truncated_detail::Status::Ok) {
    best.status=TbinomStatus::NumericalFailure;return best;
  }
  const double allowance=1e-9*x.size();
  double older=xmax,previous=xmax;
  double old_value=best.logLik,previous_value=best.logLik;
  int near_face=0;
  for(int j=1;j<=512;++j) {
    const double k=j<=128 ? double(xmax)+j : std::ceil(previous*1.5);
    if(!std::isfinite(k)||k<=previous||k+1==k)break;
    auto candidate=evaluate(k);
    if(candidate.status!=TbinomStatus::Ok)return candidate;
    if(candidate.logLik>best.logLik)best=candidate;
    if(previous_value>=old_value && previous_value>=candidate.logLik && k-older>2) {
      // Discrete ternary refinement; numerical local unimodality inside
      // each observed peak bracket, checked independently in the R tests.
      double l=older,h=k;
      while(h-l>8) {
        const double m1=std::floor(l+(h-l)/3),m2=std::ceil(h-(h-l)/3);
        const auto f1=evaluate(m1),f2=evaluate(m2);
        if(f1.status!=TbinomStatus::Ok)return f1;
        if(f2.status!=TbinomStatus::Ok)return f2;
        if(f1.logLik>best.logLik)best=f1;
        if(f2.logLik>best.logLik)best=f2;
        if(f1.logLik<f2.logLik)l=m1;else h=m2;
      }
      for(double m=l;m<=h;++m) {
        const auto f=evaluate(m);
        if(f.status!=TbinomStatus::Ok)return f;
        if(f.logLik>best.logLik)best=f;
      }
    }
    near_face=std::abs(candidate.logLik-tp.logLik)<allowance ? near_face+1 : 0;
    if(j>128 && near_face>=4) {
      if(best.logLik<=tp.logLik+allowance) {best.status=TbinomStatus::TpoisLimit;
        best.stop_detail=discrete_face(StopModel::Tpois,{tp.parameter,a,b},tp.logLik);}
      else if(std::isfinite(upper)) {
        // For N>=U the difference from the Poisson profile is bounded by
        // sum_i (c_N(x_i)-c_N(U)), c_N(k)=sum_{h<k} log(1-h/N).
        // This bound decreases in N. Comparisons use the existing likelihood
        // margin, not directed rounding; see the size-search report.
        if(upper>2000000 || upper>=4503599627370496.)return best;
        std::vector<int> counts(static_cast<std::size_t>(upper)+1,0);
        for(int v:x)++counts[static_cast<std::size_t>(v)];
        const auto correction=[&](double size) {
          double sum=0;std::size_t count=0;
          for(int h=0;h<static_cast<int>(upper);++h) {
            count+=static_cast<std::size_t>(counts[static_cast<std::size_t>(h)]);
            if(count)sum-=static_cast<double>(count)*std::log1p(-h/size);
          }
          return sum;
        };
        const auto cutoff=[&](double target) {
          double l=upper,h=upper;
          while(!(tp.logLik+correction(h)<target)) {
            if(h>=2251799813685248.)return std::numeric_limits<double>::infinity();
            h=std::max(h+1,2*h);
          }
          while(h-l>1) {
            const double mid=std::floor(l+(h-l)/2);
            if(tp.logLik+correction(mid)<target)h=mid;else l=mid;
          }
          return tp.logLik+correction(l)<target ? l : h;
        };
        double end=cutoff(best.logLik-allowance);
        // Estimate work as observation/kernel operations plus 100 scans of
        // the conditioning window per profile, with a 400-operation floor.
        // Forty million units bounds cheap profiles to 100,000 evaluations.
        double per_profile=static_cast<double>(x.size())+100*(upper-lo+1);
        for(int v:x)per_profile+=v;
        per_profile=std::max(400.,per_profile);
        double missing=end-static_cast<double>(xmax)+1;
        for(const auto &entry:evaluated)
          if(entry.first>=xmax && entry.first<=end)missing-=1;
#ifdef CPPUNIV_SIZE_SEARCH_DIAGNOSTICS
        tbinom_detail::size_search_diagnostic.cutoff=end;
        tbinom_detail::size_search_diagnostic.work=missing*per_profile;
#endif
        if(!std::isfinite(end) || end+1==end || missing*per_profile>40000000.)return best;
        const auto original=best;
        // Sizes below U have changing conditioning support and must also be
        // enumerated if the exploratory search did not already visit them.
        for(double size=xmax;size<=end;size+=1) {
          if(!std::isfinite(end) || size+1==size)return original;
          if(evaluated.find(size)!=evaluated.end())continue;
          const auto f=evaluate(size);
          if(f.status!=TbinomStatus::Ok)return original;
          if(f.logLik>best.logLik) {
            best=f;
            end=cutoff(best.logLik-allowance);
          }
        }
#ifdef CPPUNIV_SIZE_SEARCH_DIAGNOSTICS
        tbinom_detail::size_search_diagnostic.certified=true;
        tbinom_detail::size_search_diagnostic.cutoff=end;
#endif
      }
      return best;
    }
    older=previous;old_value=previous_value;previous=k;previous_value=candidate.logLik;
  }
  best.status=TbinomStatus::NumericalFailure;return best;

  }();
  if(stopped.status==TbinomStatus::LowerPointMass || stopped.status==TbinomStatus::UpperPointMass)
    stopped.stop_detail=discrete_point(x[0]);
  return stopped;
}

}
