#pragma once
#include "face_certificate.hpp"
#include "../../core/discrete_stop.hpp"
#include "../tpois/profile.hpp"
#include "../binom/dpqr.hpp"
#include "../binom/mle.hpp"
#include <map>
#include "accuracy.hpp"
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
// Conditional likelihoods only need kernel ratios to one support atom.
// Cancelling the common factorial/product first makes the cost depend on
// distance from the normalizer's mode, rather than the absolute count.
inline double anchored_kernel(double k,double eta,double size,double anchor,int maxit=512) {
  if(!std::isfinite(k) || !std::isfinite(anchor) || std::abs(k-anchor)>enumeration_budget(maxit))return NAN;
  const double lo=std::min(k,anchor),hi=std::max(k,anchor);
  double value=(hi-lo)*eta;
  for(double j=lo;j<hi;++j) {
    value-=std::log(j+1);
    if(size>0)value+=std::log1p(-j/size);
  }
  return k>=anchor?value:-value;
}
struct ProfileMass {double logz,mean;bool ok;double anchor=0;};
inline ProfileMass mass(double eta,double lo,double hi,double size,std::int64_t& remaining) {
  const double lambda=std::exp(eta);
  double mode=size<0 ? lambda : lambda/(1+lambda/size)*(1+1/size);
  mode=std::floor(std::max(lo,std::min(hi,mode)));
  if(!std::isfinite(mode))return {0,0,false};
  if(remaining==0)return {0,0,false};
  --remaining;
  double total=1,offset=0;
  for(int direction : {-1,1}) {
    double k=mode,w=1;
    int j=0;
    for(;j<2000000;++j) {
      if((direction<0 && k<=lo)||(direction>0 && k>=hi))break;
      if(remaining==0)return {0,0,false};
      --remaining;
      const double correction=size<0 ? 1 : (direction>0 ? 1-k/size : 1-(k-1)/size);
      const double ratio=direction>0 ? lambda*correction/(k+1) : k/lambda/correction;
      w*=ratio;k+=direction;total+=w;offset+=(k-mode)*w;
      if(ratio<1 && w*ratio/(1-ratio)<1e-17*total)break;
    }
    if(j==2000000)return {0,0,false};
  }
  const double logz=std::log(total);
  return {logz,mode+offset/total,std::isfinite(logz),mode};
}
inline ProfileMass mass(double eta,double lo,double hi,double size) {
  std::int64_t remaining=enumeration_budget(512);
  return mass(eta,lo,hi,size,remaining);
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
  double ll=0;for(int v:x)ll+=anchored_kernel(v,eta,size,z.anchor)-z.logz;
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
  bool certified = true;
};
namespace tbinom_detail {
inline TbinomDesignResult tbinom_design_proposal(ispan x,double a,double b,double size,
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
    std::int64_t remaining=enumeration_budget(512);
    const auto held_profile=[&](double N) {
      if(N==0)return fit(N);
      TbinomDesignResult candidate{prob,a,b,N,0,0,TbinomStatus::NumericalFailure};
      const double eta=std::log(N)+std::log(prob)-std::log1p(-prob);
      const auto z=mass(eta,lo,std::min(upper,N),N,remaining);
      if(z.ok) {
        double cost=x.size();for(int v:x)cost+=std::abs(v-z.anchor);
        if(!std::isfinite(cost) || cost>remaining)remaining=0;
        else {
          remaining-=static_cast<std::int64_t>(cost);
          double ll=0;for(int v:x)ll+=anchored_kernel(v,eta,N,z.anchor)-z.logz;
          if(std::isfinite(ll)){candidate.logLik=ll;candidate.status=TbinomStatus::Ok;}
        }
      }
      if(remaining==0)candidate.stop_detail=discrete_budget(candidate.stop_detail);
      return candidate;
    };
    auto best=held_profile(xmax);
    if(best.status!=TbinomStatus::Ok)return best;
    if(xmin==xmax && lo==xmax)return best;
    int below=0;for(int v:x)below+=v<upper;
    for(std::int64_t j=0;j<enumeration_budget(512);++j) {
      const double k=double(xmax)+j;
      auto candidate=j==0?best:held_profile(k);
      if(candidate.status!=TbinomStatus::Ok){best.certified=false;if(remaining==0)best.stop_detail=discrete_budget(best.stop_detail);return best;}
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
    best.certified=false;best.stop_detail=discrete_budget(best.stop_detail);return best;
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
  if(upper-lo==1 || xmax-lo==1)return best;
  const auto tp=tbinom_detail::profile(x,lo,upper,-1);
  if(tp.status!=truncated_detail::Status::Ok) {
    best.certified=false;return best;
  }
  const double allowance=1e-9*x.size();
  double older=xmax,previous=xmax;
  double old_value=best.logLik,previous_value=best.logLik;
  int near_face=0;
  for(int j=1;j<=512;++j) {
    const double k=j<=128 ? double(xmax)+j : std::ceil(previous*1.5);
    if(!std::isfinite(k)||k<=previous||k+1==k)break;
    auto candidate=evaluate(k);
    if(candidate.status!=TbinomStatus::Ok){best.certified=false;return best;}
    if(candidate.logLik>best.logLik)best=candidate;
    if(previous_value>=old_value && previous_value>=candidate.logLik && k-older>2) {
      // Discrete ternary refinement; numerical local unimodality inside
      // each observed peak bracket, checked independently in the R tests.
      double l=older,h=k;
      while(h-l>8) {
        const double m1=std::floor(l+(h-l)/3),m2=std::ceil(h-(h-l)/3);
        const auto f1=evaluate(m1),f2=evaluate(m2);
        if(f1.status!=TbinomStatus::Ok){best.certified=false;return best;}
        if(f2.status!=TbinomStatus::Ok){best.certified=false;return best;}
        if(f1.logLik>best.logLik)best=f1;
        if(f2.logLik>best.logLik)best=f2;
        if(f1.logLik<f2.logLik)l=m1;else h=m2;
      }
      for(double m=l;m<=h;++m) {
        const auto f=evaluate(m);
        if(f.status!=TbinomStatus::Ok){best.certified=false;return best;}
        if(f.logLik>best.logLik)best=f;
      }
    }
    near_face=std::abs(candidate.logLik-tp.logLik)<allowance ? near_face+1 : 0;
    if(j>128 && near_face>=4) {
      if(best.logLik<=tp.logLik+allowance) {best.status=TbinomStatus::TpoisLimit;
        best.stop_detail=discrete_face(StopModel::Tpois,{tp.parameter,a,b},tp.logLik);}
      return best;
    }
    older=previous;old_value=previous_value;previous=k;previous_value=candidate.logLik;
  }
  best.certified=false;return best;

  }();
  if(stopped.status==TbinomStatus::LowerPointMass || stopped.status==TbinomStatus::UpperPointMass)
    stopped.stop_detail=discrete_point(x[0]);
  return stopped;
}

// Histogram profile for exact higher-order finite-window boundaries. The
// sufficient statistics are unchanged; repeated observations cost one cell.
struct CompactProfile {double prob=NAN,ll=NAN,upper=INFINITY;bool ok=false;};
inline CompactProfile compact_profile(const std::vector<std::uint64_t>& count,
    double N,binomial_certificate_detail::Work& work) {
  const int U=static_cast<int>(count.size())-1;
  double n=0,S=0;for(int k=0;k<=U;++k){n+=count[k];S+=count[k]*double(k);}
  const double mu=S/n;
  if(!(N>=0 && mu>0 && mu<std::min(double(U),N))){work.numerical=true;return {};}
  const auto evaluate=[&](double eta,double& mean,double& variance,double& ll) {
    if(!work.charge((4+static_cast<std::int64_t>(count.size()))*static_cast<std::int64_t>(count.size())))return false;
    std::vector<double> logw(count.size()),q(count.size());double top=-INFINITY;
    for(int k=0;k<=U;++k){logw[k]=k>N?-INFINITY:kernel(k,eta,N);top=std::max(top,logw[k]);}
    double z=0;for(int k=0;k<=U;++k){q[k]=std::exp(logw[k]-top);z+=q[k];}
    mean=0;for(int k=0;k<=U;++k){q[k]/=z;mean+=k*q[k];}
    variance=0;ll=0;
    for(int k=0;k<=U;++k){variance+=q[k]*(k-mean)*(k-mean);if(count[k])ll+=count[k]*(logw[k]-top-std::log(z));}
    if(!std::isfinite(ll) || !(variance>0)){work.numerical=true;return false;}
    return true;
  };
  double a=std::log(mu)-std::log1p(-mu/N)-1,b=a+2,va=0,vb=0,ma=0,mb=0,ll=0;
  bool bracket=false;
  for(std::int64_t j=0;j<solver_budget(work.maxit);++j) {
    if(!evaluate(a,ma,va,ll)||!evaluate(b,mb,vb,ll))return {};
    if(ma<mu && mb>mu){bracket=true;break;}
    const double width=b-a;if(ma>=mu)a-=width;if(mb<=mu)b+=width;
  }
  if(!bracket){work.numerical=true;return {};}
  for(std::int64_t j=0;j<solver_budget(work.maxit);++j) {
    const double eta=a+(b-a)/2;double mean=0,variance=0,value=0;
    if(!evaluate(eta,mean,variance,value))return {};
    // In this finite exponential family |d log Var / d eta| <= U.
    // Hence the centre variance gives a lower bound throughout the bracket.
    const double vlower=variance*std::exp(-U*(b-a)/2);
    const double pad=64*std::numeric_limits<double>::epsilon()*(1+U);
    if(b-a+2*pad/vlower<2.5e-11) {
      const double p=1/(1+N*std::exp(-eta));
      const double upper=value+n*(std::abs(mean-mu)+pad)*(b-a)/2;
      return {p,value,upper,std::isfinite(p) && p>0 && p<1};
    }
    if(mean<mu)a=eta;else b=eta;
    if(eta==a && eta==b)break;
  }
  work.numerical=true;return {};
}
// An exact rational face with zero first two coefficients can nevertheless
// have a finite global winner. Cover its entire integer prefix using counts,
// and exclude its tail by Jensen and exact first-order cancellation:
// (LL_N-LL_face)/n <= E_Q sum_{j<X} j^2 / N^2, N>=2U.
// This argument does not use the sign of the third coefficient.
inline bool compact_boundary_certificate(ispan x,int U,TbinomDesignResult& best,
    binomial_certificate_detail::Work& work) {
  std::vector<std::uint64_t> count,weight;std::uint64_t Z=0,num=0,den=0;
  bool exhausted=false;
  if(!rational_window_data(x,U,work,exhausted,count,weight,Z,num,den)) {
    if(!exhausted)return false;
  } else {
    FaceRational m,aa,beta;
    if(!work.charge(16*static_cast<std::int64_t>(count.size())))exhausted=true;
    else if(!zero_second_window(count,weight,x.size(),Z,m,aa,beta))return false;
    if(!exhausted) {
      const double face=[&] {double v=0;for(int k=0;k<=U;++k)if(count[k])v+=count[k]*std::log(double(weight[k])/Z);return v;}();
      const auto initial=compact_profile(count,best.size,work);
      if(initial.ok){best.prob=initial.prob;best.logLik=initial.ll;}
      const double margin=1e-9*x.size()+8192*std::numeric_limits<double>::epsilon()*(1+std::abs(face));
      if(initial.ok && initial.ll>face+4*margin) {
        double moment=0;int mx=0;
        for(int k=0;k<=U;++k){moment+=(double(weight[k])/Z)*k*(k-1)*(2.*k-1)/6;if(count[k])mx=k;}
        const double end=std::ceil(std::max(2.*U,std::sqrt(4*x.size()*moment/(initial.ll-face-2*margin))));
        if(!std::isfinite(end) || end>=4503599627370496. || best.size>=end)work.numerical=true;
        else {
          bool ambiguous=false,complete=true;
          double runner=-INFINITY,runner_size=NAN;
          CompactProfile runner_profile;
          const auto retain_runner=[&](CompactProfile f,double N) {
            if(f.upper>runner){runner=f.upper;runner_size=N;runner_profile=f;}
          };
          for(double N=mx;N<end;++N) {
            if(++work.evaluations>certificate_budget(work.maxit)){complete=false;break;}
            const auto f=compact_profile(count,N,work);
            if(!f.ok){complete=false;break;}
            if(N==best.size)continue;
            if(f.ll>best.logLik+margin) {
              retain_runner({best.prob,best.logLik,best.logLik+margin,true},best.size);
              best.size=N;best.prob=f.prob;best.logLik=f.ll;
            } else retain_runner(f,N);
          }
          ambiguous=runner+2*margin>=best.logLik;
          if(complete && !ambiguous) {
#ifdef CPPUNIV_SIZE_SEARCH_DIAGNOSTICS
            size_search_diagnostic={true,end,double(work.cells)};
#endif
            best.status=TbinomStatus::Ok;best.certified=true;best.stop_detail={};return true;
          }
          exhausted=!complete && !work.numerical;
          best.stop_detail=discrete_face(StopModel::Tpois,{double(num)/den,best.a,best.b},face);
          best.status=TbinomStatus::NumericalFailure;
          record_witness(best.stop_detail,{best.prob,best.a,best.b,best.size},best.logLik,exhausted,ambiguous);
          if(runner_profile.ok)best.stop_detail.candidates.push_back({{runner_profile.prob,best.a,best.b,runner_size},runner_profile.ll,runner_profile.upper});
          return true;
        }
      } else if(initial.ok) {
        const double end=third_order_window_cutoff(count,weight,x.size(),Z,work,exhausted);
        if(!std::isfinite(end)) {
          if(!exhausted && !work.numerical)return false;
        } else {
          int mx=0;for(int k=0;k<=U;++k)if(count[k])mx=k;
          bool complete=true,ambiguous=false;
          for(double N=mx;N<end;++N) {
            if(++work.evaluations>certificate_budget(work.maxit)){complete=false;break;}
            const double upper=scaled_third_jensen(count,weight,x.size(),Z,m,aa,beta,N,work);
            if(upper<0)continue;
            if(work.cells>=enumeration_budget(work.maxit)){complete=false;break;}
            const auto f=compact_profile(count,N,work);
            if(!f.ok){complete=false;break;}
            if(f.ll>face+margin) {
              best.size=N;best.prob=f.prob;best.logLik=f.ll;best.status=TbinomStatus::Ok;best.stop_detail={};
              return false; // the ordinary finite-winner certificate must still cover globally
            }
            if(!(f.upper+margin<face))ambiguous=true;
          }
          best.stop_detail=discrete_face(StopModel::Tpois,{double(num)/den,best.a,best.b},face);
          if(complete && !ambiguous) {
            best.status=TbinomStatus::TpoisLimit;
            binomial_certificate_detail::certified_face(best.stop_detail,{best.prob,best.a,best.b,best.size},best.logLik);
#ifdef CPPUNIV_SIZE_SEARCH_DIAGNOSTICS
            size_search_diagnostic={true,end,double(work.cells)};
#endif
            return true;
          }
          best.status=TbinomStatus::NumericalFailure;
          record_witness(best.stop_detail,{best.prob,best.a,best.b,best.size},best.logLik,!complete && !work.numerical,ambiguous);
          return true;
        }
      }
      exhausted=!work.numerical;
    }
  }
  best.status=TbinomStatus::NumericalFailure;
  record_witness(best.stop_detail,{best.prob,best.a,best.b,best.size},best.logLik,exhausted);
  return true;
}
} // namespace tbinom_detail
inline TbinomDesignResult tbinom_design_mle(ispan x,double a,double b,double size,double prob=NAN) {
  bool candidate_ready=false;
  auto checked = [&]() -> TbinomDesignResult {
  auto best=tbinom_detail::tbinom_design_proposal(x,a,b,size,prob);
  if(!std::isnan(size)||!std::isnan(prob)||x.size()==0)return best;
  int xmin=x[0],mx=x[0];for(int v:x){xmin=std::min(xmin,v);mx=std::max(mx,v);}
  const double lo=std::max(0.,std::floor(best.a)+1),U=std::floor(best.b);
  if(xmin==mx || U-lo==1 || (mx-lo==1 && best.status==TbinomStatus::Ok && best.size==mx) || (lo==0 && U==INFINITY))return best;
  candidate_ready=(best.status==TbinomStatus::Ok || best.status==TbinomStatus::TpoisLimit) && std::isfinite(best.logLik);
  binomial_certificate_detail::Work work{0,0,512};
  if(lo==0 && std::isfinite(U) && U<=std::numeric_limits<int>::max() &&
     tbinom_detail::compact_boundary_certificate(x,static_cast<int>(U),best,work))return best;
  if(best.status==TbinomStatus::TpoisLimit) {
    bool exhausted=false;
    if(U==mx) {
      double ll=NAN;
      const double rate=binomial_certificate_detail::saturated_poisson_rate(x,static_cast<int>(lo),mx,work,exhausted,ll);
      if(std::isfinite(rate)) {
        best.stop_detail=discrete_face(StopModel::Tpois,{rate,best.a,best.b},ll);
        binomial_certificate_detail::certified_face(best.stop_detail,{best.prob,best.a,best.b,best.size},best.logLik);return best;
      }
      if(exhausted) {
        best.status=TbinomStatus::NumericalFailure;
        tbinom_detail::record_witness(best.stop_detail,{best.prob,best.a,best.b,best.size},best.logLik,true);return best;
      }
    }
    double face=best.stop_detail.limit_loglik;
    double end=binomial_certificate_detail::signed_cutoff(x,static_cast<int>(lo),U,best.stop_detail.limit_coef[0],work,exhausted);
    if(!std::isfinite(end) && !exhausted && !work.numerical && lo==0 &&
       std::isfinite(U) && U<=std::numeric_limits<int>::max()) {
      double ll=NAN;
      const double rate=tbinom_detail::exact_poisson_moment_face(x,static_cast<int>(U),work,exhausted,ll);
      if(std::isfinite(rate)) {
        best.stop_detail=discrete_face(StopModel::Tpois,{rate,best.a,best.b},ll);
        binomial_certificate_detail::certified_face(best.stop_detail,{best.prob,best.a,best.b,best.size},best.logLik);return best;
      }
      if(!exhausted && !work.numerical) {
        double second_rate=NAN;
        end=tbinom_detail::second_order_poisson_cutoff(x,static_cast<int>(U),work,exhausted,second_rate,ll);
        if(std::isfinite(end)) {
          face=ll;best.stop_detail=discrete_face(StopModel::Tpois,{second_rate,best.a,best.b},ll);
        }
      }
    }
    if(!std::isfinite(end) && (exhausted || work.numerical)) {
      best.status=TbinomStatus::NumericalFailure;
      tbinom_detail::record_witness(best.stop_detail,{best.prob,best.a,best.b,best.size},best.logLik,exhausted);return best;
    }
    if(std::isfinite(end)) {
      bool finite_seen=false;
      const auto covered=binomial_certificate_detail::cover_face(x,mx,end,face,work,[&](double N) {
        const auto f=tbinom_detail::profile(x,lo,U,N);
        if(f.status!=truncated_detail::Status::Ok)return double(NAN);
        if(!finite_seen || f.logLik>best.logLik) {best.prob=f.parameter;best.size=N;best.logLik=f.logLik;}
        finite_seen=true;
        return f.logLik;
      });
      if(covered==binomial_certificate_detail::FaceCoverage::Face) {
        binomial_certificate_detail::certified_face(best.stop_detail,{best.prob,best.a,best.b,best.size},best.logLik);return best;
      }
      if(covered==binomial_certificate_detail::FaceCoverage::Finite) {
        best.status=TbinomStatus::Ok;best.stop_detail={};
      } else if(covered==binomial_certificate_detail::FaceCoverage::Failure) {
        best.status=TbinomStatus::NumericalFailure;
        tbinom_detail::record_witness(best.stop_detail,{best.prob,best.a,best.b,best.size},best.logLik,!work.numerical);return best;
      }
    }
    if(best.status!=TbinomStatus::Ok) {
      tbinom_detail::record_witness(best.stop_detail,{best.prob,best.a,best.b,best.size},best.logLik,false,true);return best;
    }
  }
  if(best.status!=TbinomStatus::Ok)return best;
  // This public fitter has no iterlim; retain a stated fixed work budget.
  const double end=binomial_certificate_detail::cutoff(x,static_cast<int>(lo),U,false,NAN,best.logLik,work);
  if(std::isfinite(end) && best.size>=end)work.numerical=true;
  if(!std::isfinite(end) || work.numerical) {
    best.status=TbinomStatus::NumericalFailure;
    tbinom_detail::record_witness(best.stop_detail,{best.prob,best.a,best.b,best.size},best.logLik,!work.numerical);return best;
  }
  double incumbent=best.logLik;
  const bool complete=binomial_certificate_detail::cover(x,mx,end,incumbent,work,[&](double N) {
    const auto f=tbinom_detail::profile(x,lo,U,N);
    if(f.status!=truncated_detail::Status::Ok)return double(NAN);
    if(f.logLik>best.logLik)best={f.parameter,best.a,best.b,N,f.logLik,f.iterations,TbinomStatus::Ok};
    return f.logLik;
  });
  if(!complete) {
    best.status=TbinomStatus::NumericalFailure;
    tbinom_detail::record_witness(best.stop_detail,{best.prob,best.a,best.b,best.size},best.logLik,!work.numerical);
    return best;
  }
#ifdef CPPUNIV_SIZE_SEARCH_DIAGNOSTICS
  tbinom_detail::size_search_diagnostic={true,end,double(work.cells)};
#endif
  best.certified=true;
  return best;
  };
  auto best=checked();
  if(candidate_ready && best.status!=TbinomStatus::Ok &&
     (best.stop_detail.comparison_tie || !std::isfinite(best.stop_detail.loglik_upper)) &&
     std::isfinite(best.logLik) && best.size>=0 && best.prob>=0 && best.prob<=1) {
    best.status=TbinomStatus::Ok;best.certified=false;
  }
  if(best.status==TbinomStatus::Ok && !best.certified && std::isnan(prob)) {
    if(!tbinom_detail::fallback_accuracy(x,best.size,best.a,best.b,best.prob)) {
      best.status=TbinomStatus::NumericalFailure;return best;
    }
    const double eta=std::log(best.size)+std::log(best.prob)-std::log1p(-best.prob);
    const auto z=tbinom_detail::mass(eta,std::max(0.,std::floor(best.a)+1),std::floor(best.b),best.size);
    best.logLik=0;
    for(int v:x)best.logLik+=tbinom_detail::anchored_kernel(v,eta,best.size,z.anchor)-z.logz;
  }
  return best;
}

} // namespace cppunivariate
