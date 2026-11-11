#pragma once
#include "../../core/precision.hpp"
#include "../../core/span.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include "../../core/work_budget.hpp"
namespace cppunivariate { namespace tbinom_detail {
// Conditional mean minus the empirical mean, centred at the mode so a
// large integer observation never masks a small score by mean subtraction.
struct AccuracyScore { ExtendedReal value,margin; };
inline AccuracyScore accuracy_score(ispan x,double N,double L,double U,double p) {
  const ExtendedReal odds=static_cast<ExtendedReal>(p)/(1-static_cast<ExtendedReal>(p));
  const double mode=std::floor(std::max(L,std::min(U,(N+1)*p)));
  ExtendedReal total=1,offset=0,absolute=0,target=0;
  for(int v:x)target+=static_cast<ExtendedReal>(v)-mode;
  target/=x.size();
  std::int64_t remaining=enumeration_budget(512);
  for(int direction:{-1,1}) {
    double k=mode;ExtendedReal w=1;
    while((direction<0 && k>L)||(direction>0 && k<U)) {
      if(remaining--==0)return {NAN,NAN};
      const ExtendedReal ratio=direction>0?odds*(N-k)/(k+1):k/(odds*(N-k+1));
      w*=ratio;k+=direction;total+=w;offset+=(k-mode)*w;absolute+=std::abs(k-mode)*w;
      if(ratio<1) {
        const ExtendedReal d=1-ratio,tail=w*ratio/d;
        const ExtendedReal moment=tail*(std::abs(k-mode)+1+ratio/d);
        if(moment<.01*std::numeric_limits<ExtendedReal>::epsilon()*total)break;
      }
    }
  }
  return {offset/total-target,128*std::numeric_limits<ExtendedReal>::epsilon()*(1+std::abs(target)+absolute/total)};
}
inline bool fallback_accuracy(ispan x,double N,double a,double b,double& p) {
  if(p==0||p==1)return true;
  const double L=std::max(0.,std::floor(a)+1),U=std::min(N,std::floor(b));
  double lo=0,hi=1;constexpr double tol=1e-10;
  // The binomial conditional mean is strictly increasing in probability.
  for(int j=0;j<512;++j) {
    const double mid=lo+(hi-lo)/2;
    if(hi-lo<=tol/32*std::max(lo,std::numeric_limits<double>::min())) {
      if(!(p>=lo&&p<=hi))p=mid;
      const auto l=accuracy_score(x,N,L,U,std::max(0.,p-tol*p/16));
      const auto h=accuracy_score(x,N,L,U,std::min(1.,p+tol*p/16));
      return l.value+l.margin<0 && h.value-h.margin>0;
    }
    const auto score=accuracy_score(x,N,L,U,mid);
    if(!std::isfinite(score.value))return false;
    if(score.value<0)lo=mid;else hi=mid;
  }
  return false;
}
} }
