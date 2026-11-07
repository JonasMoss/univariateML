#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/span.hpp"
#include "../../core/discrete_stop.hpp"
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
#include <queue>
#include <cstdint>
#include <limits>
#include <vector>
#include <map>
namespace cppunivariate {
namespace binomial_certificate_detail {
// Retained-support certificate shared only by the four binomial relatives.
// All bounds are conditional on computed comparisons; see ztbinom/README.md.
struct Work {
  std::int64_t cells=0, evaluations=0;
  int maxit;
  bool numerical=false;
  bool charge(std::int64_t n) {
    if(n>enumeration_budget(maxit)-cells)return false;
    cells+=n;return true;
  }
};
struct Mass { double logz,mean; bool ok; };
inline Mass mass(double eta,int L,int K,Work& work) {
  if(!work.charge(static_cast<std::int64_t>(K)-L+1))return {0,0,false};
  const int mode=static_cast<int>(std::max(double(L),std::min(double(K),std::floor(std::exp(eta)))));
  double total=1,offset=0,w=1;
  for(int j=mode;j<K;++j){w*=std::exp(eta)/(j+1.);total+=w;offset+=(j+1.-mode)*w;}
  w=1;
  for(int j=mode;j>L;--j){w*=j*std::exp(-eta);total+=w;offset+=(j-1.-mode)*w;}
  const double logz=mode*eta-special::lgamma(mode+1.)+std::log(total);
  if(!std::isfinite(logz))work.numerical=true;
  return {logz,mode+offset/total,std::isfinite(logz)};
}
struct Segment {double lo,hi,upper;bool operator<(const Segment& s)const{return upper<s.upper;}};
// Upper-bound the retained-support supremum, or refuse. Inflation NaN is free;
// mixed=false denotes ordinary conditioning on L,...,K.
inline double retained_upper(ispan x,int L,int K,bool mixed,double inflation,
                             double target,Work& work) {
  double S=0,zeros=0,factorial=0;
  for(int v:x){S+=v;zeros+=v==0;factorial+=special::lgamma(v+1.);}
  const double n=x.size(),k=mixed?n-zeros:n;
  if(!(k>0))return INFINITY;
  const double lowmean=mixed?S/n:S/n,highmean=mixed?S/k:S/n;
  if(!(lowmean>L && highmean<K))return INFINITY;
  double lo=-1,hi=1;
  bool bracket=false;
  for(std::int64_t j=0;j<solver_budget(work.maxit);++j) {
    const auto a=mass(lo,L,K,work),b=mass(hi,L,K,work);
    if(!a.ok||!b.ok)return INFINITY;
    if(a.mean<lowmean && b.mean>highmean){bracket=true;break;}
    if(a.mean>=lowmean)lo=lo*2-1;
    if(b.mean<=highmean)hi=hi*2+1;
  }
  if(!bracket){work.numerical=true;return INFINITY;}
  const auto bound=[&](double a,double b) -> double {
    const double t=a+(b-a)/2;
    // Positive conditional likelihood is concave in eta.
    const auto p=mass(t,mixed?1:L,K,work);
    if(!p.ok)return INFINITY;
    const double gradient=S-k*p.mean;
    double upper=S*t-k*p.logz-factorial+std::abs(gradient)*(b-a)/2;
    if(mixed) {
      const auto za=mass(a,0,K,work),zb=mass(b,0,K,work);
      if(!za.ok||!zb.ok)return INFINITY;
      const double pi=std::isnan(inflation)?0:inflation;
      const double qlo=pi+(1-pi)*std::exp(-zb.logz);
      const double qhi=std::isnan(inflation)?1:pi+(1-pi)*std::exp(-za.logz);
      const double q=std::max(qlo,std::min(qhi,zeros/n));
      if(zeros>0)upper+=zeros*std::log(q);
      upper+=k*std::log1p(-q);
    }
    return upper+4096*std::numeric_limits<double>::epsilon()*(1+std::abs(upper)+n*K);
  };
  std::priority_queue<Segment> queue;
  queue.push({lo,hi,bound(lo,hi)});
  while(!queue.empty()) {
    const auto s=queue.top();
    if(s.upper<target)return s.upper;
    if(++work.evaluations>certificate_budget(work.maxit))return INFINITY;
    queue.pop();
    const double mid=s.lo+(s.hi-s.lo)/2;
    if(mid==s.lo||mid==s.hi){work.numerical=true;return INFINITY;}
    // If a retained witness already exceeds target, this K cannot exclude
    // the tail. Larger retained supports can still tighten the bound.
    const auto z=mass(mid,L,K,work);
    if(!z.ok)return INFINITY;
    double ll=S*mid-n*z.logz-factorial;
    if(mixed) {
      const double f0=std::exp(-z.logz),fraction=zeros/n;
      const double pi=std::isnan(inflation)?std::max(0.,(fraction-f0)/(1-f0)):inflation;
      ll=S*mid-k*z.logz-factorial+k*std::log1p(-pi);
      if(zeros>0)ll+=zeros*std::log(pi+(1-pi)*f0);
    }
    if(ll>=target)return INFINITY;
    queue.push({s.lo,mid,bound(s.lo,mid)});
    queue.push({mid,s.hi,bound(mid,s.hi)});
  }
  return INFINITY;
}
inline double correction(ispan x,int K,double N,Work& work) {
  if(!work.charge(static_cast<std::int64_t>(K)+x.size()))return INFINITY;
  std::vector<std::size_t> counts(static_cast<std::size_t>(K)+1,0);
  for(int v:x)++counts[static_cast<std::size_t>(v)];
  double d=0;std::size_t below=0;
  for(int j=0;j<K;++j){below+=counts[j];d-=below*std::log1p(-j/N);}
  return d;
}
// Returns an integer cutoff whose entire tail is strictly below the incumbent.
inline double cutoff(ispan x,int L,double U,bool mixed,double pi,double best,Work& work) {
  int mx=0;for(int v:x)mx=std::max(mx,v);
  const double margin=1e-9*x.size()+4096*std::numeric_limits<double>::epsilon()*(1+std::abs(best));
  std::int64_t K=static_cast<std::int64_t>(mx)+1;
  if(std::isfinite(U) && U<K)K=static_cast<std::int64_t>(U);
  for(std::int64_t step=0;step<solver_budget(work.maxit);++step) {
    if(K>std::numeric_limits<int>::max()){work.numerical=true;return INFINITY;}
    if(K>enumeration_budget(work.maxit)-work.cells)return INFINITY;
    const double upper=retained_upper(x,L,static_cast<int>(K),mixed,pi,best-2*margin,work);
    if(upper<best-2*margin) {
      double N=double(K);
      for(std::int64_t j=0;j<solver_budget(work.maxit);++j) {
        const double d=correction(x,static_cast<int>(K),N,work);
        if(upper+d+margin<best)return N;
        N*=2;
        if(N>=4503599627370496.){work.numerical=true;return INFINITY;}
      }
      return INFINITY;
    }
    if(std::isfinite(U) && K==U)return INFINITY;
    K=(std::isfinite(U) && U<2*K)?static_cast<std::int64_t>(U):2*K;
  }
  return INFINITY;
}
// Complete block coverage without any integer-profile unimodality claim.
// delta_N(k)=c_N(k)-c_A(k) is discrete convex. Subtract its supporting
// line at h; the nonnegative residual increases with N, hence is bounded
// by its value at B. The supporting line is absorbed into log odds.
inline double block_error(ispan x,double A,double B,Work& work) {
  int mx=0;double mean=0;for(int v:x){mx=std::max(mx,v);mean+=v;}
  mean/=x.size();
  if(!work.charge(static_cast<std::int64_t>(mx)+x.size()))return INFINITY;
  const int h=std::max(0,std::min(mx-1,static_cast<int>(std::floor(mean))));
  const double slope=std::log1p(-h/B)-std::log1p(-h/A);
  std::vector<double> delta(static_cast<std::size_t>(mx)+1,0);
  for(int j=0;j<mx;++j)delta[j+1]=delta[j]+std::log1p(-j/B)-std::log1p(-j/A);
  double error=0;
  for(int v:x)error+=delta[v]-delta[h]-(v-h)*slope;
  return std::max(0.,error);
}
template<class Evaluate>
inline bool cover(ispan x,double first,double end,double& best,Work& work,Evaluate evaluate) {
  std::vector<std::pair<double,double>> pending{{first,end-1}};
  std::map<double,double> cache;
  while(!pending.empty()) {
    if(++work.evaluations>certificate_budget(work.maxit))return false;
    const auto segment=pending.back();pending.pop_back();
    const double A=segment.first,B=segment.second;
    if(A>B)continue;
    auto found=cache.find(A);
    if(found==cache.end()) {
      double cost=x.size();for(int v:x)cost+=v;
      if(cost>enumeration_budget(work.maxit)-work.cells)return false;
      if(!work.charge(static_cast<std::int64_t>(cost)))return false;
      const double ll=evaluate(A);
      if(!std::isfinite(ll)){work.numerical=true;return false;}
      found=cache.emplace(A,ll).first;
    }
    best=std::max(best,found->second);
    if(A==B)continue;
    const double error=block_error(x,A,B,work);
    const double margin=1e-9*x.size()+4096*std::numeric_limits<double>::epsilon()*(1+std::abs(best));
    if(found->second+error+margin<best)continue;
    const double mid=std::floor(A+(B-A)/2);
    pending.push_back({mid+1,B});pending.push_back({A,mid});
  }
  return true;
}
inline void witness(StopDetail& d,std::vector<double> coef,double ll,bool budget,bool comparison=false) {
  if(!std::isfinite(ll)||!valid_stop_coefficients(StopModel::None,coef)) {
    d.loglik_upper=INFINITY;d.budget_exhausted=budget;return;
  }
  d.candidates.push_back({coef,ll,ll});
  d.loglik_lower=std::isnan(d.loglik_lower)?ll:std::max(d.loglik_lower,ll);
  d.loglik_upper=INFINITY;d.budget_exhausted=budget;d.comparison_tie=comparison;
}
} // namespace binomial_certificate_detail
} // namespace cppunivariate
