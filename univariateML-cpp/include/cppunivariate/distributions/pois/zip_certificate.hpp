#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/precision.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
namespace zip_certificate_detail {
// Shared with the held-inflation ZIP face of ZINB. See zip/README.md.
struct Peak { double lo,hi,ll,lower,upper; };
enum class Status { Ok, Budget, Numerical, Tie, Resolution };
struct Result {
  Status status=Status::Numerical;
  std::vector<Peak> peaks;
  std::size_t best=0;
  double upper=INFINITY;
};
// evaluate(t) returns a stable full log likelihood; arithmetic is its practical
// error margin. All roots lie in (S/n,S/k); only increasing h branches are peaks.
template <typename Evaluate,typename Pad>
inline Result held_rate(double n,double k,double S,double pi,int maxit,
                        Evaluate evaluate,Pad pad,double tol=1e-13) {
  Result out;
  const double u=S/n,v=S/k,d=v-u;
  if(!(u>0 && u<v && pi>0 && pi<1) || !std::isfinite(v))return out;
  std::vector<double> cuts{u};
  // h'= -1 + 1/(t-u) + 1/(v-t). A repeated turning point is
  // harmless in exact arithmetic, but its computed classification is refused.
  if(std::abs(d-4)<=pad(d))return out;
  if(d>4) {
    const double root=std::sqrt(d*(d-4));
    // Stable distance of the lower turning point from u (avoid d-root).
    const double offset=2*d/(d+root);
    cuts.push_back(u+offset);cuts.push_back(v-offset);
    if(!(cuts[1]>u && cuts[2]>cuts[1] && cuts[2]<v))return out;
  }
  cuts.push_back(v);
  const auto h=[&](double t) -> std::pair<CPPUNIV_EXTENDED_REAL,CPPUNIV_EXTENDED_REAL> {
    if(t==u)return {-INFINITY,0};
    if(t==v)return {INFINITY,0};
    // Use the residuals of n*t and k*t directly, eliminating the rounding
    // of S/n and S/k in the two near-endpoint logarithms.
    const auto a=std::log(std::fma(static_cast<CPPUNIV_EXTENDED_REAL>(n),t,-S));
    const auto b=std::log(std::fma(static_cast<CPPUNIV_EXTENDED_REAL>(-k),t,S));
    const auto c=std::log(static_cast<CPPUNIV_EXTENDED_REAL>(pi));
    const auto d=std::log1p(-static_cast<CPPUNIV_EXTENDED_REAL>(pi));
    const auto error=8*std::numeric_limits<CPPUNIV_EXTENDED_REAL>::epsilon()*
      (1+t+std::abs(a)+std::abs(b)+std::abs(c)+std::abs(d));
    return {-t+a-b-c+d,error};
  };
  for(std::size_t j=1;j+1<cuts.size();++j) {
    const double value=h(cuts[j]).first;
    if(!std::isfinite(value) || std::abs(value)<=pad(v+std::abs(std::log(pi))))return out;
  }
  const auto sign=[&](double t) {
    if(t==u)return -1;
    if(t==v)return 1;
    const auto test=h(t);
    return test.first>test.second?1:test.first< -test.second?-1:0;
  };
  bool exhausted=false,unresolved=false;
  for(std::size_t j=1;j<cuts.size();++j) {
    double lo=cuts[j-1],hi=cuts[j];
    if(!(sign(lo)<0 && sign(hi)>0))continue;
    // This branch is strictly increasing, hence contains exactly one maximum.
    const auto closed=[&](double accuracy) {
      return hi-lo+2*std::numeric_limits<double>::epsilon()*hi<=accuracy*lo;
    };
    // A tighter likelihood enclosure helps compare competing peaks.
    const double preferred=std::min(tol,1e-13);
    std::int64_t iter=0;
    for(;iter<solver_budget(maxit) && !closed(preferred);++iter) {
      const double mid=lo+(hi-lo)/2;
      if(mid==lo || mid==hi)break;
      // Only move to a point with a computed sign beyond the rounding margin.
      const int at=sign(mid);
      if(at>0)hi=mid;
      else if(at<0)lo=mid;
      else {
        const double l=lo+(mid-lo)/2,r=mid+(hi-mid)/2;
        const bool below=sign(l)<0,above=sign(r)>0;
        if(!below && !above)break;
        if(below)lo=l;
        if(above)hi=r;
      }
    }
    if(!closed(tol)) {
      if(iter==solver_budget(maxit))exhausted=true;
      else unresolved=true;
    }
    const double rate=lo+(hi-lo)/2,ll=evaluate(rate);
    const double arithmetic=pad(std::abs(ll)+k*rate+S*std::abs(std::log(rate)));
    // |ell'| <= n+S/lo throughout the root bracket.
    const double error=(hi-lo)*(n+S/lo)+arithmetic;
    if(!std::isfinite(ll) || !std::isfinite(error))return out;
    out.peaks.push_back({lo,hi,ll,ll-arithmetic,ll+error});
  }
  if(out.peaks.empty())return out;
  out.upper=-INFINITY;
  for(std::size_t i=0;i<out.peaks.size();++i) {
    out.upper=std::max(out.upper,out.peaks[i].upper);
    if(out.peaks[i].lower>out.peaks[out.best].lower)out.best=i;
  }
  if(exhausted){out.status=Status::Budget;return out;}
  if(unresolved){out.status=Status::Resolution;return out;}
  out.status=Status::Ok;
  for(std::size_t i=0;i<out.peaks.size();++i)
    if(i!=out.best && out.peaks[i].upper>=out.peaks[out.best].lower)
      out.status=Status::Tie;
  return out;
}
} // namespace zip_certificate_detail
} // namespace cppunivariate
