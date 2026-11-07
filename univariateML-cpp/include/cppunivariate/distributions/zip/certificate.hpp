#pragma once
#include "../../core/work_budget.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
namespace zip_certificate_detail {
// Shared with the held-inflation ZIP face of ZINB. See zip/README.md.
struct Peak { double lo,hi,ll,lower,upper; };
enum class Status { Ok, Budget, Numerical, Tie };
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
                        Evaluate evaluate,Pad pad) {
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
  auto h=[&](double t) -> double {
    if(t==u)return -INFINITY;
    if(t==v)return INFINITY;
    return -t+std::log(n/k)+std::log(t-u)-std::log(v-t)
      -std::log(pi)+std::log1p(-pi);
  };
  for(std::size_t j=1;j+1<cuts.size();++j) {
    const double value=h(cuts[j]);
    if(!std::isfinite(value) || std::abs(value)<=pad(v+std::abs(std::log(pi))))return out;
  }
  bool exhausted=false;
  for(std::size_t j=1;j<cuts.size();++j) {
    double lo=cuts[j-1],hi=cuts[j];
    if(!(h(lo)<0 && h(hi)>0))continue;
    // This branch is strictly increasing, hence contains exactly one maximum.
    for(std::int64_t iter=0;iter<solver_budget(maxit);++iter) {
      if(hi-lo<=1e-13*lo)break;
      const double mid=lo+(hi-lo)/2;
      if(mid==lo || mid==hi)break;
      const double value=h(mid);
      if(!std::isfinite(value))return out;
      if(value>0)hi=mid;else lo=mid;
    }
    if(hi-lo>1e-13*lo)exhausted=true;
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
  out.status=Status::Ok;
  for(std::size_t i=0;i<out.peaks.size();++i)
    if(i!=out.best && out.peaks[i].upper>=out.peaks[out.best].lower)
      out.status=Status::Tie;
  return out;
}
} // namespace zip_certificate_detail
} // namespace cppunivariate
