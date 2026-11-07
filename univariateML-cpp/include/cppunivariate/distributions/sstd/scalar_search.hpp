#pragma once
#include "../../core/work_budget.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
namespace skew_scalar_detail {
// Conditional exact-arithmetic enclosures. The caller pads computed score
// ranges by 128 epsilon times their absolute term sum (see each dossier).
struct Range {double lo=INFINITY,hi=-INFINITY;};
inline void include(Range& r,double v){if(std::isnan(v)){r={NAN,NAN};return;}r.lo=std::min(r.lo,v);r.hi=std::max(r.hi,v);}
struct Point {double x,ll,error;};
struct Search {Point witness{NAN,NAN,NAN};std::vector<Point> points;bool budget=false,ok=false;int evaluations=0;};
template<class Score,class Value>
inline Search enumerate(double lo,double hi,int maxit,Score score,Value value) {
 Search out;
 if(!(std::isfinite(lo)&&std::isfinite(hi)&&lo<=hi))return out;
 const auto seed=value(lo+(hi-lo)/2);
 if(!(std::isfinite(seed.lo)&&std::isfinite(seed.hi)))return out;
 out.witness={lo+(hi-lo)/2,seed.lo,seed.hi};
 struct Cell {double lo,hi;};
 std::vector<Cell> pending{{lo,hi}};
 const auto cap=certificate_budget(maxit);
 while(!pending.empty()) {
  if(out.evaluations>=cap){out.budget=true;return out;}
  ++out.evaluations;
  const auto cell=pending.back();pending.pop_back();
  const auto s=score(cell.lo,cell.hi);
  if(!(std::isfinite(s.lo)&&std::isfinite(s.hi)&&s.lo<=s.hi))return out;
  if(s.lo>0 || s.hi<0)continue;
  const double mid=cell.lo+(cell.hi-cell.lo)/2;
  bool accepted=false;
  if(cell.hi-cell.lo<=2e-11*(1+std::abs(mid))) {
   // Value returns {log likelihood, practical point-rounding allowance}.
   // A small coefficient interval alone does not resolve a sharp peak.
   const auto v=value(mid);
   const double remainder=std::max(std::abs(s.lo),std::abs(s.hi))*(cell.hi-cell.lo)/2;
   const double error=v.hi+remainder;
   if(!(std::isfinite(v.lo)&&std::isfinite(error)))return out;
   if(remainder<=v.hi) {
    out.points.push_back({mid,v.lo,error});accepted=true;
   }
  }
  if(!accepted) {
   if(mid==cell.lo || mid==cell.hi)return out;
   pending.push_back({mid,cell.hi});pending.push_back({cell.lo,mid});
  }
 }
 if(out.points.empty())return out;
 // Adjacent boxes can enclose the same stationary component. Their full
 // span must still satisfy the coefficient-width requirement.
 std::vector<Point> merged;double anchor=-INFINITY;
 for(const auto& p:out.points) {
  if(!merged.empty() && p.x-anchor<1e-9*(1+std::abs(p.x))) {
   const double upper=std::max(merged.back().ll+merged.back().error,p.ll+p.error);
   if(p.ll>merged.back().ll)merged.back()=p;
   merged.back().error=std::max(merged.back().error,upper-merged.back().ll);
  } else {merged.push_back(p);anchor=p.x;}
 }
 out.points=merged;out.ok=true;return out;
}
inline double margin(double magnitude){return 128*std::numeric_limits<double>::epsilon()*(1+magnitude);}
} // namespace skew_scalar_detail
} // namespace cppunivariate
