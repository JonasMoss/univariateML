#pragma once
#include "../../core/precision.hpp"
#include <cmath>
#include <vector>
#include <algorithm>
#include <limits>
#include <array>
namespace cppunivariate {
namespace logconcave_detail {
// Fixed-shape location/inverse-scale profile shared by the two normal
// convolutions. The outer grid and face decision are numerical, not a
// global certificate. A failed inner solve is never used as an estimate.
struct Result { double u,b,k,ll; bool ok; bool faces_only=false; };
enum class FallbackReason { None, InnerFailure, PolishFailure, UnresolvedFaces };
#ifdef CPPUNIVARIATE_LOGCONCAVE_TEST_HOOK
inline thread_local unsigned fallback_count=0;
inline thread_local FallbackReason fallback_reason=FallbackReason::None;
inline void reset_fallback() {fallback_reason=FallbackReason::None;}
inline void record_fallback(FallbackReason reason) {++fallback_count;fallback_reason=reason;}
#else
inline void reset_fallback() {}
inline void record_fallback(FallbackReason) {}
#endif
template<class Density> Result inner(const std::vector<double>& x,double k,Density density,int maxit,double start_u=0,double start_b=0) {
  const double n=x.size(); double mean=0,var=0;
  for(double z:x)mean+=z/n;
  for(double z:x)var+=(z-mean)*(z-mean)/n;
  if(!(var>0))return {0,0,k,-std::numeric_limits<double>::infinity(),false};
  CPPUNIV_EXTENDED_REAL b=start_b>0?start_b:1/std::sqrt(var),u=start_b>0?start_u:b*mean;
  auto fun=[&](CPPUNIV_EXTENDED_REAL a,CPPUNIV_EXTENDED_REAL c) {
    if(!(c>0&&std::isfinite(a)&&std::isfinite(c)))return -std::numeric_limits<CPPUNIV_EXTENDED_REAL>::infinity();
    CPPUNIV_EXTENDED_REAL ans=n*std::log(c);for(double z:x)ans+=density(c*z-a,k)[0];
    return ans;
  };
  CPPUNIV_EXTENDED_REAL val=fun(u,b);
  for(int it=0;it<maxit;++it) {
    CPPUNIV_EXTENDED_REAL gu=0,gb=n/b,huu=0,hub=0,hbb=-n/(b*b),su=1,sb=n;
    for(double xval:x) {
      auto v=density(b*xval-u,k);gu-=v[1];gb+=xval*v[1];su+=std::abs(v[1]);sb+=std::abs(b*xval*v[1]);
      huu+=v[2];hub-=xval*v[2];hbb+=xval*xval*v[2];
    }
    if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};
    const CPPUNIV_EXTENDED_REAL det=huu*hbb-hub*hub;
    const bool newton=huu<0&&det>0&&std::isfinite(det);
    const CPPUNIV_EXTENDED_REAL du=newton?((-hbb*gu+hub*gb)/det):gu/n;
    const CPPUNIV_EXTENDED_REAL db=newton?((hub*gu-huu*gb)/det):gb*b*b/n;
    bool accepted=false;
    double step=1;
    for(std::int64_t backtrack=0;backtrack<solver_budget(maxit) && step>=1e-10;++backtrack,step*=0.5) {
      const CPPUNIV_EXTENDED_REAL aa=u+step*du,bb=b+step*db,ff=fun(aa,bb);
      if(bb>0&&ff>=val-8*std::numeric_limits<CPPUNIV_EXTENDED_REAL>::epsilon()*(1+std::abs(val))&&(aa!=u||bb!=b)) {u=aa;b=bb;val=ff;accepted=true;break;}
    }
    if(!accepted)break;
  }
  return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),false};
}
template<class Density> Result search(const std::vector<double>& x,Density density,int maxit) {
  Result best={0,0,0,-std::numeric_limits<double>::infinity(),false};
  auto eval=[&](double z) {return inner(x,std::exp(z),density,maxit);};
  std::vector<Result> grid(65);grid[32]=eval(0);if(!grid[32].ok)return best;
  // Adjacent shape values have nearby optima; initialize outward from k=1.
  for(int d=1;d<=32;++d) {
    for(int sign:{-1,1}) {const int i=32+sign*d;const auto prev=grid[i-sign];
      grid[i]=inner(x,std::exp(-8+i*0.25),density,maxit,prev.u,prev.b);
      if(!grid[i].ok)return best;
    }
  }
  for(int i=1;i<64;++i) {
    if(!grid[i].ok || (grid[i-1].ok&&grid[i].ll<grid[i-1].ll) || (grid[i+1].ok&&grid[i].ll<grid[i+1].ll))continue;
    double lo=-8+(i-1)*0.25,hi=lo+0.5;
    double a=hi-(hi-lo)*0.6180339887498949,c=lo+(hi-lo)*0.6180339887498949;
    Result fa=eval(a),fc=eval(c);
    for(int j=0;j<maxit && hi-lo>1e-9;++j) {
      if(!fa.ok||!fc.ok)return {0,0,0,-std::numeric_limits<double>::infinity(),false};
      if(fa.ll<fc.ll){lo=a;a=c;fa=fc;c=lo+(hi-lo)*0.6180339887498949;fc=eval(c);}
      else{hi=c;c=a;fc=fa;a=hi-(hi-lo)*0.6180339887498949;fa=eval(a);}
    }
    if(!fa.ok||!fc.ok)return {0,0,0,-std::numeric_limits<double>::infinity(),false};
    if(grid[i].ok&&grid[i].ll>best.ll)best=grid[i];
    if(fa.ok&&fa.ll>best.ll)best=fa;
    if(fc.ok&&fc.ll>best.ll)best=fc;
  }
  // No observed interior maximum: return a comparison value, not this
  // finite-grid endpoint as an estimate. The caller compares its faces.
  if(!best.ok) {
    best=grid.front().ll>=grid.back().ll?grid.front():grid.back();
    best.faces_only=true;
  }
  return best;
}
}
}
