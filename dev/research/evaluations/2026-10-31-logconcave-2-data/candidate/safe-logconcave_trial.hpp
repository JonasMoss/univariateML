#pragma once
#include "precision.hpp"
#include <cmath>
#include <vector>
#include <algorithm>
#include <limits>
#include <array>
namespace cppunivariate {
namespace logconcave_trial {
struct Result { double u,b,k,ll; bool ok; };
template<class Density> Result inner(const std::vector<double>& x,double k,Density density,int maxit) {
  const double n=x.size(); double mean=0,var=0;
  for(double z:x)mean+=z/n;
  for(double z:x)var+=(z-mean)*(z-mean)/n;
  if(!(var>0))return {0,0,k,-INFINITY,false};
  double b=1/std::sqrt(var),u=b*mean;
  auto fun=[&](double a,double c) {
    if(!(c>0&&std::isfinite(a)&&std::isfinite(c)))return -std::numeric_limits<double>::infinity();
    CPPUNIV_EXTENDED_REAL ans=n*std::log(c);for(double z:x)ans+=density(c*z-a,k)[0];
    return static_cast<double>(ans);
  };
  double val=fun(u,b);
  for(int it=0;it<std::min(maxit,100);++it) {
    CPPUNIV_EXTENDED_REAL gu=0,gb=n/b,huu=0,hub=0,hbb=-n/(b*b);
    for(double xval:x) {
      auto v=density(b*xval-u,k);gu-=v[1];gb+=xval*v[1];
      huu+=v[2];hub-=xval*v[2];hbb+=xval*xval*v[2];
    }
    if(std::hypot(static_cast<double>(gu),static_cast<double>(gb*b))<1e-10*n) return {u,b,k,val,true};
    const CPPUNIV_EXTENDED_REAL det=huu*hbb-hub*hub;
    if(!(huu<0&&det>0&&std::isfinite(det)))break;
    const double du=static_cast<double>((-hbb*gu+hub*gb)/det),db=static_cast<double>((hub*gu-huu*gb)/det);
    bool accepted=false;
    for(double step=1;step>=1e-10;step*=0.5) {
      const double aa=u+step*du,bb=b+step*db,ff=fun(aa,bb);
      if(bb>0&&ff>=val) {u=aa;b=bb;val=ff;accepted=true;break;}
    }
    if(!accepted)break;
  }
  return {u,b,k,val,false};
}
template<class Density> Result search(const std::vector<double>& x,Density density,int maxit) {
  Result best={0,0,0,-INFINITY,false};
  auto eval=[&](double z) {return inner(x,std::exp(z),density,maxit);};
  std::vector<Result> grid; for(int i=0;i<=64;++i) { grid.push_back(eval(-8+i*0.25)); if(!grid.back().ok)return best; }
  for(int i=1;i<64;++i) {
    if(!grid[i].ok || (grid[i-1].ok&&grid[i].ll<grid[i-1].ll) || (grid[i+1].ok&&grid[i].ll<grid[i+1].ll))continue;
    double lo=-8+(i-1)*0.25,hi=lo+0.5;
    double a=hi-(hi-lo)*0.6180339887498949,c=lo+(hi-lo)*0.6180339887498949;
    Result fa=eval(a),fc=eval(c);
    for(int j=0;j<40;++j) {
      if(!fa.ok||!fc.ok)return {0,0,0,-INFINITY,false};
      if(fa.ll<fc.ll){lo=a;a=c;fa=fc;c=lo+(hi-lo)*0.6180339887498949;fc=eval(c);}
      else{hi=c;c=a;fc=fa;a=hi-(hi-lo)*0.6180339887498949;fa=eval(a);}
    }
    if(!fa.ok||!fc.ok)return {0,0,0,-INFINITY,false};
    if(grid[i].ok&&grid[i].ll>best.ll)best=grid[i];
    if(fa.ok&&fa.ll>best.ll)best=fa;
    if(fc.ok&&fc.ll>best.ll)best=fc;
  }
  return best;
}
}
}
