#pragma once
#include <cmath>
#include <vector>
#include <algorithm>
#include <limits>
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
    double ans=n*std::log(c);for(double z:x)ans+=density(c*z-a,k);
    return std::isfinite(ans)?ans:-INFINITY;
  };
  double val=fun(u,b);
  for(int it=0;it<std::min(maxit,100);++it) {
    const double h=1e-4*(1+std::abs(u)),j=1e-4*b;
    const double up=fun(u+h,b),um=fun(u-h,b),bp=fun(u,b+j),bm=fun(u,b-j);
    const double gu=(up-um)/(2*h),gb=(bp-bm)/(2*j);
    if(std::hypot(gu,gb*b)<1e-5) return {u,b,k,val,true};
    const double huu=(up-2*val+um)/(h*h),hbb=(bp-2*val+bm)/(j*j);
    const double hub=(fun(u+h,b+j)-fun(u+h,b-j)-fun(u-h,b+j)+fun(u-h,b-j))/(4*h*j);
    const double det=huu*hbb-hub*hub;
    if(!(huu<0&&det>0&&std::isfinite(det)))break;
    const double du=(-hbb*gu+hub*gb)/det,db=(hub*gu-huu*gb)/det;
    bool accepted=false;
    for(double step=1;step>=1e-10;step*=0.5) {
      const double aa=u+step*du,bb=b+step*db,ff=fun(aa,bb);
      if(bb>0&&ff>val) {u=aa;b=bb;val=ff;accepted=true;break;}
    }
    if(!accepted)break;
  }
  return {u,b,k,val,false};
}
template<class Density> Result search(const std::vector<double>& x,Density density,int maxit) {
  Result best={0,0,0,-INFINITY,false};
  auto eval=[&](double z) {return inner(x,std::exp(z),density,maxit);};
  std::vector<Result> grid; for(int i=0;i<=24;++i)grid.push_back(eval(-6+i*0.5));
  for(int i=1;i<24;++i) {
    if(!grid[i].ok || grid[i].ll<grid[i-1].ll || grid[i].ll<grid[i+1].ll)continue;
    double lo=-6+(i-1)*0.5,hi=lo+1;
    double a=hi-(hi-lo)*0.6180339887498949,c=lo+(hi-lo)*0.6180339887498949;
    Result fa=eval(a),fc=eval(c);
    for(int j=0;j<38;++j) {
      if(!fa.ok||!fc.ok)break;
      if(fa.ll<fc.ll){lo=a;a=c;fa=fc;c=lo+(hi-lo)*0.6180339887498949;fc=eval(c);}
      else{hi=c;c=a;fc=fa;a=hi-(hi-lo)*0.6180339887498949;fa=eval(a);}
    }
    if(grid[i].ok&&grid[i].ll>best.ll)best=grid[i];
    if(fa.ok&&fa.ll>best.ll)best=fa;
    if(fc.ok&&fc.ll>best.ll)best=fc;
  }
  return best;
}
}
}
