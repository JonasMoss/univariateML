#pragma once
#include <algorithm>
#include "../../core/likelihood_polish.hpp"
#include "../../core/span.hpp"
#include "power_one_accuracy.hpp"
#include "pair_accuracy.hpp"
namespace cppunivariate {
// Damped analytic Newton steps can bring a loose search proposal into the
// contraction region; acceptance still uses the common correction test.
template<class Objective>
inline bool continuous_fallback_polish(std::array<double,3>& q,const std::array<bool,3>& free,
    double tol,int maxit,const Objective& objective) {
  std::array<int,3> index{};int n=0;for(int i=0;i<3;++i)if(free[i])index[n++]=i;
  int remaining=static_cast<int>(solver_budget(maxit));
  while(remaining>1) {
    std::array<accuracy_detail::Jet,3> z;for(int i=0;i<3;++i)z[i]=accuracy_detail::variable(q[i],i,free[i]);
    const auto f=objective(z);std::array<std::array<ExtendedReal,3>,3> L{};std::array<ExtendedReal,3> y{},step{};
    bool positive=true;
    const auto factor=[&](ExtendedReal shift) {
      L={};
      for(int i=0;i<n;++i)for(int j=0;j<=i;++j){ExtendedReal v=-f.h[index[i]][index[j]]+(i==j?shift:0);for(int k=0;k<j;++k)v-=L[i][k]*L[j][k];
        if(i==j){if(!(v>0)||!std::isfinite(v))return false;L[i][j]=std::sqrt(v);}else L[i][j]=v/L[j][j];}
      return true;
    };
    if(!factor(0)) {
      positive=false;ExtendedReal shift=0,magnitude=1;
      for(int i=0;i<n;++i){ExtendedReal row=f.h[index[i]][index[i]];magnitude=std::max(magnitude,std::abs(row));for(int j=0;j<n;++j)if(i!=j)row+=std::abs(f.h[index[i]][index[j]]);shift=std::max(shift,row);}
      if(!factor(shift+magnitude*.01))return false;
    }
    for(int i=0;i<n;++i){ExtendedReal v=f.g[index[i]];for(int j=0;j<i;++j)v-=L[i][j]*y[j];y[i]=v/L[i][i];}
    for(int i=n-1;i>=0;--i){ExtendedReal v=y[i];for(int j=i+1;j<n;++j)v-=L[j][i]*step[j];step[i]=v/L[i][i];}
    double error=0;for(int i=0;i<n;++i)error=std::max(error,static_cast<double>(std::abs(step[i])));
    if(!std::isfinite(error))return false;
    if(positive&&error<=.125)break;
    double fraction=std::min(1.,.125/error);
    bool moved=false;
    while(remaining>1) {
      auto trial=q;std::array<accuracy_detail::Jet,3> t;
      for(int i=0;i<n;++i)trial[index[i]]+=static_cast<double>(step[i])*fraction;
      for(int i=0;i<3;++i)t[i]=accuracy_detail::Jet(trial[i]);
      --remaining;
      if(objective(t).v>=f.v){q=trial;moved=true;break;}
      fraction*=.5;
    }
    if(!moved)return false;
  }
  return accuracy_detail::polish(q,free,tol,remaining,objective);
}
// TwoSum retains the subtraction residual in double-only builds too.
inline ExtendedReal burr_excess_log_ratio(double v,double shift,double unit) {
  const double y=v-shift,b=y-v,e=(v-(y-b))+(-shift-b);
  const ExtendedReal h=y;
  const ExtendedReal l=y>unit*.5&&y<unit*2?std::log1p((h-unit)/unit):std::log(h)-std::log(static_cast<ExtendedReal>(unit));
  return l+std::log1p(static_cast<ExtendedReal>(e)/y);
}
// A local refutation, not a comparison of global likelihood winners.
// After profiling scale, |k*dL/dlog(k)+A(c)| <= 64*n^3/k for k>=100*n,
// with A(c)=sum(v_i(c)^2)/2-n and sum(v_i(c))=n. A negative upper bound
// throughout the requested c-box excludes every stationary point in the box.
inline bool burr_shape_box_refuted(span x,double k,double c,double tol,bool inverse=false,double shift=0) {
  const ExtendedReal n=x.size(),minimum_k=static_cast<ExtendedReal>(k)/(1+tol);
  if(x.empty()||!(tol>0&&tol<.5)||!(c>0)||!(minimum_k>=100*n)||!std::isfinite(k+c))return false;
  const double unit=x[0]-shift;if(!(unit>0)||!std::isfinite(unit))return false;
  ExtendedReal low=INFINITY,high=-INFINITY,conditioning=1;
  for(double v:x){const double y=v-shift;if(!(y>0)||!std::isfinite(y))return false;
    const ExtendedReal l=(inverse?-1:1)*burr_excess_log_ratio(v,shift,unit);low=std::min(low,l);high=std::max(high,l);
    if(!(y>unit*.5&&y<unit*2))conditioning=std::max(conditioning,static_cast<ExtendedReal>(c)*(1+std::abs(std::log(static_cast<ExtendedReal>(y)))+std::abs(std::log(static_cast<ExtendedReal>(unit)))));}
  accuracy_detail::ScalarSum total,square;
  for(double v:x){const ExtendedReal l=(inverse?-1:1)*burr_excess_log_ratio(v,shift,unit),w=std::exp(c*(l-high));total.add(w);square.add(w*w);}
  const ExtendedReal range=high-low,S2=n*n*square.value/(total.value*total.value);
  const ExtendedReal E=static_cast<ExtendedReal>(c)*range*tol/(1-tol);
  const ExtendedReal upper=S2*std::exp(2*E)/2-n;
  const ExtendedReal margin=4096*std::numeric_limits<double>::epsilon()*n*n*(conditioning+c*range);
  const ExtendedReal remainder=64*n*n*n/minimum_k;
  return std::isfinite(upper+margin+remainder)&&upper+margin+remainder<0;
}
// Local likelihood refinement only. The caller retains its global decision.
inline bool burr_local_accuracy(span x,double& a,double& b,double& s,
    const std::array<bool,3>& free,double tol,int maxit,bool inverse=false,double shift=0) {
  tol=std::min(tol,1e-10);
  const bool held_power=free[0]&&!free[1]&&free[2];
  if(held_power&&!x.empty()&&s>0&&std::isfinite(s)) {
    double extreme=x[0];for(double v:x)extreme=inverse?std::min(extreme,v):std::max(extreme,v);
    const ExtendedReal log_rate=(inverse?-1:1)*b*burr_excess_log_ratio(extreme,shift,s);
    if(log_rate<std::log(ExtendedReal(.01))&&burr_power_one_detail::fit(x,a,s,tol,maxit,inverse,shift,b))return true;
  }
  if(free[0]&&free[1]&&free[2]&&a>1e100&&!x.empty()) {
    accuracy_detail::ScalarSum mean,variance;
    for(double v:x)mean.add(burr_excess_log_ratio(v,shift,x[0]-shift)/x.size());
    for(double v:x){const auto d=burr_excess_log_ratio(v,shift,x[0]-shift)-mean.value;variance.add(d*d/x.size());}
    if(variance.value>0){a=1;b=static_cast<double>(1.8137993642342178506/std::sqrt(variance.value));s=static_cast<double>((x[0]-shift)*std::exp(mean.value));}
  }
  const auto weak_joint=[&](double shape,double power,double scale) {
    if(!(free[1]&&(free[0]||free[2])&&shape>32&&scale>0))return false;
    ExtendedReal high=-INFINITY;for(double value:x)high=std::max(high,(inverse?-1:1)*power*burr_excess_log_ratio(value,shift,scale));
    return high<std::log(ExtendedReal(.01));
  };
  if(weak_joint(a,b,s)&&burr_power_one_detail::joint_fit(x,a,b,s,free,tol,maxit,inverse,shift))return true;
  const double unit=s;
  std::array<double,3> q{{std::log(a),std::log(b),free[2]?std::log(a):0}};
  const auto objective=[&](const std::array<accuracy_detail::Jet,3>& z) {
    const auto k=free[0]?accuracy_detail::exp(z[0]):accuracy_detail::Jet(a),c=free[1]?accuracy_detail::exp(z[1]):accuracy_detail::Jet(b);
    accuracy_detail::JetSum f;
    for(double v:x) {
      const ExtendedReal l=burr_excess_log_ratio(v,shift,unit);
      const auto t=accuracy_detail::Jet(inverse?-1:1)*c*(accuracy_detail::Jet(l)-z[2]);
      if(free[2]) {
        // D=log(k)-sign*c*log(s/unit): the finite Weibull rate.
        const auto h=z[2]+accuracy_detail::Jet(inverse?-1:1)*c*accuracy_detail::Jet(l),w=accuracy_detail::exp(h),ratio=w/k;
        if(ratio.v<.01)f.add(z[1]+h-w-accuracy_detail::log1p(ratio)-k*accuracy_detail::log1pmx(ratio));
        else {const auto u=h-z[0];f.add(z[0]+z[1]-accuracy_detail::softplus(-u)-k*accuracy_detail::softplus(u));}
      } else f.add(z[0]+z[1]-accuracy_detail::softplus(-t)-k*accuracy_detail::softplus(t));
    }
    return f.value;
  };
  const double internal_tol=tol*std::min(1.,b);
  const auto compensated_current=[&]() {
    if(!(free[1]&&(free[0]||free[2])))return false;
    double k=free[0]?std::exp(q[0]):a,c=std::exp(q[1]);
    double scale=free[2]?static_cast<double>(static_cast<ExtendedReal>(unit)*std::exp((inverse?-1:1)*((free[0]?static_cast<ExtendedReal>(q[0]):std::log(static_cast<ExtendedReal>(a)))-q[2])/c)):unit;
    if(!weak_joint(k,c,scale)||!burr_power_one_detail::joint_fit(x,k,c,scale,free,tol,maxit,inverse,shift))return false;
    a=k;b=c;s=scale;return true;
  };
  if(!continuous_fallback_polish(q,free,internal_tol,maxit,objective))return compensated_current();
  if(free[2]) {
    const double power=free[1]?std::exp(q[1]):b,coordinate=static_cast<double>(((free[0]?static_cast<ExtendedReal>(q[0]):std::log(static_cast<ExtendedReal>(a)))-q[2])/power);
    const double sensitivity=2/power+std::abs(coordinate);
    if(internal_tol*sensitivity>tol*4&&!continuous_fallback_polish(q,free,tol*4/sensitivity,maxit,objective))return compensated_current();
  }
  if(free[0])a=std::exp(q[0]);
  if(free[1])b=std::exp(q[1]);
  const ExtendedReal logshape=free[0]?static_cast<ExtendedReal>(q[0]):std::log(static_cast<ExtendedReal>(a));
  const ExtendedReal native_power=free[1]?std::exp(static_cast<ExtendedReal>(q[1])):b;
  const double logscale=free[2]?static_cast<double>((inverse?-1:1)*(logshape-q[2])/native_power):0;
  if(free[2])s=static_cast<double>(static_cast<ExtendedReal>(unit)*std::exp(static_cast<ExtendedReal>(logscale)));
  if(held_power&&!x.empty()&&s>0&&std::isfinite(s)) {
    double extreme=x[0];for(double v:x)extreme=inverse?std::min(extreme,v):std::max(extreme,v);
    const ExtendedReal log_rate=(inverse?-1:1)*b*burr_excess_log_ratio(extreme,shift,s);
    if(log_rate<std::log(ExtendedReal(.01)))return burr_power_one_detail::fit(x,a,s,tol,maxit,inverse,shift,b);
  }
  if(weak_joint(a,b,s))return burr_power_one_detail::joint_fit(x,a,b,s,free,tol,maxit,inverse,shift);
  if(free[0]&&free[1]&&free[2]&&burr_shape_box_refuted(x,a,b,tol,inverse,shift))return false;
  return std::isfinite(a+b+s)&&a>0&&b>0&&s>0&&
    (!free[2]||accuracy_detail::scale_projection(s,unit,logscale,tol));
}
inline double burr_local_loglik(span x,double a,double b,double s,bool inverse=false,double shift=0) {
  accuracy_detail::ScalarSum ll;
  for(double v:x){
    const ExtendedReal l=burr_excess_log_ratio(v,shift,s);
    const ExtendedReal t=(inverse?-1:1)*b*l,soft=std::log1p(std::exp(-std::abs(t)));
    ll.add(std::log(static_cast<ExtendedReal>(a))+std::log(static_cast<ExtendedReal>(b))-burr_excess_log_ratio(v,shift,1)-std::max(-t,ExtendedReal(0))-soft-a*(std::max(t,ExtendedReal(0))+soft));}
  return static_cast<double>(ll.value);
}

}
