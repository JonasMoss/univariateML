#pragma once
#include "../../core/span.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
enum class GpdStatus { Ok, InvalidData, InvalidShape, IterationFailure };
struct GpdResult {
  double mu, sigma, xi, logLik;
  int iterations;
  GpdStatus status;
};
// log1p(xi*z)/xi, including the interior exponential member.
inline double gpd_log_ratio(double z, double xi) {
  const double v = xi*z;
  if (std::abs(v) < 1e-5)
    return z*(1-v/2+v*v/3-v*v*v/4+v*v*v*v/5);
  return std::log1p(v)/xi;
}
inline double gpd_loglik(span y, double sigma, double xi) {
  double ll = -static_cast<double>(y.size())*std::log(sigma);
  for (double v : y) {
    if (!(sigma > 0) || !(1+xi*(v/sigma) > 0))
      return -std::numeric_limits<double>::infinity();
    ll -= (1+xi)*gpd_log_ratio(v/sigma, xi);
  }
  return ll;
}
// For fixed xi > -1 the scale score is strictly decreasing in sigma
// after multiplication by sigma: (1+xi)*mean(y/(sigma+xi*y))-1.
inline GpdResult gpd_scale(span y, double mu, double xi, double tol, int maxit) {
  double m = 0, M = 0;
  for (double v : y) { m += v/static_cast<double>(y.size()); M=std::max(M,v); }
  if (xi == 0) return {mu,m,xi,gpd_loglik(y,m,xi),0,GpdStatus::Ok};
  double lo = std::max(0.0,-xi*M), hi = std::max(M,lo*2);
  auto score = [&](double s) {
    long double v=0;
    for (double z : y) v += (1+static_cast<long double>(xi))*z/(s+static_cast<long double>(xi)*z);
    return v/y.size()-1;
  };
  while (score(hi)>0 && std::isfinite(hi)) hi*=2;
  int it=0;
  for (; it<maxit; ++it) {
    const double mid=lo+(hi-lo)/2;
    if (score(mid)>0) lo=mid; else hi=mid;
    if (hi-lo <= tol*hi) break;
  }
  const double s=lo+(hi-lo)/2;
  return {mu,s,xi,gpd_loglik(y,s,xi),it+1,
          it<maxit ? GpdStatus::Ok : GpdStatus::IterationFailure};
}
inline GpdResult gpd_mle(span x, double tol=1e-12, int maxit=200,
                         double mu=0, double xi=std::numeric_limits<double>::quiet_NaN()) {
  GpdResult bad{mu,0,xi,0,0,GpdStatus::InvalidData};
  if (x.size()==0 || !std::isfinite(mu) || !(tol>0) || maxit<1) return bad;
  std::vector<double> y;
  double M=0, minimum=std::numeric_limits<double>::infinity();
  for (double v : x) {
    const double z=v-mu;
    if (!(z>0) || !std::isfinite(z)) return bad;
    y.push_back(z); M=std::max(M,z); minimum=std::min(minimum,z);
  }
  const span ys{y.data(),y.size()};
  if (!std::isnan(xi)) {
    if (!std::isfinite(xi) || xi<-.5) { bad.status=GpdStatus::InvalidShape; return bad; }
    return gpd_scale(ys,mu,xi,tol,maxit);
  }
  auto best=gpd_scale(ys,mu,0,tol,maxit);
  auto floor=gpd_scale(ys,mu,-.5,tol,maxit);
  if (floor.status!=GpdStatus::Ok) return floor;
  if (floor.logLik>best.logLik) best=floor;
  // Grimshaw profile: differentiating in xi at fixed a=xi/sigma gives
  // xi=L=mean(log1p(a*y)); the remaining score is (1+L)*Q-1,
  // Q=mean(1/(1+a*y)). Dimensionless b=a*max(y), t=log1p(b).
  // Cache normalized moments once; the divided score loses precision near
  // zero even with log1p, whereas this series has no cancellation.
  double moments[13] = {};
  for (double z:y) {
    double power=1, r=z/M;
    for (int k=1;k<=12;++k) { power*=r; moments[k]+=power/y.size(); }
  }
  auto profile = [&](double t, bool shape) {
    const double b=std::expm1(t);
    double L=0,Q=0;
    if (std::abs(b)<1e-3 && !shape) {
      double sum=0,power=1;
      for(int k=2;k<=12;++k) {
        double c=moments[k];
        for(int j=1;j<=k;++j) c-=moments[j]*moments[k-j]/j;
        c-=moments[k]/k;
        sum+=(k%2 ? -1:1)*c*power; power*=b;
      }
      return sum;
    }
    for(double z:y) { const double v=b*(z/M); L+=std::log1p(v); Q+=1/(1+v); }
    L/=y.size(); Q/=y.size();
    return shape ? L : ((1+L)*Q-1)/(b*b);
  };
  // L is monotone; below L=-1/2 no stationary candidate is admissible.
  double left=-.5*y.size(), right=0;
  for(int j=0;j<50;++j) { auto mid=(left+right)/2; if(profile(mid,true)<-.5) left=mid; else right=mid; }
  const double lower=right;
  double upper=1;
  // Beyond this bound (1+L)Q < 1 follows from min(y), max(y).
  while (1+upper >= 1+std::expm1(upper)*(static_cast<double>(minimum)/M)) upper*=2;
  bool root_failure=false;
  auto root = [&](double l,double h) {
    double fl=profile(l,false);
    int it=0;
    for(;it<maxit;++it) {
      const double mid=(l+h)/2, fm=profile(mid,false);
      if((fl<0)==(fm<0)) {l=mid;fl=fm;} else h=mid;
      if(h-l<tol*(1+std::abs(mid))) break;
    }
    if(it==maxit) { root_failure=true; return; }
    const double t=(l+h)/2, shape=profile(t,true);
    const double scale=shape*M/std::expm1(t);
    const double ll=gpd_loglik(ys,scale,shape);
    if(shape>=-.5 && ll>best.logLik) best={mu,scale,shape,ll,it+1,GpdStatus::Ok};
  };
  auto scan=[&](double low,double high) {
    const int cells=32;
    double before=low, fb=profile(before,false);
    double prev=low, fp=fb;
    for(int j=1;j<=cells;++j) {
      const double next=low+(high-low)*j/cells, fn=profile(next,false);
      if ((fp<0)!=(fn<0)) root(prev,next);
      // A pair of roots can lie in one cell. Refine every sampled local
      // extremum of the score towards zero, including same-sign neighbours.
      if(j>1 && (fb<0)==(fp<0) && (fp<0)==(fn<0)
          && std::abs(fp)<std::abs(fb) && std::abs(fp)<std::abs(fn)) {
        double l=before,h=next;
        const double sign=fp<0 ? -1 : 1;
        for(int k=0;k<48;++k) {
          const double u=l+(h-l)*0.3819660112501051;
          const double v=h-(h-l)*0.3819660112501051;
          if(sign*profile(u,false)<sign*profile(v,false)) h=v; else l=u;
        }
        const double mid=(l+h)/2, fm=profile(mid,false);
        if((fm<0)!=(fb<0)) { root(before,mid); root(mid,next); }
      }
      before=prev; fb=fp; prev=next; fp=fn;
    }
  };
  scan(lower,-1e-12); scan(1e-12,upper);
  if (root_failure) best.status=GpdStatus::IterationFailure;
  return best;
}
} // namespace cppunivariate
