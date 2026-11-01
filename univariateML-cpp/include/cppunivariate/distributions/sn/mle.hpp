#pragma once
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include <vector>
#include <limits>
namespace cppunivariate {
enum class SnStatus { Ok, InvalidInput, IterationFailure, RightHalfNormalBoundary, LeftHalfNormalBoundary };
struct SnResult { double xi,omega,alpha,logLik; int iterations; SnStatus status; };
namespace sn_detail {
struct Inner { double eta,h,ll; bool converged; };
// Normalized free-coordinate score, evaluated at the point actually returned.
inline double score_residual(const std::vector<double>& x,double a,double xi,
                             double omega,double eta,double h) {
  const bool fx=std::isfinite(xi),fw=std::isfinite(omega);
  if(fx && fw)return 0;
  const double n=static_cast<double>(x.size());
  double ge=n/eta,gh=0,fg=n/eta;
  for(double v:x) {
    const double z=eta*v-h,g=-z+a*inverse_mills(a*z);
    ge+=v*g;gh-=g;
    if(fx)fg+=(v-xi)*g;
  }
  if(fx)return std::abs(fg)*eta/n;
  if(fw)return std::abs(gh)/n;
  return std::hypot(ge*eta,gh)/n;
}
// Newton maximisation of a strictly concave objective on an affine subspace.
// Fixed xi imposes h=xi*eta; fixed omega imposes eta=1/omega.
inline Inner inner(const std::vector<double>& x,double a,double xi,double omega,double tol,int maxit) {
  const bool fx=std::isfinite(xi),fw=std::isfinite(omega);
  double eta=fw ? 1/omega : 1/std::hypot(1.0,fx ? xi : 0.0);
  double h=fx ? xi*eta : 0;
  const double n=static_cast<double>(x.size());
  // Each Newton iteration starts with the accepted point in this cache.
  // Line-search trials overwrite it; only acceptance starts another step.
  // Reuse its special functions for the next score and information matrix.
  std::vector<double> log_cdf(x.size()),tail_mills(x.size()),tail_derivative(x.size());
  const auto objective=[&](double e,double b) {
    if (!(e>0)) return special::kNegInf;
    double ll=n*std::log(e);
    for(std::size_t i=0;i<x.size();++i) {
      const double z=e*x[i]-b;
      const double t=a*z;
      if(t < -10) {
        double f=0,df=0;
        for(int k=24;k>=1;--k) {
          const double den=-t+f;
          df=-k*(1+df)/(den*den); f=k/den;
        }
        tail_mills[i]=-t+f; tail_derivative[i]=-1-df;
        log_cdf[i]=-0.5*t*t-0.5*std::log(2*M_PI)-std::log(-t+f);
      } else log_cdf[i]=log_phi_cdf(t);
      ll+=std::log(2.0)-0.5*std::log(2*M_PI)-0.5*z*z+log_cdf[i];
    }
    return ll;
  };
  double ll=objective(eta,h);
  if(fx && fw) return {eta,h,ll,true};
  for(int it=0;it<maxit;++it) {
    double ge=n/eta,gh=0,weight=0,weighted_mean=0,central=0;
    double fixed_gradient=n/eta,fixed_information=n/(eta*eta);
    for(std::size_t i=0;i<x.size();++i) {
      const double v=x[i],z=eta*v-h,t=a*z;
      double r,dr;
      if(t < -10) {
        r=tail_mills[i]; dr=tail_derivative[i];
      } else {
        r=std::exp(-0.5*t*t-0.5*std::log(2*M_PI)-log_cdf[i]);
        dr=-r*(t+r);
      }
      const double g=-z+a*r,d=-1+a*a*dr;
      ge+=v*g; gh-=g;
      const double w=-d,diff=v-weighted_mean,new_weight=weight+w;
      weighted_mean+=w/new_weight*diff;
      central+=w*diff*(v-weighted_mean); weight=new_weight;
      if(fx) { fixed_gradient+=(v-xi)*g; fixed_information+=w*(v-xi)*(v-xi); }
    }
    double de=0,dh=0,residual=0;
    if(fx && fw) return {eta,h,ll,true};
    if(fx) {
      de=fixed_gradient/fixed_information; dh=xi*de;
      residual=std::abs(fixed_gradient)*eta/n;
    } else if(fw) { dh=gh/weight; residual=std::abs(gh)/n; }
    else {
      const double schur=n/(eta*eta)+central;
      de=(ge+weighted_mean*gh)/schur; dh=gh/weight+weighted_mean*de;
      residual=std::max(std::abs(ge)*eta,std::abs(gh))/n;
    }
    if(residual<tol) return {eta,h,ll,true};
    // Once the Newton gain is below rounding of the log likelihood, a
    // stricter Armijo comparison cannot reliably distinguish either step.
    if(residual<1e-7 && ge*de+gh*dh<1e-13*(1+std::abs(ll)))
      return {eta,h,ll,true};
    double step=1;
    bool accepted=false;
    for(int k=0;k<60;++k) {
      const double e=eta+step*de,b=h+step*dh,candidate=objective(e,b);
      if(e>0 && candidate>=ll+1e-4*step*(ge*de+gh*dh)) {
        eta=e; h=b; ll=candidate; accepted=true; break;
      }
      step*=0.5;
    }
    if(!accepted) return {eta,h,ll,score_residual(x,a,xi,omega,eta,h)<std::max(tol,1e-3/n)};
  }
  return {eta,h,ll,score_residual(x,a,xi,omega,eta,h)<std::max(tol,1e-3/n)};
}
}
inline SnResult sn_mle(span data,double tol=1e-9,int maxit=100,
                       double xi=special::kNaN,double omega=special::kNaN,double alpha=special::kNaN) {
  const double nan=special::kNaN;
  SnResult bad={nan,nan,nan,nan,0,SnStatus::InvalidInput};
  if(data.size()<2 || !(tol>0) || maxit<1 || (!std::isnan(xi) && !std::isfinite(xi)) ||
     (!std::isnan(omega) && !(std::isfinite(omega) && omega>0)) || (!std::isnan(alpha) && !std::isfinite(alpha))) return bad;
  double low=data[0],high=data[0];
  for(double v:data) { if(!std::isfinite(v)) return bad; low=std::min(low,v); high=std::max(high,v); }
  const bool constant=low==high;
  if(constant && !std::isfinite(omega) && (!std::isfinite(xi) || xi==low)) return bad;
  const double origin=low+(high-low)/2;
  const double scale=constant ? (std::isfinite(omega) ? omega : std::abs(xi-low)) : (high-low)/2;
  if(!(scale>0) || !std::isfinite(scale)) return bad;
  std::vector<double> x;
  for(double v:data) x.push_back((v-origin)/scale);
  double mean=0,ss=0;
  for(double v:x) mean+=v/x.size();
  for(double v:x) ss+=(v-mean)*(v-mean)/x.size();
  const double sd=constant ? 1 : std::sqrt(ss),center=origin+scale*mean,unit=scale*sd;
  for(double& v:x) v=(v-mean)/sd;
  const double fx=std::isfinite(xi) ? ((xi-origin)/scale-mean)/sd : nan;
  const double fw=std::isfinite(omega) ? omega/unit : nan;
  const double n=static_cast<double>(x.size());
  sn_detail::Inner best={0,0,special::kNegInf,false}; double besta=0; int evaluations=0;
  const auto fit=[&](double a) { ++evaluations; return sn_detail::inner(x,a,fx,fw,tol,maxit); };
  const auto consider=[&](double a,const sn_detail::Inner& v) { if(v.ll>best.ll) {best=v; besta=a;} };
  if(std::isfinite(alpha) || (constant && xi==low)) {
    besta=std::isfinite(alpha) ? alpha : 0;
    best=fit(besta);
  }
  else if(std::isfinite(fx)) {
    // With fixed xi, beta=alpha*eta separates shape from inverse scale:
    // ell=n log(eta)-eta^2 sum(d^2)/2+sum(log Phi(beta*d))+constant.
    double square=0; bool positive=false,negative=false;
    for(double v:x) { const double d=v-fx; square+=d*d; positive=positive || d>0; negative=negative || d<0; }
    if(positive && negative) {
      const double eta=std::isfinite(fw) ? 1/fw : std::sqrt(n/square);
      const auto score=[&](double beta) {
        double sum=0;
        for(double v:x) { const double d=v-fx; sum+=d*sn_detail::inverse_mills(beta*d); }
        return sum;
      };
      double l=-1,r=1;
      while(score(l)<0) l*=2;
      while(score(r)>0) r*=2;
      for(int k=0;k<std::min(maxit,160);++k) {
        const double mid=l+(r-l)/2;
        if(mid==l || mid==r) break;
        if(score(mid)>0) l=mid; else r=mid;
      }
      if (r-l>1e-10*(1+std::abs(l)+std::abs(r))) {bad.status=SnStatus::IterationFailure;return bad;}
      besta=(l+(r-l)/2)/eta;
      double ll=n*std::log(eta);
      for(double v:x) ll+=sn_detail::log_density(eta*(v-fx),besta);
      best={eta,fx*eta,ll,true}; ++evaluations;
    }
    // One-sided data have a monotone shape likelihood; compare faces below.
  } else {
    // Uniform central delta grid plus logarithmic alpha wings. Every sampled
    // local maximum is refined, including the normal member at zero.
    std::vector<double> grid;
    for(int i=-49;i<=49;++i) { const double d=i/50.0; grid.push_back(d/std::sqrt(1-d*d)); }
    for(double a=8;a<=4294967296.0;a*=2) { grid.push_back(a); grid.push_back(-a); }
    std::sort(grid.begin(),grid.end());
    std::vector<sn_detail::Inner> values;
    for(double a:grid) { values.push_back(fit(a)); consider(a,values.back()); }
    for(std::size_t i=1;i+1<grid.size();++i) {
      if(values[i].ll<values[i-1].ll || values[i].ll<values[i+1].ll) continue;
      double l=std::asinh(grid[i-1]),r=std::asinh(grid[i+1]);
      const double ratio=(std::sqrt(5.0)-1)/2;
      double u=r-ratio*(r-l),v=l+ratio*(r-l);
      auto fu=fit(std::sinh(u)),fv=fit(std::sinh(v));
      for(int k=0;k<std::min(maxit,70) && r-l>1e-9;++k) {
        consider(std::sinh(u),fu); consider(std::sinh(v),fv);
        if(fu.ll<fv.ll) { l=u;u=v;fu=fv;v=l+ratio*(r-l);fv=fit(std::sinh(v)); }
        else { r=v;v=u;fv=fu;u=r-ratio*(r-l);fu=fit(std::sinh(u)); }
      }
      if(r-l>1e-9) {bad.status=SnStatus::IterationFailure;return bad;}
    }
  }
  if(!std::isfinite(alpha) && !(constant && xi==low)) {
    for(double sign:{-1.0,1.0}) {
      double endpoint=sign>0 ? -std::numeric_limits<double>::infinity() : std::numeric_limits<double>::infinity();
      for(double v:x) endpoint=sign>0 ? std::max(endpoint,-v) : std::min(endpoint,-v);
      // Right endpoint is min(x); left endpoint is max(x).
      endpoint=-endpoint;
      const double c=std::isfinite(fx) ? fx : endpoint;
      bool feasible=true; double square=0,penalty=0;
      for(double v:x) { if(sign*(v-c)<0) feasible=false; square+=(v-c)*(v-c); if(std::isfinite(fx) && v==c) penalty+=std::log(2.0); }
      const double w=std::isfinite(fw) ? fw : std::sqrt(square/n);
      if(!feasible || !(w>0)) continue;
      const double ll=n*(0.5*std::log(2/M_PI)-std::log(w))-square/(2*w*w)-penalty;
      if(ll>=best.ll-1e-8*n) return {center+unit*c,unit*w,sign*std::numeric_limits<double>::infinity(),ll-n*std::log(unit),evaluations,
        sign>0 ? SnStatus::RightHalfNormalBoundary : SnStatus::LeftHalfNormalBoundary};
    }
  }
  return {std::isfinite(xi) ? xi : center+unit*best.h/best.eta,
    std::isfinite(omega) ? omega : unit/best.eta,besta,best.ll-n*std::log(unit),evaluations,
    best.converged ? SnStatus::Ok : SnStatus::IterationFailure};
}
}
