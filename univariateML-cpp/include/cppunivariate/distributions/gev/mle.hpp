#pragma once
#include "../../core/span.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
inline constexpr double kGevXiLower = -1.0;
inline constexpr double kGevXiUpper = 1.0;

enum class GevStatus { Ok, InvalidData, InvalidShape, IterationFailure, NoMLE };
struct GevResult {
  double mu, sigma, xi, logLik;
  int iterations;
  GevStatus status; unsigned restricted = 0;
};
inline double gev_log_ratio(double z, double xi) {
  const double v=xi*z;
  if (std::abs(v)<1e-4) return z*(1-v/2+v*v/3-v*v*v/4+v*v*v*v/5-v*v*v*v*v/6);
  return std::log1p(v)/xi;
}
inline double gev_loglik(span x, double mu, double sigma, double xi) {
  if (!(sigma>0)) return -std::numeric_limits<double>::infinity();
  double ll=-static_cast<double>(x.size())*std::log(sigma);
  for (double v:x) {
    const double z=(v-mu)/sigma;
    if (xi == -1) {
      if (v>mu+sigma && z>1+8*std::numeric_limits<double>::epsilon()) return -std::numeric_limits<double>::infinity();
      ll += z-1;
      continue;
    }
    if (!(1+xi*z>0)) return -std::numeric_limits<double>::infinity();
    const double L=gev_log_ratio(z,xi);
    ll-=(1+xi)*L+std::exp(-L);
  }
  return ll;
}
// Hold-specific scalar search. Enumerate sampled local maxima and refine
// each bracket; this is numerical root discovery, not a global certificate.
namespace extreme_detail {
struct Maximum { double x, value; int iterations; bool ok; };
template<class F>
inline Maximum maximize(F f, double lo, double hi, double tol, int maxit,
                        bool closed=true) {
  const int cells=128;
  Maximum best{lo,f(lo),0,false};
  auto keep=[&](double x, double value, int it, bool ok) {
    if(std::isfinite(value) && (!std::isfinite(best.value) || value>best.value))
      best={x,value,it,ok};
  };
  if(closed && std::isfinite(best.value)) best.ok=true;
  double a=lo, fa=best.value, b=lo+(hi-lo)/cells, fb=f(b);
  for(int j=2;j<=cells;++j) {
    const double c=lo+(hi-lo)*j/cells, fc=f(c);
    if(std::isfinite(fb) && fb>=fa && fb>=fc) {
      double l=a,h=c;
      double u=h-(h-l)*.6180339887498949,v=l+(h-l)*.6180339887498949;
      double fu=f(u),fv=f(v);
      int it=0;
      for(;it<maxit && h-l>tol*(1+std::abs((h+l)/2));++it) {
        if(fu>fv) {h=v;v=u;fv=fu;u=h-(h-l)*.6180339887498949;fu=f(u);}
        else {l=u;u=v;fu=fv;v=l+(h-l)*.6180339887498949;fv=f(v);}
      }
      const double x=(l+h)/2;
      keep(x,f(x),it,it<maxit);
    }
    a=b;fa=fb;b=c;fb=fc;
  }
  if(closed) keep(hi,fb,0,true);
  // An unresolved noncompact edge is a failure, never a returned fit.
  if(!closed && (f(lo)>best.value || f(hi)>best.value)) best.ok=false;
  if(maxit<2) best.ok=false;
  return best;
}
} // namespace extreme_detail

// Fixed location and/or scale, standardized by the caller.
inline GevResult gev_held_profile(span x,double mu,double sigma,double xi,
                                  double tol,int maxit) {
  const bool hm=!std::isnan(mu), hs=!std::isnan(sigma);
  if(hm && hs) return {mu,sigma,xi,gev_loglik(x,mu,sigma,xi),0,GevStatus::Ok};
  if (xi == -1) {
    const double M=x[x.size()-1];
    if (!hs) {
      double average=0; for(double v:x) average+=v/x.size();
      sigma=hm ? std::max(M-mu,mu-average) : M-average;
    }
    if (!hm) mu=M-sigma;
    const double ll=gev_loglik(x,mu,sigma,xi);
    return {mu,sigma,xi,ll,0,std::isfinite(ll) ? GevStatus::Ok : GevStatus::InvalidData};
  }
  double low=0;
  if(hm) for(double v:x) low=std::max(low,-xi*(v-mu));
  auto location=[&](double u) {
    const double pivot=xi>=0 ? x[0] : x[x.size()-1];
    return pivot-sigma*(xi==0 ? u : std::expm1(xi*u)/xi);
  };
  if(hs && xi==0) {
    double se=0;
    for(double v:x) se+=std::exp(-(v-x[0])/sigma);
    mu=x[0]-sigma*std::log(se/x.size());
    return {mu,sigma,xi,gev_loglik(x,mu,sigma,xi),0,GevStatus::Ok};
  }
  auto objective=[&](double u) {
    return gev_loglik(x,hm ? mu : location(u),hs ? sigma : low+std::exp(u),xi);
  };
  const auto opt=extreme_detail::maximize(objective,-32,32,tol,maxit,false);
  return {hm ? mu : location(opt.x),hs ? sigma : low+std::exp(opt.x),xi,
          opt.value,opt.iterations,opt.ok ? GevStatus::Ok : GevStatus::IterationFailure};
}
inline GevResult gev_held_mle(span x,double tol,int maxit,double xi,double mu,double sigma) {
  GevResult bad{mu,sigma,xi,0,0,GevStatus::InvalidData};
  const bool hm=!std::isnan(mu), hs=!std::isnan(sigma);
  if(x.size()==0 || !(tol>0) || !std::isfinite(tol) || maxit<1 ||
     (hm && !std::isfinite(mu)) || (hs && (!(sigma>0) || !std::isfinite(sigma)))) return bad;
  if(!std::isnan(xi) && !std::isfinite(xi)) {
    bad.status=GevStatus::InvalidShape;return bad;
  }
  std::vector<double> y(x.begin(),x.end());
  for(double v:y) if(!std::isfinite(v)) return bad;
  std::sort(y.begin(),y.end());
  if(!std::isnan(xi) && xi < -1 && !(hm && (hs || y.back()<=mu))) {bad.status=GevStatus::InvalidShape;return bad;}
  bool collapse_face=false;
  if(!hs && hm && mu==y[0]) {
    std::size_t k=0;for(double v:y) if(v==y[0]) ++k;
    if(k==y.size() || (std::isnan(xi) ? 2*k>y.size() : (xi>0 && xi*k>=y.size()-k))) {
      bad.status=GevStatus::NoMLE;return bad;
    }
    collapse_face=std::isnan(xi) && 2*k==y.size();
  }
  const double origin=y[0];
  const double scale=hs ? sigma : std::max(y.back()-origin,std::abs(origin-mu));
  if(!(scale>0) || !std::isfinite(scale)) return bad;
  for(double &v:y) v=(v-origin)/scale;
  const double m=hm ? (mu-origin)/scale : mu,s=hs ? sigma/scale : sigma;
  const span data{y.data(),y.size()};
  GevResult fit=bad;
  if(!std::isnan(xi)) fit=gev_held_profile(data,m,s,xi,tol,maxit);
  else {
    // Intersect the shape interval with support when both are held.
    double lo=-1,hi=1;
    if(hm && hs) for(double v:y) {
      const double z=(v-m)/s;
      if(z>0) lo=std::max(lo,-1/z);
      if(z<0) hi=std::min(hi,-1/z);
    }
    if(!(hi>lo)) return bad;
    auto objective=[&](double k) {return gev_held_profile(data,m,s,k,tol,maxit).logLik;};
    const auto opt=extreme_detail::maximize(objective,lo,hi,tol,maxit);
    fit=gev_held_profile(data,m,s,opt.x,tol,maxit);
    if(!opt.ok) fit.status=GevStatus::IterationFailure;
  }
  fit.mu=hm ? mu : origin+scale*fit.mu;
  fit.sigma=hs ? sigma : scale*fit.sigma;
  fit.logLik=gev_loglik(x,fit.mu,fit.sigma,fit.xi);
  if(collapse_face) {
    double face=0;
    for(double v:x) face+=v==mu ? -1 : -2*std::log(v-mu);
    if(fit.logLik<=face+tol*x.size()) fit.status=GevStatus::NoMLE;
  }
  if(!std::isfinite(fit.logLik)) fit.status=GevStatus::InvalidData;
  return fit;
}
struct GevProfile { GevResult fit; double score, curvature; };
// At pivot x0 (minimum for xi>=0, maximum otherwise), write
// L_i=log1p(xi*(x_i-x0)/s0)/xi. The location/scale orbit has
// g=log(n/sum(exp(-L_i))), sigma=s0*exp(xi*g),
// mu=x0+s0*expm1(xi*g)/xi. Only u=log(s0) remains to solve.
// Internal helper: x must be sorted, finite and nonconstant.
inline GevProfile gev_profile(span x, double xi, double tol, int maxit) {
  if (xi == -1) {
    const double nan=std::numeric_limits<double>::quiet_NaN();
    return {gev_held_profile(x,nan,nan,xi,tol,maxit),0,0};
  }
  const double n=static_cast<double>(x.size());
  const double pivot=xi<0 ? x[x.size()-1] : x[0];
  struct Eval { double ll, s, ds, g, shape, curvature; };
  auto eval=[&](double u) {
    const double inv=std::exp(-u);
    const double shift=-gev_log_ratio((x[0]-pivot)*inv,xi);
    double sl=0,sa=0,sb=0,se=0,sea=0,seaa=0,seb=0,sd=0,sed=0,sd2=0,sed2=0,sedd=0,sad=0,saa=0;
    for(double v:x) {
      const double w=(v-pivot)*inv,t=1+xi*w;
      const double L=gev_log_ratio(w,xi),a=w/t,b=a/t;
      const double e=std::exp(-L-shift);
      const double q=xi*w;
      const double d=std::abs(q)<1e-4 ? w*w*(-.5+q*(2./3+q*(-.75+q*(.8-q*5./6)))) : (a-L)/xi;
      const double d2=std::abs(q)<1e-4 ? w*w*w*(2./3+q*(-1.5+q*(2.4-q*10./3))) : (-a*a-2*d)/xi;
      sd2+=d2;sed2+=e*d2;sedd+=e*d*d;sad+=e*a*d;saa+=a*a;
      sl+=L; sa+=a; sb+=b; se+=e; sea+=e*a; seaa+=e*a*a; seb+=e*b; sd+=d; sed+=e*d;
    }
    const double g=std::log(n/se)-shift, ea=sea/se,ed=sed/se;
    const double ds=-(1+xi)*sb/n+seb/se-seaa/se+ea*ea;
    const double cross=sa/n-(1+xi)*saa/n+seaa/se+sad/se-ea*ed;
    const double shape2=sed2/se-sedd/se+ed*ed-2*sd/n-(1+xi)*sd2/n;
    return Eval{-n*u+n*g-n-(1+xi)*sl,
      (1+xi)*sa/n-ea-1, ds,
      g,n*ed-sl-(1+xi)*sd,n*(shape2-cross*cross/ds)};
  };
  double u=std::log(x[x.size()-1]-x[0])-1, lo=u-2,hi=u+2;
  int it=0;
  while(eval(lo).s<=0 && it++<maxit) lo-=2;
  while(eval(hi).s>=0 && it++<maxit) hi+=2;
  u=std::max(lo,std::min(hi,u));
  Eval e=eval(u);
  bool done=false;
  for(;it<maxit;++it) {
    e=eval(u);
    if(std::abs(e.s)<tol) {done=true;break;}
    if(e.s>0) lo=u; else hi=u;
    double next=u-e.s/e.ds;
    if(!std::isfinite(next) || next<=lo || next>=hi) next=(lo+hi)/2;
    if(std::abs(next-u)<tol*(1+std::abs(u))) {u=next;e=eval(u);done=true;break;}
    u=next;
  }
  const double s0=std::exp(u),mu=pivot+s0*(xi==0 ? e.g : std::expm1(xi*e.g)/xi);
  return {{mu,s0*std::exp(xi*e.g),xi,e.ll,it+1,
    done && std::isfinite(e.ll) ? GevStatus::Ok : GevStatus::IterationFailure},e.shape,e.curvature};
}
inline GevResult gev_mle_impl(span x, double tol=1e-10, int maxit=100,
                         double xi=std::numeric_limits<double>::quiet_NaN(),
                         double mu=std::numeric_limits<double>::quiet_NaN(),
                         double sigma=std::numeric_limits<double>::quiet_NaN()) {
  if(!std::isnan(mu) || !std::isnan(sigma)) return gev_held_mle(x,tol,maxit,xi,mu,sigma);
  GevResult bad{0,0,xi,0,0,GevStatus::InvalidData};
  if(x.size()<3 || !(tol>0) || !std::isfinite(tol) || maxit<1) return bad;
  if(!std::isnan(xi) && (!std::isfinite(xi) || xi < -1)) {
    bad.status=GevStatus::InvalidShape;return bad;
  }
  std::vector<double> y(x.begin(),x.end());
  for(double v:y) if(!std::isfinite(v)) return bad;
  std::sort(y.begin(),y.end());
  std::size_t minimum_count=1;
  while(minimum_count<y.size() && y[minimum_count]==y[0]) ++minimum_count;
  // At positive xi the lower-endpoint collapse is controlled only by
  // the multiplicity of the minimum. A strict majority (k>n/2) diverges
  // to infinity regardless of xi -- genuinely no MLE. Exact equality
  // (k==n/2, only possible for even n) is different when xi is also
  // free: the xi->1 collapse face then has a finite but unattained
  // supremum (A_free below, notes/evaluations/2026-10-29-p0-extreme.md),
  // and the ordinary profile search over xi in (-1,1) can still find a
  // genuinely attained interior maximum elsewhere that beats it -- TASK-85/87
  // used to reject this case outright (conservatively, as InvalidData)
  // without ever comparing against that face. Held xi has no such
  // escape hatch (there is no "elsewhere" to search), so xi*q==1 (q=1 at
  // k=n/2) stays "never" there, unchanged.
  const bool tie_half = std::isnan(xi) && 2*minimum_count==y.size();
  if(std::isnan(xi) ? 2*minimum_count>y.size() :
      (xi>0 && xi*static_cast<double>(minimum_count)>=y.size()-minimum_count)) return bad;
  // Standardization prevents scale and translation from setting tolerances.
  const double origin=y[0],range=y.back()-origin;
  if(!(range>0) || !std::isfinite(range)) return bad;
  for(double &v:y) v=(v-origin)/range;
  const span data{y.data(),y.size()};
  auto restore=[&](GevResult r) {
    r.mu=origin+range*r.mu;r.sigma*=range;
    r.logLik-=y.size()*std::log(range);return r;
  };
  if(!std::isnan(xi)) return restore(gev_profile(data,xi,tol,maxit).fit);
  auto prev=gev_profile(data,-1+1e-6,tol,maxit);
  auto best=gev_profile(data,-1,tol,maxit).fit;
  if(prev.fit.logLik>best.logLik) best=prev.fit;
  bool failed=best.status!=GevStatus::Ok;
  auto retain=[&](const GevProfile &p) {
    failed=failed || p.fit.status!=GevStatus::Ok;
    if(p.fit.logLik>best.logLik) best=p.fit;
  };
  auto root=[&](GevProfile left,GevProfile right) {
    double l=left.fit.xi,h=right.fit.xi;
    double mid=(l*right.score-h*left.score)/(right.score-left.score);
    bool done=false;
    for(int k=0;k<maxit;++k) {
      if(!std::isfinite(mid) || mid<=l || mid>=h) mid=(l+h)/2;
      const auto candidate=gev_profile(data,mid,tol,maxit);
      retain(candidate);
      if(std::abs(candidate.score)<tol*y.size() || h-l<tol) {
        // Prefer the score-converged point over rounding ties in logLik.
        if(candidate.fit.logLik>=best.logLik-1e-12*y.size()) best=candidate.fit;
        done=true;break;
      }
      if(candidate.score>0) l=mid; else h=mid;
      mid-=candidate.score/candidate.curvature;
    }
    failed=failed || !done;
  };
  // Enumerate profile-score sign changes, retaining both attained bounds.
  // Curvature turns also expose pairs of roots within a single mesh cell.
  // This is root discovery, not an assumption of global concavity.
  //
  // At the k==n/2 tie, xi=1 itself is the unattained collapse face: the
  // mu/sigma profile search run there (gev_profile's own Newton solve
  // for a stationary score) has no root to find, since the supremum is
  // only approached, never attained, even at this one fixed xi (the
  // held-xi row's "xi*q==1: never, finite unattained collapse face",
  // q=k/(n-k)=1 here). So the scan's upper end is nudged strictly below
  // 1 -- mirroring the existing "-1+1e-6" nudge off the opposite boundary
  // a few lines up -- and the known closed-form supremum A_free is
  // compared against the best genuinely attained interior point instead.
  const double xi_hi = tie_half ? kGevXiUpper-1e-6 : kGevXiUpper;
  const int cells=8;
  for(int j=1;j<=cells;++j) {
    const auto next=gev_profile(data,-1+(xi_hi+1.0)*j/cells,tol,maxit);
    retain(next);
    if(prev.score>0 && next.score<0) root(prev,next);
    else if((prev.score>0)==(next.score>0) &&
            ((prev.score>0 && prev.curvature<0 && next.curvature>0) ||
             (prev.score<0 && prev.curvature>0 && next.curvature<0))) {
      auto l=prev,h=next;
      for(int k=0;k<maxit;++k) {
        const auto mid=gev_profile(data,(l.fit.xi+h.fit.xi)/2,tol,maxit);
        retain(mid);
        if((mid.score>0)!=(prev.score>0)) {
          if(prev.score>0) root(prev,mid); else root(mid,next);
          break;
        }
        if(h.fit.xi-l.fit.xi<tol) break;
        if((mid.curvature>0)==(l.curvature>0)) l=mid; else h=mid;
      }
    }
    prev=next;
  }
  if(failed) best.status=GevStatus::IterationFailure;
  GevResult result=restore(best);
  if(result.status==GevStatus::Ok && tie_half) {
    // A_free = n*log(2) - n - 2*sum_{x_i>m} log(x_i-m), m=min(x): the
    // xi->1 collapse face's limiting log-likelihood with mu and sigma
    // both free (notes/evaluations/2026-10-29-p0-extreme.md; verified
    // numerically here against a direct 2D Nelder-Mead optimization of
    // gev_loglik at xi=.9,.99,...,.999999, converging to this value
    // from below as xi->1 -- see the derivation in
    // notes/evaluations/2026-10-29-tail85-report.md). Computed on the
    // original data (matching gev_held_mle's analogous `face` for held
    // mu), directly comparable to `result.logLik`, which restore()
    // above already carries back to the original scale.
    double sum_log_d=0;
    for(double v:x) if(v>origin) sum_log_d+=std::log(v-origin);
    const double n=static_cast<double>(y.size());
    const double a_free=n*std::log(2.0)-n-2.0*sum_log_d;
    if(!(result.logLik>a_free+tol*n)) result.status=GevStatus::NoMLE;
  }
  return result;
}

inline GevResult gev_mle(span x,double tol=1e-10,int maxit=100,double xi=std::numeric_limits<double>::quiet_NaN(),double mu=std::numeric_limits<double>::quiet_NaN(),double sigma=std::numeric_limits<double>::quiet_NaN()) {
  auto r=gev_mle_impl(x,tol,maxit,xi,mu,sigma);
  if(r.status==GevStatus::Ok && (std::isnan(xi)) && r.xi==kGevXiLower) r.restricted|=16u;
  if(r.status==GevStatus::Ok && (std::isnan(xi)) && r.xi==kGevXiUpper) r.restricted|=32u;
  return r;
}
} // namespace cppunivariate
