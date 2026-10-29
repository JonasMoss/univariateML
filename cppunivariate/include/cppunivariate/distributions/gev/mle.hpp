#pragma once
#include "../../core/span.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
enum class GevStatus { Ok, InvalidData, InvalidShape, IterationFailure };
struct GevResult {
  double mu, sigma, xi, logLik;
  int iterations;
  GevStatus status;
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
    if (!(1+xi*z>0)) return -std::numeric_limits<double>::infinity();
    const double L=gev_log_ratio(z,xi);
    ll-=(1+xi)*L+std::exp(-L);
  }
  return ll;
}
struct GevProfile { GevResult fit; double score, curvature; };
// At pivot x0 (minimum for xi>=0, maximum otherwise), write
// L_i=log1p(xi*(x_i-x0)/s0)/xi. The location/scale orbit has
// g=log(n/sum(exp(-L_i))), sigma=s0*exp(xi*g),
// mu=x0+s0*expm1(xi*g)/xi. Only u=log(s0) remains to solve.
// Internal helper: x must be sorted, finite and nonconstant.
inline GevProfile gev_profile(span x, double xi, double tol, int maxit) {
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
inline GevResult gev_mle(span x, double tol=1e-10, int maxit=100,
                         double xi=std::numeric_limits<double>::quiet_NaN()) {
  GevResult bad{0,0,xi,0,0,GevStatus::InvalidData};
  if(x.size()<3 || !(tol>0) || !std::isfinite(tol) || maxit<1) return bad;
  if(!std::isnan(xi) && (!std::isfinite(xi) || xi<-.5 || xi>1)) {
    bad.status=GevStatus::InvalidShape;return bad;
  }
  std::vector<double> y(x.begin(),x.end());
  for(double v:y) if(!std::isfinite(v)) return bad;
  std::sort(y.begin(),y.end());
  std::size_t minimum_count=1;
  while(minimum_count<y.size() && y[minimum_count]==y[0]) ++minimum_count;
  // At positive xi the lower-endpoint collapse is controlled only by
  // the multiplicity of the minimum. Equality is an unattained face.
  if(std::isnan(xi) ? 2*minimum_count>=y.size() :
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
  auto prev=gev_profile(data,-.5,tol,maxit);
  auto best=prev.fit;
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
  const int cells=8;
  for(int j=1;j<=cells;++j) {
    const auto next=gev_profile(data,-.5+1.5*j/cells,tol,maxit);
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
  return restore(best);
}
} // namespace cppunivariate
