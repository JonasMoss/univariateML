#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/face_compare.hpp"
#include "cppunivariate/core/constants.hpp"
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include <vector>
#include <limits>
namespace cppunivariate {
enum class SnStatus { Ok, InvalidInput, IterationFailure, RightHalfNormalBoundary, LeftHalfNormalBoundary };
struct SnResult { double xi,omega,alpha,logLik; int iterations; SnStatus status;  StopDetail stop_detail{}; bool budget_exhausted=false; };
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
// Gaussian curvature minorant: ell(y) <= ell(v)+g'(y-v)
// - (y-v)'G(y-v)/2. It bounds both the global gap and distance.
struct Acceptance { double gap, eta_error, h_error; bool accepted; };
inline Acceptance acceptance(const std::vector<double>& x,double a,double xi,
                             double omega,double eta,double h) {
  const bool fx=std::isfinite(xi),fw=std::isfinite(omega);
  if(!(eta>0) || !std::isfinite(eta) || !std::isfinite(h))return {INFINITY,INFINITY,INFINITY,false};
  if(fx && fw)return {0,0,0,true};
  const long double n=x.size(),e=eta,b=h;
  long double ge=n/e,gh=0,fg=n/e,mean=0,central=0,square=0;
  std::size_t count=0;
  for(double v:x) {
    const double z=eta*v-h,g=-z+a*inverse_mills(a*z);
    ge+=static_cast<long double>(v)*g;gh-=g;
    const long double d=static_cast<long double>(v)-xi;
    if(fx){fg+=d*g;square+=d*d;}
    const long double delta=v-mean;mean+=delta/++count;central+=delta*(v-mean);
  }
  long double norm2=0,de=0,dh=0;
  if(fx) {
    if(!(square>0))return {INFINITY,INFINITY,INFINITY,false};
    norm2=fg*fg/square;de=std::abs(fg)/square;dh=std::abs(xi)*de;
  } else if(fw) {norm2=gh*gh/n;dh=std::abs(gh)/n;}
  else {
    if(!(central>0))return {INFINITY,INFINITY,INFINITY,false};
    norm2=(ge+mean*gh)*(ge+mean*gh)/central+gh*gh/n;
    de=std::sqrt(norm2/central);
    dh=std::sqrt(norm2*(1/n+mean*mean/central));
  }
  const long double location_error=fx?0:(dh+std::abs(b/e)*de)/(e-de);
  const bool ok=std::isfinite(norm2) && de<e && norm2/2<=5e-9L*n &&
    de/(e-de)<=5e-7L && location_error<=5e-7L*(1+std::abs(b/e));
  return {static_cast<double>(norm2/2),static_cast<double>(de),static_cast<double>(dh),ok};
}
// Newton maximisation of a strictly concave objective on an affine subspace.
// Fixed xi imposes h=xi*eta; fixed omega imposes eta=1/omega.
inline Inner inner(const std::vector<double>& x,double a,double xi,double omega,double tol,int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
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
        log_cdf[i]=-0.5*t*t-0.5*std::log(2*cppunivariate::pi)-std::log(-t+f);
      } else log_cdf[i]=log_phi_cdf(t);
      ll+=std::log(2.0)-0.5*std::log(2*cppunivariate::pi)-0.5*z*z+log_cdf[i];
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
        r=std::exp(-0.5*t*t-0.5*std::log(2*cppunivariate::pi)-log_cdf[i]);
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
    if(residual<tol && acceptance(x,a,xi,omega,eta,h).accepted) return {eta,h,ll,true};
    // Once the Newton gain is below rounding of the log likelihood, a
    // stricter Armijo comparison cannot reliably distinguish either step.
    if(residual<1e-7 && ge*de+gh*dh<1e-13*(1+std::abs(ll)) &&
       acceptance(x,a,xi,omega,eta,h).accepted)
      return {eta,h,ll,true};
    double step=1;
    bool accepted=false;
    for(int k=0;k<maxit;++k) {
      const double e=eta+step*de,b=h+step*dh,candidate=objective(e,b);
      if(e>0 && candidate>=ll+1e-4*step*(ge*de+gh*dh)) {
        eta=e; h=b; ll=candidate; accepted=true; break;
      }
      step*=0.5;
    }
    if(!accepted) return {eta,h,ll,acceptance(x,a,xi,omega,eta,h).accepted};
  }
  return {eta,h,ll,acceptance(x,a,xi,omega,eta,h).accepted};
}
// Global shape bounds, conditional on floating-point comparisons.
struct ShapeTangent { long double c,q,s; };
inline ShapeTangent shape_tangent(const std::vector<double>& x,double a,const Inner& v) {
  long double c=0,q=0,s=0;
  for(double y:x) {
    const double t=a*(v.eta*y-v.h),r=inverse_mills(t);
    c+=static_cast<long double>(log_phi_cdf(t))-static_cast<long double>(r)*t;
    q+=static_cast<long double>(r)*y; s+=r;
  }
  c+=static_cast<long double>(x.size())*(std::log(2.0)-0.5*std::log(2*cppunivariate::pi));
  return {c,q,s};
}
inline double shape_upper(const std::vector<double>& x,double omega,double a,const ShapeTangent& t) {
  const long double n=x.size(); long double sx=0,sxx=0;
  for(double y:x) { sx+=y;sxx+=static_cast<long double>(y)*y; }
  const long double aa=a,linear=aa*(t.q-t.s*sx/n),quad=sxx-sx*sx/n;
  if(!(quad>0) && !std::isfinite(omega)) return std::numeric_limits<double>::infinity();
  long double eta;
  if(std::isfinite(omega)) eta=1/static_cast<long double>(omega);
  else { const long double root=std::sqrt(linear*linear+4*quad*n);
    eta=linear>=0 ? (linear+root)/(2*quad) : 2*n/(root-linear); }
  const long double h=(eta*sx-aa*t.s)/n;
  const long double v=t.c+n*std::log(eta)-(eta*eta*sxx-2*eta*h*sx+n*h*h)/2+aa*(eta*t.q-h*t.s);
  return std::isfinite(v) ? static_cast<double>(v) : std::numeric_limits<double>::infinity();
}
// Tail bound: minimize weighted Q by its monotone derivative, then use the
// exact minimum for its active quadratic segment (avoids optimistic bisection).
inline double shape_tail_upper(const std::vector<double>& x,double omega,double a,double sign,int maxit=100) {
  maxit = static_cast<int>(solver_budget(maxit));
  const long double n=x.size(),w=static_cast<long double>(a)*a;
  long double lo=std::numeric_limits<long double>::infinity(),hi=-lo;
  for(double y:x) { const long double z=sign*y;lo=std::min(lo,z);hi=std::max(hi,z); }
  for(int i=0;i<maxit;++i) {
    const long double mid=(lo+hi)/2;long double sw=0,sx=0;
    for(double y:x) {const long double z=sign*y,weight=1+(z<mid ? w:0);sw+=weight;sx+=weight*(z-mid);}
    if(sx>0) lo=mid;else hi=mid;
  }
  const long double mid=(lo+hi)/2;
  long double sw=0,delta=0,q=0;
  for(double y:x) {const long double z=sign*y,weight=1+(z<mid ? w:0),d=z-mid;sw+=weight;delta+=weight*d;q+=weight*d*d;}
  // This quadratic is a lower bound on Q only while the active set is valid.
  const long double optimum=mid+delta/sw;
  bool valid=true;for(double y:x) if((sign*y<mid)!=(sign*y<optimum)) valid=false;
  if(!valid) return std::numeric_limits<double>::infinity();
  q-=delta*delta/sw;
  if(!std::isfinite(q) || q<0 || (q==0 && !std::isfinite(omega))) return std::numeric_limits<double>::infinity();
  const long double eta=std::isfinite(omega) ? 1/static_cast<long double>(omega):std::sqrt(n/q);
  const long double value=n*(std::log(2.0)-.5L*std::log(2*cppunivariate::pi)+std::log(eta))-eta*eta*q/2;
  return std::isfinite(value)?static_cast<double>(value):std::numeric_limits<double>::infinity();
}
}
inline SnResult sn_mle(span data,double tol=1e-9,int maxit=100,
                       double xi=special::kNaN,double omega=special::kNaN,double alpha=special::kNaN, std::int64_t work_budget=-2) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=special::kNaN;
  SnResult bad={nan,nan,nan,nan,0,SnStatus::InvalidInput};
  if(data.size()<2 || !(tol>0) || !std::isfinite(tol) || maxit<1 || (work_budget<0 && work_budget!= -2) || (!std::isnan(xi) && !std::isfinite(xi)) ||
     (!std::isnan(omega) && !(std::isfinite(omega) && omega>0)) || (!std::isnan(alpha) && !std::isfinite(alpha))) return bad;
  double low=data[0],high=data[0];
  for(double v:data) { if(!std::isfinite(v)) return bad; low=std::min(low,v); high=std::max(high,v); }
  const bool constant=low==high;
  if(constant && !std::isfinite(omega) && (!std::isfinite(xi) || xi==low)) return bad;
  const double origin=low+(high-low)/2;
  const double scale=constant ? (std::isfinite(omega) ? omega : std::abs(xi-low)) : (high-low)/2;
  if(!(scale>0) || !std::isfinite(scale)) {bad.status=SnStatus::IterationFailure;return bad;}
  std::vector<double> x;
  for(double v:data) x.push_back((v-origin)/scale);
  double mean=0,ss=0;
  for(double v:x) mean+=v/x.size();
  for(double v:x) ss+=(v-mean)*(v-mean)/x.size();
  const double sd=constant ? 1 : std::sqrt(ss),center=origin+scale*mean,unit=scale*sd;
  if(!(unit>0) || !std::isfinite(unit) || !std::isfinite(center)) {
    bad.status=SnStatus::IterationFailure;return bad;
  }
  for(double& v:x) v=(v-mean)/sd;
  const double fx=std::isfinite(xi) ? ((xi-origin)/scale-mean)/sd : nan;
  const double fw=std::isfinite(omega) ? omega/unit : nan;
  if((std::isfinite(xi) && !std::isfinite(fx)) ||
     (std::isfinite(omega) && !(fw>0 && std::isfinite(fw))) ||
     !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);})) {
    bad.status=SnStatus::IterationFailure;return bad;
  }
  // Endpoint factors and monotone-shape branches use exact residual signs.
  // A rounded standardization must not turn a strict residual into a tie.
  if(std::isfinite(xi)) for(std::size_t i=0;i<x.size();++i) {
    if((data[i]<xi)!=(x[i]<fx) || (data[i]>xi)!=(x[i]>fx)) {
      bad.status=SnStatus::IterationFailure;return bad;
    }
  }
  const double n=static_cast<double>(x.size());
  sn_detail::Inner best={0,0,special::kNegInf,false}; double besta=0; int evaluations=0;
  const auto fit=[&](double a) { ++evaluations; return sn_detail::inner(x,a,fx,fw,tol,maxit); };
  const auto consider=[&](double a,const sn_detail::Inner& v) { if(v.converged && v.ll>best.ll) {best=v; besta=a;} };
  if(constant && xi==low && std::isfinite(omega) && !std::isfinite(alpha)) {
    const double ll=-n*(std::log(omega)+.5*std::log(2*cppunivariate::pi));
    bad.status=SnStatus::IterationFailure;
    bad.stop_detail=comparison_detail({{xi,omega,0},ll,ll,StopModel::None},
                                      {{xi,omega,1},ll,ll,StopModel::None});
    bad.stop_detail.loglik_upper=ll;
    return bad;
  }
  if(std::isfinite(alpha)) {
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
      for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (score(l)<0); ++bracket_pass) l*=2;
      for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (score(r)>0); ++bracket_pass) r*=2;
      if(!(score(l)>=0 && score(r)<=0)){bad.status=SnStatus::IterationFailure;return bad;}
      bool certified=false;
      for(int k=0;k<maxit;++k) {
        const double sl=score(l),sr=score(r),width=r-l;
        // Concavity bounds the loss at the midpoint by either endpoint
        // tangent. The signed bracket encloses beta*, hence alpha*.
        if(std::isfinite(sl) && std::isfinite(sr) && sl>=0 && sr<=0 &&
           width/eta<=1e-9*(1+std::abs((l+width/2)/eta)) &&
           width*std::max(sl,-sr)/2<=5e-9*n) {certified=true;break;}
        const double mid=l+width/2;
        if(mid==l || mid==r) break;
        if(score(mid)>0) l=mid; else r=mid;
      }
      if(!certified) {
        bad.status=SnStatus::IterationFailure;
        const double witness=(l+(r-l)/2)/eta;
        const double w=std::isfinite(omega)?omega:unit/eta;
        double ll=0;
        if(w>0 && std::isfinite(w) && std::isfinite(witness))
          for(double v:data)ll+=distributions::dsn(v,xi,w,witness,true);
        else ll=nan;
        if(std::isfinite(witness) && std::isfinite(ll) && w>0 && std::isfinite(w)) {
          bad.stop_detail.candidates.push_back({{xi,w,witness},ll,ll,StopModel::None});
          bad.stop_detail.loglik_lower=ll;bad.stop_detail.loglik_upper=INFINITY;
        }
        return bad;
      }
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
      for(int k=0;k<maxit && r-l>1e-9;++k) {
        consider(std::sinh(u),fu); consider(std::sinh(v),fv);
        if(fu.ll<fv.ll) { l=u;u=v;fu=fv;v=l+ratio*(r-l);fv=fit(std::sinh(v)); }
        else { r=v;v=u;fv=fu;u=r-ratio*(r-l);fu=fit(std::sinh(u)); }
      }
      if(r-l>1e-9) {bad.status=SnStatus::IterationFailure;return bad;}
    }
    // The face is also a feasible member of the closure. Its likelihood is
    // an incumbent, not an upper bound on nearby finite shapes.
    double incumbent=best.ll;
    for(double sign:{-1.0,1.0}) {
      double c=sign>0 ? *std::min_element(x.begin(),x.end()) : *std::max_element(x.begin(),x.end());
      double square=0;for(double y:x) square+=(y-c)*(y-c);
      const double w=std::isfinite(fw) ? fw : std::sqrt(square/n);
      if(w>0) incumbent=std::max(incumbent,n*(.5*std::log(2/cppunivariate::pi)-std::log(w))-square/(2*w*w));
    }
    // Leave half the 1e-8 n allowance for floating-point error. A face can
    // subsequently win a tie within 1e-8 n; the resulting total allowance is
    // 1.5e-8 n, conditional on the computed comparisons.
    const double margin=5e-9*n;
    double cutoff=8;
    bool tails_closed=false;
    for(int k=0;k<maxit;++k) {
      if(sn_detail::shape_tail_upper(x,fw,cutoff,1,maxit)<=incumbent+margin &&
         sn_detail::shape_tail_upper(x,fw,cutoff,-1,maxit)<=incumbent+margin) {tails_closed=true;break;}
      cutoff*=2;
    }
    if(!tails_closed || !std::isfinite(cutoff)) {bad.status=SnStatus::IterationFailure;return bad;}
    struct Node {double a; sn_detail::Inner v; sn_detail::ShapeTangent tangent;};
    struct Interval {Node l,r;};
    std::vector<Node> nodes;
    for(std::size_t i=0;i<grid.size();++i) if(grid[i]>-cutoff && grid[i]<cutoff)
      nodes.push_back({grid[i],values[i],sn_detail::shape_tangent(x,grid[i],values[i])});
    for(double a:{-cutoff,cutoff}) {
      auto v=fit(a);consider(a,v);
      nodes.push_back({a,v,sn_detail::shape_tangent(x,a,v)});
    }
    std::sort(nodes.begin(),nodes.end(),[](const Node& a,const Node& b){return a.a<b.a;});
    std::vector<Interval> pending;
    for(std::size_t i=1;i<nodes.size();++i) pending.push_back({nodes[i-1],nodes[i]});
    const std::int64_t cap=work_budget== -2?certificate_budget(maxit):work_budget;
    std::int64_t budget=cap;
    while(!pending.empty()) {
      const Interval interval=pending.back();pending.pop_back();
      const auto bound=[&](const sn_detail::ShapeTangent& t) {
        return std::max(sn_detail::shape_upper(x,fw,interval.l.a,t),sn_detail::shape_upper(x,fw,interval.r.a,t));
      };
      const double upper=std::min(bound(interval.l.tangent),bound(interval.r.tangent));
      if(std::isfinite(upper) && upper<=std::max(incumbent,best.ll)+margin) continue;
      const double a=interval.l.a+(interval.r.a-interval.l.a)/2;
      if(--budget<0) {
        bad.status=SnStatus::IterationFailure;bad.budget_exhausted=true;
        bad.stop_detail.loglik_lower=special::kNegInf;bad.stop_detail.loglik_upper=INFINITY;
        const double c= center+unit*best.h/best.eta;
        const double w=std::isfinite(omega)?omega:unit/best.eta;
        double ll=0;
        if(std::isfinite(c) && w>0 && std::isfinite(w) && std::isfinite(besta))
          for(double v:data)ll+=distributions::dsn(v,c,w,besta,true);
        else ll=nan;
        if(std::isfinite(ll)) {
          bad.stop_detail.candidates.push_back({{c,w,besta},ll,ll,StopModel::None});
          bad.stop_detail.loglik_lower=ll;
        }
        for(double sign:{-1.,1.}) {
          const double endpoint=sign>0?low:high;
          const double normalized=sign>0?*std::min_element(x.begin(),x.end()):*std::max_element(x.begin(),x.end());
          double square=0;for(double z:x)square+=(z-normalized)*(z-normalized);
          const double scale_face=std::isfinite(omega)?omega:unit*std::sqrt(square/n);
          double face=0;
          if(scale_face>0 && std::isfinite(scale_face))
            for(double v:data)face+=std::log(2.)+special::dnorm(v,endpoint,scale_face,true);
          else face=nan;
          if(std::isfinite(face)) {
            bad.stop_detail.candidates.push_back({{endpoint,scale_face},face,face,sign>0?StopModel::Hnorm:StopModel::ReflectedHnorm});
            bad.stop_detail.loglik_lower=std::max(bad.stop_detail.loglik_lower,face);
          }
        }
        if(bad.stop_detail.candidates.empty()) {
          bad.budget_exhausted=false;bad.stop_detail.loglik_lower=nan;
        }
        return bad;
      }
      if(a==interval.l.a || a==interval.r.a) {bad.status=SnStatus::IterationFailure;return bad;}
      const auto v=fit(a);consider(a,v);
      const Node middle={a,v,sn_detail::shape_tangent(x,a,v)};
      pending.push_back({interval.l,middle});pending.push_back({middle,interval.r});
    }
  }
  // Check the actual returned coordinates, rather than only their
  // standardized representation (large held shapes amplify roundoff).
  double original_best_ll=nan;
  if(std::isfinite(best.ll)) {
    const double c=std::isfinite(xi)?xi:center+unit*best.h/best.eta;
    const double w=std::isfinite(omega)?omega:unit/best.eta;
    double actual=0;
    if(std::isfinite(c) && w>0 && std::isfinite(w) && std::isfinite(besta)) {
      for(double v:data) actual+=distributions::dsn(v,c,w,besta,true);
    } else actual=nan;
    original_best_ll=actual;
    if(!std::isfinite(actual) || std::abs(actual-(best.ll-n*std::log(unit)))>1e-9*n) {
      bad.status=SnStatus::IterationFailure;
      if(std::isfinite(actual)) {
        bad.stop_detail.candidates.push_back({{c,w,besta},actual,actual,StopModel::None});
        bad.stop_detail.loglik_lower=actual;bad.stop_detail.loglik_upper=INFINITY;
      }
      return bad;
    }
  }
  if(!std::isfinite(alpha) && !(constant && xi==low)) {
    SnResult face={nan,nan,nan,special::kNegInf,evaluations,SnStatus::IterationFailure};
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
      const double ll=n*(0.5*std::log(2/cppunivariate::pi)-std::log(w))-square/(2*w*w)-penalty;
      const double original_c=std::isfinite(xi)?xi:(sign>0?low:high),original_w=std::isfinite(omega)?omega:unit*w;
      double actual=0;
      for(double v:data) {
        actual+=std::log(2.0)+special::dnorm(v,original_c,original_w,true);
        if(std::isfinite(xi) && v==xi)actual-=std::log(2.0);
      }
      if(!(original_w>0) || !std::isfinite(original_w) || !std::isfinite(actual) ||
         !std::isfinite(ll) || std::abs(actual-(ll-n*std::log(unit)))>1e-9*n) {
        bad.status=SnStatus::IterationFailure;
        if(original_w>0 && std::isfinite(original_w) && std::isfinite(actual)) {
          bad.stop_detail.candidates.push_back({{original_c,original_w},actual,actual,
            sign>0?StopModel::Hnorm:StopModel::ReflectedHnorm});
          bad.stop_detail.loglik_lower=actual;bad.stop_detail.loglik_upper=INFINITY;
        }
        return bad;
      }
      if(ll>face.logLik+n*std::log(unit)) face={original_c,original_w,sign*std::numeric_limits<double>::infinity(),actual,evaluations,
        sign>0 ? SnStatus::RightHalfNormalBoundary : SnStatus::LeftHalfNormalBoundary};
    }
    if(face.status!=SnStatus::IterationFailure && face_numerical_tie(face.logLik+n*std::log(unit),best.ll,1e-8*n)) {
      const double ll=original_best_ll;bad.status=SnStatus::IterationFailure;
      bad.stop_detail=comparison_detail({{face.xi,face.omega},face.logLik,face.logLik,face.status==SnStatus::RightHalfNormalBoundary?StopModel::Hnorm:StopModel::ReflectedHnorm},{{center+unit*best.h/best.eta,unit/best.eta,besta},ll,ll,StopModel::None});return bad;
    }
    if(face.status!=SnStatus::IterationFailure && face.logLik+n*std::log(unit)>=best.ll-1e-8*n) {
      face.stop_detail=limit_detail({face.xi,face.omega},face.logLik,face.status==SnStatus::RightHalfNormalBoundary?StopModel::Hnorm:StopModel::ReflectedHnorm);return face;
    }
  }
  const double result_xi=std::isfinite(xi)?xi:center+unit*best.h/best.eta;
  const double result_omega=std::isfinite(omega)?omega:unit/best.eta;
  if(!std::isfinite(result_xi) || !(result_omega>0) || !std::isfinite(result_omega) ||
     !std::isfinite(besta) || !std::isfinite(best.ll-n*std::log(unit))) {
    bad.status=SnStatus::IterationFailure;return bad;
  }
  if(!best.converged) {
    bad.status=SnStatus::IterationFailure;
    const double ll=original_best_ll;
    bad.stop_detail.candidates.push_back({{result_xi,result_omega,besta},ll,ll,StopModel::None});
    bad.stop_detail.loglik_lower=ll;bad.stop_detail.loglik_upper=INFINITY;
    return bad;
  }
  return {std::isfinite(xi) ? xi : center+unit*best.h/best.eta,
    std::isfinite(omega) ? omega : unit/best.eta,besta,original_best_ll,evaluations,
    best.converged ? SnStatus::Ok : SnStatus::IterationFailure};
}
}
