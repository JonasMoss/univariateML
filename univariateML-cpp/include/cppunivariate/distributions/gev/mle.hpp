#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/likelihood_polish.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/span.hpp"
#include "../../special/tails.hpp"
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <utility>
#include <vector>

namespace cppunivariate {
inline constexpr double kGevXiLower = -1.0;
inline constexpr double kGevXiUpper = 1.0;

enum class GevStatus { Ok, InvalidData, InvalidShape, IterationFailure, NoMLE, NumericalFailure, SearchContradicted };
struct GevResult {
  double mu, sigma, xi, logLik;
  int iterations;
  GevStatus status; unsigned restricted = 0; StopDetail stop_detail{}; bool certified = false;
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
// Compare the closed endpoint before division: a negative residual can
// underflow to negative zero. TwoSum recovers the rounding error of mu+sigma.
inline bool gev_public_contains(double x,double mu,double sigma,double xi) {
  if(xi!= -1)return special::support_residual(x,mu,sigma,xi)>0;
  const double endpoint=mu+sigma;
  if(x<endpoint)return true;
  if(x>endpoint)return false;
  const double b=endpoint-mu;
  const double error=(mu-(endpoint-b))+(sigma-b);
  return error>=0;
}
inline double gev_public_loglik(span x, double mu, double sigma, double xi) {
  if (!(sigma>0)) return -std::numeric_limits<double>::infinity();
  double ll=-static_cast<double>(x.size())*std::log(sigma);
  for (double v:x) {
    const double z=special::scaled_difference(v,mu,sigma);
    const double residual=special::support_residual(v,mu,sigma,xi);
    if (xi == -1) {
      if (!gev_public_contains(v,mu,sigma,xi)) return -std::numeric_limits<double>::infinity();
      ll += z-1;
      continue;
    }
    if (!(residual>0)) return -std::numeric_limits<double>::infinity();
    const double L=xi==0 ? z : (std::abs(xi*z)<0.5 ? gev_log_ratio(z,xi) : std::log(residual)/xi);
    ll-=(1+xi)*L+std::exp(-L);
  }
  return ll;
}
// Repeat a fallback rejection after the witnesses have been restored to the
// caller's public coordinates. A comparison lost on restoration is unresolved.
inline void gev_verify_contradiction(span x,GevResult& r) {
  if(r.status!=GevStatus::SearchContradicted || r.stop_detail.candidates.empty())return;
  const auto search=r.stop_detail.candidates.back();
  const double value=gev_public_loglik(x,search.coef[0],search.coef[1],search.coef[2]);
  bool strict=false;double lower=-INFINITY;
  std::vector<FitCandidate> feasible;
  for(auto c:r.stop_detail.candidates) {
    const double actual=gev_public_loglik(x,c.coef[0],c.coef[1],c.coef[2]);
    if(!std::isfinite(actual))continue;
    const double margin=4096*std::numeric_limits<double>::epsilon()*
      (x.size()+std::abs(actual)+(std::isfinite(value)?std::abs(value):0));
    c.loglik_lower=actual-margin;c.loglik_upper=actual+margin;
    feasible.push_back(c);lower=std::max(lower,c.loglik_lower);
    if(std::isfinite(value)&&actual>value+margin)strict=true;
  }
  if(strict||!std::isfinite(value)) {
    r.stop_detail.candidates=std::move(feasible);
    r.stop_detail.loglik_lower=lower;r.stop_detail.loglik_upper=INFINITY;
    if(!std::isfinite(value))r.status=GevStatus::NumericalFailure;
    return;
  }
  r.mu=search.coef[0];r.sigma=search.coef[1];r.xi=search.coef[2];r.logLik=value;
  r.status=GevStatus::Ok;r.certified=false;r.stop_detail={};
}
// Public support uses compensated residuals on the original observations.
// Only free coefficients can move; each repair is followed by the same check.
inline bool gev_restore_support(span x,GevResult& r,bool held_mu,bool held_sigma,
                                double tol,int maxit) {
  const double initial_mu=r.mu,initial_sigma=r.sigma;
  const auto inside=[&](double v,double location,double scale) {
    return gev_public_contains(v,location,scale,r.xi);
  };
  for(int it=0;it<solver_budget(maxit);++it) {
    bool contained=true;
    for(double v:x) {
      const double t=special::support_residual(v,r.mu,r.sigma,r.xi);
      if(inside(v,r.mu,r.sigma))continue;
      contained=false;
      if(!std::isfinite(t))return false;
      bool repaired=false;
      if(!held_sigma) {
        double scale=r.sigma+std::max(0.,-t)*r.sigma;
        if(std::isfinite(scale) && !inside(v,r.mu,scale))
          scale=std::nextafter(scale,INFINITY);
        if(std::isfinite(scale) && std::abs(scale-initial_sigma)<=tol*initial_sigma) {
          r.sigma=scale;repaired=true;
        }
      }
      // A location ulp can be harmless after translation while the same
      // absolute change would spoil a much smaller, accurately fitted scale.
      if(!repaired && !held_mu && r.xi!=0) {
        const double direction=r.xi>0 ? -INFINITY : INFINITY;
        double location=std::fma(t,r.sigma/r.xi,r.mu);
        if(std::isfinite(location) && !inside(v,location,r.sigma))
          location=std::nextafter(location,direction);
        if(std::isfinite(location) && std::abs(location-initial_mu)<=
            tol*std::max(std::abs(initial_mu),initial_sigma)) {
          r.mu=location;repaired=true;
        }
      }
      if(!repaired)return false;
    }
    if(!(std::isfinite(r.mu)&&std::isfinite(r.sigma)&&r.sigma>0) ||
       std::abs(r.mu-initial_mu)>tol*std::max(std::abs(initial_mu),initial_sigma) ||
       std::abs(r.sigma-initial_sigma)>tol*initial_sigma)return false;
    if(contained)return true;
  }
  return false;
}
} // namespace cppunivariate
#include "shape_certificate.hpp"
namespace cppunivariate {
// Hold-specific scalar search. Enumerate sampled local maxima and refine
// each bracket; this is numerical root discovery, not a global certificate.
namespace extreme_detail {
struct Maximum { double x, value; int iterations; bool ok; };
template<class F>
inline Maximum maximize(F f, double lo, double hi, double tol, int maxit,
                        bool closed=true) {
  maxit = static_cast<int>(solver_budget(maxit));
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
// Verify a public held scale is the exact rational count multiple of the
// exact data range, rather than merely the rounded unconstrained coefficient.
inline bool exact_count_scale(double minimum,double maximum,std::size_t numerator,
                              std::size_t denominator,double scale) {
  if(static_cast<std::uint64_t>(numerator)>9007199254740992ULL ||
     static_cast<std::uint64_t>(denominator)>9007199254740992ULL)return false;
  const double range=maximum-minimum,virtual_b=maximum-range;
  const double error=(maximum-(range+virtual_b))+(virtual_b-minimum);
  const double a=static_cast<double>(numerator),b=static_cast<double>(denominator);
  const double product=a*range;
  return std::isnormal(range) && std::isfinite(error) && error==0 &&
    std::isfinite(product) && std::isfinite(scale) && scale>0 &&
    std::fma(a,range,-product)==0 && std::fma(b,scale,-product)==0;
}
} // namespace extreme_detail

// Fixed location and/or scale, standardized by the caller.
inline GevResult gev_held_profile(span x,double mu,double sigma,double xi,
                                  double tol,int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
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
  if(hs && xi > -1 && xi < 0) {
    // Strict concavity and individual score crossings bracket every maximum.
    const double shift=-sigma*std::expm1(-xi*std::log1p(xi))/xi;
    double lo=std::max(x[0]+shift,x[x.size()-1]+sigma/xi);
    double hi=x[x.size()-1]+shift;
    auto score=[&](double m) {
      double value=0;
      for(double v:x) {
        const double z=(v-m)/sigma,t=1+xi*z;
        if(!(t>0))return std::numeric_limits<double>::infinity();
        const double L=gev_log_ratio(z,xi);
        value+=(xi-std::expm1(-L))/t;
      }
      return value;
    };
    GevResult result{mu,sigma,xi,NAN,0,GevStatus::IterationFailure};
    if(lo==hi && std::abs(score(lo))<1e-3) {
      result.mu=lo;result.logLik=gev_loglik(x,lo,sigma,xi);
      result.status=std::isfinite(result.logLik) ? GevStatus::Ok : GevStatus::NumericalFailure;
      return result;
    }
    for(int j=0;j<maxit;++j) {
      const double mid=lo+(hi-lo)/2;
      if(mid==lo || mid==hi) {
        result.status=GevStatus::NumericalFailure;return result;
      }
      if(score(mid)>0)lo=mid;else hi=mid;
      if(hi-lo<=tol*sigma && std::abs(score(lo+(hi-lo)/2))<1e-3) {
        result.mu=lo+(hi-lo)/2;result.iterations=j+1;
        result.logLik=gev_loglik(x,result.mu,sigma,xi);
        result.status=std::isfinite(result.logLik) && std::isfinite(score(result.mu)) ? GevStatus::Ok : GevStatus::NumericalFailure;
        return result;
      }
    }
    result.mu=lo+(hi-lo)/2;result.iterations=maxit;
    result.logLik=gev_loglik(x,result.mu,sigma,xi);
    result.stop_detail.loglik_upper=INFINITY;
    if(std::isfinite(result.logLik)) {
      result.stop_detail.loglik_lower=result.logLik;
      result.stop_detail.candidates.push_back({{result.mu,sigma,xi},result.logLik,result.logLik});
    }
    return result;
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
#include "accuracy.hpp"
inline GevResult gev_two_point_certificate(span x,double minimum,double maximum,
                                           std::size_t k,double tol);
inline GevResult gev_held_certificate(span data,double mu,double sigma,double tol,int maxit,GevResult fallback,
                                      double public_origin=0,double public_unit=1);
inline GevResult gev_held_mle(span x,double tol,int maxit,double xi,double mu,double sigma) {
  maxit = static_cast<int>(solver_budget(maxit));
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
  // A fully held member needs only a raw public-coordinate evaluation.
  // Standardization near a support endpoint can destroy its small residual.
  if(hm && hs && !std::isnan(xi)) {
    const double ll=gev_public_loglik(x,mu,sigma,xi);
    GevResult member{mu,sigma,xi,ll,0,GevStatus::Ok};
    if(!gev_restore_support(x,member,true,true,tol,maxit))member.status=GevStatus::InvalidData;
    else if(!std::isfinite(ll))member.status=GevStatus::NumericalFailure;
    return member;
  }
  // A hold containing the unique unconstrained member preserves its global
  // certificate. Require exact public-coordinate equality, never a near hold.
  if(std::isnan(xi) && y.size()>=3 && y.front()<y.back()) {
    const auto k=static_cast<std::size_t>(std::count(y.begin(),y.end(),y.front()));
    bool two_point=2*k<=y.size();
    for(double v:y)two_point=two_point && (v==y.front() || v==y.back());
    if(two_point) {
      auto member=gev_two_point_certificate(x,y.front(),y.back(),k,tol);
      const double location=y.back()-member.sigma;
      const double virtual_b=y.back()-location;
      const double error=(y.back()-(location+virtual_b))+(virtual_b-member.sigma);
      if(extreme_detail::exact_count_scale(y.front(),y.back(),k,y.size(),member.sigma) &&
         (!hm || (mu==member.mu && member.mu==location && error==0)) &&
         (!hs || sigma==member.sigma)) {return member;}
    }
  }
  if(std::isnan(xi) && hm && mu==y[0]) {
    const auto k=static_cast<std::size_t>(std::count(y.begin(),y.end(),y[0]));
    if(2*k==y.size()) {
      bool two_point=true;
      for(std::size_t j=k;j<y.size();++j)two_point=two_point && y[j]==y.back();
      double face=-static_cast<double>(k);
      for(double v:y)if(v>mu) {
        const double gap=v-mu;
        face-=2*(std::isfinite(gap)?std::log(gap):std::log(v/2-mu/2)+std::log(2.));
      }
      const double endpoint_scale=y.back()-mu;
      if(two_point && (!hs || sigma==endpoint_scale)) {
        const double ll=gev_loglik(x,mu,endpoint_scale,-1);
        GevResult result{mu,endpoint_scale,-1,ll,0,GevStatus::NumericalFailure};
        if(endpoint_scale>0 && std::isfinite(endpoint_scale) && std::isfinite(ll) &&
           std::abs(ll-face)<=1e-7*x.size()) {result.status=GevStatus::Ok;result.certified=true;}
        return result;
      }
      if(!hs) {
        bad.status=GevStatus::NoMLE;
        bad.stop_detail.loglik_lower=face;bad.stop_detail.loglik_upper=face;
        return bad;
      }
    }
  }
  if(std::isnan(xi) && hm && hs)return gev_shape_certificate(x,mu,sigma,tol,maxit);
  if(!hs && hm && mu==y[0]) {
    std::size_t k=0;for(double v:y) if(v==y[0]) ++k;
    if(k==y.size() || (std::isnan(xi) ? 2*k>y.size() : (xi>0 && xi*k>=y.size()-k))) {
      bad.status=GevStatus::NoMLE;return bad;
    }
  }
  const double origin=y[0],public_maximum=y.back();
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
  if(fit.status==GevStatus::Ok&&std::isnan(xi)&&hm!=hs &&
     !gev_accuracy_detail::polish(data,fit,hm,hs,tol,maxit))fit.status=GevStatus::NumericalFailure;
  const auto standardized_search=fit;
  std::vector<FitCandidate> restored;
  bool lost_witness=false;
  for(auto c:fit.stop_detail.candidates) {
    c.coef[0]=hm ? mu : origin+scale*c.coef[0];c.coef[1]=hs ? sigma : scale*c.coef[1];
    c.loglik_lower-=x.size()*std::log(scale);c.loglik_upper-=x.size()*std::log(scale);
    const double evaluated=gev_public_loglik(x,c.coef[0],c.coef[1],c.coef[2]);
    if(!std::isfinite(c.coef[0]) || !std::isfinite(c.coef[1]) || !std::isfinite(evaluated)) {
      lost_witness=true;continue;
    }
    c.loglik_lower=std::min(c.loglik_lower,evaluated);
    c.loglik_upper=std::max(c.loglik_upper,evaluated);
    restored.push_back(c);
  }
  fit.stop_detail.candidates=std::move(restored);
  fit.stop_detail.loglik_lower-=x.size()*std::log(scale);
  fit.stop_detail.loglik_upper-=x.size()*std::log(scale);
  if(!fit.stop_detail.candidates.empty()) {
    fit.stop_detail.loglik_lower=-INFINITY;
    for(const auto& c:fit.stop_detail.candidates)
      fit.stop_detail.loglik_lower=std::max(fit.stop_detail.loglik_lower,c.loglik_lower);
  } else if(lost_witness)fit.stop_detail.loglik_lower=NAN;
  if(lost_witness)fit.status=GevStatus::NumericalFailure;
  const double standardized_ll=fit.logLik;
  fit.mu=hm ? mu : origin+scale*fit.mu;
  fit.sigma=hs ? sigma : scale*fit.sigma;
  if(fit.status==GevStatus::Ok && !gev_restore_support(x,fit,hm,hs,tol,maxit))
    fit.status=GevStatus::NumericalFailure;
  fit.logLik=gev_public_loglik(x,fit.mu,fit.sigma,fit.xi);
  const double predicted=standardized_ll-x.size()*std::log(scale);
  const double likelihood_margin=std::max(1e-7*x.size(),256*std::numeric_limits<double>::epsilon()*
      (x.size()+std::abs(predicted)+std::abs(fit.logLik)));
  if(fit.status==GevStatus::Ok &&
     (!std::isfinite(fit.logLik) || std::abs(fit.logLik-predicted)>likelihood_margin))
    fit.status=GevStatus::NumericalFailure;
  if(fit.status==GevStatus::Ok && std::isnan(xi) && hm!=hs &&
     ((!hm&&!accuracy_detail::location_projection(fit.mu,origin,scale,standardized_search.mu,fit.sigma,tol)) ||
      (!hs&&!accuracy_detail::scale_projection(fit.sigma,scale,std::log(standardized_search.sigma),tol))))
    fit.status=GevStatus::NumericalFailure;
  if(fit.status==GevStatus::Ok && std::isnan(xi) && hm!=hs) {
    auto candidate=gev_held_certificate(data,m,s,tol,maxit,standardized_search,origin,scale);
    if(candidate.status==GevStatus::SearchContradicted) {
      candidate.mu=hm?mu:origin+scale*candidate.mu;
      candidate.sigma=hs?sigma:scale*candidate.sigma;
      if(gev_restore_support(x,candidate,hm,hs,tol*.5,maxit)) {
        candidate.logLik=gev_public_loglik(x,candidate.mu,candidate.sigma,candidate.xi);
        const double error=4096*std::numeric_limits<double>::epsilon()*
          (x.size()+std::abs(candidate.logLik)+std::abs(fit.logLik));
        if(std::isfinite(candidate.logLik)&&candidate.logLik>fit.logLik+2*error) {
          candidate.stop_detail={};candidate.stop_detail.loglik_lower=candidate.logLik-error;
          candidate.stop_detail.loglik_upper=INFINITY;
          candidate.stop_detail.candidates.push_back({{fit.mu,fit.sigma,fit.xi},fit.logLik-error,fit.logLik+error});
          candidate.stop_detail.candidates.push_back({{candidate.mu,candidate.sigma,candidate.xi},candidate.logLik-error,candidate.logLik+error});
          return candidate;
        }
      }
    }
    if(candidate.certified) {
      const double certificate_tol=std::min(tol,.01);
      const double expected=candidate.logLik-x.size()*std::log(scale);
      const double location_error=256*std::numeric_limits<double>::epsilon()*
        (std::abs(origin)+scale*std::abs(candidate.mu));
      candidate.mu=hm?mu:origin+scale*candidate.mu;
      candidate.sigma=hs?sigma:scale*candidate.sigma;
      // Preserve the coupled corner in original coordinates rather than
      // subtracting two independently normalized extreme/hold values.
      if(candidate.xi== -1&&standardized_search.xi== -1) {
        if(hm&&standardized_search.sigma==data[data.size()-1]-m)
          candidate.sigma=public_maximum-mu;
        else if(hs&&standardized_search.mu==data[data.size()-1]-s)
          candidate.mu=public_maximum-sigma;
      }
      const bool precise=(hm?std::isnormal(candidate.sigma):
        (std::isnormal(std::max(std::abs(candidate.mu),candidate.sigma))&&
         location_error<=certificate_tol*.25*std::max(std::abs(candidate.mu),candidate.sigma)));
      if(precise && gev_restore_support(x,candidate,hm,hs,certificate_tol*.5,maxit)) {
        candidate.logLik=gev_public_loglik(x,candidate.mu,candidate.sigma,candidate.xi);
        if(std::isfinite(candidate.logLik)&&std::abs(candidate.logLik-expected)<=likelihood_margin)
          fit=candidate;
      }
    }
  }
  return fit;
}
struct GevProfile { GevResult fit; double score, curvature; };
// At pivot x0 (minimum for xi>=0, maximum otherwise), write
// L_i=log1p(xi*(x_i-x0)/s0)/xi. The location/scale orbit has
// g=log(n/sum(exp(-L_i))), sigma=s0*exp(xi*g),
// mu=x0+s0*expm1(xi*g)/xi. Only u=log(s0) remains to solve.
// Internal helper: x must be sorted, finite and nonconstant.
inline GevProfile gev_profile(span x, double xi, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
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
// Weighted pair bounds prove this global member for two distinct values,
// with minimum fraction p<=1/2. Counts avoid cancellation in M-mean(x).
inline GevResult gev_two_point_certificate(span x,double minimum,double maximum,
                                           std::size_t k,double tol) {
  const double n=static_cast<double>(x.size()),p=static_cast<double>(k)/n;
  const double range=maximum-minimum;
  const double half=std::isfinite(range)?range/2:maximum/2-minimum/2;
  const double log_range=std::isfinite(range)?std::log(range):std::log(half)+std::log(2.);
  const double face=-n*(std::log(p)+log_range+1);
  double scale=std::isfinite(range)?p*range:(2*p)*half;
  double location=maximum-scale;
  const double precision_tol=std::min(tol,.01);
  const double margin=64*std::numeric_limits<double>::epsilon();
  bool precise=precision_tol>=16*margin && scale>0 && std::isfinite(scale);
  // Error-free TwoDiff, conditional on finite IEEE round-to-nearest operations.
  auto difference_error=[](double a,double b,double value) {
    const double virtual_b=a-value,virtual_a=value+virtual_b;
    return (a-virtual_a)+(virtual_b-b);
  };
  if(2*k==x.size()) {
    if(std::isfinite(range))
      precise=precise && std::abs(std::fma(2.,scale,-range))<=precision_tol*.25*range;
    if(std::signbit(minimum)!=std::signbit(maximum)) {
      const double sum=minimum+maximum;
      location=sum/2;
      precise=precise && std::abs(std::fma(2.,location,-sum))<=precision_tol*.25*std::abs(sum);
    }
    // Preserve the accurate midpoint while making the stored endpoint feasible.
    const double gap=maximum-location,error=difference_error(maximum,location,gap);
    const double upper_gap=error>0?std::nextafter(gap,INFINITY):gap;
    const double adjusted=std::max(scale,upper_gap);
    precise=precise && std::isfinite(error) && std::isfinite(adjusted) &&
      adjusted-scale<=precision_tol*.25*scale;
    scale=adjusted;
  } else {
    // Round location upwards if necessary so mu+sigma contains the maximum.
    const double error=difference_error(maximum,scale,location);
    if(error>0)location=std::nextafter(location,INFINITY);
    const double uncertainty=margin*std::abs(maximum)+margin*scale;
    precise=precise && std::isfinite(error) && std::isnormal(scale) && location!=0 &&
      uncertainty<=precision_tol*.25*std::abs(location);
  }
  const double ll=gev_loglik(x,location,scale,-1);
  GevResult result{location,scale,-1,ll,0,GevStatus::NumericalFailure};
  result.stop_detail.loglik_lower=face;result.stop_detail.loglik_upper=face;
  if(precise && std::isfinite(location) && std::isfinite(ll) &&
     std::abs(ll-face)<=1e-7*n) {result.status=GevStatus::Ok;result.certified=true;result.stop_detail={};}
  else if(2*k!=x.size() && std::isnormal(scale) && precision_tol>=16*margin &&
          std::isfinite(location) && std::isfinite(ll) && std::abs(ll-face)<=1e-7*n &&
          margin*(std::abs(maximum)+scale)<=precision_tol*.25*std::max(std::abs(location),scale)) {
    // Cancellation near zero defeats the relative-location inclusion, but
    // does not invalidate the finite analytic search result.
    result.status=GevStatus::Ok;result.certified=false;result.stop_detail={};
  }
  return result;
}
#include "joint_certificate.hpp"
#include "held_certificate.hpp"
inline GevResult gev_mle_impl(span x, double tol=1e-10, int maxit=100,
                         double xi=std::numeric_limits<double>::quiet_NaN(),
                         double mu=std::numeric_limits<double>::quiet_NaN(),
                         double sigma=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
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
  // A strict minimum majority diverges. Exact half ties are resolved by
  // the global pair bound below, including its attained two-point exception.
  const bool tie_half = std::isnan(xi) && 2*minimum_count==y.size();
  if(std::isnan(xi) ? 2*minimum_count>y.size() :
      (xi>0 && xi*static_cast<double>(minimum_count)>=y.size()-minimum_count)) return bad;
  // Standardization prevents scale and translation from setting tolerances.
  const double origin=y[0],range=y.back()-origin;
  if(std::isnan(xi)) {
    bool two_point=true;
    for(std::size_t j=minimum_count;j<y.size();++j)if(y[j]!=y.back()) {two_point=false;break;}
    if(two_point)return gev_two_point_certificate(x,origin,y.back(),minimum_count,tol);
  }
  if(tie_half) {
    const double n=static_cast<double>(y.size());
    double face=n*std::log(2.)-n;
    for(double v:y)if(v>origin) {
      const double gap=v-origin;
      face-=2*(std::isfinite(gap)?std::log(gap):std::log(v/2-origin/2)+std::log(2.));
    }
    bad.status=GevStatus::NoMLE;
    bad.stop_detail.loglik_lower=face;bad.stop_detail.loglik_upper=face;
    return bad;
  }
  if(!(range>0) || !std::isfinite(range)) return bad;
  for(double &v:y) v=(v-origin)/range;
  const span data{y.data(),y.size()};
  auto restore=[&](GevResult r) {
    const double standardized_mu=r.status==GevStatus::SearchContradicted&&!r.stop_detail.candidates.empty()?r.stop_detail.candidates.back().coef[0]:r.mu;
    const double standardized_sigma=r.status==GevStatus::SearchContradicted&&!r.stop_detail.candidates.empty()?r.stop_detail.candidates.back().coef[1]:r.sigma;
    r.mu=origin+range*r.mu;r.sigma*=range;
    const double jac=y.size()*std::log(range);
    r.logLik-=jac;
    if(!std::isnan(r.stop_detail.loglik_lower))r.stop_detail.loglik_lower-=jac;
    if(!std::isnan(r.stop_detail.loglik_upper))r.stop_detail.loglik_upper-=jac;
    if(!std::isnan(r.stop_detail.segment_loglik_upper))r.stop_detail.segment_loglik_upper-=jac;
    for(auto& c:r.stop_detail.candidates) {
      c.coef[0]=origin+range*c.coef[0];c.coef[1]*=range;
      c.loglik_lower-=jac;c.loglik_upper-=jac;
    }
    gev_verify_contradiction(x,r);
    if(r.status==GevStatus::Ok) {
      if(!gev_restore_support(x,r,false,false,tol,maxit))r.status=GevStatus::NumericalFailure;
      const double eps=32*std::numeric_limits<double>::epsilon();
      const double error=eps*std::abs(origin)+(eps*range)*std::abs(standardized_mu);
      const double actual=gev_public_loglik(x,r.mu,r.sigma,r.xi);
      const double likelihood_margin=std::max(1e-7*y.size(),256*std::numeric_limits<double>::epsilon()*
          (y.size()+std::abs(actual)+std::abs(r.logLik)));
      if(!std::isfinite(actual) ||
          !accuracy_detail::location_projection(r.mu,origin,range,standardized_mu,r.sigma,tol) ||
          !accuracy_detail::scale_projection(r.sigma,range,std::log(standardized_sigma),tol) ||
          error>tol*.25*std::max(std::abs(r.mu),r.sigma) || eps>tol*.25 ||
          std::abs(actual-r.logLik)>likelihood_margin) {
        r.status=GevStatus::NumericalFailure;r.certified=false;
        r.stop_detail.loglik_upper=INFINITY;
        if(std::isfinite(actual)) {
          r.stop_detail.loglik_lower=actual;
          r.stop_detail.candidates.push_back({{r.mu,r.sigma,r.xi},actual,actual});
        }
      }
      r.logLik=actual;
    }
    return r;
  };
  if(!std::isnan(xi)) return restore(gev_profile(data,xi,tol,maxit).fit);
  return restore(gev_joint_certificate(data,tol,maxit,false,tol));
}

inline GevResult gev_mle(span x,double tol=1e-10,int maxit=100,double xi=std::numeric_limits<double>::quiet_NaN(),double mu=std::numeric_limits<double>::quiet_NaN(),double sigma=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto r=gev_mle_impl(x,tol,maxit,xi,mu,sigma);
  if(r.status==GevStatus::Ok) {
    if(!gev_restore_support(x,r,!std::isnan(mu),!std::isnan(sigma),tol,maxit))
      r.status=GevStatus::NumericalFailure;
    const double actual=gev_public_loglik(x,r.mu,r.sigma,r.xi);
    if(!std::isfinite(actual))r.status=GevStatus::NumericalFailure;
    r.logLik=actual;
    // Witness bounds must describe the represented public coefficients too.
    const double margin=256*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(actual));
    if(std::isfinite(r.stop_detail.loglik_lower))
      r.stop_detail.loglik_lower=std::min(r.stop_detail.loglik_lower,actual-margin);
    if(std::isfinite(r.stop_detail.loglik_upper))
      r.stop_detail.loglik_upper=std::max(r.stop_detail.loglik_upper,actual+margin);
    std::vector<FitCandidate> public_candidates;
    for(auto candidate:r.stop_detail.candidates) {
      const double value=gev_public_loglik(x,candidate.coef[0],candidate.coef[1],candidate.coef[2]);
      if(!std::isfinite(value))continue;
      const double m=256*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(value));
      candidate.loglik_lower=value-m;candidate.loglik_upper=value+m;
      public_candidates.push_back(candidate);
    }
    r.stop_detail.candidates=std::move(public_candidates);
  }
  if(r.status==GevStatus::Ok && (std::isnan(xi)) && r.xi==kGevXiLower) r.restricted|=16u;
  if(r.status==GevStatus::Ok && (std::isnan(xi)) && r.xi==kGevXiUpper) r.restricted|=32u;
  return r;
}
} // namespace cppunivariate
