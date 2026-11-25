#pragma once
#include "../../core/outcome.hpp"
#include "../../core/span.hpp"
#include "../../core/likelihood_polish.hpp"
#include "../invweibull/mle.hpp"
#include "../invweibull/dpqr.hpp"
#include "dpqr.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <utility>
#include <vector>

namespace cppunivariate {
enum class ExpweibullStatus { Ok, InvalidData, InvalidFixed, Unbounded,
  PowerBoundary, PowerBoundaryTied, IterationFailure, AccuracyFailure, InvweibullBoundary, ExponentOverflow };
struct ExpweibullResult {
  double scale=NAN,shape=NAN,exponent=NAN,logLik=NAN;
  int iterations=0;
  ExpweibullStatus status=ExpweibullStatus::InvalidData;
  bool certified=false;
  StopDetail stop_detail{}; bool singleton_proved = false;
};
namespace expweibull_fit_detail {
inline double public_loglik(span x,double a,double b,double c) {
  accuracy_detail::ScalarSum total;
  for(double v:x)total.add(distributions::dexpweibull(v,a,b,c,true));
  return static_cast<double>(total.value);
}
struct WeibullScaleEnclosure {
  double value=NAN;
  ExtendedReal log_error=INFINITY;
  bool valid=false,parent_unsafe=false;
};
// For c=1 and held b, log(a_hat)=log(mean(exp(b*y)))/b.
// The second derivative of this log MGF is a weighted variance <= D^2/4,
// so 0 <= log(a_hat)-mean(y) <= b*D^2/8. This avoids dividing a rounded
// subnormal expm1 reduction by b. The arithmetic allowance is practical,
// not outward-rounded interval arithmetic.
inline WeibullScaleEnclosure weibull_scale_enclosure(span x,double b,double tol) {
  WeibullScaleEnclosure out;
  if(x.empty() || !(b>0) || !(tol>0))return out;
  const ExtendedReal eps=std::numeric_limits<ExtendedReal>::epsilon();
  out.parent_unsafe=32*std::numeric_limits<ExtendedReal>::denorm_min()/b>
    static_cast<ExtendedReal>(tol)/16;
  const double anchor=x[x.size()/2];
  const ExtendedReal la=std::log(static_cast<ExtendedReal>(anchor));
  accuracy_detail::ScalarSum total;
  ExtendedReal lo=0,hi=0;
  for(double v:x) {
    const ExtendedReal d=(static_cast<ExtendedReal>(v)-anchor)/anchor;
    const ExtendedReal z=std::abs(d)<.5?std::log1p(d):std::log(static_cast<ExtendedReal>(v))-la;
    if(!std::isfinite(z))return out;
    total.add(z);lo=std::min(lo,z);hi=std::max(hi,z);
  }
  const ExtendedReal n=x.size(),mean=total.value/n;
  const ExtendedReal round=64*eps*(1+n*eps)*(1+std::abs(la)+std::abs(mean)+hi-lo);
  const ExtendedReal width=hi-lo+2*round;
  const ExtendedReal bound=static_cast<ExtendedReal>(b)*width*width/8;
  const ExtendedReal ideal=la+mean;
  const double factor=std::exp(static_cast<double>(mean));
  out.value=anchor*factor;
  if(!(out.value>0) || !std::isfinite(out.value))out.value=std::exp(static_cast<double>(ideal));
  if(!(out.value>0) || !std::isfinite(out.value))return out;
  const ExtendedReal projection=std::abs(std::log(static_cast<ExtendedReal>(out.value))-ideal);
  out.log_error=bound+round+projection;
  const ExtendedReal eta=static_cast<ExtendedReal>(b)*out.log_error;
  // For |delta|<=log_error, the likelihood loss is at most
  // n*(exp(b*|delta|)-1-b*|delta|) <= n*eta^2*exp(eta)/2.
  out.valid=out.log_error<.1 && std::expm1(out.log_error)<=static_cast<ExtendedReal>(tol)*.8 &&
    eta<.1 && n*eta*eta*std::exp(eta)/2<=1e-12;
  return out;
}
// h(exp(z)), retaining both very small hazards and their derivatives.
inline accuracy_detail::Jet logq(const accuracy_detail::Jet& z) {
  const auto t=accuracy_detail::exp(z);
  if(z.v < -18) return z-t/accuracy_detail::Jet(2)+t*t/accuracy_detail::Jet(24)-t*t*t*t/accuracy_detail::Jet(2880);
  if(t.v < .6931471805599453) return accuracy_detail::log(-accuracy_detail::expm1(-t));
  // Near the inverse-Weibull path, 1-exp(-t) rounds to one before h(t) does.
  return accuracy_detail::log1p(-accuracy_detail::exp(-t));
}
struct Likelihood {
  span y;
  double unit;
  bool profile;
  accuracy_detail::Jet operator()(const std::array<accuracy_detail::Jet,3>& p) const {
    const auto b=accuracy_detail::exp(p[1]);
    accuracy_detail::JetSum hs,rest;
    for(double v:y) {
      const auto z=b*(accuracy_detail::Jet(v)-p[0]);
      const auto h=logq(z);
      hs.add(h); rest.add(z-h-accuracy_detail::exp(z));
    }
    const auto h=hs.value/accuracy_detail::Jet(y.size());
    const auto c=profile ? -accuracy_detail::Jet(1)/h : accuracy_detail::exp(p[2]);
    return p[1]-accuracy_detail::Jet(std::log(unit))+accuracy_detail::log(c)+
      rest.value/accuracy_detail::Jet(y.size())+c*h;
  }
};
// Damped Newton search with positive-definite modified information. Only
// the independent undamped polish below can accept a candidate.
inline bool search(std::array<double,3>& p,const std::array<bool,3>& free,
                   const Likelihood& objective,int maxit,int& iterations) {
  std::array<int,3> ix{};int n=0;for(int i=0;i<3;++i)if(free[i])ix[n++]=i;
  for(std::int64_t it=0;it<solver_budget(maxit);++it) {
    ++iterations;
    std::array<accuracy_detail::Jet,3> v;
    for(int i=0;i<3;++i)v[i]=accuracy_detail::variable(p[i],i,free[i]);
    const auto f=objective(v);if(!std::isfinite(f.v))return false;
    std::array<ExtendedReal,3> step{},forward{};
    std::array<std::array<ExtendedReal,3>,3> L{};
    bool positive=false; double ridge=0;
    for(std::int64_t k=0;k<solver_budget(maxit);++k) {
      positive=true;L={};
      for(int i=0;i<n && positive;++i)for(int j=0;j<=i;++j) {
        ExtendedReal a=-f.h[ix[i]][ix[j]]+(i==j?ridge:0);
        for(int l=0;l<j;++l)a-=L[i][l]*L[j][l];
        if(i==j){if(!(a>0)||!std::isfinite(a)){positive=false;break;}L[i][j]=std::sqrt(a);}
        else L[i][j]=a/L[j][j];
      }
      if(positive)break;
      ridge=ridge==0?1e-4:ridge*10;
    }
    if(!positive)return false;
    for(int i=0;i<n;++i){ExtendedReal a=f.g[ix[i]];for(int j=0;j<i;++j)a-=L[i][j]*forward[j];forward[i]=a/L[i][i];}
    double norm=0;
    for(int i=n-1;i>=0;--i){ExtendedReal a=forward[i];for(int j=i+1;j<n;++j)a-=L[j][i]*step[j];step[i]=a/L[i][i];norm=std::max(norm,static_cast<double>(std::abs(step[i])));}
    if(!std::isfinite(norm))return false;
    if(ridge==0 && norm<1e-5)return true;
    double fraction=norm>1?1/norm:1;
    bool accepted=false;
    for(std::int64_t k=0;k<solver_budget(maxit);++k) {
      auto q=p;for(int i=0;i<n;++i)q[ix[i]]+=fraction*static_cast<double>(step[i]);
      for(int i=0;i<3;++i)v[i]=accuracy_detail::Jet(q[i]);
      const auto trial=objective(v);
      ExtendedReal slope=0;for(int i=0;i<n;++i)slope+=f.g[ix[i]]*step[i];
      if(std::isfinite(trial.v) && trial.v>=f.v+1e-4*fraction*slope){p=q;accepted=true;break;}
      fraction*=.5;
    }
    if(!accepted)return false;
  }
  return false;
}

// Practical score-roundoff allowance for the conditional certificate.
// An inconclusive allowance withdraws certification, not a successful local
// Newton result. This is not outward-rounded interval arithmetic.
inline double noise_bound(const std::array<double,3>& p,const std::array<bool,3>& free,
                          const Likelihood& full) {
  std::array<int,3> ix{};int n=0;
  std::array<accuracy_detail::Jet,3> v;
  for(int i=0;i<3;++i){v[i]=accuracy_detail::variable(p[i],i,free[i]);if(free[i])ix[n++]=i;}
  if(n==0)return 0;
  const auto f=full(v);
  std::array<std::array<ExtendedReal,3>,3> L{};
  for(int i=0;i<n;++i)for(int j=0;j<=i;++j) {
    ExtendedReal a=-f.h[ix[i]][ix[j]];
    for(int k=0;k<j;++k)a-=L[i][k]*L[j][k];
    if(i==j){if(!(a>0)||!std::isfinite(a))return INFINITY;L[i][j]=std::sqrt(a);}
    else L[i][j]=a/L[j][j];
  }
  std::array<ExtendedReal,3> magnitude{},error{};
  const auto b=accuracy_detail::exp(v[1]),c=accuracy_detail::exp(v[2]);
  for(double y:full.y) {
    const auto z=b*(accuracy_detail::Jet(y)-v[0]);
    const auto h=logq(z);
    const auto term=v[1]+v[2]+z-h-accuracy_detail::exp(z)+c*h;
    for(int i=0;i<n;++i)magnitude[i]+=std::abs(term.g[ix[i]])/full.y.size();
  }
  for(int col=0;col<n;++col) {
    std::array<ExtendedReal,3> a{},solution{};
    for(int i=0;i<n;++i){ExtendedReal z=i==col?1:0;for(int j=0;j<i;++j)z-=L[i][j]*a[j];a[i]=z/L[i][i];}
    for(int i=n-1;i>=0;--i){ExtendedReal z=a[i];for(int j=i+1;j<n;++j)z-=L[j][i]*solution[j];solution[i]=z/L[i][i];}
    for(int i=0;i<n;++i)error[i]+=std::abs(solution[i])*16*std::numeric_limits<double>::epsilon()*(1+magnitude[col]);
  }
  double bound=0;
  for(int i=0;i<n;++i)bound=std::max(bound,static_cast<double>(error[i])*(ix[i]==0?full.unit:1));
  return bound;
}

inline double restore_scale(double anchor,double shift) {
  if(std::abs(shift)<.5)return std::fma(anchor,std::expm1(shift),anchor);
  const double ratio=std::exp(shift),scale=anchor*ratio;
  return std::isnormal(ratio) && scale>0 && std::isfinite(scale)?scale:
    std::exp(std::log(anchor)+shift);
}

// For t <= 712, maximizing over c bounds the log-data density by
// b*t/[e*(1-exp(-t))] <= b*(1+t)/e. For t > 712 and c >= 1, the
// bound is b*DBL_MAX*t*exp(-t) < b*713/e. For c < 1 it is at most b.
// Exhibit a feasible real exponent above this universal binary64 bound.
inline bool exponent_overflows(span relative_logs,double shape) {
  constexpr double cutoff=712, t_min=2*cutoff;
  const double low=*std::min_element(relative_logs.begin(),relative_logs.end());
  accuracy_detail::ScalarSum weights,rest;
  for(double y:relative_logs) {
    const double offset=t_min*std::expm1(shape*(y-low));
    if(!std::isfinite(offset))return false;
    weights.add(std::exp(-offset));
    rest.add(std::log1p(offset/t_min)-offset);
  }
  // Set log(c) = t_min - log(mean(exp(-offset))), so c*mean(exp(-t)) = 1.
  // Since -log(1-exp(-t)) <= exp(-t)/(1-exp(-t)), the mean penalty
  // is at most 1+4 eps. All comparisons omit the common log(shape).
  const double log_weight=std::log(static_cast<double>(weights.value/relative_logs.size()));
  const double value=std::log(t_min)-log_weight+
    static_cast<double>(rest.value/relative_logs.size())-
    (1+4*std::numeric_limits<double>::epsilon());
  const double bound=std::log(1+cutoff)-1;
  return std::isfinite(value) && value>bound+1e-9;
}

struct Faces {
  double power=NAN,inverse=NAN; // Mean log likelihood in normalized log data.
  double beta=NAN,k=NAN,location=NAN,scale=NAN,ascent=NAN;
};

inline Faces fit_faces(span x,span y,double anchor,double endpoint,double unit,
                       double tol,int maxit) {
  Faces faces;
  accuracy_detail::ScalarSum gap;
  for(double v:x)gap.add(-transformed_detail::log_ratio(v,endpoint));
  faces.beta=static_cast<double>(x.size()/gap.value);
  faces.power=-std::log(static_cast<double>(gap.value/x.size())/unit)-1;

  const auto inv=invweibull_mle(x,tol,maxit);
  faces.k=inv.status==InvWeibullStatus::Ok?inv.shape*unit:NAN;
  if(!(faces.k>0 && std::isfinite(faces.k))) {
    // The original delegate can fail to restore a scale on subnormal data.
    // Solve the same profile on the exponentiated normalized log data.
    std::vector<double> proxy;
    proxy.reserve(y.size());
    for(double v:y)proxy.push_back(std::exp(v));
    const auto normalized=invweibull_mle({proxy.data(),proxy.size()},tol,maxit);
    if(normalized.status!=InvWeibullStatus::Ok)return faces;
    faces.k=normalized.shape;
  }

  // Reconstruct the profiled location before rounding it to the public
  // scale. At adjacent observations the rounding loss can reverse P vs I.
  const double low=*std::min_element(y.begin(),y.end());
  accuracy_detail::ScalarSum weights;
  for(double v:y)weights.add(std::exp(-faces.k*(v-low)));
  const double location_shift=-std::log(static_cast<double>(weights.value/y.size()));
  faces.location=low+location_shift/faces.k;
  faces.scale=restore_scale(anchor,unit*faces.location);
  accuracy_detail::ScalarSum offsets,ascent;
  for(double v:y) {
    const double u=faces.k*(v-low)-location_shift;
    offsets.add(u);
    ascent.add(u+.5*u*u*std::expm1(-u));
  }
  faces.inverse=std::log(faces.k)-static_cast<double>(offsets.value/y.size())-1;
  faces.ascent=static_cast<double>(ascent.value/y.size());
  return faces;
}

}
inline ExpweibullResult expweibull_mle(span x,double tol=1e-10,int maxit=100,
    double scale=NAN,double shape=NAN,double exponent=NAN) {
  ExpweibullResult out;out.scale=scale;out.shape=shape;out.exponent=exponent;
  const std::array<bool,3> free{std::isnan(scale),std::isnan(shape),std::isnan(exponent)};
  const std::array<double,3> held{scale,shape,exponent};
  for(int i=0;i<3;++i)if(!free[i] && (!(held[i]>0)||!std::isfinite(held[i]))){out.status=ExpweibullStatus::InvalidFixed;return out;}
  if(x.empty() || !(tol>0)||!std::isfinite(tol)||maxit<1)return out;
  const double goal=tol;
  for(double v:x)if(!(v>0)||!std::isfinite(v))return out;
  const auto mm=std::minmax_element(x.begin(),x.end());
  const bool constant=*mm.first==*mm.second;
  if(constant && ((free[0] && (free[1]||free[2])) ||
      (!free[0] && free[1] && (x[0]==scale || (free[2] && x[0]>scale))))) {
    out.status=ExpweibullStatus::Unbounded;out.certified=true;return out;
  }
  // The attained c=1 submodel has a dedicated, bracketed Weibull solver.
  // Guard subnormal fixed-shape reductions before delegating to it.
  if(!free[2] && exponent==1) {
    const auto closed_scale=[&](double value,bool verified) {
      out.scale=value;out.shape=shape;out.exponent=1;
      out.logLik=expweibull_fit_detail::public_loglik(x,value,shape,1);
      out.status=verified && std::isfinite(out.logLik)?ExpweibullStatus::Ok:ExpweibullStatus::AccuracyFailure;
      out.certified=out.status==ExpweibullStatus::Ok;out.singleton_proved=out.certified;
      if(!out.certified)record_accuracy(out,tol,{out.scale,out.shape,1},{out.scale,0,0});
      return out;
    };
    if(free[0] && !free[1]) {
      if(constant)return closed_scale(x[0],true);
      const auto enclosure=expweibull_fit_detail::weibull_scale_enclosure(x,shape,goal);
      if(enclosure.valid)return closed_scale(enclosure.value,true);
      if(enclosure.parent_unsafe)return closed_scale(enclosure.value,false);
    }
    // A rounded closed form alone cannot justify a sub-spacing request.
    // Retain exact constant-scale roots above; otherwise use the accuracy
    // refinement and its structured tolerance diagnostic below.
    if(goal>=4*std::numeric_limits<double>::epsilon() || (!free[0] && !free[1])) {
    const auto w=weibull_mle(x,goal,maxit,shape,scale);
    if(w.status==WeibullStatus::Ok) {
      out.scale=w.scale;out.shape=w.shape;out.exponent=1;out.logLik=w.logLik;
      out.iterations=w.iterations;
      const double ll=expweibull_fit_detail::public_loglik(x,out.scale,out.shape,1);
      if(std::isfinite(ll) && std::isfinite(out.logLik) &&
         std::abs(ll-out.logLik)<=1e-10*std::max(1.,std::abs(out.logLik))) {
        out.status=ExpweibullStatus::Ok;out.certified=true;out.singleton_proved=true;return out;
      }
      out.logLik=ll;out.status=ExpweibullStatus::AccuracyFailure;
      record_accuracy(out,tol,{out.scale,out.shape,1},{free[0]?out.scale:0,free[1]?out.shape:0,0});
      return out;
    }
    }
  }
  // Relative logs preserve distinctions and scale equivariance near a point mass.
  const double anchor=free[0]?x[x.size()/2]:scale;
  std::vector<double> y;y.reserve(x.size());ExtendedReal sum=0;
  for(double v:x) {
    const double z=transformed_detail::log_ratio(v,anchor);
    y.push_back(z);sum+=z;
  }
  const double mean=static_cast<double>(sum/x.size());
  const double logmean=std::log(anchor)+mean;
  if(!free[0] && free[1] && free[2] && *mm.second<=scale) {
    ExtendedReal gap=0;std::size_t ties=0;
    for(std::size_t i=0;i<x.size();++i){gap-=y[i];if(x[i]==scale)++ties;}
    const double beta=static_cast<double>(x.size()/gap);
    const double ll=x.size()*(std::log(beta)-logmean-1)-ties*std::log(std::expm1(1.));
    if(!(beta>0)||!std::isfinite(beta)||!std::isfinite(ll)){out.status=ExpweibullStatus::IterationFailure;return out;}
    out.status=ties?ExpweibullStatus::PowerBoundaryTied:ExpweibullStatus::PowerBoundary;
    out.logLik=ll;out.certified=true;
    out.stop_detail=limit_detail({scale,beta},ll,ties?StopModel::None:StopModel::Power);
    const double pad=256*std::numeric_limits<double>::epsilon()*(1+x.size())*(1+std::abs(ll));
    out.stop_detail.loglik_lower=ll-pad;out.stop_detail.loglik_upper=ll+pad;return out;
  }
  if(free[0] && !free[1] && free[2] &&
      expweibull_fit_detail::exponent_overflows({y.data(),y.size()},shape)) {
    out.status=ExpweibullStatus::ExponentOverflow;
    return out;
  }
  double unit=0;for(double v:y)unit=std::max(unit,std::abs(v-mean));
  if(!free[1] && std::isfinite(1/shape))unit=1/shape;
  if(!(unit>0))unit=1;
  // If scale is held, zero must remain zero under normalization.
  for(double& v:y)v/=unit;
  const span logs{y.data(),y.size()};
  const bool all_free=free[0] && free[1] && free[2];
  const auto faces=all_free?expweibull_fit_detail::fit_faces(x,logs,anchor,*mm.second,unit,goal,maxit):
    expweibull_fit_detail::Faces{};
  const bool both_faces=std::isfinite(faces.inverse) && std::isfinite(faces.power);
  const bool inverse_wins=both_faces && faces.inverse>=faces.power;
  const double face=inverse_wins?faces.inverse:faces.power;
  const double log_unit=std::log(unit);
  const double jacobian=log_unit+logmean;
  // Use normalized mean log likelihoods so offset and scale do not inflate
  // the comparison margin. Allow for rounding in the reductions and kernels.
  const double pad=256*std::numeric_limits<double>::epsilon()*(1+x.size())*(1+std::abs(face));
  expweibull_fit_detail::Likelihood objective{logs,unit,free[2]},full{logs,unit,false};
  std::array<bool,3> search_free{free[0],free[1],false};
  const double local_tol=goal*.125*(free[0]?std::min(1.,1/unit):1.);
  double best=-INFINITY;bool found=false,winner_margin=false;
  std::array<double,3> winner{},attempt{};double attempted=-INFINITY,attempt_conditioning=NAN;bool have_attempt=false;
  // Quantile-matched starts span the two closure directions. Each is only
  // a search seed; no numerical truncation is used as a face certificate.
  auto sorted=y;std::sort(sorted.begin(),sorted.end());
  std::vector<std::array<double,3>> starts;
  for(double cseed:{.1,1.,10.}) {
    if(!free[2] && cseed!=1.)continue;
    const double c=free[2]?cseed:exponent;
    const double tlo=-std::log(-std::expm1(std::log(.25)/c));
    const double thi=-std::log(-std::expm1(std::log(.75)/c));
    const double width=sorted[3*(y.size()-1)/4]-sorted[(y.size()-1)/4];
    double b0=width>0?(std::log(thi)-std::log(tlo))/width:1.;
    if(!free[0] && free[1]) {
      double distance=0;for(double v:y)distance=std::max(distance,std::abs(v));
      if(distance>0)b0=std::min(b0,1/distance);
    }
    for(double multiplier:{.5,1.,2.}) {
      if(!free[1] && multiplier!=1.)continue;
      const double b=free[1]?b0*multiplier:shape*unit;
      const double tm=-std::log(-std::expm1(std::log(.5)/c));
      starts.push_back({free[0]?sorted[y.size()/2]-std::log(tm)/b:0,std::log(b),std::log(c)});
    }
  }
  if(faces.ascent>0) {
    const double log_exponent=16;
    const double b=faces.k/log_exponent;
    starts.push_back({faces.location-std::log(log_exponent)/b,std::log(b),log_exponent});
  }
  for(auto p:starts) {
    if(free[0]||free[1])expweibull_fit_detail::search(p,search_free,objective,maxit,out.iterations);
    if(free[2]) {
      accuracy_detail::JetSum h;
      for(double v:y)h.add(expweibull_fit_detail::logq(accuracy_detail::Jet(std::exp(p[1])*(v-p[0]))));
      const double fitted=static_cast<double>(-static_cast<ExtendedReal>(y.size())/h.value.v);
      if(!(fitted>0)||!std::isfinite(fitted))continue;
      p[2]=std::log(fitted);
    }
    StopDetail detail;
    const bool polished=accuracy_detail::polish(p,free,local_tol,maxit,full,detail);
    std::array<accuracy_detail::Jet,3> av;for(int i=0;i<3;++i)av[i]=accuracy_detail::Jet(p[i]);
    const double al=static_cast<double>(full(av).v+(all_free?log_unit:0))-(all_free?0:logmean);
    const double noise=polished?expweibull_fit_detail::noise_bound(p,free,full):
      detail.conditioning_bound*(free[0]?std::max(1.,unit):1.)*8;
    if(std::isfinite(al) && al>attempted) {
      attempt=p;attempted=al;have_attempt=true;attempt_conditioning=noise;
    }
    if(polished && std::isfinite(al) && al>best) {
      winner=p;best=al;found=true;winner_margin=noise<goal*.5;
    }
  }
  out.status=ExpweibullStatus::IterationFailure;
  // A budget-limited delegate or an unresolved finite witness above the
  // faces does not supply evidence for a limit. A separated power witness
  // can still refute a finite point when the inverse delegate failed.
  if(both_faces && (!found || best<face-pad) && attempted<=face+pad) {
    const auto detail_for=[&](bool inverse) {
      if(!inverse)return limit_detail({*mm.second,faces.beta},
        (faces.power-jacobian)*x.size(),StopModel::Power);
      const double k=faces.k/unit;
      accuracy_detail::ScalarSum total;
      for(double v:x)total.add(distributions::dinvweibull(v,k,faces.scale,true));
      return limit_detail({k,faces.scale},static_cast<double>(total.value),StopModel::InvWeibull);
    };
    auto detail=detail_for(inverse_wins);
    if(!detail.candidates.empty()) {
      detail.loglik_lower=detail.limit_loglik-
        256*std::numeric_limits<double>::epsilon()*(1+x.size())*(1+std::abs(detail.limit_loglik));
      detail.loglik_upper=INFINITY;
      if(std::abs(faces.power-faces.inverse)<=pad) {
        const auto other=detail_for(!inverse_wins);
        if(!other.candidates.empty())detail.candidates.push_back(other.candidates.front());
      }
      out.status=inverse_wins?ExpweibullStatus::InvweibullBoundary:ExpweibullStatus::PowerBoundary;
      out.logLik=detail.limit_loglik;
      out.stop_detail=std::move(detail);
      return out;
    }
  }
  if(!found) {
    if(have_attempt) {
      out.stop_detail.conditioning_bound=attempt_conditioning;
      out.scale=free[0]?expweibull_fit_detail::restore_scale(anchor,unit*attempt[0]):scale;
      out.shape=free[1]?std::exp(attempt[1])/unit:shape;
      out.exponent=free[2]?std::exp(attempt[2]):exponent;
      out.logLik=expweibull_fit_detail::public_loglik(x,out.scale,out.shape,out.exponent);
      record_accuracy(out,tol,{out.scale,out.shape,out.exponent},{free[0]?out.scale:0,free[1]?out.shape:0,free[2]?out.exponent:0});
      if(out.stop_detail.accuracy_stop!=AccuracyStop::None)out.status=ExpweibullStatus::AccuracyFailure;
    }
    return out;
  }
  out.scale=free[0]?expweibull_fit_detail::restore_scale(anchor,unit*winner[0]):scale;
  out.shape=free[1]?std::exp(winner[1])/unit:shape;
  out.exponent=free[2]?std::exp(winner[2]):exponent;
  out.logLik=(best-(all_free?jacobian:0))*x.size();
  for(double v:{out.scale,out.shape,out.exponent,out.logLik})if(!std::isfinite(v))return out;
  if(!(out.scale>0 && out.shape>0 && out.exponent>0))return out;
  if((free[0] && !accuracy_detail::scale_projection(out.scale,anchor,unit*winner[0],tol)) ||
     (free[1] && !accuracy_detail::scale_projection(out.shape,1/unit,winner[1],tol)) ||
     (free[2] && !accuracy_detail::scale_projection(out.exponent,1,winner[2],tol))) {
    out.status=ExpweibullStatus::AccuracyFailure;
    out.logLik=expweibull_fit_detail::public_loglik(x,out.scale,out.shape,out.exponent);
    record_accuracy(out,tol,{out.scale,out.shape,out.exponent},{free[0]?out.scale:0,free[1]?out.shape:0,free[2]?out.exponent:0});return out;
  }
  const double public_ll=expweibull_fit_detail::public_loglik(x,out.scale,out.shape,out.exponent);
  if(all_free && std::isfinite(face) && best<face-pad) {
    // The finite point is refuted even if the limit decision is unresolved.
    out.stop_detail.limit_loglik=(face-jacobian)*x.size();
    out.stop_detail.loglik_upper=INFINITY;
    if(std::isfinite(public_ll))out.stop_detail.candidates.push_back(
      {{out.scale,out.shape,out.exponent},public_ll,public_ll,StopModel::None});
    return out;
  }
  if(!std::isfinite(public_ll) || std::abs(public_ll-out.logLik)>
      1e-10*std::max(1.,std::abs(out.logLik))) {
    // On steep free fits the rounded scale can move visibly in normalized
    // log data, even after passing the public coefficient projection.
    const double shift=transformed_detail::log_ratio(out.scale,anchor);
    const double projection=std::abs(shift/unit-winner[0]);
    const double spacing=std::max(
      std::abs(transformed_detail::log_ratio(std::nextafter(out.scale,0.),out.scale)),
      std::abs(transformed_detail::log_ratio(std::nextafter(out.scale,INFINITY),out.scale)));
    if(!(all_free && both_faces && best>face+pad && std::isfinite(public_ll) &&
         std::isfinite(spacing) && projection<=.5*spacing/unit+
           8*std::numeric_limits<double>::epsilon()*(1+std::abs(winner[0])))) {
      out.status=ExpweibullStatus::AccuracyFailure;
      out.logLik=public_ll;
      record_accuracy(out,tol,{out.scale,out.shape,out.exponent},{free[0]?out.scale:0,free[1]?out.shape:0,free[2]?out.exponent:0});
      return out;
    }
    out.logLik=public_ll;
  }
  out.status=ExpweibullStatus::Ok;
  out.certified=(!free[2] || (!free[0] && !free[1])) && winner_margin;
  out.singleton_proved=out.certified;
  return out;
}

inline FitOutcome outcome(const ExpweibullResult& r) {
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  switch (r.status) {
    case ExpweibullStatus::Ok: return r.singleton_proved ? FitOutcome::singleton(r.certified) : FitOutcome::unknown_cardinality(r.certified);
    case ExpweibullStatus::InvalidData: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case ExpweibullStatus::InvalidFixed: return FitOutcome::failure(OutcomeReason::InvalidArgument);
    case ExpweibullStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case ExpweibullStatus::InvweibullBoundary:
    case ExpweibullStatus::PowerBoundary: return FitOutcome::limit(r.certified);
    case ExpweibullStatus::PowerBoundaryTied: return FitOutcome::limit(true);
    case ExpweibullStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case ExpweibullStatus::AccuracyFailure:
    case ExpweibullStatus::ExponentOverflow: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}


}
