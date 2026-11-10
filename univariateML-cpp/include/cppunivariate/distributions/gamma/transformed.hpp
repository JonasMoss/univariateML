#pragma once
#include "mle.hpp"
#include "../../core/transformed_data.hpp"

namespace cppunivariate {
namespace transformed_detail {
enum class GammaMap { Reciprocal, Square, Log };
struct RawGammaResult { double shape,scale,logLik; int iterations; GammaStatus status; bool certified=true; };
// Fit the gamma likelihood from raw-data deviations. The public second
// coordinate is inverse-gamma beta, Nakagami E(X^2), or log-gamma rate.
inline RawGammaResult gamma_raw(span x,double tol,int maxit,double shape,double parameter,GammaMap map) {
  maxit=static_cast<int>(solver_budget(maxit));
  const RawGammaResult bad{NAN,NAN,NAN,0,GammaStatus::TransformFailure};
  if(x.empty() || !(tol>0 && std::isfinite(tol)) || maxit<1 ||
     (!std::isnan(shape)&&!(shape>0 && std::isfinite(shape))) ||
     (!std::isnan(parameter)&&!(parameter>0 && std::isfinite(parameter))))
    return {NAN,NAN,NAN,0,GammaStatus::InvalidInput};
  double anchor=x[0];bool constant=true;
  for(double v:x){
    if(!(v>0 && std::isfinite(v)) || (map==GammaMap::Log && !(v>1)))
      return {NAN,NAN,NAN,0,GammaStatus::InvalidInput};
    anchor=map==GammaMap::Reciprocal?std::min(anchor,v):std::max(anchor,v);
    constant=constant && v==x[0];
  }
  const bool free_shape=std::isnan(shape),free_parameter=std::isnan(parameter);
  if(constant && free_shape && free_parameter)return {NAN,NAN,NAN,0,GammaStatus::Degenerate};
  const double n=double(x.size());
  const Pair log_anchor=log_pair(anchor);
  const Pair yref=map==GammaMap::Reciprocal?div({1,0},{anchor,0}):
      map==GammaMap::Square?mul({anchor,0},{anchor,0}):log_anchor;
  const bool reference_finite=yref.hi>0 && std::isfinite(yref.hi) && std::isfinite(yref.lo);
  const double log_yref=map==GammaMap::Reciprocal?-value(log_anchor):
      map==GammaMap::Square?2*value(log_anchor):std::log(value(log_anchor));
  std::vector<double> delta,logdelta;Sum ds,relative_total,ls,jac;
  for(double v:x){
    const double r=(v-anchor)/anchor,l=log_ratio(v,anchor);
    const double d=map==GammaMap::Reciprocal?(anchor-v)/v:
        map==GammaMap::Square?r*(2+r):l/value(log_anchor);
    const double ld=map==GammaMap::Reciprocal?-l:map==GammaMap::Square?2*l:
        (std::abs(d)<.1?std::log1p(d):std::log(std::log(v))-log_yref);
    delta.push_back(d);logdelta.push_back(ld);ds.push(d);ls.push(ld/n);
    const double relative_y=map==GammaMap::Reciprocal?anchor/v:
        map==GammaMap::Square?(v/anchor)*(v/anchor):std::log(v)/value(log_anchor);
    relative_total.push(relative_y);
    jac.push(map==GammaMap::Reciprocal?-2*std::log(v):map==GammaMap::Square?std::log(2.)+std::log(v):-std::log(v));
  }
  const Pair mean_delta=div({ds.hi,ds.lo},{n,0});
  const double md=value(mean_delta);
  const Pair mean_pair=std::abs(md)<.1?add({1,0},mean_delta):div({relative_total.hi,relative_total.lo},{n,0});
  const double mean_ratio=value(mean_pair);
  if(!(mean_ratio>0 && std::isfinite(mean_ratio)))return bad;
  double s=0;
  // At concentration the centered deviations carry the whole Jensen gap;
  // no absolute transformed observation or cancelling pair of logs is used.
  if(std::abs(md)<.1){
    Sum gap;for(double d:delta){const double u=(d-md)/mean_ratio;gap.push(gamma_detail::phi(u,1+u)/n);}s=gap.get();
  } else s=std::log(mean_ratio)-ls.get();
  const double meanlog=log_yref+ls.get();
  Pair modelmean=yref;
  if(free_parameter) {
    modelmean=map==GammaMap::Square?mul({anchor,0},mul({anchor,0},mean_pair)):mul(yref,mean_pair);
    if(map==GammaMap::Square && !(modelmean.hi>0 && std::isfinite(modelmean.hi) && std::isfinite(modelmean.lo)))return bad;
  }
  else if(map==GammaMap::Square)modelmean={parameter,0};
  else if(!free_shape)modelmean=map==GammaMap::Reciprocal?div({shape,0},{parameter,0}):div({shape,0},{parameter,0});
  int iterations=0;
  if(free_shape) {
    if(free_parameter || map==GammaMap::Square) {
      if(!free_parameter) {
        const double offset=reference_finite?value(div(add(yref,neg(modelmean)),modelmean)):std::expm1(log_yref-std::log(parameter));
        s=0;for(std::size_t i=0;i<delta.size();++i){const double u=offset+(1+offset)*delta[i];s+=gamma_detail::phi(u,std::exp(log_yref-std::log(parameter)+logdelta[i]))/n;}
      }
      if(!(s>0 && std::isfinite(s)))return constant?RawGammaResult{NAN,NAN,NAN,0,GammaStatus::Degenerate}:bad;
      double lo=.25/s,hi=2/s;
      if(!(lo>0 && std::isfinite(hi)))return bad;
      double mid=std::sqrt(lo)*std::sqrt(hi);
      for(;iterations<maxit;++iterations){
        const double f=gamma_detail::log_minus_digamma(mid)-s;
        if(f==0){lo=hi=mid;break;}
        if(f>0)lo=mid;else hi=mid;
        // Newton proposes; actual signed evaluations retain the bracket.
        // Tight probes let a rapidly converged proposal close its bracket
        // without spending another fifty pure bisections on ordinary data.
        if(std::abs(f)<64*std::numeric_limits<double>::epsilon()*s) {
          const double radius=8*std::numeric_limits<double>::epsilon()*mid;
          const double left=std::max(lo,mid-radius),right=std::min(hi,mid+radius);
          if(gamma_detail::log_minus_digamma(left)>=s)lo=left;
          if(gamma_detail::log_minus_digamma(right)<=s)hi=right;
        }
        if(hi/lo-1<=std::min(tol/8,4*std::numeric_limits<double>::epsilon()))break;
        const double r=1/mid,r2=r*r;
        const double derivative=mid>=100?
            -r*(.5+r*(1./6-r2*(1./30-r2*(1./42-r2/30)))):
            1-mid*special::trigamma(mid);
        const double proposal=mid*std::exp(-f/derivative);
        mid=proposal>lo && proposal<hi && std::isfinite(proposal)?proposal:std::sqrt(lo)*std::sqrt(hi);
      }
      if(iterations==maxit)return {NAN,NAN,NAN,iterations,GammaStatus::IterationFailure};
      shape=std::sqrt(lo)*std::sqrt(hi);
    } else {
      // Fixed gamma rate: psi(shape) = log(rate) + mean(log Y).
      const double target=std::log(parameter)+meanlog;
      double lo=std::log(std::numeric_limits<double>::denorm_min()),hi=std::log(std::numeric_limits<double>::max());
      if(!(special::digamma(std::exp(lo))<=target && special::digamma(std::exp(hi))>=target))return bad;
      for(;iterations<maxit;++iterations){const double m=lo+(hi-lo)/2;
        if(special::digamma(std::exp(m))<target)lo=m;else hi=m;
        if(hi-lo<=std::min(tol/8,2e-13))break;
      }
      if(iterations==maxit)return {NAN,NAN,NAN,iterations,GammaStatus::IterationFailure};
      shape=std::exp(lo+(hi-lo)/2);
    }
  }
  if(free_parameter) {
    if(map==GammaMap::Square)parameter=value(modelmean);
    else if(map==GammaMap::Reciprocal) {
      const Pair product=mul({shape,0},{anchor,0});
      parameter=value(product.hi>=std::numeric_limits<double>::min() && std::isfinite(product.hi)?
          div(product,mean_pair):mul({shape,0},div({anchor,0},mean_pair)));
    } else parameter=value(div({shape,0},modelmean));
  }
  if(!(shape>0 && parameter>0 && std::isfinite(shape) && std::isfinite(parameter)))return bad;
  // Ratio of the raw reference Y to the fitted gamma mean; retain its
  // small displacement through products/divisions before rounding.
  Pair ratio{NAN,0};
  const double logratio0=map==GammaMap::Square?log_yref-std::log(parameter):
      std::log(parameter)+log_yref-std::log(shape);
  if(map==GammaMap::Square)ratio=reference_finite?div(yref,{parameter,0}):mul(div({anchor,0},{parameter,0}),{anchor,0});
  else if(map==GammaMap::Reciprocal)ratio=div({parameter,0},mul({shape,0},{anchor,0}));
  else ratio=div(mul({parameter,0},log_anchor),{shape,0});
  const bool ratio_finite=ratio.hi>0 && std::isfinite(ratio.hi) && std::isfinite(ratio.lo);
  const double offset=ratio_finite?value(add(ratio,{-1,0})):std::expm1(logratio0);
  Sum penalty;
  for(std::size_t i=0;i<delta.size();++i){
    const double u=ratio_finite?offset+value(ratio)*delta[i]:std::expm1(logratio0+logdelta[i]);
    const double logz=(ratio_finite?std::log(value(ratio)):logratio0)+logdelta[i];
    const double phi=std::abs(u)<.1?gamma_detail::phi(u,1+u):std::expm1(logz)-logz;
    penalty.push(phi);
  }
  const double normalizer=shape>=100?.5*std::log(shape)-.91893853320467274178-gamma_detail::stirlerr(shape):
      shape*std::log(shape)-shape-std::lgamma(shape);
  const double ll=n*(normalizer-meanlog)-shape*penalty.get()+jac.get();
  const double ideal=n*(normalizer-meanlog-shape*s)+jac.get();
  const bool certified=!free_parameter || restored(ideal,ll,tol,std::abs(ideal)+std::abs(ll));
  const double mean_error=ratio_finite?value(add(mul(ratio,mean_pair),{-1,0})):
      std::expm1(logratio0+std::log(mean_ratio));
  if(free_parameter && std::abs(mean_error)>tol+128*std::numeric_limits<double>::epsilon()*(1+std::abs(mean_error)))return bad;
  if(!std::isfinite(ll))return bad;
  return {shape,parameter,ll,iterations+1,GammaStatus::Ok,certified};
}
} // namespace transformed_detail
} // namespace cppunivariate
