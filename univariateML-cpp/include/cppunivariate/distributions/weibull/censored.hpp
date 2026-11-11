// Right-censored Weibull fits for every hold set. Shape held: closed form
// (exp on x^k). Shape free: the score is strictly decreasing in the shape,
// so a sign-certified bisection on log shape meets tol. Arguments: the
// comments below and dev/research/evaluations/2026-11-11-cens-easy-report.md.
#pragma once
#include "../exp/censored_transform.hpp"
#include "mle.hpp"
#include <vector>
namespace cppunivariate {
enum class WeibullCensoredStatus { Ok, InvalidInput, AllCensored, Unbounded, NotImplemented, NumericalFailure };
struct WeibullCensoredResult {
  double shape, scale, logLik;
  WeibullCensoredStatus status=WeibullCensoredStatus::Ok;
};
namespace weibull_censored_detail {
inline WeibullCensoredStatus map(ExpCensoredStatus s) {
  return s==ExpCensoredStatus::Ok?WeibullCensoredStatus::Ok:
    s==ExpCensoredStatus::InvalidInput?WeibullCensoredStatus::InvalidInput:
    s==ExpCensoredStatus::AllCensored?WeibullCensoredStatus::AllCensored:
    s==ExpCensoredStatus::Unbounded?WeibullCensoredStatus::Unbounded:
    s==ExpCensoredStatus::NotImplemented?WeibullCensoredStatus::NotImplemented:
    WeibullCensoredStatus::NumericalFailure;
}
// log(v / anchor), keeping relative differences near the anchor.
inline double log_ratio(double v,double anchor,double log_anchor) {
  const double r=(v-anchor)/anchor;
  return std::abs(r)<.5?std::log1p(r):std::log(v)-log_anchor;
}
// Root of a strictly decreasing score, bisected on log shape and certified
// by the signs at both ends; NaN when that fails within maxit steps.
template<class Score>
inline double decreasing_root(Score score,double tol,int maxit) {
  double lo=std::log(std::numeric_limits<double>::denorm_min()),hi=std::log(std::numeric_limits<double>::max());
  if(!(score(std::exp(lo))>0 && score(std::exp(hi))<0))return NAN;
  for(int it=0;it<maxit && hi-lo>.01*tol;++it) {
    const double mid=lo+(hi-lo)/2;const auto g=score(std::exp(mid));
    if(g==0) {
      const double left=std::max(lo,mid-.2*tol),right=std::min(hi,mid+.2*tol);
      if(score(std::exp(left))>0 && score(std::exp(right))<0) {lo=left;hi=right;}
      break;
    }
    if(g>0)lo=mid;else hi=mid;
  }
  if(hi-lo>.01*tol)return NAN;
  return std::exp(lo+(hi-lo)/2);
}
} // namespace weibull_censored_detail

// shape, scale: NaN estimates, a value holds. tol is the relative accuracy
// of the free coefficients (at most 1e-10); maxit bounds bisection steps.
inline WeibullCensoredResult weibull_censored_mle(CensoredData x, double shape=NAN, double scale=NAN,
    double tol=1e-10, int maxit=400) {
  namespace wd=weibull_censored_detail;
  const double nan=std::numeric_limits<double>::quiet_NaN();
  maxit=static_cast<int>(solver_budget(maxit));
  const auto c=classify_censored(x,0);
  if(c.status!=CensoredStatus::Ok || x.lower.empty() || !(tol>0 && std::isfinite(tol)) || maxit<=0 ||
     (!std::isnan(shape) && !(shape>0 && std::isfinite(shape))) ||
     (!std::isnan(scale) && !(scale>0 && std::isfinite(scale))))
    return {nan,nan,nan,WeibullCensoredStatus::InvalidInput};
  if(c.left || c.interval)return {nan,nan,nan,WeibullCensoredStatus::NotImplemented};
  for(std::size_t i=0;i<x.lower.size();++i) {
    const double v=x.lower[i];
    if(!(v>=0 && std::isfinite(v)) || (v==0 && v==x.upper[i]))
      return {nan,nan,nan,WeibullCensoredStatus::InvalidInput};
  }
  tol=std::min(tol,1e-10);
  const double d=static_cast<double>(c.exact);

  if(!std::isnan(shape) && std::isnan(scale)) {
    const auto r=censored_detail::fit(x,censored_detail::Transform::Weibull,shape);
    // Preserve exact-data rounding when its result agrees with the stable
    // closed form well inside the 1e-10 coefficient accuracy contract.
    if(r.status==ExpCensoredStatus::Ok && c.exact==x.lower.size()) {
      const auto e=weibull_mle(x.lower,1e-10,1000,NAN,shape,NAN);
      if(e.status==WeibullStatus::Ok && std::abs(e.scale/r.coefficient-1)<1e-12)
        return {shape,e.scale,e.logLik};
    }
    return {shape,r.coefficient,r.logLik,wd::map(r.status)};
  }

  // Positive times only: a time censored at zero contributes S(0) = 1.
  std::vector<double> z;std::vector<char> exact;
  if(std::isnan(scale)) {
    // Both free; profile the scale: scale^k = sum_all x^k / d. With
    // t = log(x / x_max) <= 0 and w = exp(k t), the profile score
    //   d/k + sum_exact t - d sum(w t) / sum(w)
    // strictly decreases in k (the weighted mean of t has derivative its
    // weighted variance, positive unless all times are equal). It tends to
    // +inf as k -> 0 and to sum_exact t as k -> inf, so the maximiser exists
    // and is unique exactly when some exact observation is below the largest
    // time (exact or censored). Otherwise the fit collapses: Unbounded.
    if(!c.exact)return {nan,nan,nan,WeibullCensoredStatus::AllCensored};
    double top=0;for(double v:x.lower)top=std::max(top,v);
    const double log_top=std::log(top);
    double sum_exact=0;
    for(std::size_t i=0;i<x.lower.size();++i)if(x.lower[i]>0) {
      z.push_back(wd::log_ratio(x.lower[i],top,log_top));
      exact.push_back(x.lower[i]==x.upper[i]);
      if(exact.back())sum_exact+=z.back();
    }
    if(!(sum_exact<0))return {nan,nan,nan,WeibullCensoredStatus::Unbounded};
    const auto score=[&](double k) {
      CPPUNIV_EXTENDED_REAL w=0,wt=0;
      for(double t:z) {
        const CPPUNIV_EXTENDED_REAL e=std::exp(static_cast<CPPUNIV_EXTENDED_REAL>(k)*t);
        w+=e;wt+=e*t;
      }
      return static_cast<CPPUNIV_EXTENDED_REAL>(d)/k+sum_exact-d*wt/w;
    };
    shape=wd::decreasing_root(score,tol,maxit);
    if(std::isnan(shape))return {nan,nan,nan,WeibullCensoredStatus::NumericalFailure};
    return weibull_censored_mle(x,shape,NAN,tol,maxit);
  }

  // Scale held, z = log(x / scale). The log likelihood
  //   d log k + sum_exact (k z - log x) - sum_all exp(k z)
  // is strictly concave in k. Its score d/k - sum_exact z expm1(k z)
  // - sum_censored z exp(k z) tends to +inf as k -> 0 and, as k -> inf, to
  // -inf if some z > 0, else to sum_exact z. So the maximiser exists exactly
  // unless every time is at most the scale and every exact one equals it.
  const double log_scale=std::log(scale);
  bool above=false;double sum_exact=0;
  for(std::size_t i=0;i<x.lower.size();++i)if(x.lower[i]>0) {
    z.push_back(wd::log_ratio(x.lower[i],scale,log_scale));
    exact.push_back(x.lower[i]==x.upper[i]);
    above=above || z.back()>0;
    if(exact.back())sum_exact+=z.back();
  }
  if(std::isnan(shape)) {
    if(!c.exact)return {nan,scale,nan,WeibullCensoredStatus::AllCensored};
    if(!above && !(sum_exact<0))return {nan,scale,nan,WeibullCensoredStatus::Unbounded};
    const auto score=[&](double k) {
      CPPUNIV_EXTENDED_REAL g=static_cast<CPPUNIV_EXTENDED_REAL>(d)/k;
      if(!std::isfinite(g))return g;
      for(std::size_t i=0;i<z.size();++i)if(z[i]!=0) {
        const CPPUNIV_EXTENDED_REAL kz=static_cast<CPPUNIV_EXTENDED_REAL>(k)*z[i];
        const CPPUNIV_EXTENDED_REAL term=-static_cast<CPPUNIV_EXTENDED_REAL>(z[i])*(exact[i]?std::expm1(kz):std::exp(kz));
        if(!std::isfinite(term))return -std::numeric_limits<CPPUNIV_EXTENDED_REAL>::infinity();
        g+=term;
      }
      return g;
    };
    shape=wd::decreasing_root(score,tol,maxit);
    if(std::isnan(shape))return {nan,scale,nan,WeibullCensoredStatus::NumericalFailure};
  }
  // Log likelihood at (shape, scale): exact log k - log x + k z - exp(k z),
  // censored -exp(k z).
  CPPUNIV_EXTENDED_REAL ll=0;
  for(double t:z)ll-=std::exp(static_cast<CPPUNIV_EXTENDED_REAL>(shape)*t);
  for(std::size_t i=0;i<x.lower.size();++i)if(x.lower[i]==x.upper[i]) {
    const double v=x.lower[i];
    ll+=std::log(shape)-std::log(v)+static_cast<CPPUNIV_EXTENDED_REAL>(shape)*wd::log_ratio(v,scale,log_scale);
  }
  const double out=static_cast<double>(ll);
  return {shape,scale,out,std::isfinite(out)?WeibullCensoredStatus::Ok:WeibullCensoredStatus::NumericalFailure};
}
} // namespace cppunivariate
