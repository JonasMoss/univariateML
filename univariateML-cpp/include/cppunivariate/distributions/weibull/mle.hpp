// Weibull MLE via Gumbel transform on -log(x). Parameters recovered as
// shape = 1/sigma, scale = exp(-mu + s) where s = -mean(log_x).

#pragma once
#include "../../core/stop_detail.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"
#include "../gumbel/mle.hpp"
#include "dpqr.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class WeibullStatus { Ok, InvalidInput, Unbounded, IterationFailure, AccuracyFailure };

struct WeibullResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
  WeibullStatus status = WeibullStatus::Ok;
 StopDetail stop_detail{}; };

// shape0 is an optional positive starting shape; NaN keeps the automatic start.
inline WeibullResult weibull_mle(span x, double tol, int maxit,
                                  double shape0 = std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, WeibullStatus::InvalidInput};

  for (double v : x) if (!(v > 0 && std::isfinite(v)))
    return {NAN,NAN,NAN,0,WeibullStatus::InvalidInput};

  // Preserve relative differences before taking logarithms near a point
  // mass: subtracting two independently rounded logs loses shape accuracy.
  const double anchor=x[0],log_anchor=std::log(anchor);
  std::vector<double> log_x(n);
  double sum_log_x=0;
  for(std::size_t i=0;i<n;++i) {
    const double relative=(x[i]-anchor)/anchor;
    log_x[i]=-(std::abs(relative)<.5?std::log1p(relative):std::log(x[i])-log_anchor);
    sum_log_x+=log_x[i];
  }
  const double dn=static_cast<double>(n),center=-sum_log_x/dn;
  const double s=log_anchor+center;
  for(auto& v:log_x)v+=center;
  sum_log_x-=dn*log_anchor;

  GumbelResult g = fit_gumbel_newton(span{log_x.data(), n}, tol, maxit,
      std::isnan(shape0) ? shape0 : -std::log(shape0), true);
  const double shape = 1.0 / g.sigma;
  const double factor=std::exp(center-g.mu);
  const double scale=(factor>1e-150 && factor<1e150)?anchor*factor:std::exp(-g.mu+s);
  if(g.status==GumbelStatus::Ok && (!(shape>0 && std::isfinite(shape)) ||
     !(scale>0 && std::isfinite(scale)) ||
     (factor>1e-150 && factor<1e150 ? std::abs(scale/anchor-factor)>.25*tol*factor :
       std::abs(std::log(scale)-(-g.mu+s))>.25*tol)))
    {WeibullResult out{shape,scale,g.logLik+sum_log_x,g.iterations,WeibullStatus::AccuracyFailure};if(!std::isfinite(out.logLik)&&shape>0&&scale>0&&std::isfinite(shape+scale)){out.logLik=0;for(double v:x)out.logLik+=distributions::dweibull(v,shape,scale,true);}
      {if(out.shape>0 && out.scale>0)record_accuracy(out,tol,{shape,scale},{out.shape,out.scale});else{out.stop_detail.accuracy_stop=AccuracyStop::None;out.stop_detail.attainable_tol=NAN;}}return out;}
  // Transform the fitted Gumbel likelihood, avoiding shape times
  // a difference of rounded log scales near a point mass.
  const double logLik = g.logLik + sum_log_x;
  WeibullResult out{shape, scale, logLik, g.iterations,
      g.status == GumbelStatus::Ok ? WeibullStatus::Ok :
      g.status == GumbelStatus::Unbounded ? WeibullStatus::Unbounded :
      g.status == GumbelStatus::AccuracyFailure ? WeibullStatus::AccuracyFailure :
      g.status == GumbelStatus::InvalidInput ? WeibullStatus::InvalidInput :
      WeibullStatus::IterationFailure};
  if(out.status==WeibullStatus::AccuracyFailure){if(out.shape>0 && out.scale>0)record_accuracy(out,tol,{shape,scale},{out.shape,out.scale});else{out.stop_detail.accuracy_stop=AccuracyStop::None;out.stop_detail.attainable_tol=NAN;}}
  return out;
}

// Strictly decreasing fixed-scale shape score, using relative log ratios.
inline WeibullResult weibull_mle(span x,double tol,int maxit,double shape0,double shape,double scale) {
  maxit = static_cast<int>(solver_budget(maxit));
  const bool free_shape=std::isnan(shape),free_scale=std::isnan(scale);
  if (free_shape && free_scale) return weibull_mle(x,tol,maxit,shape0);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.empty() || !(tol>0 && std::isfinite(tol)) || maxit<=0 ||
     (!std::isnan(shape) && !(shape>0 && std::isfinite(shape))) ||
     (!std::isnan(scale) && !(scale>0 && std::isfinite(scale))))
    return {nan,nan,nan,0,WeibullStatus::InvalidInput};
  tol=std::min(tol,1e-10);
  std::vector<double> logs;
  for(double v:x) {
    if(!(v>0 && std::isfinite(v)))return {nan,nan,nan,0,WeibullStatus::InvalidInput};
    logs.push_back(std::log(v));
  }
  const double n=static_cast<double>(x.size());
  int iterations=0;
  if(std::isnan(scale)) {
    double top=logs[0];for(double v:logs)top=std::max(top,v);
    CPPUNIV_EXTENDED_REAL shifted=0,c=0;
    for(double v:logs) {
      const CPPUNIV_EXTENDED_REAL y=std::expm1(static_cast<CPPUNIV_EXTENDED_REAL>(shape)*(v-top))-c,t=shifted+y;
      c=(t-shifted)-y;shifted=t;
    }
    const double ideal=top+static_cast<double>(std::log1p(shifted/n)/shape);
    scale=std::exp(ideal);
    if(!(scale>0 && std::isfinite(scale)) || std::abs(std::log(scale)-ideal)>.25*tol)
      {WeibullResult out{shape,scale,nan,0,WeibullStatus::AccuracyFailure};if(!std::isfinite(out.logLik)&&shape>0&&scale>0&&std::isfinite(shape+scale)){out.logLik=0;for(double v:x)out.logLik+=distributions::dweibull(v,shape,scale,true);}
      {if(out.shape>0 && out.scale>0)record_accuracy(out,tol,{shape,scale},{free_shape?out.shape:0,free_scale?out.scale:0});else{out.stop_detail.accuracy_stop=AccuracyStop::None;out.stop_detail.attainable_tol=NAN;}}return out;}
  }
  const double ls=std::log(scale);
  std::vector<double> ratios(x.size());
  for(std::size_t i=0;i<x.size();++i) {
    const double d=(x[i]-scale)/scale;
    const double q=x[i]/scale;
    ratios[i]=std::abs(d)<.5?std::log1p(d):
      (q>0 && std::isfinite(q)?std::log(q):logs[i]-ls);
  }
  if(std::isnan(shape)) {
    bool equal=true;for(double v:x)equal=equal && v==scale;
    if(equal)return {nan,scale,nan,0,WeibullStatus::Unbounded};
    const auto score=[&](double k) {
      CPPUNIV_EXTENDED_REAL g=static_cast<CPPUNIV_EXTENDED_REAL>(n)/k,c=0;
      if(!std::isfinite(g))return g;
      for(double z:ratios)if(z!=0) {
        const CPPUNIV_EXTENDED_REAL term=-static_cast<CPPUNIV_EXTENDED_REAL>(z)*std::expm1(static_cast<CPPUNIV_EXTENDED_REAL>(k)*z);
        if(!std::isfinite(term))return -std::numeric_limits<CPPUNIV_EXTENDED_REAL>::infinity();
        const CPPUNIV_EXTENDED_REAL y=term-c,t=g+y;c=(t-g)-y;g=t;
      }
      return g;
    };
    double lo=std::log(std::numeric_limits<double>::denorm_min()),hi=std::log(std::numeric_limits<double>::max());
    if(!(score(std::exp(lo))>0 && score(std::exp(hi))<0))
      return {nan,scale,nan,0,WeibullStatus::IterationFailure};
    bool accurate=false;
    for(;iterations<maxit && hi-lo>.01*tol;++iterations) {
      const double mid=lo+(hi-lo)/2;const auto g=score(std::exp(mid));
      if(g==0) {
        if(maxit-iterations<3)break;
        iterations+=2;
        const double left=std::max(lo,mid-.2*tol),right=std::min(hi,mid+.2*tol);
        if(score(std::exp(left))>0 && score(std::exp(right))<0) {lo=left;hi=right;accurate=true;}
        break;
      }
      if(g>0)lo=mid;else hi=mid;
    }
    if(!accurate && hi-lo>.01*tol)return {nan,scale,nan,iterations,WeibullStatus::IterationFailure};
    const double selected=lo+(hi-lo)/2;shape=std::exp(selected);
    if(!(shape>0 && std::isfinite(shape)) || std::abs(std::log(shape)-selected)>.25*tol)
      {WeibullResult out{shape,scale,nan,iterations,WeibullStatus::AccuracyFailure};if(!std::isfinite(out.logLik)&&shape>0&&scale>0&&std::isfinite(shape+scale)){out.logLik=0;for(double v:x)out.logLik+=distributions::dweibull(v,shape,scale,true);}
      {if(out.shape>0 && out.scale>0)record_accuracy(out,tol,{shape,scale},{free_shape?out.shape:0,free_scale?out.scale:0});else{out.stop_detail.accuracy_stop=AccuracyStop::None;out.stop_detail.attainable_tol=NAN;}}return out;}
  }
  CPPUNIV_EXTENDED_REAL ll=0,c=0;
  for(std::size_t i=0;i<x.size();++i) {
    const CPPUNIV_EXTENDED_REAL z=static_cast<CPPUNIV_EXTENDED_REAL>(shape)*ratios[i];
    const CPPUNIV_EXTENDED_REAL y=std::log(shape)-logs[i]+z-std::exp(z)-c,t=ll+y;
    c=(t-ll)-y;ll=t;
  }
  return {shape,scale,static_cast<double>(ll),iterations};
}

}  // namespace cppunivariate
