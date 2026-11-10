#pragma once
#include "transformed_data.hpp"
#include "work_budget.hpp"

namespace cppunivariate {
namespace transformed_detail {
// Positive-to-negative root bracket. Each expansion and bisection uses
// the supplied evaluation cap (solver or certificate units at the call).
// Exhaustion never returns an unchecked last iterate.
template<class Score> inline double root(Score score,double tol,int maxit) {
  double lo=-1,hi=1;
  for(int j=0;j<maxit && score(lo)<0;++j)lo*=2;
  for(int j=0;j<maxit && score(hi)>0;++j)hi*=2;
  if(!(score(lo)>=0 && score(hi)<=0))return NAN;
  for(int j=0;j<maxit;++j){
    const double m=lo+(hi-lo)/2,g=score(m);
    if(std::isnan(g))return NAN;
    if(g==0 || m==lo || m==hi)return m;
    if(g>0)lo=m;else hi=m;
    if(hi-lo<=std::min(tol/16,4*std::numeric_limits<double>::epsilon()))return lo+(hi-lo)/2;
  }
  return NAN;
}
inline double sigmoid(double z){return z>=0?1/(1+std::exp(-z)):std::exp(z)/(1+std::exp(z));}
inline double softplus(double z){return std::max(z,0.)+std::log1p(std::exp(-std::abs(z)));}
struct LocationFit { double shape,scale,ll; int status; bool certified=true; };
// Burr with fixed first shape, log-logistic (a=1), or inverse Weibull
// (Gumbel). Their log-data likelihood is strictly concave in (b,b*mu).
// Standardized raw log differences retain dispersion without large logs.
inline LocationFit location_fit(span x,double power,double scale,double a,
    bool gumbel,double sign,double tol,int maxit) {
  const auto l=logs(x);const LocationFit bad{NAN,NAN,NAN,3};
  if(!l.valid || !(tol>0 && std::isfinite(tol)) || maxit<1 ||
     (!std::isnan(power)&&!(power>0 && std::isfinite(power))) ||
     (!std::isnan(scale)&&!(scale>0 && std::isfinite(scale))) || !(a>0 && std::isfinite(a)))return {NAN,NAN,NAN,1};
  maxit=static_cast<int>(solver_budget(maxit));
  const bool free_power=std::isnan(power),free_scale=std::isnan(scale);
  const double n=double(x.size());double unit=0;
  for(double d:l.delta)unit=std::max(unit,std::abs(d));
  const double held_shift=free_scale?0:sign*log_ratio(scale,x[0]);
  if(unit==0) {
    if(free_power && (free_scale || held_shift==0))return {NAN,NAN,NAN,2};
    unit=std::abs(held_shift);if(unit==0)unit=1;
  }
  std::vector<double> y;for(double d:l.delta)y.push_back(sign*d/unit);
  const auto location=[&](double b) {
    if(!free_scale)return b*held_shift/unit;
    if(gumbel){double low=INFINITY;for(double v:y)low=std::min(low,v);
      double range=0;for(double v:y)range=std::max(range,b*(v-low));
      Sum s;
      if(range<.1) {
        for(double v:y)s.push(std::expm1(-b*(v-low))/n);
        return b*low-std::log1p(s.get());
      }
      for(double v:y)s.push(std::exp(-b*(v-low))/n);
      return b*low-std::log(s.get());
    }
    return root([&](double c){Sum s;for(double v:y)s.push((a+1)*sigmoid(b*v-c));return s.get()-n;},tol,maxit);
  };
  double b=free_power?NAN:power*unit;
  if(free_power) {
    const double t=root([&](double z){const double k=std::exp(z),c=location(k);Sum s;s.push(n);
      for(double v:y){const double u=k*v-c;
        s.push(k*(free_scale?v:v-held_shift/unit)*(gumbel?std::expm1(-u):1-(a+1)*sigmoid(u)));
      }return s.get();},tol,maxit);
    b=std::exp(t);power=b/unit;
  }
  const double c=location(b),shift=c/power;
  if(free_scale)scale=std::abs(shift)<.5?std::fma(x[0],std::expm1(sign*shift),x[0]):std::exp(value(add(l.anchor,{sign*shift,0})));
  if(!(power>0 && scale>0 && std::isfinite(power) && std::isfinite(scale) && std::isfinite(c)))return bad;
  const double actual_shift=sign*log_ratio(scale,x[0]);
  Sum ll,ideal;const double constant=std::log(power)+(gumbel?0:std::log(a));
  for(std::size_t i=0;i<x.size();++i){
    const double u=power*(sign*l.delta[i]-actual_shift),z=b*y[i]-c;
    const double tail=gumbel?-u-std::exp(-u):-softplus(-u)-a*softplus(u);
    const double itail=gumbel?-z-std::exp(-z):-softplus(-z)-a*softplus(z);
    ll.push(constant+tail);ideal.push(constant+itail);
  }
  // Coefficient rounding can have a visible likelihood cost at an ulp-wide
  // sample even though the continuous root and relative coefficients are
  // resolved. Retain that finite fit explicitly uncertified; do not invent
  // nonexistence or optimizer failure. A separated coefficient error stops.
  if(free_scale && std::abs(std::expm1(actual_shift-shift))>
      tol+128*std::numeric_limits<double>::epsilon()*(1+std::abs(shift)))return bad;
  const bool certified=!free_scale || restored(ideal.get(),ll.get(),tol,std::abs(ideal.get())+std::abs(ll.get()));
  const double result=ll.get()+l.jacobian;
  return {power,scale,result,std::isfinite(result)?0:3,certified};
}
} // namespace transformed_detail
} // namespace cppunivariate
