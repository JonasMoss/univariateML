// Package-defined, mean/standard-deviation parameterisation.
#pragma once
#include "../snorm/fs.hpp"
#include <array>
#include <limits>
#include "../../special/skew_coordinate.hpp"
#include "tails.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace ged_detail {
struct Radius { double value,error,scale; };
inline double zero_rounding_threshold() {
 return special::skew_probability::zero_rounding_threshold();
}
inline special::three_word::Number logscale_uncached(double nu) {
 if(nu>=1e30)return special::three_word::multiply({.5},special::three_word::logarithm({3}));
 const auto z=special::three_word::divide({1},{nu});
 return special::three_word::multiply(special::three_word::add(special::three_word::log_gamma(z),special::three_word::negative(special::three_word::log_gamma(special::three_word::multiply(z,{3})))),{.5});
}
inline special::three_word::Number logscale_precise(double nu) {
 static thread_local double last=0;
 static thread_local special::three_word::Number saved{0};
 if(nu!=last){saved=logscale_uncached(nu);last=nu;}
 return saved;
}
inline Radius radius_precise(special::three_word::Number v,double nu,double input_error=0) {
 const auto ls=logscale_precise(nu),lv=special::three_word::logarithm(v);
 const auto w=special::three_word::add(lv,special::three_word::negative(ls));
 const double e=special::three_word::margin(std::abs(ls.high)+std::abs(lv.high))+std::abs(w.low)+4*std::numeric_limits<double>::epsilon()*std::abs(w.high);
 return {w.high,e+(nu>=1e30?2/nu:0)+(v.high>input_error?input_error/(v.high-input_error):input_error==0?0:special::kPosInf),ls.high};
}
inline double density_precise(Radius r,double nu,double normalizer,bool log,
                              double natural_budget=1e-11) {
 const double gamma=special::lgamma1p(1/nu);
 const double cost=std::exp(nu*r.value),ld=normalizer-std::log(2.)-r.scale-gamma-cost;
 if(!std::isfinite(cost)) {
  if(nu*(r.value-r.error)>std::log(std::numeric_limits<double>::max())+2)return log?special::kNegInf:0;
  return special::kNaN;
 }
 const double change=nu*r.error<.5?cost*std::expm1(nu*r.error)
   :std::max(std::exp(nu*(r.value+r.error))-cost,cost-std::exp(nu*(r.value-r.error)));
 const double margin=change+16*std::numeric_limits<double>::epsilon()*(1+cost+std::abs(ld)+std::abs(normalizer)+std::abs(r.scale)+std::abs(gamma));
 if(!log && ld+margin<zero_rounding_threshold())return 0;
 if(!(margin>=0) || margin>(log?1e-11*std::max(1.,std::abs(ld)):natural_budget))return special::kNaN;
 return log?ld:std::exp(ld);
}
// Compare the selected tail itself at both uncertainty endpoints. A
// log-error multiplied by the rounded center tail is unsafe after that
// center has underflowed: an endpoint can still have appreciable mass.
inline bool probability_interval(double point,double left,double right,bool log) {
 return special::skew_probability::interval(point,left,right,log);
}
inline double gamma_log_margin(double value) {
 return std::isfinite(value)?2e-12+16*std::numeric_limits<double>::epsilon()*std::abs(value):0;
}
inline double probability_precise(Radius r,double nu,double logweight,bool negative,bool lower,bool log,
                                  bool allow_subnormal_rounding=false) {
 const double base=gamma_power_log(r.value,nu,false),direct=logweight+base;
 const auto tails=special::tails_from_log(direct,negative);
 const double selected=lower?tails.lower:tails.upper;
 const double left=gamma_power_log(r.value-r.error,nu,false),right=gamma_power_log(r.value+r.error,nu,false);
 const double weight_error=16*std::numeric_limits<double>::epsilon()*(1+std::abs(logweight));
 const auto low=special::tails_from_log(logweight+right-gamma_log_margin(right)-weight_error,negative);
 const auto high=special::tails_from_log(std::min(0.,logweight+left+gamma_log_margin(left)+weight_error),negative);
 const double a=lower?low.lower:low.upper,b=lower?high.lower:high.upper;
 if(!probability_interval(selected,a,b,log)) {
  // Natural-scale relative accuracy can be impossible on the subnormal
  // grid. Round the checked log tail there using the usual exp conversion.
  if(log || !allow_subnormal_rounding || !(std::exp(selected)<std::numeric_limits<double>::min()) ||
     !probability_interval(selected,a,b,true))return special::kNaN;
 }
 return special::tail_probability(tails,lower,log);
}
inline Radius physical_radius(double x,double mean,double sd,double nu) {
 auto delta=special::three_word::add({x},{-mean});
 special::three_word::Number lv;
 if(std::isfinite(delta.high)) {
  if(delta.high<0)delta=special::three_word::negative(delta);
  lv=special::three_word::logarithm(delta);
 } else {
  delta=special::three_word::add({x*.5},{-mean*.5});
  if(delta.high<0)delta=special::three_word::negative(delta);
  lv=special::three_word::add(special::three_word::logarithm(delta),special::three_word::log_two());
 }
 const auto ls=logscale_precise(nu),logsd=special::three_word::logarithm({sd});
 const auto w=special::three_word::add(special::three_word::add(lv,special::three_word::negative(logsd)),special::three_word::negative(ls));
 const double error=special::three_word::margin(std::abs(lv.high)+std::abs(logsd.high)+std::abs(ls.high))+std::abs(w.low)+4*std::numeric_limits<double>::epsilon()*std::abs(w.high)+(nu>=1e30?2/nu:0);
 return {w.high,error,ls.high};
}

// Practical absolute error in the double log radius, including the
// rounding of nu*w expressed back in radius units. The unit allowance
// covers the relative error in the distance and the gamma recurrence
// in logscale (for nu >= 1).
inline double fast_radius_error(double log_distance,double log_sd,double ls,double w) {
 return 8*std::numeric_limits<double>::epsilon()*
     (1+std::abs(log_distance)+std::abs(log_sd)+std::abs(ls)+std::abs(w));
}
// Allocate one tenth of the natural-tail tolerance to the fast radius.
constexpr double fast_tail_budget=1e-11;

// The upper gamma continued fraction has denominator b_0-r_1,
// with b_n=x+1-a+2n and r_n=n(n-a)/(b_n-r_{n+1}).
// The recurrence maps [0,n+1] into [0,n] for x>0 and a<=1.
inline special::three_word::Number quantile_upper_denominator(
    special::three_word::Number x,special::three_word::Number a) {
 namespace tw = special::three_word;
 const auto b=tw::add(tw::add(x,{1}),tw::negative(a));
 for(int depth=8;depth<=256;depth*=2) {
  tw::Number lo{0},hi{double(depth+1)};
  for(int n=depth;n>=1;--n) {
   const auto bn=tw::add(b,{2.*n});
   const auto numerator=tw::multiply({double(n)},tw::add({double(n)},tw::negative(a)));
   lo=tw::divide(numerator,tw::add(bn,tw::negative(lo)));
   hi=tw::divide(numerator,tw::add(bn,tw::negative(hi)));
  }
  if(tw::add(hi,tw::negative(lo)).high<=1e-34*x.high)
   return tw::add(b,tw::negative(tw::multiply({.5},tw::add(lo,hi))));
 }
 return {special::kNaN};
}

// Recover the low word of w from the bracketed double inverse. Use the
// lower series at small gamma arguments, and the upper continued fraction
// where subtraction from unit mass would lose the tail.
inline special::three_word::Number quantile_radius(double w,double nu,double p,bool log_p) {
 namespace tw = special::three_word;
 const bool central=w<=std::log(8.)/nu;
 static thread_local double last=0;
 static thread_local tw::Number a{0}, log_gamma{0}, log_a{0};
 if(nu!=last) {
  a=tw::divide({1},{nu});
  log_gamma=tw::log_gamma(tw::add({1},a));
  log_a=tw::logarithm(a);
  last=nu;
 }
 const auto probability=log_p?tw::exponential({p}):tw::Number{p};
 const auto centred=tw::add(tw::multiply({2},probability),{-1});
 const auto mass=centred.high<0?tw::negative(centred):centred;
 tw::Number target{0};
 if(!central) {
  const bool small=log_p?p<=-std::log(2.):p<=.5;
  const auto complement=tw::add({1},tw::negative(probability));
  const auto log_tail=log_p && small ? tw::Number{p} : tw::logarithm(small?probability:complement);
  target=tw::add(log_tail,tw::log_two());
  // Beyond this point replacing x by -log Q in log(x) changes w by
  // O(log(x)/(nu*x)), below the expansion's precision.
  if(target.high<-1e100)
   return tw::divide(tw::logarithm(tw::negative(target)),{nu});
 }
 auto radius=tw::Number{w};
 for(int attempt=0;attempt<3;++attempt) {
  const auto x=tw::exponential(tw::multiply(radius,{nu}));
  tw::Number step;
  if(central) {
   if(!(x.high<=8.01))return {w};
   tw::Number term{1},sum{0};
   for(int n=1;n<=96;++n) {
    term=tw::divide(tw::multiply(term,tw::negative(x)),{double(n)});
    sum=tw::add(sum,tw::divide(term,tw::add(a,{double(n)})));
    if(std::abs(term.high)<1e-39)break;
   }
   const auto factor=tw::exponential(tw::add(radius,tw::negative(log_gamma)));
   const auto value=tw::multiply(factor,tw::add({1},tw::multiply(a,sum)));
   const auto slope=tw::multiply(factor,tw::exponential(tw::negative(x)));
   step=tw::divide(tw::add(mass,tw::negative(value)),slope);
  } else {
   if(!(x.high>=7.99 && std::isfinite(x.high)))return {w};
   const auto denominator=quantile_upper_denominator(x,a);
   const auto normalizer=tw::add(log_a,tw::negative(log_gamma));
   const auto log_tail=tw::add(tw::add(radius,tw::negative(x)),
       tw::add(normalizer,tw::negative(tw::logarithm(denominator))));
   // The derivative of log Q with respect to w is -nu*(b_0-r_1).
   step=tw::divide(tw::divide(tw::add(log_tail,tw::negative(target)),denominator),{nu});
  }
  const double change=std::abs(step.high);
  if(!(change<=1e-10) || (x.high>.01 && change*nu>1e-4))return {w};
  // Check the refined radius before accepting it. In this local range
  // the log slope varies smoothly (by O(nu*step) in the gamma power).
  if(attempt==2) {
   if(change<=1e-30*std::max(1.,std::abs(w)))return radius;
   break;
  }
  radius=tw::add(radius,step);
 }
 return {w};
}

// Two binary64 words for the fast quantile residual. Round the high
// word only after forming the affine sum.
namespace quantile_double {
struct Number { double high, low=0; };

inline Number normalize(double high,double low) {
 const double sum=high+low;
 return {sum,special::addition_residual(high,low,sum)};
}
inline Number add(Number a,Number b) {
 const double sum=a.high+b.high;
 const double residual=special::addition_residual(a.high,b.high,sum);
 return normalize(sum,(a.low+b.low)+residual);
}
inline Number negative(Number a) {return {-a.high,-a.low};}
inline Number multiply(Number a,Number b) {
 const double product=a.high*b.high;
 const double cross=std::fma(a.high,b.low,a.low*b.high);
 return normalize(product,std::fma(a.high,b.high,-product)+cross);
}
inline Number divide(Number a,Number b) {
 const double q=a.high/b.high;
 const auto residual=add(a,negative(multiply({q},b)));
 return normalize(q,residual.high/b.high);
}
inline Number exponential(Number x) {
 static const auto coefficients=[] {
  std::array<Number,13> c{};
  c[0]={1};
  for(int n=1;n<=12;++n)c[n]=divide(c[n-1],{double(n)});
  return c;
 }();
 constexpr Number log_two{.6931471805599453,2.3190468138462996e-17};
 const int e=static_cast<int>(std::floor(x.high/log_two.high+.5));
 const auto r=multiply(add(x,multiply(log_two,{-double(e)})),{.0625});
 auto sum=coefficients.back();
 for(int n=11;n>=0;--n)sum=add(coefficients[n],multiply(r,sum));
 for(int n=0;n<4;++n)sum=multiply(sum,sum);
 return {std::scalbn(sum.high,e),std::scalbn(sum.low,e)};
}

inline Number logarithm(Number x) {
 int e=0;
 const double high=std::frexp(x.high,&e);
 const Number mantissa{high,std::scalbn(x.low,-e)};
 const double seed=std::log(high);
 const auto value=exponential({seed});
 const auto correction=divide(add(mantissa,negative(value)),value);
 constexpr Number log_two{.6931471805599453,2.3190468138462996e-17};
 return add(add({seed},correction),multiply(log_two,{double(e)}));
}

struct Shape {
 double nu=0;
 Number a{}, log_gamma{}, log_scale{}, log_a{};
 std::array<Number,64> coefficients{};
};
inline special::three_word::Number log_gamma_1p(special::three_word::Number a) {
 namespace tw = special::three_word;
 // Gamma recurrence in product form. Here 0<a<=3, so the
 // product through a+63 is normal and far below overflow.
 tw::Number product{1};
 for(int n=1;n<=63;++n)product=tw::multiply(product,tw::add(a,{double(n)}));
 const auto z=tw::add(a,{64});
 const auto main=tw::add(tw::multiply(tw::add(z,{-.5}),tw::logarithm(z)),tw::negative(z));
 return tw::add(tw::add(main,tw::negative(tw::logarithm(product))),
                tw::add(tw::log_sqrt_two_pi(),tw::stirling(z)));
}
inline const Shape& shape(double nu) {
 namespace tw = special::three_word;
 static thread_local Shape cached;
 if(cached.nu!=nu) {
  const auto a=tw::divide({1},{nu});
  const auto g=log_gamma_1p(a);
  const auto g3=log_gamma_1p(tw::multiply({3},a));
  const auto ls=tw::multiply({.5},tw::add(tw::logarithm({3}),tw::add(g,tw::negative(g3))));
  cached.nu=nu;
  cached.log_gamma={g.high,g.low};
  cached.log_scale={ls.high,ls.low};
  const Number reciprocal_shape{a.high,a.low};
  cached.a=reciprocal_shape;
  cached.log_a=logarithm(reciprocal_shape);
  Number factorial{-1};
  for(int n=1;n<=64;++n) {
   cached.coefficients[n-1]=divide(multiply(reciprocal_shape,factorial),
                                  add(reciprocal_shape,{double(n)}));
   factorial=divide(factorial,{-double(n+1)});
  }
 }
 return cached;
}

struct Correction { double step, error, curvature; };
constexpr double unit=0x1p-53;

inline Correction central_correction(double w,Number t,const Shape& constants,double p,bool log_p) {
 if(log_p && p<-80)return {0,special::kPosInf,1};
 const int degree=t.high<=.0625?16:t.high<=1?28:64;
 auto series=constants.coefficients[degree-1];
 for(int n=degree-2;n>=0;--n)
  series=add(constants.coefficients[n],multiply(t,series));
 series=add({1},multiply(t,series));
 const auto factor=exponential(add({w},negative(constants.log_gamma)));
 const auto probability=log_p?exponential({p}):Number{p};
 auto mass=add(multiply({2},probability),{-1});
 if(mass.high<0)mass=negative(mass);
 const double inverse_exp=std::exp(t.high);
 const double slope=factor.high/inverse_exp;
 const double step=add(mass,negative(multiply(factor,series))).high/slope;
 const double error=0x1p-83*inverse_exp*inverse_exp +
     (log_p?0x1p-88*probability.high/slope:0.) +
     32*unit*(1+t.high)*std::abs(step);
 return {step,error,1+2*constants.nu*t.high};
}

// Enclose the upper-gamma fraction using its positive backwards
// recurrence. Only r_1 is needed. The power itself keeps its low word.
inline Correction upper_correction(double w,Number t,const Shape& constants,double p,bool log_p) {
 const double a=constants.a.high;
 double lo=0,hi=0;
 if(constants.nu!=1) {
  constexpr int depth=32;
  const double al=a*(1-2*unit),ah=std::min(1.,a*(1+2*unit));
  const double tl=t.high*(1-2*unit),th=t.high*(1+2*unit);
  hi=depth+1;
  for(int n=depth;n>=1;--n) {
   const double b=1+2.*n;
   lo=(n*(n-ah)/(th+b-al-lo))*(1-16*unit);
   hi=(n*(n-al)/(tl+b-ah-hi))*(1+16*unit);
  }
 }
 const auto denominator=add(add(t,add({1},negative(constants.a))),{-(lo+hi)*.5});
 constexpr Number log_two{.6931471805599453,2.3190468138462996e-17};
 const bool direct=log_p && p<=-log_two.high;
 const double small=direct?1.:log_p?-std::expm1(p):std::min(p,1-p);
 const auto target=add(direct?Number{p}:logarithm({small}),log_two);
 const auto log_tail=constants.nu==1 ? negative(t)
     : add(add({w},negative(t)),
           add(add(constants.log_a,negative(constants.log_gamma)),negative(logarithm(denominator))));
 const double slope=constants.nu*denominator.high;
 const double step=add(log_tail,negative(target)).high/slope;
 const double fraction_error=2*(hi-lo)/denominator.high;
 const double error=(fraction_error + (log_p && !direct?4*unit:0.) +
      0x1p-83*(1+t.high+std::abs(target.high)+std::abs(constants.log_a.high)))/slope
      +32*unit*std::abs(step);
 return {step,error,1+constants.nu};
}

// Check the Newton remainder and the physical affine error at the
// double seed. Extreme powers and unrepresentable affine intermediates
// retain the scaled reconstruction.
inline double affine(double w,double mean,double sd,double nu,bool negative_side,
                     double p,bool log_p) {
 if(!(w>=-40 && w<=16 && nu<=1e12 && sd>=0x1p-450 && sd<=0x1p450 &&
      std::abs(mean)<=0x1p450))return special::kNaN;
 const auto power=multiply({w},{nu});
 if(power.high>32)return special::kNaN;
 const auto t=power.high<-80?Number{0}:exponential(power);
 const auto& constants=shape(nu);
 const auto correction=t.high<=8.01 ? central_correction(w,t,constants,p,log_p)
                                    : upper_correction(w,t,constants,p,log_p);
 const double step=correction.step,evaluation=correction.error,curvature=correction.curvature;
 const double delta=std::abs(step)+evaluation;
 if(!(delta*curvature<=1e-4 && nu*delta<=1e-3))return special::kNaN;
 const double radius_error=2*evaluation+4*curvature*delta*delta;
 const auto z=multiply(exponential(add({w},constants.log_scale)),add({1},{step}));
 const auto distance=multiply({negative_side?-sd:sd},z);
 const double result=add({mean},distance).high;
 const double error=(2*radius_error+4*delta*delta+0x1p-89)*std::abs(distance.high)
     +0x1p-89*std::abs(mean);
 // Allocate eight unit roundoffs in relative physical coordinates.
 if(!(std::abs(result)>=0x1p-500 && error<=0x1p-50*std::abs(result)))return special::kNaN;
 return result;
}
} // namespace quantile_double

inline double quantile_affine(double w,double mean,double sd,double nu,bool negative,
                              double p,bool log_p) {
 namespace tw = special::three_word;
 if(nu<1 || !std::isfinite(w)) {
  const double distance=std::exp(std::log(sd)+distributions::fs::ged_logscale(nu)+w);
  return mean+std::copysign(distance,negative?-1.:1.);
 }
 const double fast=quantile_double::affine(w,mean,sd,nu,negative,p,log_p);
 if(std::isfinite(fast))return fast;
 const auto logz=tw::add(logscale_precise(nu),quantile_radius(w,nu,p,log_p));
 // Even with the most extreme binary64 sd, these offsets are below
 // half the least subnormal or beyond twice the largest finite coordinate.
 if(logz.high<-1500)return mean;
 if(logz.high>1500)return negative?special::kNegInf:special::kPosInf;
 const int ez=static_cast<int>(std::floor(logz.high/std::log(2.)+.5));
 const auto z=tw::exponential(tw::add(logz,tw::multiply(tw::log_two(),{-double(ez)})));
 int es=0,em=0;
 const double scale=std::frexp(negative?-sd:sd,&es);
 std::frexp(mean,&em);
 es+=ez;
 const int exponent=mean==0?es:std::max(es,em);
 // The power-of-two factors stay outside the product and sum. Keep the
 // FMA product residual and the low words of z through cancellation.
 const auto distance=tw::multiply({std::scalbn(scale,es-exponent)},z);
 const auto coordinate=tw::add({std::scalbn(mean,-exponent)},distance);
 return std::scalbn(coordinate.high,exponent);
}
}
namespace distributions {
inline double dged(double x, double mean, double sd, double nu, bool log) {
  if (!(fs::valid(mean, sd, nu, 1.0)) || std::isnan(x)) return cppunivariate::special::kNaN;
  if(nu>=1e6 && std::isfinite(x) && x!=mean)
    return ged_detail::density_precise(ged_detail::physical_radius(x,mean,sd,nu),nu,-std::log(sd),log,1e-10);
  const double z=(x-mean)/sd, log_sd=std::log(sd);
  const bool scaled_log=(std::isinf(z) ||
      (x!=mean && std::abs(z)<std::numeric_limits<double>::min())) && std::isfinite(x);
  const double ls=fs::ged_logscale(nu);
  const double log_distance=scaled_log?special::log_abs_difference(x,mean):std::log(std::abs(z));
  const double w=(scaled_log?log_distance-log_sd:log_distance)-ls;
  const double cost=std::exp(nu*w);
  const double constant=-std::log(2.0)-ls;
  const double gamma_constant=nu>=1 ? constant-special::lgamma1p(1/nu)
      : std::log(nu)-std::log(2.0)-ls-std::lgamma(1/nu);
  const double ld=scaled_log ? constant-std::lgamma(1+1/nu)-log_sd-cost
      : gamma_constant-cost-log_sd;
  if(nu>=1 && std::isfinite(ld) && x!=mean) {
    // The ordinary density also rounds the standardized difference
    // before taking its log. The unit allowance covers both roundings.
    const double h=nu*ged_detail::fast_radius_error(log_distance,scaled_log?log_sd:0,ls,w);
    const double budget=log ? 1e-11*std::max(1.,std::abs(ld)) : 1e-11;
    // Here h < log(2), so the largest cost over the interval is < 2*cost.
    if((log || ld>=ged_detail::zero_rounding_threshold()-1) &&
       h*cost>budget*.5)
      return ged_detail::density_precise(ged_detail::physical_radius(x,mean,sd,nu),nu,-log_sd,log,1e-10);
  }
  return log ? ld : std::exp(ld);
}
inline double pged(double q, double mean, double sd, double nu, bool lower_tail, bool log_p) {
  if (!(fs::valid(mean, sd, nu, 1.0)) || std::isnan(q)) return cppunivariate::special::kNaN;
  if(nu>=1e6 && std::isfinite(q) && q!=mean)
    return ged_detail::probability_precise(ged_detail::physical_radius(q,mean,sd,nu),nu,-std::log(2.),q<mean,lower_tail,log_p,true);
  const double log_distance=special::log_abs_difference(q,mean);
  const double log_sd=std::log(sd), ls=fs::ged_logscale(nu);
  const double la=log_distance-log_sd-ls;
  if(nu>=1 && std::isfinite(q) && q!=mean) {
    const double h=nu*ged_detail::fast_radius_error(log_distance,log_sd,ls,la);
    // For a=1/nu <= 1, Gamma(a,x) >= x^a exp(-x)/(x+1-a),
    // from log(1+t/x) <= t/x in its shifted integral. The
    // log upper-tail slope in log(x) is therefore <= x+1.
    // Evaluate that bound at the upper end of the radius interval.
    if(h*(1+std::exp(nu*la+h))>ged_detail::fast_tail_budget)
      return ged_detail::probability_precise(ged_detail::physical_radius(q,mean,sd,nu),nu,-std::log(2.),q<mean,lower_tail,log_p,true);
  }
  const double small = -std::log(2.0) + ged_detail::gamma_power_log(la,nu,false);
  const double result = ((q < mean) == lower_tail) ? small : fs::log1mexp(small);
  return log_p ? result : std::exp(result);
}
inline double qged(double p, double mean, double sd, double nu, bool lower_tail, bool log_p) {
  if (!(fs::valid(mean, sd, nu, 1.0))) return cppunivariate::special::kNaN;
  const double lp = fs::logprob(p, log_p);
  if (std::isnan(lp)) return lp;
  const double h=log_p ? (p+std::log(2.0))+2.3190468138462995584e-17 : 0;
  if ((!log_p && std::abs(p-0.5)<1e-8) || (log_p && std::abs(h)<1e-8)) {
    if(!log_p && p==0.5) return mean;
    const double mass=log_p ? std::expm1(h) : 2*(p-0.5);
    const double la=nu==1 ? std::log(-std::log1p(-std::abs(mass)))
                         : ged_detail::gamma_power_logq(std::log(std::abs(mass)),nu,true);
    return ged_detail::quantile_affine(la,mean,sd,nu,(mass>0)!=lower_tail,p,log_p);
  }
  const bool small = lp <= -std::log(2.0);
  const double tail = (small ? lp : fs::log1mexp(lp)) + std::log(2.0);
  // At a=1 the upper gamma probability is exp(-t).
  const double la = nu==1 ? std::log(-tail) : ged_detail::gamma_power_logq(tail,nu,false);
  return ged_detail::quantile_affine(la,mean,sd,nu,small==lower_tail,p,log_p);
}
template <typename Rng>
inline double rged(Rng &rng, double mean, double sd, double nu) {
  if (!(fs::valid(mean, sd, nu, 1.0))) return cppunivariate::special::kNaN;
  // Gamma augmentation: G_a = G_(a+1) U^(1/a). Apply the
  // GED power before exponentiation so U survives even for a near zero.
  const double a=1/nu;
  double logamplitude;
  if(nu>=16) {
    const double g=rng.rgamma(1+a,1);
    if(!(g>0) || !std::isnormal(g)) return special::kNaN;
    const double u=rng.runif(0,1);
    logamplitude=std::log(g)*a+std::log(u);
  } else {
    // Preserve the ordinary-shape primitive stream. A lost gamma draw is
    // a numerical failure, rather than a spurious atom at the center.
    const double r=rng.rgamma(a,1);
    if(!(r>0) || !std::isnormal(r)) return special::kNaN;
    logamplitude=std::log(r)/nu;
  }
  const double sign = cppunivariate::dens::sign1(rng.runif(0.0, 1.0) - 0.5);
  const double z = std::exp(fs::ged_logscale(nu) + logamplitude) * sign;
  return z * sd + mean;
}

} // namespace distributions
} // namespace cppunivariate
