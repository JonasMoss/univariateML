#pragma once
#include "special.hpp"
#include "tails.hpp"
#include <cmath>
namespace cppunivariate { namespace special { namespace compensated_log {
struct Number {double high,low;};
inline Number sum(double a,double b) {
 const double high=a+b;
 return std::isfinite(high)?Number{high,addition_residual(a,b,high)}:Number{high,0};
}
inline Number add(Number a,Number b) {
 const Number first=sum(a.high,b.high),last=sum(first.low,a.low+b.low);
 return sum(first.high,last.high+last.low);
}
inline Number negative(Number a) {return {-a.high,-a.low};}
inline Number multiply(Number a,Number b) {
 const double high=a.high*b.high;
 if(!std::isfinite(high))return {high,0};
 return sum(high,std::fma(a.high,b.high,-high)+a.high*b.low+a.low*b.high+a.low*b.low);
}
inline Number divide(Number a,Number b) {
 const double high=a.high/b.high;
 if(!std::isfinite(high))return {high,0};
 const Number residual=add(a,negative(multiply({high,0},b)));
 return sum(high,(residual.high+residual.low)/b.high);
}
inline Number log_two() {return {.69314718055994528623,2.3190468138462995584e-17};}
// m in [1,2), hence |(m-1)/(m+1)| <= 1/3. After 48 terms
// the absolute atanh-series remainder is below 2*(1/3)^97/(97*(1-1/9)).
inline Number logarithm(Number x) {
 if(!(x.high>0) || !std::isfinite(x.high))return {kNaN,0};
 int exponent=0;std::frexp(x.high,&exponent);--exponent;
 const Number m{std::scalbn(x.high,-exponent),std::scalbn(x.low,-exponent)};
 const Number z=divide(add(m,{-1,0}),add(m,{1,0})),z2=multiply(z,z);
 Number power=z,total=z;
 for(int k=1;k<48;++k) {
  power=multiply(power,z2);
  total=add(total,divide(power,{double(2*k+1),0}));
 }
 return add(multiply(total,{2,0}),multiply(log_two(),{double(exponent),0}));
}
inline Number logarithm(double x) {return logarithm(Number{x,0});}
// Bounded reduced exponential series; its argument has magnitude <= log(2)/2.
inline Number exponential_negative(Number x) {
 if(std::isnan(x.high) || x.high>0)return {kNaN,0};
 if(x.high < -746)return {0,0};
 const int exponent=static_cast<int>(std::floor(x.high/.6931471805599453+.5));
 const Number r=add(x,negative(multiply(log_two(),{double(exponent),0})));
 Number term{1,0},total{1,0};
 for(int k=1;k<=48;++k) {term=divide(multiply(term,r),{double(k),0});total=add(total,term);}
 return {std::scalbn(total.high,exponent),std::scalbn(total.low,exponent)};
}
// Form expm1 directly near zero: subtracting an extended exp from one
// would leave only a single-word displacement when the two nearly agree.
inline Number log_one_minus_exp(Number x) {
 if(!(x.high<0))return {kNaN,0};
 if(x.high>=-.5) {
  Number term=x,total=x;
  for(int k=2;k<=48;++k) {term=divide(multiply(term,x),{double(k),0});total=add(total,term);}
  return logarithm(negative(total));
 }
 return logarithm(add({1,0},negative(exponential_negative(x))));
}
inline Number log_distance(double x,double location) {
 if(std::isfinite(x-location))return logarithm(sum(x,-location));
 return add(logarithm(sum(x*.5,-location*.5)),log_two());
}
inline double margin(double magnitude) {
 const double eps=std::numeric_limits<double>::epsilon();
 return 128*eps*eps*(1+magnitude)+64*std::numeric_limits<double>::denorm_min();
}
} } }
