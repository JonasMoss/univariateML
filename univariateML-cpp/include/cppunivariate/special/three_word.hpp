#pragma once
#include "special.hpp"
#include "tails.hpp"
#include <array>
namespace cppunivariate { namespace special { namespace three_word {
// A short nonoverlapping expansion. Every accumulation uses error-free
// TwoSum, every product uses FMA. Truncation discards fourth-order terms.
struct Number { double high, low=0, tiny=0; };
inline Number add_scalar(Number a,double b) {
 const double h=a.high+b, e=addition_residual(a.high,b,h);
 const double l=a.low+e, f=addition_residual(a.low,e,l);
 const double t=a.tiny+f;
 const double m=l+t, n=addition_residual(l,t,m);
 const double s=h+m, r=addition_residual(h,m,s);
 const double u=r+n;
 return {s,u,addition_residual(r,n,u)};
}
inline Number add(Number a,Number b) {
 a=add_scalar(a,b.high);a=add_scalar(a,b.low);return add_scalar(a,b.tiny);
}
inline Number negative(Number a) {return {-a.high,-a.low,-a.tiny};}
inline Number multiply(Number a,Number b) {
 Number out{0};
 const std::array<double,3> av{{a.high,a.low,a.tiny}},bv{{b.high,b.low,b.tiny}};
 for(int i=0;i<3;++i)for(int j=0;j<3-i;++j) {
  const double p=av[i]*bv[j];out=add_scalar(out,p);
  out=add_scalar(out,std::fma(av[i],bv[j],-p));
 }
 return out;
}
inline Number divide(Number a,Number b) {
 Number out{0};
 for(int i=0;i<3;++i) {
  const double q=a.high/b.high;
  if(!std::isfinite(q))return {q};
  out=add_scalar(out,q);a=add(a,negative(multiply({q},b)));
 }
 return out;
}
inline Number root(Number a) {
 Number out{std::sqrt(a.high)};
 for(int i=0;i<2;++i)out=add(out,divide(add(a,negative(multiply(out,out))),multiply({2},out)));
 return out;
}
inline Number log_two() {return {0.6931471805599453,2.3190468138462996e-17,5.707708438416212e-34};}
inline Number log_sqrt_two_pi() {return {0.9189385332046728,-3.8782941580672414e-17,-1.323971596849807e-33};}
inline Number log_pi() {return {1.1447298858494002,1.0265951162707826e-17,-1.3722612652165766e-34};}
inline Number stirling(Number x) {
 const std::array<Number,20> c{{
{0.08333333333333333,4.625929269271485e-18,2.5679065925163143e-34},
{-0.002777777777777778,1.0601087908747154e-19,3.4773735106991755e-36},
{0.0007936507936507937,6.883823317368282e-22,5.970764956557651e-40},
{-0.0005952380952380953,5.36938218754726e-20,-1.8342189946545105e-36},
{0.0008417508417508417,3.6870174889237694e-20,-6.889900895324708e-37},
{-0.0019175269175269176,1.0675702776872475e-19,6.568342495426554e-37},
{0.00641025641025641,2.2240044563805217e-19,1.975312763474088e-35},
{-0.029550653594771242,4.861760957508855e-19,1.316681517535326e-35},
{0.17964437236883057,-6.401600482710946e-19,9.779977439678332e-36},
{-1.3924322169059011,1.5837056989230303e-17,5.2056012685038854e-34},
{13.402864044168393,-6.154114101993966e-16,1.3610436598016077e-34},
{-156.84828462600203,9.391823141715389e-15,1.6570392471086158e-31},
{2193.1033333333335,-1.3339255626002948e-13,6.731613057885968e-31},
{-36108.77125372499,5.897583353514365e-13,7.049709715793733e-31},
{691472.268851313,2.5585296305158e-11,-1.2521722821640843e-27},
{-15238221.539407415,-8.76774522490625e-10,-1.9672353593923997e-26},
{382900751.39141417,-2.4082684757733585e-08,-4.344787055834085e-25},
{-10882266035.784391,3.141830930219749e-07,-2.013934646419947e-23},
{347320283765.00226,-6.048528997747748e-06,5.341649216919011e-23},
{-12369602142269.275,0.0009363732896507286,3.299942635958079e-20}
 }};
 const auto t=divide({1},x),t2=multiply(t,t);auto v=c.back();
 for(int i=18;i>=0;--i)v=add(c[i],multiply(t2,v));
 return multiply(t,v);
}
inline Number logarithm(Number x) {
 if(!(x.high>0) || !std::isfinite(x.high))return {std::log(x.high)};
 int exponent=0;std::frexp(x.high,&exponent);--exponent;
 const Number m{std::scalbn(x.high,-exponent),std::scalbn(x.low,-exponent),std::scalbn(x.tiny,-exponent)};
 const auto z=divide(add(m,{-1}),add(m,{1})),z2=multiply(z,z);
 auto power=z,total=z;
 // |z|<=1/3; the tail after 64 terms is < 2e-63.
 for(int k=1;k<64;++k){power=multiply(power,z2);total=add(total,divide(power,{double(2*k+1)}));}
 return add(multiply(total,{2}),multiply(log_two(),{double(exponent)}));
}
inline Number log1p(Number x) {
 if(std::abs(x.high)>.5)return logarithm(add({1},x));
 const auto z=divide(x,add({2},x)),z2=multiply(z,z);auto power=z,total=z;
 for(int k=1;k<64;++k){power=multiply(power,z2);total=add(total,divide(power,{double(2*k+1)}));}
 return multiply({2},total);
}
inline Number exponential(Number x) {
 if(!std::isfinite(x.high) || x.high>709 || x.high<-745)return {std::exp(x.high)};
 const int exponent=static_cast<int>(std::floor(x.high/.6931471805599453+.5));
 const auto r=add(x,negative(multiply(log_two(),{double(exponent)})));
 Number term{1},total{1};
 for(int k=1;k<=48;++k){term=divide(multiply(term,r),{double(k)});total=add(total,term);}
 return {std::scalbn(total.high,exponent),std::scalbn(total.low,exponent),std::scalbn(total.tiny,exponent)};
}
inline Number log_gamma(Number x) {
 if(!(x.high>0) || !std::isfinite(x.high))return {kNaN};
 Number correction{0};
 while(x.high<64){correction=add(correction,negative(logarithm(x)));x=add(x,{1});}
 return add(correction,add(add(multiply(add(x,{-.5}),logarithm(x)),negative(x)),add(log_sqrt_two_pi(),stirling(x))));
}
inline double margin(double magnitude) {
 const double eps=std::numeric_limits<double>::epsilon();
 // Practical floating-point margin, conditional on normal intermediates.
 // Covers the fixed series, <=64 recurrences and expansion truncations.
 return 32768*eps*eps*eps*(1+magnitude)+128*std::numeric_limits<double>::denorm_min();
}
} } }
