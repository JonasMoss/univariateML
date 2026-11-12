#pragma once
#include "skew_probability.hpp"
#include "three_word.hpp"
namespace cppunivariate { namespace special { namespace skew_coordinate {
struct Moment {three_word::Number value;double error;bool valid;};
inline Moment ged_uncached(double nu) {
 if(nu>=1e24) {
  const auto v=three_word::multiply({.5},three_word::root({3}));
  return {v,(32/nu)/nu+three_word::margin(1),true};
 }
 const auto z=three_word::divide({1},{nu});
 const auto a=three_word::log_gamma(z),b=three_word::log_gamma(three_word::multiply(z,{2})),c=three_word::log_gamma(three_word::multiply(z,{3}));
 const auto ln=three_word::add(b,three_word::negative(three_word::multiply(three_word::add(a,c),{.5})));
 const auto v=three_word::exponential(ln);
 const double e=three_word::margin(std::abs(a.high)+std::abs(b.high)+std::abs(c.high))*v.high;
 return {v,e,v.high>0 && v.high<1 && std::isfinite(e)};
}
inline Moment student_uncached(double nu) {
 // Cancel the leading Stirling terms before evaluating the half-ratio.
 // At enormous nu use the explicit normal-limit bound.
 const auto z=three_word::multiply(three_word::add({nu},{-1}),{.5});
 if(nu>=1e30) {
  const auto v=three_word::exponential(three_word::multiply({.5},three_word::add(three_word::log_two(),three_word::negative(three_word::log_pi()))));
  return {v,2/nu+three_word::margin(1),true};
 }
 // Shift the ratio before using the Stirling difference. Cancel the
 // leading terms algebraically, retaining log(1+1/(2z)).
 auto w=z;three_word::Number correction{0};
 while(w.high<64) {
  correction=three_word::add(correction,three_word::negative(three_word::log1p(three_word::divide({.5},w))));
  w=three_word::add(w,{1});
 }
 const auto h=three_word::divide({.5},w);
 const auto lr=three_word::add(three_word::multiply({.5},three_word::logarithm(w)),
   three_word::add(three_word::add(three_word::multiply(w,three_word::log1p(h)),{-.5}),
      three_word::add(three_word::stirling(three_word::add(w,{.5})),three_word::negative(three_word::stirling(w)))));
 const auto ln=three_word::add(three_word::multiply(three_word::add(three_word::logarithm(three_word::add({nu},{-2})),three_word::negative(three_word::log_pi())),{.5}),three_word::negative(three_word::add(correction,lr)));
 const auto v=three_word::exponential(ln);
 const double e=three_word::margin(std::abs(std::log(nu)))*v.high;
 return {v,e,v.high>0 && v.high<1 && std::isfinite(e)};
}
inline Moment ged(double nu) {
 static thread_local double last=0;
 static thread_local Moment saved{};
 if(nu!=last){saved=ged_uncached(nu);last=nu;}
 return saved;
}
inline Moment student(double nu) {
 static thread_local double last=0;
 static thread_local Moment saved{};
 if(nu!=last){saved=student_uncached(nu);last=nu;}
 return saved;
}
struct Coordinate {
 double value,log_normalizer,error,centering_error,square,log_value,log_error;
 bool thin,negative,valid;
 three_word::Number radius;
 double arithmetic_error;
};
inline Coordinate coordinate(double x,double mean,double sd,double xi,Moment m) {
 if(!m.valid || !std::isfinite(x))return {};
 const double sign=xi<=1?1:-1;
 const auto t=xi<=1?three_word::Number{xi}:three_word::divide({1},{xi});
 const auto t2=three_word::multiply(t,t),m2=three_word::multiply(m.value,m.value);
 const auto variance=three_word::add(three_word::multiply(three_word::add({1},three_word::negative(m2)),three_word::add({1},three_word::multiply(t2,t2))),three_word::multiply(three_word::add(three_word::multiply({2},m2),{-1}),t2));
 const auto sigma=three_word::root(variance);
 auto difference=three_word::add({x},{-mean});
 auto w=std::isfinite(difference.high)?three_word::divide(difference,{sd}):three_word::divide(three_word::add({x*.5},{-mean*.5}),{sd*.5});
 w=three_word::multiply(w,{sign});
 auto n=three_word::add(three_word::multiply(w,sigma),three_word::multiply(m.value,three_word::add(t2,{-1})));
 if(!std::isfinite(n.high) || !(sigma.high>0))return {};
 const bool thin=n.high>=0;
 const double centering=three_word::margin(std::abs(w.high)+m.value.high)+m.error+2*(std::abs(w.high)*m.error);
 // The one-sided tail formulas require a resolved side of the skew join.
 // At xi=1 the centering shift vanishes identically.
 if(xi!=1 && !(std::abs(n.high)>centering))return {};
 auto radius=thin && t2.high>0?three_word::divide(n,t2):n;
 if(radius.high<0)radius=three_word::negative(radius);
 const double value=thin && t2.high==0?kPosInf:radius.high;
 const double arithmetic=thin?centering/t2.high:centering;
 const double eps=std::numeric_limits<double>::epsilon();
 const double error=arithmetic+2*eps*value;
 const double ln=std::log(2.)+std::log(sigma.high)+sigma.low/sigma.high-std::log1p(t2.high)-std::log(sd);
 const auto logn=three_word::logarithm(n.high<0?three_word::negative(n):n);
 const auto lv=three_word::add(logn,thin?three_word::negative(three_word::multiply({2},three_word::logarithm(t))):three_word::Number{0});
 const double relative=centering/std::abs(n.high);
 return {value,ln,error,centering,t2.high,lv.high,relative<.5?2*relative+4*eps*(1+std::abs(lv.high)):kPosInf,thin,sign*(thin?1:-1)<0,true,radius,arithmetic};
}
// Central-mass representation avoids subtracting two nearly equal half
// probabilities on the broad side. Tail error is radius error times a
// hazard bound, with a reserved allowance for the base-tail evaluator.
template<class Upper,class Central,class Hazard>
inline double probability(const Coordinate& c,double xi,bool lower,bool log,Upper upper,Central central,Hazard hazard) {
 if(!c.valid || !std::isfinite(c.value))return kNaN;
 const double lt=xi<=1?std::log(xi):-std::log(xi);
 const double direct=std::log(2.)-std::log1p(c.square)+(c.thin?2*lt:0)+upper(c.value);
 auto tails=tails_from_log(direct,c.negative);
 double margin=c.error*hazard(c.value,c.error)+2e-12+32*std::numeric_limits<double>::epsilon();
 if(!c.thin) {
  const double opposite=log_sum_exp(2*lt,central(c.value))-std::log1p(c.square);
  if(opposite<direct) {
   tails=tails_from_log(opposite,!c.negative);
   // The central Student density is unbounded as nu approaches two.
   // Enclose the central mass directly; a fixed Lipschitz constant
   // would miss that amplification near the skew join.
   const double low=std::max(0.,c.value-c.error),high=c.value+c.error;
   const double left=log_sum_exp(2*lt,low>0?central(low):kNegInf)-std::log1p(c.square);
   const double right=log_sum_exp(2*lt,central(high))-std::log1p(c.square);
   margin=std::max(std::abs(left-opposite),std::abs(right-opposite))
       +2e-12+32*std::numeric_limits<double>::epsilon()*(1+std::abs(opposite));
  }
 }
 return skew_probability::enclose(std::min(tails.lower,tails.upper),margin,tails.lower<=tails.upper,lower,log);
}
} } }
