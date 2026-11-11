#pragma once
#include "compensated_log.hpp"
#include <array>
namespace cppunivariate { namespace special { namespace skew_moments {
struct Moment {compensated_log::Number value;double error;bool valid;};
inline compensated_log::Number stirling(compensated_log::Number x) {
 const std::array<compensated_log::Number,12> coefficient{{{0.08333333333333333,4.625929269271485e-18},{-0.002777777777777778,1.0601087908747154e-19},{0.0007936507936507937,6.883823317368282e-22},{-0.0005952380952380953,5.36938218754726e-20},{0.0008417508417508417,3.6870174889237694e-20},{-0.0019175269175269176,1.0675702776872475e-19},{0.00641025641025641,2.2240044563805217e-19},{-0.029550653594771242,4.861760957508855e-19},{0.17964437236883057,-6.401600482710946e-19},{-1.3924322169059011,1.5837056989230303e-17},{13.402864044168393,-6.154114101993966e-16},{-156.84828462600203,9.391823141715389e-15}}};
 const auto t=compensated_log::divide({1,0},x),t2=compensated_log::multiply(t,t);
 auto value=coefficient.back();
 for(int k=10;k>=0;--k)value=compensated_log::add(coefficient[static_cast<std::size_t>(k)],compensated_log::multiply(t2,value));
 return compensated_log::multiply(t,value);
}
// The first omitted Stirling term at x>=32 is below 5.2e-35.
inline compensated_log::Number log_gamma(compensated_log::Number x) {
 if(!(x.high>0) || !std::isfinite(x.high))return {kNaN,0};
 compensated_log::Number correction{0,0};
 while(x.high<32) {correction=compensated_log::add(correction,compensated_log::negative(compensated_log::logarithm(x)));x=compensated_log::add(x,{1,0});}
 const auto main=compensated_log::multiply(compensated_log::add(x,{-.5,0}),compensated_log::logarithm(x));
 return compensated_log::add(correction,compensated_log::add(compensated_log::add(main,compensated_log::negative(x)),compensated_log::add({0.9189385332046728,-3.8782941580672414e-17},stirling(x))));
}
// log Gamma(z+1/2)/Gamma(z). The alternating Laplace expansion has
// remainder bounded by its first omitted term (<8e-33 at z>=32).
inline compensated_log::Number gamma_half_ratio(compensated_log::Number z) {
 compensated_log::Number correction{0,0};
 while(z.high<32) {
  const auto ratio=compensated_log::divide({.5,0},z);
  correction=compensated_log::add(correction,compensated_log::negative(compensated_log::logarithm(compensated_log::add({1,0},ratio))));
  z=compensated_log::add(z,{1,0});
 }
 const std::array<compensated_log::Number,11> coefficient{{{-0.125,0.0},{0.005208333333333333,2.8912057932946783e-19},{-0.0015625,8.673617379884036e-20},{0.0011858258928571428,9.29316147844718e-20},{-0.001681857638888889,9.637352644315594e-20},{0.0038341175426136365,-1.182766006347823e-19},{-0.012819730318509616,5.337610695313253e-19},{0.059100405375162764,-3.23815048849004e-18},{-0.359287374159869,-2.6122894697062506e-17},{2.784861777958117,1.8698493046318425e-16},{-26.80572169735318,1.691768418476429e-16}}};
 const auto t=compensated_log::divide({1,0},z),t2=compensated_log::multiply(t,t);
 auto value=coefficient.back();
 for(int k=9;k>=0;--k)value=compensated_log::add(coefficient[static_cast<std::size_t>(k)],compensated_log::multiply(t2,value));
 return compensated_log::add(correction,compensated_log::add(compensated_log::multiply(compensated_log::logarithm(z),{.5,0}),compensated_log::multiply(t,value)));
}
inline Moment student(double nu) {
 const double eps=std::numeric_limits<double>::epsilon();
 // Gamma log-convexity gives m_inf*sqrt(1-1/(nu-1)) <= m_nu < m_inf,
 // hence 0 <= m_inf-m_nu <= 2/nu. Use the limit only below the DD margin.
 if(2/nu<=64*eps*eps)return {{0.7978845608028654,-4.98465440455546e-17},2/nu+32*eps*eps,true};
 const auto z=compensated_log::multiply(compensated_log::sum(nu,-1),{.5,0});
 const auto ln=compensated_log::add(compensated_log::multiply(compensated_log::add(compensated_log::logarithm(compensated_log::sum(nu,-2)),compensated_log::negative({1.1447298858494002,1.0265951162707826e-17})),{.5,0}),compensated_log::negative(gamma_half_ratio(z)));
 const auto value=compensated_log::exponential_negative(ln);
 const double error=512*eps*eps*(1+std::abs(ln.high)+std::abs(std::log(nu)))*value.high;
 return {value,error,value.high>0 && value.high<1 && std::isfinite(error)};
}
inline Moment ged(double nu) {
 const double eps=std::numeric_limits<double>::epsilon();
 // The log-Gamma(1+z) series bounds |m-m_inf| by 32/nu^2 for nu>=8.
 const double bound=(32/nu)/nu;
 if(nu>=8 && bound<=64*eps*eps)return {{0.8660254037844386,5.0175421109034514e-17},bound+32*eps*eps,true};
 const auto z=compensated_log::divide({1,0},{nu,0});
 const auto a=log_gamma(z),b=log_gamma(compensated_log::multiply(z,{2,0})),c=log_gamma(compensated_log::multiply(z,{3,0}));
 const auto ln=compensated_log::add(b,compensated_log::negative(compensated_log::multiply(compensated_log::add(a,c),{.5,0})));
 const auto value=compensated_log::exponential_negative(ln);
 const double error=512*eps*eps*(1+std::abs(a.high)+std::abs(b.high)+std::abs(c.high))*value.high;
 return {value,error,value.high>0 && value.high<1 && std::isfinite(error)};
}
} } }
