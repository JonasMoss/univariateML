#pragma once
#include "../../core/transformed_data.hpp"
#include <array>
#include <cmath>

namespace cppunivariate {
namespace tnorm_detail {

struct NearFaceFit {
  double mean=NAN, sd=NAN;
  int iterations=0;
};

inline std::array<transformed_detail::Pair,5> near_face_integrals(
    transformed_detail::Pair linear, transformed_detail::Pair quadratic, bool bounded) {
  std::array<transformed_detail::Pair,5> integrals{};
  if (bounded) {
    // Taylor coefficients of exp(linear*u + quadratic*u^2). Cauchy's
    // estimate on |u|=4 bounds the integrated degree-80 tail by 2e-40
    // on the admitted finite domain.
    transformed_detail::Pair previous{0,0}, current{1,0};
    for (int j=0; j<80; ++j) {
      for (int k=j%2; k<5; k+=2)
        integrals[k]=transformed_detail::add(integrals[k],
            transformed_detail::div(current,{double(j+k+1),0}));
      const auto next=transformed_detail::div(transformed_detail::add(
          transformed_detail::mul(linear,current),
          transformed_detail::mul(transformed_detail::mul({2,0},quadratic),previous)),
          {double(j+1),0});
      previous=current;
      current=next;
    }
  } else {
    // Integrate the alternating Taylor polynomial for exp(quadratic*u^2)
    // against exp(-rate*u) on [0, infinity). The absolute remainder is
    // bounded by the integral of the first omitted term.
    const auto rate=transformed_detail::neg(linear);
    const auto factor=transformed_detail::div(quadratic,transformed_detail::mul(rate,rate));
    auto start=transformed_detail::div({1,0},rate);
    for (int k=0; k<5; ++k) {
      auto term=start;
      for (int j=0; j<20; ++j) {
        integrals[k]=transformed_detail::add(integrals[k],term);
        const auto ratio=transformed_detail::div(
            {double((k+2*j+1)*(k+2*j+2)),0},{double(j+1),0});
        term=transformed_detail::mul(term,transformed_detail::mul(ratio,factor));
      }
      start=transformed_detail::div(transformed_detail::mul(start,{double(k+1),0}),rate);
    }
  }
  for (int k=4; k>0; --k) integrals[k]=transformed_detail::div(integrals[k],integrals[0]);
  return integrals;
}

inline NearFaceFit refine_near_face(span x,double a,double b,double mean,double sd,
    double held_mean,double face_rate,double tol,int maxit) {
  const bool bounded=std::isfinite(a) && std::isfinite(b);
  const bool held=std::isfinite(held_mean);
  if ((held && !bounded) || (bounded && (2*(a*.5)!=a || 2*(b*.5)!=b))) return {};
  const double direction=std::isfinite(a)?1:-1;
  const transformed_detail::Pair origin=bounded ?
      transformed_detail::add({a*.5,0},{b*.5,0}) :
      transformed_detail::Pair{direction>0?a:b,0};
  const transformed_detail::Pair scale=bounded ?
      transformed_detail::add({b*.5,0},{-a*.5,0}) :
      transformed_detail::Pair{direction/std::abs(face_rate),0};
  if (!(std::isfinite(scale.hi) && std::abs(scale.hi)>=std::numeric_limits<double>::min()))
    return {};
  const auto coordinate=[&](double v) {
    return transformed_detail::div(transformed_detail::add({v,0},
        transformed_detail::neg(origin)),scale);
  };
  const auto location=coordinate(held?held_mean:mean);
  if (held && !(std::abs(location.hi)<=8)) return {};
  const auto loading=held?transformed_detail::mul({-2,0},location):transformed_detail::Pair{};
  const double ratio=scale.hi/sd;
  const double q=std::isfinite(ratio)?-.5*ratio*ratio:0;
  transformed_detail::Pair quadratic{q,0}, linear=held?
      transformed_detail::mul(loading,quadratic):
      transformed_detail::mul(location,{-2*q,0});
  if (!std::isfinite(linear.hi) || q==0)
    linear=held?transformed_detail::Pair{}:transformed_detail::mul({face_rate,0},scale);
  const double linear_limit=bounded?4:.1;
  const double quadratic_limit=bounded?.01:.0002;
  const auto in_domain=[&](double t,double k) {
    return std::abs(t+(bounded?0:1))<linear_limit && k<=0 && k>-quadratic_limit;
  };
  if (!in_domain(linear.hi,quadratic.hi)) return {};
  transformed_detail::Pair u1{},u2{};
  for (double v:x) {
    const auto u=coordinate(v);
    u1=transformed_detail::add(u1,u);
    u2=transformed_detail::add(u2,transformed_detail::mul(u,u));
  }
  const double n=double(x.size());
  u1=transformed_detail::div(u1,{n,0});
  u2=transformed_detail::div(u2,{n,0});
  if (!std::isfinite(u2.hi)) return {};
  const double eps=std::numeric_limits<double>::epsilon();
  const double noise=5e-26+64*n*eps*eps*(1+std::abs(u1.hi)+u2.hi);
  const auto public_coefficients=[&](transformed_detail::Pair t,transformed_detail::Pair k) {
    const auto mu=transformed_detail::add(origin,transformed_detail::mul(scale,
        transformed_detail::div(t,transformed_detail::mul({-2,0},k))));
    return NearFaceFit{held?held_mean:transformed_detail::value(mu),
        std::abs(scale.hi)/std::sqrt(-2*transformed_detail::value(k)),0};
  };
  for (int it=0; it<maxit; ++it) {
    const auto m=near_face_integrals(linear,quadratic,bounded);
    const auto g1=transformed_detail::add(u1,transformed_detail::neg(m[1]));
    const auto g2=transformed_detail::add(u2,transformed_detail::neg(m[2]));
    const double g=held?transformed_detail::value(transformed_detail::add(g2,
        transformed_detail::mul(loading,g1))):g2.hi;
    const double l=loading.hi;
    const double v1=m[2].hi-m[1].hi*m[1].hi;
    const double cov=m[3].hi-m[1].hi*m[2].hi;
    const double v2=m[4].hi-m[2].hi*m[2].hi;
    // The normalized density is bounded below by exp(-8.01) times
    // uniform on [-1,1], or .9*exp(-1.1002) times uniform on [0,1].
    // The uniform information matrices have least eigenvalues >= 4/45
    // and > 1/500, respectively. For held location the loading adds
    // a linear term to the quadratic sufficient statistic.
    const double information=bounded?2.9e-5:5e-4;
    const double radius=2*(std::hypot(held?0:g1.hi,g)+(1+std::abs(l))*noise)/information;
    if (quadratic.hi < -radius && quadratic.hi-radius > -quadratic_limit &&
        std::abs(linear.hi+(bounded?0:1))+radius*(1+std::abs(l))<linear_limit) {
      const auto result=public_coefficients(linear,quadratic);
      double min_mean=INFINITY,max_mean=-INFINITY,min_sd=INFINITY,max_sd=0;
      for (double dt:{-radius,radius}) for (double dk:{-radius,radius}) {
        const auto k=transformed_detail::add(quadratic,{dk,0});
        const auto t=held?transformed_detail::mul(loading,k):
            transformed_detail::add(linear,{dt,0});
        const auto end=public_coefficients(t,k);
        if (!(std::isfinite(end.mean) && std::isfinite(end.sd) && end.sd>0)) return {};
        min_mean=std::min(min_mean,end.mean);
        max_mean=std::max(max_mean,end.mean);
        min_sd=std::min(min_sd,end.sd);
        max_sd=std::max(max_sd,end.sd);
      }
      const double mean_scale=std::max({min_sd,min_mean,-max_mean});
      if (tol>=8*eps && std::isfinite(result.mean) && std::isfinite(result.sd) &&
          std::max(result.mean-min_mean,max_mean-result.mean)<=.75*tol*mean_scale &&
          std::max(result.sd-min_sd,max_sd-result.sd)<=.75*tol*min_sd)
        return {result.mean,result.sd,it+1};
    }
    const double det=v1*v2-cov*cov;
    const double dk=held?g/(v2+2*l*cov+l*l*v1):(v1*g-cov*g1.hi)/det;
    const double dt=held?0:(v2*g1.hi-cov*g)/det;
    const auto next_k=transformed_detail::add(quadratic,{dk,0});
    const auto next_t=held?transformed_detail::mul(loading,next_k):
        transformed_detail::add(linear,{dt,0});
    if (!in_domain(next_t.hi,next_k.hi) ||
        (next_k.hi==quadratic.hi && next_k.lo==quadratic.lo &&
         next_t.hi==linear.hi && next_t.lo==linear.lo)) break;
    quadratic=next_k;
    linear=next_t;
  }
  return {};
}
}
}
