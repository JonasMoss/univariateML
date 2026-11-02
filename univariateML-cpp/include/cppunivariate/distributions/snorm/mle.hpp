// Exact one-dimensional two-piece normal profile, returned in fGarch's
// standardised (mean, sd, xi) coordinates.

#pragma once
#include "cppunivariate/core/constants.hpp"
#include "../../core/precision.hpp"

#include "../../core/reparam.hpp"
#include "../../core/span.hpp"
#include "../../special/skewed_densities.hpp"

#include <algorithm>
#include <vector>
#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class SnormStatus { Ok = 0, BfgsFailure, InvalidStart, Unbounded,
                         RightHalfNormalBoundary, LeftHalfNormalBoundary };

struct SnormResult {
  double mu;
  double sigma;
  double xi;
  double logLik;
  int iterations;
  SnormStatus status;
};

namespace snorm_detail {
// Golden-section minimization on a finite bracket; caller establishes tail bounds.
template <typename Objective>
inline double minimize(Objective objective, double lo, double hi) {
  constexpr double ratio = 0.6180339887498948482;
  double l = hi-ratio*(hi-lo), r = lo+ratio*(hi-lo);
  double fl = objective(l), fr = objective(r);
  for (int it=0; it<120; ++it) {
    if (hi-lo <= 1e-11*(1+std::abs(l)+std::abs(r))) break;
    if (fl < fr) { hi=r; r=l; fr=fl; l=hi-ratio*(hi-lo); fl=objective(l); }
    else { lo=l; l=r; fl=fr; r=lo+ratio*(hi-lo); fr=objective(r); }
  }
  return fl < fr ? l : r;
}
// Scan before refinement: unlike a single golden search, retains competing modes.
template <typename Objective>
inline double scan(Objective objective, double lo, double hi, int cells=160) {
  std::vector<double> values;
  for (int i=0; i<=cells; ++i) values.push_back(objective(lo+(hi-lo)*i/cells));
  int best=0;
  for (int i=1; i<=cells; ++i) if(values[i]<values[best]) best=i;
  double arg=lo+(hi-lo)*best/cells, value=values[best];
  for (int i=1; i<cells; ++i) if(values[i]<=values[i-1] && values[i]<=values[i+1]) {
    const double t=minimize(objective,lo+(hi-lo)*(i-1)/cells,lo+(hi-lo)*(i+1)/cells);
    const double f=objective(t);
    if(f<value) { value=f; arg=t; }
  }
  return arg;
}

inline SnormResult held_fit(span x, double mu, double sd, double xi) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double inf=std::numeric_limits<double>::infinity();
  SnormResult out{nan,nan,nan,nan,0,SnormStatus::InvalidStart};
  const bool hm=!std::isnan(mu), hs=!std::isnan(sd), hx=!std::isnan(xi);
  if(x.size()==0 || (hm && !std::isfinite(mu)) ||
     (hs && !(sd>0 && std::isfinite(sd))) || (hx && !(xi>0 && std::isfinite(xi)))) return out;
  double lo=inf, hi=-inf;
  for(double v:x) { if(!std::isfinite(v)) return out; lo=std::min(lo,v); hi=std::max(hi,v); }
  if(!hs && lo==hi && (!hm || mu==lo)) { out.status=SnormStatus::Unbounded; return out; }
  if(hx && xi==1) {
    double mean=mu, sigma=sd, sum=0;
    if(!hm) { for(double v:x) sum+=v-lo; mean=lo+sum/x.size(); }
    double ss=0;
    for(double v:x) ss+=(v-mean)*(v-mean);
    if(!hs) sigma=std::sqrt(ss/x.size());
    if(!(sigma>0)) return out;
    return {mean,sigma,xi,-static_cast<double>(x.size())*(std::log(sigma)+0.5*std::log(2*cppunivariate::pi))-ss/(2*sigma*sigma),0,SnormStatus::Ok};
  }
  const double origin=hm ? mu : lo+(hi-lo)/2;
  double unit=hs ? sd : std::max(hi-lo,std::max(std::abs(lo-origin),std::abs(hi-origin)));
  if(!(unit>0) || !std::isfinite(unit)) return out;
  std::vector<double> y;
  for(double v:x) y.push_back((v-origin)/unit);
  const double n=static_cast<double>(x.size()), yl=(lo-origin)/unit, yh=(hi-origin)/unit;
  const double m1=std::sqrt(2/cppunivariate::pi), h=std::sqrt(1-m1*m1);
  double best_mu=0, best_sd=1;
  auto profile=[&](double lx) {
    const double skew=hx ? xi : std::exp(lx);
    const double q=skew-1/skew, shift=m1*q;
    const double scale=std::hypot(1.0,h*q);
    auto cost=[&](double mean, double sigma) {
      double neg=n*(std::log(sigma)-std::log(scale));
      for(double v:y) {
        const double d=(v-mean)/sigma;
        double z=d*scale+shift;
        // Rationalize cancellation at a held-moment half-normal endpoint.
        if(d*shift<0 && std::abs(z)<0.01*std::abs(shift))
          z=(d*d+q*q*(h*d-m1)*(h*d+m1))/(d*scale-shift);
        neg-=dens::log_dsnorm_core(z,skew);
      }
      return neg;
    };
    auto at_scale=[&](double ls) {
      const double sigma=hs ? sd/unit : std::exp(ls);
      auto location=[&](double mean) { return cost(mean,sigma); };
      // Mode of a location-only log-concave likelihood lies between data extremes.
      const double mean=hm ? 0 : minimize(location,yl+sigma*shift/scale,yh+sigma*shift/scale);
      best_mu=mean; best_sd=sigma;
      return cost(mean,sigma);
    };
    if(hs) return at_scale(0);
    const double ls=minimize(at_scale,-32,32);
    return at_scale(ls);
  };
  const double lx=hx ? std::log(xi) : scan(profile,-18,18,180);
  const double neg=profile(lx);
  out={hm ? mu : origin+unit*best_mu,hs ? sd : unit*best_sd,
       hx ? xi : std::exp(lx),-neg-n*std::log(unit),0,SnormStatus::Ok};
  if(!std::isfinite(out.logLik)) { out.status=SnormStatus::BfgsFailure; return out; }
  if(!hx) for(int side : {-1,1}) {
    // Half-normal with mean=c+side*m1*b, sd=h*b. Retain held moments.
    double b=hs ? sd/unit/h : 0;
    double sum=0, ss=0;
    const double centre=hm ? 0 : (side==1 ? yl : yh);
    for(double v:y) { sum+=v-centre; ss+=(v-centre)*(v-centre); }
    if(!hs) {
      const double linear=hm ? side*m1*sum : 0;
      b=(linear+std::sqrt(linear*linear+4*n*ss))/(2*n);
      if(hm) b=std::max(b,side==1 ? -yl/m1 : yh/m1);
    }
    if(!(b>0)) continue;
    const double c=hm ? -side*m1*b : centre;
    if((side==1 && c>yl+1e-12) || (side==-1 && c<yh-1e-12)) continue;
    double ll=n*(0.5*std::log(2/cppunivariate::pi)-std::log(b)-std::log(unit));
    for(double v:y) {
      ll-=0.5*((v-c)/b)*((v-c)/b);
      // If both moments are held, the mode approaches this endpoint from
      // inside the sample: the shrinking side retains a finite residual.
      if(hm && hs && std::abs(v-c)<=1e-14*(1+std::abs(c)))
        ll-=m1*m1/(8*h*h*h*h);
    }
    if(ll>=out.logLik-1e-8*n) {
      out.logLik=ll;
      out.status=side==1 ? SnormStatus::RightHalfNormalBoundary : SnormStatus::LeftHalfNormalBoundary;
    }
  }
  if(out.status==SnormStatus::Ok &&
     ((!hs && std::abs(std::log(best_sd))>31.9) || (!hx && std::abs(lx)>17.99)))
    out.status=SnormStatus::BfgsFailure;
  return out;
}

// Recursive derivative isolation: between successive derivative roots a
// polynomial is monotone. Bisection therefore finds every sign-changing root;
// derivative roots themselves are retained to include all multiple roots.
inline CPPUNIV_EXTENDED_REAL evaluate(const std::vector<CPPUNIV_EXTENDED_REAL>& p, CPPUNIV_EXTENDED_REAL t) {
  CPPUNIV_EXTENDED_REAL y = 0;
  for (std::size_t i = p.size(); i-- > 0;) y = y * t + p[i];
  return y;
}
inline std::vector<CPPUNIV_EXTENDED_REAL> roots(std::vector<CPPUNIV_EXTENDED_REAL> p) {
  while (p.size() > 1 && p.back() == 0) p.pop_back();
  if (p.size() <= 1) return {};
  std::vector<CPPUNIV_EXTENDED_REAL> d(p.size() - 1);
  for (std::size_t i = 1; i < p.size(); ++i) d[i-1] = i * p[i];
  auto cuts = roots(d);
  cuts.push_back(0); cuts.push_back(1);
  std::sort(cuts.begin(), cuts.end());
  std::vector<CPPUNIV_EXTENDED_REAL> out = cuts;
  for (std::size_t i = 1; i < cuts.size(); ++i) {
    CPPUNIV_EXTENDED_REAL l = cuts[i-1], r = cuts[i], fl = evaluate(p, l);
    const CPPUNIV_EXTENDED_REAL fr = evaluate(p, r);
    if ((fl < 0 && fr > 0) || (fl > 0 && fr < 0)) {
      for (int j = 0; j < 80; ++j) {
        const CPPUNIV_EXTENDED_REAL m = (l+r)/2, fm = evaluate(p, m);
        if (m == l || m == r) break;
        if ((fl < 0) == (fm < 0)) { l = m; fl = fm; }
        else r = m;
      }
      out.push_back((l+r)/2);
    }
  }
  std::sort(out.begin(), out.end());
  out.erase(std::unique(out.begin(), out.end()), out.end());
  return out;
}
inline std::vector<CPPUNIV_EXTENDED_REAL> multiply(const std::vector<CPPUNIV_EXTENDED_REAL>& a,
                                         const std::vector<CPPUNIV_EXTENDED_REAL>& b) {
  std::vector<CPPUNIV_EXTENDED_REAL> p(a.size()+b.size()-1, 0);
  for (std::size_t i=0; i<a.size(); ++i)
    for (std::size_t j=0; j<b.size(); ++j) p[i+j] += a[i]*b[j];
  return p;
}
} // namespace snorm_detail

inline SnormResult snorm_mle(span x, double /*tol*/, int /*maxit*/,
    double fixed_mean=std::numeric_limits<double>::quiet_NaN(),
    double fixed_sd=std::numeric_limits<double>::quiet_NaN(),
    double fixed_xi=std::numeric_limits<double>::quiet_NaN()) {
  if(!std::isnan(fixed_mean) || !std::isnan(fixed_sd) || !std::isnan(fixed_xi))
    return snorm_detail::held_fit(x,fixed_mean,fixed_sd,fixed_xi);
  constexpr double nan = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {nan,nan,nan,nan,0,SnormStatus::InvalidStart};
  std::vector<CPPUNIV_EXTENDED_REAL> y;
  for (double v : x) {
    if (!std::isfinite(v)) return {nan,nan,nan,nan,0,SnormStatus::InvalidStart};
    y.push_back(v);
  }
  std::sort(y.begin(), y.end());
  const CPPUNIV_EXTENDED_REAL origin = y.front(), scale = y.back()-origin;
  if (scale == 0) return {nan,nan,nan,nan,0,SnormStatus::Unbounded};
  // Affine normalisation keeps the degree-seven coefficients bounded.
  for (auto& v : y) v = (v-origin)/scale;
  // Left moments about 0 and right moments about 1 avoid cancellation
  // at the half-normal endpoints; never subtract prefix sums from totals.
  std::vector<CPPUNIV_EXTENDED_REAL> s(n+1,0), ss(n+1,0), z(n+1,0), zz(n+1,0);
  for (std::size_t i=0; i<n; ++i) { s[i+1]=s[i]+y[i]; ss[i+1]=ss[i]+y[i]*y[i]; }
  for (std::size_t i=n; i-- > 0;) { z[i]=z[i+1]+(1-y[i]); zz[i]=zz[i+1]+(1-y[i])*(1-y[i]); }
  CPPUNIV_EXTENDED_REAL best = std::numeric_limits<CPPUNIV_EXTENDED_REAL>::infinity();
  CPPUNIV_EXTENDED_REAL mode=0, ca=0, cb=0;
  auto consider = [&](CPPUNIV_EXTENDED_REAL c, std::size_t k) {
    const CPPUNIV_EXTENDED_REAL A=std::max(CPPUNIV_EXTENDED_LITERAL(0.0,0.0L), ss[k]-2*c*s[k]+k*c*c);
    const CPPUNIV_EXTENDED_REAL B=std::max(CPPUNIV_EXTENDED_LITERAL(0.0,0.0L), zz[k]-2*(1-c)*z[k]+(n-k)*(1-c)*(1-c));
    // Minimising the cube root of Q is equivalent and avoids cubing.
    const CPPUNIV_EXTENDED_REAL a=std::cbrt(A), b=std::cbrt(B), q=a+b;
    if (q < best || (q == best && c>0 && c<1)) {
      best=q; mode=c; ca=a; cb=b;
    }
  };
  consider(0,0); consider(1,n);
  for (std::size_t k=1; k<n; ++k) {
    const CPPUNIV_EXTENDED_REAL l=y[k-1], h=y[k]-l;
    if (h == 0) continue;
    const std::vector<CPPUNIV_EXTENDED_REAL> A={ss[k]-2*l*s[k]+k*l*l,
                                      2*h*(k*l-s[k]), k*h*h};
    const std::vector<CPPUNIV_EXTENDED_REAL> B={zz[k]-2*(1-l)*z[k]+(n-k)*(1-l)*(1-l),
                                      -2*h*((n-k)*(1-l)-z[k]), (n-k)*h*h};
    const std::vector<CPPUNIV_EXTENDED_REAL> u={k*l-s[k],k*h}, v={(n-k)*(1-l)-z[k],-static_cast<CPPUNIV_EXTENDED_REAL>(n-k)*h};
    auto p=snorm_detail::multiply(snorm_detail::multiply(snorm_detail::multiply(u,u),u),snorm_detail::multiply(B,B));
    const auto q=snorm_detail::multiply(snorm_detail::multiply(snorm_detail::multiply(v,v),v),snorm_detail::multiply(A,A));
    for (std::size_t j=0; j<p.size(); ++j) p[j]-=q[j];
    for (CPPUNIV_EXTENDED_REAL t : snorm_detail::roots(p)) consider(l+h*t,k);
  }
  const double ll=static_cast<double>(n*(CPPUNIV_EXTENDED_LITERAL(0.5,0.5L)*std::log(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L)/cppunivariate::pi)-std::log(scale)
                     -CPPUNIV_EXTENDED_LITERAL(1.5,1.5L)*std::log(best)+CPPUNIV_EXTENDED_LITERAL(0.5,0.5L)*std::log(static_cast<CPPUNIV_EXTENDED_REAL>(n))-CPPUNIV_EXTENDED_LITERAL(0.5,0.5L)));
  if (mode == 0) return {nan,nan,nan,ll,0,SnormStatus::RightHalfNormalBoundary};
  if (mode == 1) return {nan,nan,nan,ll,0,SnormStatus::LeftHalfNormalBoundary};
  // For the two-piece normal: E[X]=c+m1*(b-a),
  // Var[X]=a*a-a*b+b*b-m1*m1*(b-a)^2, xi=sqrt(b/a).
  const CPPUNIV_EXTENDED_REAL factor=scale*std::sqrt(best/n), a=ca*factor, b=cb*factor;
  const CPPUNIV_EXTENDED_REAL m1=std::sqrt(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L)/cppunivariate::pi);
  const double mu=static_cast<double>(origin+scale*mode+m1*(b-a));
  const double sigma=static_cast<double>(std::sqrt(a*a-a*b+b*b-m1*m1*(b-a)*(b-a)));
  return {mu,sigma,static_cast<double>(std::sqrt(b/a)),ll,0,SnormStatus::Ok};
}

}  // namespace cppunivariate
