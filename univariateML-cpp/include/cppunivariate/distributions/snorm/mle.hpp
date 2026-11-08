// Exact one-dimensional two-piece normal profile, returned in fGarch's
// standardised (mean, sd, xi) coordinates.

#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/face_compare.hpp"
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
 StopDetail stop_detail{}; };

#include "certificate.hpp"

namespace snorm_detail {
// Retained for the existing skew-t/GED consumers; snorm uses certificates.
// Golden-section minimization on a finite bracket; caller establishes tail bounds.
template <typename Objective>
inline double minimize(Objective objective, double lo, double hi,int maxit=200) {
  maxit = static_cast<int>(solver_budget(maxit));
  constexpr double ratio = 0.6180339887498948482;
  double l = hi-ratio*(hi-lo), r = lo+ratio*(hi-lo);
  double fl = objective(l), fr = objective(r);
  for (int it=0; it<solver_budget(maxit); ++it) {
    if (hi-lo <= 1e-11*(1+std::abs(l)+std::abs(r))) break;
    if (fl < fr) { hi=r; r=l; fr=fl; l=hi-ratio*(hi-lo); fl=objective(l); }
    else { lo=l; l=r; fl=fr; r=lo+ratio*(hi-lo); fr=objective(r); }
  }
  return hi-lo<=1e-11*(1+std::abs(l)+std::abs(r))?(fl < fr ? l : r):NAN;
}
// Scan before refinement: unlike a single golden search, retains competing modes.
template <typename Objective>
inline double scan(Objective objective, double lo, double hi, int cells=160,int maxit=200) {
  maxit = static_cast<int>(solver_budget(maxit));
  std::vector<double> values;
  for (int i=0; i<=cells; ++i) values.push_back(objective(lo+(hi-lo)*i/cells));
  int best=0;
  for (int i=1; i<=cells; ++i) if(values[i]<values[best]) best=i;
  double arg=lo+(hi-lo)*best/cells, value=values[best];
  for (int i=1; i<cells; ++i) if(values[i]<=values[i-1] && values[i]<=values[i+1]) {
    const double t=minimize(objective,lo+(hi-lo)*(i-1)/cells,lo+(hi-lo)*(i+1)/cells,maxit);
    if(!std::isfinite(t))return NAN;
    const double f=objective(t);
    if(f<value) { value=f; arg=t; }
  }
  return arg;
}

// Recursive derivative isolation: between successive derivative roots a
// polynomial is monotone. Bisection therefore finds every sign-changing root;
// derivative roots themselves are retained to include all multiple roots.
inline CPPUNIV_EXTENDED_REAL evaluate(const std::vector<CPPUNIV_EXTENDED_REAL>& p, CPPUNIV_EXTENDED_REAL t) {
  CPPUNIV_EXTENDED_REAL y = 0;
  for (std::size_t i = p.size(); i-- > 0;) y = y * t + p[i];
  return y;
}
inline std::vector<CPPUNIV_EXTENDED_REAL> roots(std::vector<CPPUNIV_EXTENDED_REAL> p,int maxit,bool& resolved) {
  maxit = static_cast<int>(solver_budget(maxit));
  while (p.size() > 1 && p.back() == 0) p.pop_back();
  if (p.size() <= 1) return {};
  std::vector<CPPUNIV_EXTENDED_REAL> d(p.size() - 1);
  for (std::size_t i = 1; i < p.size(); ++i) d[i-1] = i * p[i];
  auto cuts = roots(d,maxit,resolved);
  cuts.push_back(0); cuts.push_back(1);
  std::sort(cuts.begin(), cuts.end());
  std::vector<CPPUNIV_EXTENDED_REAL> out = cuts;
  for (std::size_t i = 1; i < cuts.size(); ++i) {
    CPPUNIV_EXTENDED_REAL l = cuts[i-1], r = cuts[i], fl = evaluate(p, l);
    const CPPUNIV_EXTENDED_REAL fr = evaluate(p, r);
    if ((fl < 0 && fr > 0) || (fl > 0 && fr < 0)) {
      for (int j = 0; j < solver_budget(maxit); ++j) {
        const CPPUNIV_EXTENDED_REAL m = (l+r)/2, fm = evaluate(p, m);
        if (m == l || m == r) break;
        if ((fl < 0) == (fm < 0)) { l = m; fl = fm; }
        else r = m;
      }
      if(r-l>1e-12*(1+std::abs(l)+std::abs(r)))resolved=false;
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

inline SnormResult snorm_mle(span x, double /*tol*/, int maxit,
    double fixed_mean=std::numeric_limits<double>::quiet_NaN(),
    double fixed_sd=std::numeric_limits<double>::quiet_NaN(),
    double fixed_xi=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(!std::isnan(fixed_mean) || !std::isnan(fixed_sd) || !std::isnan(fixed_xi))
    return snorm_certificate::fit(x,maxit,fixed_mean,fixed_sd,fixed_xi);
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
    bool resolved=true;
    const auto roots=snorm_detail::roots(p,maxit,resolved);
    if(!resolved)return {nan,nan,nan,nan,maxit,SnormStatus::BfgsFailure};
    for (CPPUNIV_EXTENDED_REAL t : roots) consider(l+h*t,k);
  }
  const double ll=static_cast<double>(n*(CPPUNIV_EXTENDED_LITERAL(0.5,0.5L)*std::log(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L)/cppunivariate::pi)-std::log(scale)
                     -CPPUNIV_EXTENDED_LITERAL(1.5,1.5L)*std::log(best)+CPPUNIV_EXTENDED_LITERAL(0.5,0.5L)*std::log(static_cast<CPPUNIV_EXTENDED_REAL>(n))-CPPUNIV_EXTENDED_LITERAL(0.5,0.5L)));
  if(mode==0 || mode==1) {
    SnormResult face={nan,nan,nan,ll,0,mode==0?SnormStatus::RightHalfNormalBoundary:SnormStatus::LeftHalfNormalBoundary};
    face.stop_detail=limit_detail({static_cast<double>(origin+scale*mode),static_cast<double>(scale*std::sqrt(best*best*best/n))},ll,mode==0?StopModel::Hnorm:StopModel::ReflectedHnorm);
    return face;
  }
  // For the two-piece normal: E[X]=c+m1*(b-a),
  // Var[X]=a*a-a*b+b*b-m1*m1*(b-a)^2, xi=sqrt(b/a).
  const CPPUNIV_EXTENDED_REAL factor=std::sqrt(best/n), a=ca*factor, b=cb*factor;
  const CPPUNIV_EXTENDED_REAL m1=std::sqrt(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L)/cppunivariate::pi);
  const double mu=static_cast<double>(origin+scale*(mode+m1*(b-a)));
  const double sigma=static_cast<double>(scale*std::sqrt(a*a-a*b+b*b-m1*m1*(b-a)*(b-a)));
  return {mu,sigma,static_cast<double>(std::sqrt(b/a)),ll,0,SnormStatus::Ok};
}

}  // namespace cppunivariate
