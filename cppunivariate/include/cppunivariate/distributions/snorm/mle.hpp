// Exact one-dimensional two-piece normal profile, returned in fGarch's
// standardised (mean, sd, xi) coordinates.

#pragma once

#include "../../core/reparam.hpp"
#include "../../core/span.hpp"
#include "../../special/skewed_densities.hpp"

#include <algorithm>
#include <vector>
#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class SnormStatus { Ok = 0, BfgsFailure, InvalidStart,
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
// Recursive derivative isolation: between successive derivative roots a
// polynomial is monotone. Bisection therefore finds every sign-changing root;
// derivative roots themselves are retained to include all multiple roots.
inline long double evaluate(const std::vector<long double>& p, long double t) {
  long double y = 0;
  for (std::size_t i = p.size(); i-- > 0;) y = y * t + p[i];
  return y;
}
inline std::vector<long double> roots(std::vector<long double> p) {
  while (p.size() > 1 && p.back() == 0) p.pop_back();
  if (p.size() <= 1) return {};
  std::vector<long double> d(p.size() - 1);
  for (std::size_t i = 1; i < p.size(); ++i) d[i-1] = i * p[i];
  auto cuts = roots(d);
  cuts.push_back(0); cuts.push_back(1);
  std::sort(cuts.begin(), cuts.end());
  std::vector<long double> out = cuts;
  for (std::size_t i = 1; i < cuts.size(); ++i) {
    long double l = cuts[i-1], r = cuts[i], fl = evaluate(p, l);
    const long double fr = evaluate(p, r);
    if ((fl < 0 && fr > 0) || (fl > 0 && fr < 0)) {
      for (int j = 0; j < 80; ++j) {
        const long double m = (l+r)/2, fm = evaluate(p, m);
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
inline std::vector<long double> multiply(const std::vector<long double>& a,
                                         const std::vector<long double>& b) {
  std::vector<long double> p(a.size()+b.size()-1, 0);
  for (std::size_t i=0; i<a.size(); ++i)
    for (std::size_t j=0; j<b.size(); ++j) p[i+j] += a[i]*b[j];
  return p;
}
} // namespace snorm_detail

inline SnormResult snorm_mle(span x, double /*tol*/, int /*maxit*/) {
  constexpr double nan = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n < 2) return {nan,nan,nan,nan,0,SnormStatus::InvalidStart};
  std::vector<long double> y;
  for (double v : x) {
    if (!std::isfinite(v)) return {nan,nan,nan,nan,0,SnormStatus::InvalidStart};
    y.push_back(v);
  }
  std::sort(y.begin(), y.end());
  const long double origin = y.front(), scale = y.back()-origin;
  if (!(scale > 0)) return {nan,nan,nan,nan,0,SnormStatus::InvalidStart};
  // Affine normalisation keeps the degree-seven coefficients bounded.
  for (auto& v : y) v = (v-origin)/scale;
  // Left moments about 0 and right moments about 1 avoid cancellation
  // at the half-normal endpoints; never subtract prefix sums from totals.
  std::vector<long double> s(n+1,0), ss(n+1,0), z(n+1,0), zz(n+1,0);
  for (std::size_t i=0; i<n; ++i) { s[i+1]=s[i]+y[i]; ss[i+1]=ss[i]+y[i]*y[i]; }
  for (std::size_t i=n; i-- > 0;) { z[i]=z[i+1]+(1-y[i]); zz[i]=zz[i+1]+(1-y[i])*(1-y[i]); }
  long double best = std::numeric_limits<long double>::infinity();
  long double mode=0, ca=0, cb=0;
  auto consider = [&](long double c, std::size_t k) {
    const long double A=std::max(0.0L, ss[k]-2*c*s[k]+k*c*c);
    const long double B=std::max(0.0L, zz[k]-2*(1-c)*z[k]+(n-k)*(1-c)*(1-c));
    // Minimising the cube root of Q is equivalent and avoids cubing.
    const long double a=std::cbrt(A), b=std::cbrt(B), q=a+b;
    if (q < best || (q == best && c>0 && c<1)) {
      best=q; mode=c; ca=a; cb=b;
    }
  };
  consider(0,0); consider(1,n);
  for (std::size_t k=1; k<n; ++k) {
    const long double l=y[k-1], h=y[k]-l;
    if (h == 0) continue;
    const std::vector<long double> A={ss[k]-2*l*s[k]+k*l*l,
                                      2*h*(k*l-s[k]), k*h*h};
    const std::vector<long double> B={zz[k]-2*(1-l)*z[k]+(n-k)*(1-l)*(1-l),
                                      -2*h*((n-k)*(1-l)-z[k]), (n-k)*h*h};
    const std::vector<long double> u={k*l-s[k],k*h}, v={(n-k)*(1-l)-z[k],-static_cast<long double>(n-k)*h};
    auto p=snorm_detail::multiply(snorm_detail::multiply(snorm_detail::multiply(u,u),u),snorm_detail::multiply(B,B));
    const auto q=snorm_detail::multiply(snorm_detail::multiply(snorm_detail::multiply(v,v),v),snorm_detail::multiply(A,A));
    for (std::size_t j=0; j<p.size(); ++j) p[j]-=q[j];
    for (long double t : snorm_detail::roots(p)) consider(l+h*t,k);
  }
  const double ll=static_cast<double>(n*(0.5L*std::log(2.0L/M_PI)-std::log(scale)
                     -1.5L*std::log(best)+0.5L*std::log(static_cast<long double>(n))-0.5L));
  if (mode == 0) return {nan,nan,nan,ll,0,SnormStatus::RightHalfNormalBoundary};
  if (mode == 1) return {nan,nan,nan,ll,0,SnormStatus::LeftHalfNormalBoundary};
  // For the two-piece normal: E[X]=c+m1*(b-a),
  // Var[X]=a*a-a*b+b*b-m1*m1*(b-a)^2, xi=sqrt(b/a).
  const long double factor=scale*std::sqrt(best/n), a=ca*factor, b=cb*factor;
  const long double m1=std::sqrt(2.0L/M_PI);
  const double mu=static_cast<double>(origin+scale*mode+m1*(b-a));
  const double sigma=static_cast<double>(std::sqrt(a*a-a*b+b*b-m1*m1*(b-a)*(b-a)));
  return {mu,sigma,static_cast<double>(std::sqrt(b/a)),ll,0,SnormStatus::Ok};
}

}  // namespace cppunivariate
