// Exact one-dimensional two-piece normal profile, returned in fGarch's
// standardised (mean, sd, xi) coordinates.

#pragma once
#include "../../core/outcome.hpp"
#include "../hnorm/dpqr.hpp"
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
                         RightHalfNormalBoundary, LeftHalfNormalBoundary, NumericalFailure };

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
struct Root {
  CPPUNIV_EXTENDED_REAL lo, hi;
  CPPUNIV_EXTENDED_REAL mid() const { return (lo+hi)/2; }
};
inline std::vector<Root> roots(std::vector<CPPUNIV_EXTENDED_REAL> p,double tol,int maxit,bool& resolved) {
  maxit = static_cast<int>(solver_budget(maxit));
  while (p.size() > 1 && p.back() == 0) p.pop_back();
  if (p.size() <= 1) return {};
  std::vector<CPPUNIV_EXTENDED_REAL> d(p.size() - 1);
  for (std::size_t i = 1; i < p.size(); ++i) d[i-1] = i * p[i];
  auto cuts = roots(d,tol,maxit,resolved);
  cuts.push_back({0,0}); cuts.push_back({1,1});
  const auto order=[](const Root& a,const Root& b) {return a.mid()<b.mid();};
  std::sort(cuts.begin(),cuts.end(),order);
  auto out=cuts;
  for (std::size_t i = 1; i < cuts.size(); ++i) {
    CPPUNIV_EXTENDED_REAL l = cuts[i-1].mid(), r = cuts[i].mid(), fl = evaluate(p, l);
    const CPPUNIV_EXTENDED_REAL fr = evaluate(p, r);
    if ((fl < 0 && fr > 0) || (fl > 0 && fr < 0)) {
      for (int j = 0; j < solver_budget(maxit); ++j) {
        const CPPUNIV_EXTENDED_REAL m = (l+r)/2, fm = evaluate(p, m);
        if (m == l || m == r) break;
        if ((fl < 0) == (fm < 0)) { l = m; fl = fm; }
        else r = m;
      }
      if(r-l>std::min(1e-12,tol))resolved=false;
      out.push_back({l,r});
    }
  }
  std::sort(out.begin(),out.end(),order);
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

inline SnormResult snorm_mle_impl(span x, double tol, int maxit,
    double fixed_mean,double fixed_sd,double fixed_xi) {
  maxit = static_cast<int>(solver_budget(maxit));
  if (!(tol>0) || !std::isfinite(tol) || maxit<1)
    return {NAN,NAN,NAN,NAN,0,SnormStatus::InvalidStart};
  if(!std::isnan(fixed_mean) || !std::isnan(fixed_sd) || !std::isnan(fixed_xi))
    return snorm_certificate::fit(x,tol,maxit,fixed_mean,fixed_sd,fixed_xi);
  constexpr double nan = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {nan,nan,nan,nan,0,SnormStatus::InvalidStart};
  std::vector<CPPUNIV_EXTENDED_REAL> y;
  for (double v : x) {
    if (!std::isfinite(v)) return {nan,nan,nan,nan,0,SnormStatus::InvalidStart};
    y.push_back(v);
  }
  std::sort(y.begin(), y.end());
  const CPPUNIV_EXTENDED_REAL origin = y.front();
  // Represent the range as two half-ranges when the full range overflows.
  const bool halve = !std::isfinite(static_cast<double>(y.back()) - static_cast<double>(origin));
  if (halve) for (auto& v : y) v /= 2;
  const CPPUNIV_EXTENDED_REAL base = halve ? origin/2 : origin;
  const CPPUNIV_EXTENDED_REAL scale = y.back()-base;
  if (scale == 0) return {nan,nan,nan,nan,0,SnormStatus::Unbounded};
  // Affine normalisation keeps the degree-seven coefficients bounded.
  for (auto& v : y) v = (v-base)/scale;
  // Left moments about 0 and right moments about 1 avoid cancellation
  // at the half-normal endpoints; never subtract prefix sums from totals.
  std::vector<CPPUNIV_EXTENDED_REAL> s(n+1,0), ss(n+1,0), z(n+1,0), zz(n+1,0);
  for (std::size_t i=0; i<n; ++i) { s[i+1]=s[i]+y[i]; ss[i+1]=ss[i]+y[i]*y[i]; }
  for (std::size_t i=n; i-- > 0;) { z[i]=z[i+1]+(1-y[i]); zz[i]=zz[i+1]+(1-y[i])*(1-y[i]); }
  CPPUNIV_EXTENDED_REAL best = std::numeric_limits<CPPUNIV_EXTENDED_REAL>::infinity();
  CPPUNIV_EXTENDED_REAL mode=0, ca=0, cb=0;
  snorm_detail::Root root{0,0};
  std::size_t split=0;
  auto consider = [&](snorm_detail::Root interval, std::size_t k) {
    const auto c=interval.mid();
    const CPPUNIV_EXTENDED_REAL A=std::max(CPPUNIV_EXTENDED_LITERAL(0.0,0.0L), ss[k]-2*c*s[k]+k*c*c);
    const CPPUNIV_EXTENDED_REAL B=std::max(CPPUNIV_EXTENDED_LITERAL(0.0,0.0L), zz[k]-2*(1-c)*z[k]+(n-k)*(1-c)*(1-c));
    // Minimising the cube root of Q is equivalent and avoids cubing.
    const CPPUNIV_EXTENDED_REAL a=std::cbrt(A), b=std::cbrt(B), q=a+b;
    if (q < best || (q == best && c>0 && c<1)) {
      best=q; mode=c; ca=a; cb=b; root=interval; split=k;
    }
  };
  consider({0,0},0); consider({1,1},n);
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
    const auto roots=snorm_detail::roots(p,tol,maxit,resolved);
    if(!resolved)return {nan,nan,nan,nan,maxit,SnormStatus::BfgsFailure};
    for (const auto& t : roots) consider({l+h*t.lo,l+h*t.hi},k);
  }
  const double ll=static_cast<double>(n*(CPPUNIV_EXTENDED_LITERAL(0.5,0.5L)*std::log(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L)/cppunivariate::pi)-std::log(scale)-(halve?std::log(2.):0)
                     -CPPUNIV_EXTENDED_LITERAL(1.5,1.5L)*std::log(best)+CPPUNIV_EXTENDED_LITERAL(0.5,0.5L)*std::log(static_cast<CPPUNIV_EXTENDED_REAL>(n))-CPPUNIV_EXTENDED_LITERAL(0.5,0.5L)));
  if(mode==0 || mode==1) {
    SnormResult face={nan,nan,nan,ll,0,mode==0?SnormStatus::RightHalfNormalBoundary:SnormStatus::LeftHalfNormalBoundary};
    face.stop_detail=limit_detail({mode==0?*std::min_element(x.begin(),x.end()):*std::max_element(x.begin(),x.end()),static_cast<double>((scale*std::sqrt(best*best*best/n))*(halve?2:1))},ll,mode==0?StopModel::Hnorm:StopModel::ReflectedHnorm);
    if(face.stop_detail.limit_coef.size()!=2){face.status=SnormStatus::NumericalFailure;return face;}
    double public_ll=0;
    for(double v:x)public_ll+=distributions::dhnorm(mode==0?v:-v,mode==0?face.stop_detail.limit_coef[0]:-face.stop_detail.limit_coef[0],face.stop_detail.limit_coef[1],true);
    if(!std::isfinite(public_ll)){face.status=SnormStatus::NumericalFailure;face.stop_detail={};return face;}
    face.logLik=public_ll;
    face.stop_detail=limit_detail(face.stop_detail.limit_coef,public_ll,mode==0?StopModel::Hnorm:StopModel::ReflectedHnorm);
    return face;
  }
  // For the two-piece normal: E[X]=c+m1*(b-a),
  // Var[X]=a*a-a*b+b*b-m1*m1*(b-a)^2, xi=sqrt(b/a).
  const CPPUNIV_EXTENDED_REAL factor=std::sqrt(best/n), a=ca*factor, b=cb*factor;
  const CPPUNIV_EXTENDED_REAL m1=std::sqrt(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L)/cppunivariate::pi);
  const double mu=static_cast<double>((base+scale*(mode+m1*(b-a)))*(halve?2:1));
  const double sigma=static_cast<double>((scale*std::sqrt(a*a-a*b+b*b-m1*m1*(b-a)*(b-a)))*(halve?2:1));
  const double skew=static_cast<double>(std::sqrt(b/a));
  // Within one data gap the left and right residual sums are monotone
  // in opposite directions. Propagate their root-bracket bounds to the
  // two scales, then to the three public coefficients.
  const auto left=[&](CPPUNIV_EXTENDED_REAL c) {
    return std::cbrt(std::max(ExtendedReal(0),ss[split]-2*c*s[split]+split*c*c));
  };
  const auto right=[&](CPPUNIV_EXTENDED_REAL c) {
    const auto d=1-c;
    return std::cbrt(std::max(ExtendedReal(0),zz[split]-2*d*z[split]+(n-split)*d*d));
  };
  const auto al=left(root.lo), ah=left(root.hi);
  const auto bl=right(root.hi), bh=right(root.lo);
  const auto fl=std::sqrt((al+bl)/n), fh=std::sqrt((ah+bh)/n);
  const auto aa=al*fl, ab=ah*fh, ba=bl*fl, bb=bh*fh;
  const auto loc_width=(halve?2:1)*(scale*(root.hi-root.lo+m1*((ab-aa)+(bb-ba))));
  const auto scale_width=(halve?2:1)*(scale*((ab-aa)+(bb-ba)));
  const auto skew_width=std::sqrt(bh/al)-std::sqrt(bl/ah);
  SnormResult result{mu,sigma,skew,ll,0,SnormStatus::Ok};
  // Standard deviation is a norm from a positive definite quadratic form.
  // Each coordinate direction has norm sqrt(1-m1*m1) < 1.
  if (!(std::isfinite(mu) && sigma>0 && std::isfinite(sigma) &&
        skew>0 && std::isfinite(skew) &&
        loc_width<=tol*std::max(std::abs(mu),sigma)/2 &&
        scale_width<=tol*sigma/2 && skew_width<=tol*skew/2)) {
    result.status=SnormStatus::BfgsFailure;
    record_accuracy(result,tol,{mu,sigma,skew},{std::max(std::abs(mu),sigma),sigma,skew});
  }
  return result;
}

inline SnormResult snorm_mle(span x,double tol=1e-10,int maxit=200,
    double fixed_mean=std::numeric_limits<double>::quiet_NaN(),
    double fixed_sd=std::numeric_limits<double>::quiet_NaN(),
    double fixed_xi=std::numeric_limits<double>::quiet_NaN()) {
  constexpr double method_floor=4*std::numeric_limits<double>::epsilon();
  const bool too_tight=tol>0 && tol<method_floor;
  auto r=snorm_mle_impl(x,too_tight?method_floor:tol,maxit,fixed_mean,fixed_sd,fixed_xi);
  const bool hm=std::isfinite(fixed_mean),hs=std::isfinite(fixed_sd),hx=std::isfinite(fixed_xi);
  // The algebraic roots are mapped to binary64 public coefficients.
  // Keep the finite representative when their requested precision is finer
  // than the arithmetic used in the bracket and mapping.
  if (r.status==SnormStatus::Ok && too_tight && !(hm && hs && hx)) {
    r.status=SnormStatus::BfgsFailure;
    r.stop_detail.accuracy_stop=AccuracyStop::Tolerance;
    r.stop_detail.attainable_tol=method_floor;
    record_accuracy(r,tol,{r.mu,r.sigma,r.xi},
        {hm?0:std::max(std::abs(r.mu),r.sigma),hs?0:r.sigma,hx?0:r.xi});
  }
  return r;
}

}  // namespace cppunivariate

namespace cppunivariate {
inline bool family_point(const SnormResult&, const FitCandidate& c) {
  return c.model == StopModel::None && c.coef.size() == 3 && std::isfinite(c.coef[0]) &&
         std::isfinite(c.coef[1]) && std::isfinite(c.coef[2]) && c.coef[1] > 0 && c.coef[2] > 0;
}
// Certificate ties list padded search witnesses and finite roots with
// likelihood bounds against half-normal faces; no family point is checked.
inline bool accurate_point(const SnormResult&, const FitCandidate&) { return false; }
// Constant data with sd held and mean free: both half-normal faces attain the
// sharp density bound, recorded as a finite global upper bound (every other
// comparison leaves it infinite). Equal limiting suprema, proved.
inline bool snorm_equal_faces(const SnormResult& r) {
  if (!std::isfinite(r.stop_detail.loglik_upper) || r.stop_detail.candidates.size() < 2) return false;
  for (const auto& c : r.stop_detail.candidates) if (!face_candidate(c)) return false;
  return true;
}

inline FitOutcome outcome(const SnormResult& r) {
  if (r.stop_detail.accuracy_stop==AccuracyStop::Tolerance)
    return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop==AccuracyStop::Conditioning)
    return FitOutcome::failure(OutcomeReason::Conditioning);
  if (r.status == SnormStatus::RightHalfNormalBoundary ||
      r.status == SnormStatus::LeftHalfNormalBoundary) {
    const auto& d = r.stop_detail;
    if (d.limit_coef.empty() || !std::isfinite(d.limit_loglik))
      return FitOutcome::failure(OutcomeReason::Evaluation);
    for (double v : d.limit_coef) if (!std::isfinite(v))
      return FitOutcome::failure(OutcomeReason::Evaluation);
    for (const auto& c : d.candidates) {
      if (!valid_stop_coefficients(c.model, c.coef) ||
          !std::isfinite(c.loglik_lower) || !std::isfinite(c.loglik_upper) ||
          c.loglik_lower > c.loglik_upper)
        return FitOutcome::failure(OutcomeReason::Evaluation);
      for (double v : c.coef) if (!std::isfinite(v))
        return FitOutcome::failure(OutcomeReason::Evaluation);
    }
  }
  if (r.status != SnormStatus::Unbounded && r.stop_detail.budget_exhausted) return FitOutcome::budget();
  if (r.status != SnormStatus::Unbounded && r.stop_detail.comparison_tie) return snorm_equal_faces(r) ? FitOutcome::limit(true) : comparison_outcome_of(r);
  switch (r.status) {
    case SnormStatus::Ok: return FitOutcome::estimate(true);
    case SnormStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
    case SnormStatus::BfgsFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
    case SnormStatus::InvalidStart: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case SnormStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case SnormStatus::RightHalfNormalBoundary: return FitOutcome::limit();
    case SnormStatus::LeftHalfNormalBoundary: return FitOutcome::limit();
  }
  return {};
}
} // namespace cppunivariate
