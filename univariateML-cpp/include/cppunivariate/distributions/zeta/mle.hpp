// Infinite Zipf: strict exponential-family score, safeguarded by bisection.
#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/discrete_stop.hpp"
#include "finite_harmonic.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
enum class ZetaStatus { Ok, PointMass, InvalidInput, IterationFailure };
struct ZetaResult { double s, logLik; int iterations; ZetaStatus status;   StopDetail stop_detail{};
};
namespace zeta_detail {
// Analytic infinite extension of Zipf's Euler--Maclaurin log moments.
inline zipf_detail::HarmonicMoments moments(double s) {
  auto h=zipf_detail::moments(1024,s);
  const double t=std::log(1024.0), v=s-1, w=std::exp(-v*t);
  h.h+=w/v; h.l+=w*(t+1/v)/v;
  h.l2+=w*(t*t+2*t/v+2/(v*v))/v;
  const auto term=[&](int r,double c,double dc,double ddc) {
    const double z=std::exp(-(s+r)*t);
    h.h-=z*c; h.l-=z*(c*t-dc); h.l2-=z*(c*t*t-2*dc*t+ddc);
  };
  term(0,.5,0,0); term(1,-s/12,-1.0/12,0);
  term(3,s*(s+1)*(s+2)/720,(3*s*s+6*s+2)/720,(6*s+6)/720);
  return h;
}
inline double logz(double s) {
  if (s<=32) return std::log(special::zeta(s));
  double h=0;
  for (int k=2;k<=1024;++k) h+=std::exp(-s*std::log(static_cast<double>(k)));
  return std::log1p(h);
}
}
inline ZetaResult zeta_mle(ispan x,double tol=1e-10,int maxit=200, double held=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto stopped = [&]() -> ZetaResult {
  const double nan=special::kNaN;
  if (!x.size() || !(tol>0) || !std::isfinite(tol) || maxit<1)
    return {nan,nan,0,ZetaStatus::InvalidInput};
  double sum=0;
  for (int k:x) { if (k<1) return {nan,nan,0,ZetaStatus::InvalidInput}; sum+=std::log(static_cast<double>(k)); }
  if (!std::isnan(held)) {
    if (!(held>1) || !std::isfinite(held)) return {nan,nan,0,ZetaStatus::InvalidInput};
    return {held,-held*sum-x.size()*zeta_detail::logz(held),0,ZetaStatus::Ok};
  }
  if (sum==0) return {special::kPosInf,0,0,ZetaStatus::PointMass};
  const double target=sum/x.size();
  double lo=1,hi=2;
  for (;;) {
    const auto h=zeta_detail::moments(hi);
    if (h.l/h.h<=target) break;
    hi=1+2*(hi-1);
  }
  double s=2;
  for (int it=1;it<=maxit;++it) {
    s=lo+(hi-lo)/2;
    const auto h=zeta_detail::moments(s);
    const double score=h.l/h.h-target;
    if (score>0) lo=s; else hi=s;
    if (std::abs(score)<=tol*target || hi-lo<=tol*(s-1))
      return {s,-s*sum-x.size()*zeta_detail::logz(s),it,ZetaStatus::Ok};
  }
  return {s,-s*sum-x.size()*zeta_detail::logz(s),maxit,ZetaStatus::IterationFailure};

  }();
  if(stopped.status==ZetaStatus::PointMass) stopped.stop_detail=discrete_point(x[0]);
  return stopped;
}
}

namespace cppunivariate {
inline FitOutcome outcome(const ZetaResult& r) {
  switch(r.status) {
    case ZetaStatus::Ok: return FitOutcome::estimate(true);
    case ZetaStatus::PointMass: return FitOutcome::limit(true);
    case ZetaStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case ZetaStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
  }
  return {};
}
} // namespace cppunivariate
