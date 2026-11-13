// Actuar scale is the exponential rate on 1/x (the reciprocal of the old
// actuar rate, which was the reciprocal of that same exponential rate).
#pragma once
#include "../../core/outcome.hpp"
#include "../exp/mle.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
enum class InvExpStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct InvExpResult { double scale; double logLik; int iterations;    InvExpStatus status = InvExpStatus::Ok;
};

inline FitOutcome outcome(const InvExpResult& r) {
  switch (r.status) {
    case InvExpStatus::Ok: return FitOutcome::estimate(true);
    case InvExpStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    // The adapter currently exposes this status as a plain range/fitting error.
    case InvExpStatus::Unbounded: return FitOutcome::failure(OutcomeReason::Evaluation);
    case InvExpStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}
inline InvExpResult invexp_mle(span x) {
  for (double v : x) {
    if (!(v > 0) || !std::isfinite(v)) {
      const double nan=std::numeric_limits<double>::quiet_NaN();
      return {nan,nan,0,InvExpStatus::InvalidInput};
    }
    if (!std::isfinite(1/v)) return {NAN,NAN,0,InvExpStatus::IterationFailure};
  }
  std::vector<double> y(x.size());
  double jacobian = 0;
  for (std::size_t i=0; i<x.size(); ++i) { y[i]=1.0/x[i]; jacobian -= 2*std::log(x[i]); }
  auto r = exp_mle(span{y.data(), y.size()});
  const InvExpStatus status = r.status == ExpStatus::Ok ? InvExpStatus::Ok :
    r.status == ExpStatus::Unbounded ? InvExpStatus::Unbounded :
    r.status == ExpStatus::InvalidInput ? InvExpStatus::InvalidInput : InvExpStatus::IterationFailure;
  return {r.rate, r.logLik+jacobian, 0, status};
}

inline InvExpResult invexp_mle(span x,double scale) {
  if(std::isnan(scale))return invexp_mle(x);
  for (double v : x) {
    if (!(v > 0 && std::isfinite(v))) return {NAN,NAN,0,InvExpStatus::InvalidInput};
    if (!std::isfinite(1/v)) return {NAN,NAN,0,InvExpStatus::IterationFailure};
  }
  std::vector<double> y;double jac=0;for(double v:x){y.push_back(1/v);jac-=2*std::log(v);}
  const auto r=exp_mle(span{y.data(),y.size()},scale);
  return {scale,r.logLik+jac,0,r.status==ExpStatus::Ok ? InvExpStatus::Ok :
    r.status==ExpStatus::Unbounded ? InvExpStatus::Unbounded :
    r.status==ExpStatus::InvalidInput ? InvExpStatus::InvalidInput : InvExpStatus::IterationFailure};
}

} // namespace cppunivariate
