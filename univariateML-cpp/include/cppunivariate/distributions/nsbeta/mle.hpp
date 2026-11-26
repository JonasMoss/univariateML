#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "physical_fit.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
struct NsbetaResult { double shape1,shape2,min,max,logLik; int iterations; BetaStatus status; bool unsupported_bounds=false; };

inline NsbetaResult nsbeta_mle(span x, double min, double max, double tol, int maxit, double a=std::numeric_limits<double>::quiet_NaN(), double b=std::numeric_limits<double>::quiet_NaN()) {
  if(std::isnan(min)||std::isnan(max)) {NsbetaResult out{a,b,min,max,NAN,0,BetaStatus::InvalidInput};out.unsupported_bounds=true;return out;}
  maxit = static_cast<int>(solver_budget(maxit));
  const NsbetaResult failure{a,b,min,max,NAN,0,BetaStatus::IterationFailure};
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min) && std::isfinite(max) && max>min;
  for(double v:x) valid=valid && std::isfinite(v) && v>min && v<max;
  if(!valid || x.empty())return {a,b,min,max,NAN,0,BetaStatus::InvalidInput};

  tol = std::min(tol,.125);
  const bool hold_a = !std::isnan(a), hold_b = !std::isnan(b);
  if ((hold_a && !(a>0 && std::isfinite(a))) || (hold_b && !(b>0 && std::isfinite(b))))
    return failure;
  if (!hold_a && !hold_b && std::all_of(x.begin(),x.end(),[&](double v) { return v==x[0]; }))
    return {a,b,min,max,NAN,0,BetaStatus::Degenerate};

  const double width_log = special::log_positive_distance(max,min);
  const auto public_likelihood = [&](double first, double second) {
    transformed_detail::Sum ll;
    for (double v:x) ll.push(special::affine_beta_log_density_positive(v,first,second,min,max));
    return ll.get();
  };
  // The unit-interval candidate also provides a warm start. Accept its
  // public values only after checking the original-coordinate problem.
  std::vector<double> y;
  y.reserve(x.size());
  for (double v:x) y.push_back((v-min)/(max-min));
  BetaResult candidate{a,b,NAN,0,BetaStatus::IterationFailure};
  if (hold_a && hold_b) {
    if (!std::isfinite(a+b)) return failure;
    double s=0,r=0;
    for (double v:y) { s+=std::log(v); r+=std::log1p(-v); }
    const double ll=(a-1)*s+(b-1)*r+x.size()*(special::lgamma(a+b)-special::lgamma(a)-special::lgamma(b)-width_log);
    candidate = {a,b,ll,0,BetaStatus::Ok};
  } else {
    candidate = beta_mle(span{y.data(),y.size()},tol,maxit,a,b);
    candidate.logLik -= x.size()*width_log;
  }

  BetaResult corrected = candidate;
  if (hold_a != hold_b) {
    const double held = hold_a ? a : b;
    transformed_detail::Sum sum;
    for (double v:x) {
      const auto logs = special::interval_log_coordinates(v,min,max);
      sum.push(hold_a ? -logs.upper : -logs.lower);
    }
    const double target = sum.get()/x.size();
    const double signal_floor = 128*std::numeric_limits<double>::denorm_min()/tol;
    if (!(target>signal_floor)) return failure;
    const auto verified = [&](double free) {
      if (!(free>0 && std::isfinite(free)) ||
          (free<20 && held<.01*free && held/(free+held)<signal_floor)) return false;
      const double radius = free*tol*.5;
      return nsbeta_detail::score_sign(free-radius,held,target,1) &&
             nsbeta_detail::score_sign(free+radius,held,target,-1);
    };
    const double old_shape = hold_a ? candidate.beta : candidate.alpha;
    if (candidate.status!=BetaStatus::Ok || !verified(old_shape)) {
      const double ratio = held/target, ceiling = std::numeric_limits<double>::max();
      double low=0, high=std::max(1.0,ratio<ceiling/2 ? 2*ratio : ceiling);
      if (!(nsbeta_detail::increment(high,held)<=target)) return failure;
      double free = 1/target;
      int iterations=0;
      if (held!=1) {
        for (; iterations<maxit; ++iterations) {
          free=low+(high-low)/2;
          if (nsbeta_detail::increment(free,held)>target) low=free; else high=free;
          if (high-low<=free*std::min(tol/8,1e-13)) break;
        }
        if (iterations==maxit) return failure;
      }
      if (!verified(free)) return failure;
      corrected = {hold_a?a:free,hold_b?b:free,NAN,iterations,BetaStatus::Ok};
    }
  } else if (!hold_a) {
    // When the ordinary scores separate all four faces, the candidate
    // already meets the physical-data contract. Concentrated samples need
    // the centered profile because these scores nearly cancel.
    const bool verified = candidate.status==BetaStatus::Ok &&
        nsbeta_detail::shape_box(x,min,max,candidate.alpha,candidate.beta,tol);
    if (!verified) {
      const auto physical = nsbeta_detail::joint_fit(x,min,max,tol,maxit,candidate.alpha+candidate.beta);
      if (physical.status!=BetaStatus::Ok) return failure;
      if (candidate.status!=BetaStatus::Ok ||
          std::abs(candidate.alpha-physical.alpha)>tol*.5*physical.alpha ||
          std::abs(candidate.beta-physical.beta)>tol*.5*physical.beta)
        corrected = physical;
    }
  }
  const double ll = public_likelihood(corrected.alpha,corrected.beta);
  if (!std::isfinite(ll)) return failure;
  if (!(std::abs(corrected.logLik-ll)<=1e-10*std::max(1.0,std::abs(ll)))) corrected.logLik=ll;
  return {corrected.alpha,corrected.beta,min,max,corrected.logLik,corrected.iterations,BetaStatus::Ok};
}

inline FitOutcome outcome(const NsbetaResult& r) {
  if(r.unsupported_bounds)return FitOutcome::failure(OutcomeReason::Unsupported);
  switch (r.status) {
    case BetaStatus::Ok: return FitOutcome::singleton(true);
    case BetaStatus::Degenerate: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case BetaStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case BetaStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
  }
  return {};
}


}
