#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "physical_fit.hpp"
#include "dpqr.hpp"
#include "../pareto4/free_threshold.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
struct NsbetaResult { double shape1,shape2,min,max,logLik; int iterations; BetaStatus status; bool unsupported_bounds=false; };

namespace nsbeta_detail {
inline NsbetaResult free_bounds(span x,double min,double max,double tol,int maxit,double a,double b) {
  NsbetaResult failure{a,b,min,max,NAN,0,BetaStatus::InvalidInput};
  maxit=static_cast<int>(solver_budget(maxit));
  const bool free_min=std::isnan(min),free_max=std::isnan(max);
  if(x.empty() || !(tol>0 && std::isfinite(tol)) || maxit<1)return failure;
  for(double value:{a,b})
    if(!std::isnan(value) && !(value>0 && std::isfinite(value)))return failure;
  if((!free_min && !std::isfinite(min)) || (!free_max && !std::isfinite(max)))return failure;
  for(double v:x)
    if(!std::isfinite(v) || (!free_min && v<min) || (!free_max && v>max))return failure;
  const auto extrema=std::minmax_element(x.begin(),x.end());
  const double low=*extrema.first,high=*extrema.second,n=x.size();
  const auto unbounded=[&]() {
    NsbetaResult out=failure;
    out.status=BetaStatus::Degenerate;
    return out;
  };
  const auto point=[&](double lo,double hi,double first,double second) {
    NsbetaResult out=failure;
    if(!(lo<hi && std::isfinite(lo) && std::isfinite(hi)))return out;
    transformed_detail::Sum ll;
    for(double v:x)ll.push(distributions::dnsbeta(v,first,second,lo,hi,true));
    if(std::isfinite(ll.get()))out={first,second,lo,hi,ll.get(),0,BetaStatus::Ok};
    return out;
  };

  if(free_min && free_max) {
    if(std::isnan(a) || std::isnan(b) || a<1 || b<1 || low==high)return unbounded();
    if(a==1)min=low;
    if(b==1)max=high;
    if(a==1 && b==1)return point(min,max,a,b);
  } else {
    const double held_shape=free_max?a:b;
    const double free_side_shape=free_max?b:a;
    const bool contact=free_max?low==min:high==max;
    if(contact && held_shape>1)return failure;
    if(contact && (std::isnan(held_shape) || held_shape<1))return unbounded();
    if(std::isnan(free_side_shape) || free_side_shape<1)return unbounded();
    if(contact && low==high)return unbounded();
  }
  failure.status=BetaStatus::IterationFailure;
  const double width_tol=std::min(tol/16,1e-12);
  const double outer_tol=std::min(tol/16,1e-7);
  if(std::isnan(min) && std::isnan(max)) {
    const double range=high-low;
    if(!(range>0 && std::isfinite(range)))return failure;
    std::vector<std::pair<double,double>> data;
    for(double v:x)data.emplace_back((v-low)/range,(high-v)/range);
    const auto location=[&](double inverse_width) {
      const double limit=1-inverse_width;
      const auto score=[&](double shift) {
        if(shift==0)return 1;
        if(shift==limit)return -1;
        transformed_detail::Sum total;
        double magnitude=0;
        for(const auto& v:data) {
          const double z=shift+inverse_width*v.first;
          const double w=(limit-shift)+inverse_width*v.second;
          const double pos=(a-1)/z,neg=(b-1)/w;
          total.push(pos-neg);magnitude+=pos+neg;
        }
        return gb2_threshold::sign(total.get(),2*magnitude);
      };
      return gb2_threshold::solve(0,limit,width_tol/4,maxit,score);
    };
    const auto score=[&](double inverse_width) {
      if(inverse_width==0)return 1;
      if(inverse_width==1)return -1;
      const auto loc=location(inverse_width);
      if(!loc.valid())return 0;
      transformed_detail::Sum lower,upper;
      double magnitude=n/inverse_width;
      lower.push(magnitude);upper.push(magnitude);
      const double limit=1-inverse_width;
      for(const auto& v:data)if(v.first>0) {
        const double l=loc.lo+inverse_width*v.first,h=loc.hi+inverse_width*v.first;
        const double wl=(limit-loc.hi)+inverse_width*v.second;
        const double wh=(limit-loc.lo)+inverse_width*v.second;
        const double pos_lo=(a-1)*v.first/h,neg_lo=(b-1)*v.first/wl;
        const double pos_hi=(a-1)*v.first/l,neg_hi=(b-1)*v.first/wh;
        lower.push(pos_lo-neg_lo);upper.push(pos_hi-neg_hi);
        magnitude+=pos_hi+neg_lo;
      }
      if(gb2_threshold::sign(lower.get(),2*magnitude)>0)return 1;
      if(gb2_threshold::sign(upper.get(),2*magnitude)<0)return -1;
      return 0;
    };
    const auto log_score=[&](double log_inverse_width) {
      return score(std::exp(log_inverse_width));
    };
    double left_log=-1;
    for(int it=0;it<maxit && log_score(left_log)<=0;++it)left_log*=2;
    const auto log_width=gb2_threshold::solve(left_log,0,outer_tol,maxit,log_score);
    if(!log_width.valid())return failure;
    const gb2_threshold::Bracket width{std::exp(log_width.lo),std::exp(log_width.hi),log_width.iterations};
    if(!(width.lo>0 && width.hi-width.lo<=tol*width.lo/8))return failure;
    const auto loc=location(width.mid());
    if(!loc.valid())return failure;
    // The conditional derivative of A with respect to B is a negative
    // weighted mean of the normalized data and lies in [-1,0].
    const double scale=range/width.mid();
    const double uncertainty=range*(width.hi-width.lo)/(width.lo*width.lo)
        +scale*(loc.hi-loc.lo+width.hi-width.lo);
    if(!std::isfinite(scale))return failure;
    const double left=low-loc.mid()*scale,right=high+(1-width.mid()-loc.mid())*scale;
    const double eps=std::numeric_limits<double>::epsilon();
    if(!(left<low && right>high &&
         uncertainty+8*eps*(std::abs(low)+scale)<=tol*std::max(1.,std::abs(left)) &&
         uncertainty+8*eps*(std::abs(high)+scale)<=tol*std::max(1.,std::abs(right))))
      return failure;
    auto fitted=point(left,right,a,b);
    fitted.iterations=width.iterations+loc.iterations;
    return fitted;
  }

  const bool right=std::isnan(max);
  const double held=right?min:max,extreme=right?high:low;
  const double width=std::abs(extreme-held);
  const double far_shape=right?b:a,near_shape=right?a:b;
  if(!(width>0 && std::isfinite(width)))return failure;
  std::vector<double> distances;
  transformed_detail::Sum log_distances;
  for(double v:x) {
    const double z=std::abs(v-held)/width;
    distances.push_back(z);
    if(std::isnan(near_shape)) {
      const double gap=std::abs(extreme-v);
      log_distances.push(std::log1p(gap/std::abs(v-held))/n);
    }
  }
  const double target0=log_distances.get();
  if(std::isnan(near_shape)) {
    if(low==high)return unbounded();
    if(!(target0>0 && std::isfinite(target0)))return failure;
  }
  const auto shape_at=[&](double log_width) {
    if(!std::isnan(near_shape))return gb2_threshold::Bracket{std::log(near_shape),std::log(near_shape)};
    const double target=target0+log_width;
    if(far_shape==1) {
      const double q=-std::log(target);
      const double error=8*std::numeric_limits<double>::epsilon()*(n+std::abs(q));
      return gb2_threshold::Bracket{q-error,q+error};
    }
    const auto score=[&](double log_q) {
      const double q=std::exp(log_q);
      return score_sign(q,far_shape,target,1)?1:score_sign(q,far_shape,target,-1)?-1:0;
    };
    const double center=std::log(far_shape)-std::log(target);
    double lo=center,hi=center;
    for(int it=0;it<maxit && score(lo)<=0;++it)lo-=2;
    for(int it=0;it<maxit && score(hi)>=0;++it)hi+=2;
    return gb2_threshold::solve(lo,hi,width_tol,maxit,score);
  };
  double log_width=0, width_error=0;
  int iterations=0;
  if(far_shape>1) {
    const auto score=[&](double t) {
      if(t==0)return 1;
      const auto q=shape_at(t);
      if(!q.valid())return 0;
      transformed_detail::Sum ratios;
      for(double v:distances)ratios.push((v/(std::expm1(t)+(1-v)))/n);
      const double r=(far_shape-1)*ratios.get();
      const double qlo=std::isnan(near_shape)?std::exp(q.lo):near_shape;
      const double qhi=std::isnan(near_shape)?std::exp(q.hi):near_shape;
      if(gb2_threshold::sign(r-qhi,2*(r+qhi))>0)return 1;
      if(gb2_threshold::sign(r-qlo,2*(r+qlo))<0)return -1;
      return 0;
    };
    double upper=std::isnan(near_shape)?1:1+std::log1p((far_shape-1)/near_shape);
    for(int it=0;it<maxit && score(upper)>=0;++it)upper*=2;
    const auto root=gb2_threshold::solve(0,upper,outer_tol,maxit,score,true);
    if(!root.valid())return failure;
    if(std::isnan(near_shape)) {
      const auto lo=shape_at(root.hi),hi=shape_at(root.lo);
      if(!lo.valid() || !hi.valid() || !(hi.hi-lo.lo<=tol/2))return failure;
    }
    log_width=root.mid();
    width_error=width*std::exp(root.hi)*(root.hi-root.lo);
    iterations=root.iterations;
  }
  const auto q=shape_at(log_width);
  if(!q.valid() || !(q.hi-q.lo<=tol/2))return failure;
  const double fitted_near=std::isnan(near_shape)?std::exp(q.mid()):near_shape;
  const double extension=width*std::expm1(log_width);
  const double boundary=right?high+extension:low-extension;
  const double eps=std::numeric_limits<double>::epsilon();
  if(far_shape>1 && ((right?boundary<=high:boundary>=low) ||
      width_error+8*eps*(std::abs(extreme)+extension)>tol*std::max(1.,std::abs(boundary))))
    return failure;
  auto fitted=right?point(held,boundary,fitted_near,far_shape):
      point(boundary,held,far_shape,fitted_near);
  fitted.iterations=iterations+q.iterations;
  return fitted;
}
} // namespace nsbeta_detail

inline NsbetaResult nsbeta_mle(span x, double min, double max, double tol, int maxit, double a=std::numeric_limits<double>::quiet_NaN(), double b=std::numeric_limits<double>::quiet_NaN()) {
  if(std::isnan(min)||std::isnan(max))return nsbeta_detail::free_bounds(x,min,max,tol,maxit,a,b);
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
