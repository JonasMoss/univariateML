// Gompertz MLE.
//
// Up-front overdispersion check: mean(x) < sd(x) ⇒ data is consistent
// with an exponential and the Gompertz b MLE doesn't exist. Otherwise
// 1D Newton on b with hybrid convergence; a is the closed-form
// maximiser given b.

#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/precision.hpp"

#include "../../core/reductions.hpp"
#include "../../core/span.hpp"

#include <cmath>
#include <algorithm>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

// NumericalFailure: the MLE of the rescaled data does not map back into
// binary64 (a coefficient overflows, or with a tolerance detail lies below
// binary64 resolution). InvalidInput: an empty sample, or an observation
// that is not finite or lies below the support.
enum class GompertzStatus { Ok = 0, Overdispersed, IterationFailure, Unbounded, NumericalFailure, InvalidInput };

// Negative log-likelihood of Gompertz(a, b) [actuar: shape=a, rate=b]:
//   log f = log(a) + b*x - (a/b) * (exp(b*x) - 1)
// Per-obs Hessian (sympy):
//   H_aa/obs = 1/a^2                                   (data-indep diagonal)
//   H_bb/obs = a*(b^2 x^2 e - 2 b x e + 2 e - 2) / b^3,  e = exp(b*x)
//   H_ab/obs = (b*x*e - e + 1) / b^2
struct GompertzResult {
  double a;
  double b;
  double logLik;
  int iterations;
  GompertzStatus status;
 StopDetail stop_detail{}; };

inline FitOutcome outcome(const GompertzResult& r) {
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  switch (r.status) {
    case GompertzStatus::Ok: return FitOutcome::estimate(true);
    case GompertzStatus::Overdispersed: return FitOutcome::limit(true);
    case GompertzStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case GompertzStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case GompertzStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
    case GompertzStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
  }
  return {};
}

inline GompertzResult gompertz_mle_impl(span x, double tol, int maxit, bool standardized) {
  maxit = static_cast<int>(solver_budget(maxit));
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, GompertzStatus::IterationFailure};

  CPPUNIV_EXTENDED_REAL total = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (auto v : x) total += v;
  const double unit = static_cast<double>(total / static_cast<CPPUNIV_EXTENDED_REAL>(n));
  if (unit > 0.0 && std::isfinite(unit) && !standardized) {
    std::vector<double> z(n);
    for (std::size_t i = 0; i < n; ++i) z[i] = x[i] / unit;
    const auto r = gompertz_mle_impl(span{z.data(), n}, tol, maxit, true);
    GompertzResult out{r.a/unit, r.b/unit, r.logLik-static_cast<double>(n)*std::log(unit), r.iterations, r.status};
    if (out.status != GompertzStatus::Ok) return out;
    // a and b scale as 1/unit. On (1, 2, 3)*denorm_min they are 1.5e322 and
    // 2.5e323 (TASK-214.3): above binary64. Below the normal range the
    // relative spacing is denorm_min/coefficient.
    const double log_tiny = std::log(std::numeric_limits<double>::denorm_min()) + std::log(unit);
    const double floor = std::exp(log_tiny - std::min(std::log(r.a), std::log(r.b)));
    if (!(std::isfinite(out.a) && std::isfinite(out.b) && out.a > 0.0 && out.b > 0.0 &&
          std::isfinite(out.logLik) && floor <= std::min(tol, 1e-10))) {
      const bool tiny = std::isfinite(out.a) && std::isfinite(out.b) && std::isfinite(out.logLik);
      GompertzResult bad{kNaN, kNaN, tiny ? out.logLik : kNaN, r.iterations, GompertzStatus::NumericalFailure};
      if (tiny) {
        bad.stop_detail.accuracy_stop = AccuracyStop::Tolerance;
        bad.stop_detail.attainable_tol = floor;
      }
      return bad;
    }
    return out;
  }

  const double dn = static_cast<double>(n);
  double sum_x = 0.0, sum_xsq = 0.0;
  for (auto v : x) {
    sum_x += v;
    sum_xsq += v * v;
  }
  const double mean_x = sum_x / dn;
  // Profile likelihood is linear minus log of a moment generating
  // function: strictly concave. Its right derivative at zero is
  // n*(mean - mean(x^2)/(2*mean)), so equality also selects exponential.
  if (sum_xsq / dn >= 2.0 * mean_x * mean_x) {
    return {kNaN, kNaN, kNaN, 0, GompertzStatus::Overdispersed};
  }

  auto f_over_df = [&](double b) {
    double sum_bx = 0.0, sum_bx_x = 0.0, sum_bx_x2 = 0.0;
    for (auto v : x) {
      const double e = std::exp(b * v);
      sum_bx += e;
      sum_bx_x += v * e;
      sum_bx_x2 += v * v * e;
    }
    const double s = sum_bx - dn;
    const double r = b * sum_bx_x - s;
    const double a_local = b * dn / s;
    const double da = -dn * r / (s * s);
    const double d2a =
        dn / (s * s) *
        (b * (2.0 * sum_bx_x * sum_bx_x / s - sum_bx_x2) - 2.0 * sum_bx_x);
    const double f = dn * da / a_local + sum_x;
    const double df = dn * (a_local * d2a - da * da) / (a_local * a_local);
    return f / df;
  };

  double b = 1e-1;
  int iter = 1;
  for (; iter <= maxit; ++iter) {
    const double step = f_over_df(b);
    b -= step;
    if (b <= 0.0 || !std::isfinite(b)) {
      return {kNaN, kNaN, kNaN, iter, GompertzStatus::IterationFailure};
    }
    if (std::abs(step) < (std::abs(b) * tol + tol)) break;
  }

  double sum_exp_bx = 0.0;
  for (auto v : x) sum_exp_bx += std::exp(b * v);
  const double a = b * dn / (sum_exp_bx - dn);
  const double logLik =
      dn * std::log(a) + b * sum_x - (a / b) * (sum_exp_bx - dn);
  return {a, b, logLik, iter,
      iter <= maxit ? GompertzStatus::Ok : GompertzStatus::IterationFailure};
}

inline GompertzResult gompertz_mle(span x, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double invalid = std::numeric_limits<double>::quiet_NaN();
  if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return v>=0 && std::isfinite(v);}))
    return {invalid,invalid,invalid,0,GompertzStatus::InvalidInput};
  if(std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];})) {
    const double nan=std::numeric_limits<double>::quiet_NaN();
    return {nan,nan,nan,0,GompertzStatus::Unbounded};
  }
  auto r=gompertz_mle_impl(x,tol,maxit,false);
  if(r.status==GompertzStatus::Overdispersed) {
    double mean=0;for(double v:x)mean+=v/x.size();
    const double rate=1/mean;
    r.stop_detail=limit_detail({rate},x.size()*(std::log(rate)-1),StopModel::Exp);
  }
  return r;
}

inline GompertzResult gompertz_mle(span x,double tol,int maxit,double a,double b) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(std::isnan(a) && std::isnan(b))return gompertz_mle(x,tol,maxit);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return v>=0 && std::isfinite(v);}))
    return {nan,nan,nan,0,GompertzStatus::InvalidInput};
  if(!(tol>0 && std::isfinite(tol)) || maxit<=0 || (!std::isnan(a) && !(a>0 && std::isfinite(a))) ||
     (!std::isnan(b) && !(b>0 && std::isfinite(b))))return {nan,nan,nan,0,GompertzStatus::IterationFailure};
  double big=0;
  for(double v:x){if(!(v>=0 && std::isfinite(v)))return {nan,nan,nan,0,GompertzStatus::InvalidInput};big=std::max(big,v);}
  const double n=static_cast<double>(x.size());int iterations=0;
  if(std::isnan(b)) {
    if(big==0) return {a,1.0,n*std::log(a),0,GompertzStatus::Ok}; // flat likelihood
    // Normalize by max(x), including the boundary comparison, to avoid
    // underflow in x*x on binary64-only platforms.
    double sumy=0,sumy2=0;for(double v:x){const double y=v/big;sumy+=y;sumy2+=y*y;}
    const double logsx=std::log(big)+std::log(sumy);
    auto ratio=[&](double t) -> double {double total=0;for(double v:x)if(v>0){const double z=std::exp(t+std::log(v));if(!std::isfinite(z))return INFINITY;double lh;
      if(z<1e-3)lh=std::log(.5+z/3+z*z/8+z*z*z/30+z*z*z*z/144);
      else if(z>50)lh=z+std::log(z-1)-2*std::log(z);
      else lh=std::log((z*std::exp(z)-std::expm1(z))/(z*z));
      total+=std::exp(std::log(a)+2*std::log(v)+lh-logsx);
    }return total;};
    if(!(sumy>(a*big)*(sumy2/2))) {
      GompertzResult r={a,nan,nan,0,GompertzStatus::Overdispersed};
      r.stop_detail=limit_detail({a},n*std::log(a)-a*big*sumy,StopModel::Exp);return r;
    }
    const double lower=std::log(std::numeric_limits<double>::denorm_min()),upper=std::log(std::numeric_limits<double>::max());
    double lo=std::max(lower,std::min(upper,-std::log(big)-2)),hi=std::min(upper,lo+4),step=32;
    for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (ratio(lo)>=1 && lo>lower); ++bracket_pass){lo=std::max(lower,lo-step);step*=2;}
    step=32;for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (ratio(hi)<=1 && hi<upper); ++bracket_pass){hi=std::min(upper,hi+step);step*=2;}
    if(!(ratio(lo)<1 && ratio(hi)>1))return {a,nan,nan,0,GompertzStatus::IterationFailure};
    for(int j=0;j<maxit && hi-lo>1e-12;++j){const double mid=lo/2+hi/2;if(ratio(mid)<1)lo=mid;else hi=mid;++iterations;}
    b=std::exp(lo/2+hi/2);
    if(!(b>0 && std::isfinite(b) && std::abs(1-ratio(std::log(b)))*std::exp(std::log(b)+logsx)<1e-3))return {a,nan,nan,iterations,GompertzStatus::IterationFailure};
  }
  // Sum integrals in log coordinates; multiply by a before exponentiating.
  double log_integral=-INFINITY;
  for(double v:x)if(v>0){const double z=b*v;const double term=z<1e-8?std::log(v)+std::log1p(z/2+z*z/6):(z>50?z:std::log(std::expm1(z)))-std::log(b);const double high=std::max(log_integral,term);log_integral=high+std::log1p(std::exp(std::min(log_integral,term)-high));}
  if(std::isnan(a) && log_integral==-INFINITY)return {nan,b,nan,iterations,GompertzStatus::Unbounded};
  if(std::isnan(a))a=std::exp(std::log(n)-log_integral);
  CPPUNIV_EXTENDED_REAL sum_bx=0;for(double v:x)sum_bx+=b*v;
  const double ll=static_cast<double>(n*std::log(a)+sum_bx)-std::exp(std::log(a)+log_integral);
  if(!(a>0 && std::isfinite(a) && std::isfinite(ll)))return {a,b,ll,iterations,GompertzStatus::IterationFailure};
  return {a,b,ll,iterations,GompertzStatus::Ok};
}

}  // namespace cppunivariate
