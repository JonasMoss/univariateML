#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "dpqr.hpp"
#include "fit_inverse.hpp"
#include "../../core/span.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
inline constexpr double kTlambdaLambdaLower = -1.0;
inline constexpr double kTlambdaLambdaUpper = 1.0;

enum class TlambdaStatus { Ok, InvalidData, InvalidShape, IterationFailure, Unbounded, CollapseLimit, InvalidSupport, CertificateFailure };
struct TlambdaResult { double mu,sigma,lambda,logLik; int iterations; TlambdaStatus status; unsigned restricted = 0; bool certified=false; double certificate_lower=NAN,certificate_upper=NAN; StopDetail stop_detail={}; };
namespace tlambda_detail {
inline TlambdaResult uniform_fit(double lo,double hi,double n,double lambda,int maxit,double tol=1e-7) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double mu=lo+(hi-lo)/2;
  double sigma=std::max(mu-lo,hi-mu)*lambda;
  for(int j=0;j<maxit && valid(mu,sigma,lambda) &&
      (slack(lo,mu,sigma,lambda)<0 || slack(hi,mu,sigma,lambda)<0);++j)
    sigma=std::nextafter(sigma,INFINITY);
  const double ll=-n*(std::log(sigma)+std::log(2/lambda));
  const double ideal=-n*logstandard(hi,lo,1);
  const double pad=512*std::numeric_limits<double>::epsilon()*(n+std::abs(ll)+std::abs(ideal));
  // A rounded midpoint can require a materially wider interval. Never call
  // that represented interval the exact uniform optimum merely because it fits.
  const bool ok=valid(mu,sigma,lambda) && std::isfinite(ll) && std::isfinite(ideal) &&
      slack(lo,mu,sigma,lambda)>=0 && slack(hi,mu,sigma,lambda)>=0 && ideal-ll<=n*tol+pad;
  TlambdaResult out{mu,sigma,lambda,ll,0,ok?TlambdaStatus::Ok:TlambdaStatus::CertificateFailure};
  if(!ok && valid(mu,sigma,lambda) && std::isfinite(ll) &&
      slack(lo,mu,sigma,lambda)>=0 && slack(hi,mu,sigma,lambda)>=0) {
    out.stop_detail.candidates.push_back({{mu,sigma,lambda},ll-pad,ll+pad});
    out.stop_detail.loglik_lower=ll-pad;out.stop_detail.loglik_upper=INFINITY;
    out.stop_detail.segment_loglik_upper=ideal+pad;
  }
  return out;
}
inline double likelihood(span x,double mu,double sigma,double lambda) {
  double ll=0;for(double v:x)ll+=distributions::dtlambda(v,mu,sigma,lambda,true);return ll;
}
}
#include "profile.hpp"
inline TlambdaResult tlambda_mle_impl(span x,double tol=1e-7,int maxit=1000,double lambda=NAN) {
  return tlambda_fit_detail::profile(x,tol,maxit,lambda);
}
inline TlambdaResult tlambda_mle_impl(span x,double tol,int maxit,double lambda,double mu,double sigma) {
  return tlambda_fit_detail::profile(x,tol,maxit,lambda,mu,sigma);
}


#include "certificate.hpp"

inline TlambdaResult tlambda_mle(span x,double tol=1e-7,int maxit=1000,double lambda=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto r=tlambda_cert_detail::fit(x,tol,maxit,lambda);
  if(r.status==TlambdaStatus::Ok && (std::isnan(lambda)) && r.lambda==kTlambdaLambdaLower) r.restricted|=16u;
  if(r.status==TlambdaStatus::Ok && (std::isnan(lambda)) && r.lambda==kTlambdaLambdaUpper) r.restricted|=32u;
  return r;
}
inline TlambdaResult tlambda_mle(span x,double tol,int maxit,double lambda,double mu,double sigma) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto r=tlambda_cert_detail::fit(x,tol,maxit,lambda,mu,sigma);
  if(r.status==TlambdaStatus::Ok && (std::isnan(lambda) && std::isnan(mu)) && r.lambda==kTlambdaLambdaLower) r.restricted|=16u;
  if(r.status==TlambdaStatus::Ok && (std::isnan(lambda)) && r.lambda==kTlambdaLambdaUpper) r.restricted|=32u;
  return r;
}

inline FitOutcome outcome(const TlambdaResult& r) {
  if (r.status == TlambdaStatus::CertificateFailure) {
    if (r.stop_detail.budget_exhausted) return FitOutcome::budget();
    if (r.stop_detail.comparison_tie) return FitOutcome::estimate(false);
  }
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  switch (r.status) {
    case TlambdaStatus::Ok: return FitOutcome::estimate(r.certified);
    case TlambdaStatus::InvalidData: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case TlambdaStatus::InvalidShape: return FitOutcome::failure(OutcomeReason::InvalidArgument);
    case TlambdaStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case TlambdaStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case TlambdaStatus::CollapseLimit: return FitOutcome::nonexistence(OutcomeReason::Unattained);
    case TlambdaStatus::InvalidSupport: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case TlambdaStatus::CertificateFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}


}
