#pragma once
#include "../../core/outcome.hpp"
#include "../norm/mle.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/face_compare.hpp"
#include "logconcave_profile.hpp"
#include "location_certificate.hpp"
#include "../../core/precision.hpp"
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include <vector>
#include <array>

#include "certificate.hpp"
namespace cppunivariate {
inline EmgResult emg_accuracy_search(span x,double tol=1e-8,int maxit=500,
    double mu=special::kNaN,double sigma=special::kNaN,double lambda=special::kNaN) {
  maxit = static_cast<int>(solver_budget(maxit));
  EmgResult bad={special::kNaN,special::kNaN,special::kNaN,special::kNaN,0,EmgStatus::InvalidInput};
  if(x.empty() || !(tol>0 && std::isfinite(tol)) || maxit<1 || (!std::isnan(mu)&&!std::isfinite(mu)) || (!std::isnan(sigma)&&!(sigma>0&&std::isfinite(sigma))) || (!std::isnan(lambda)&&!(lambda>0&&std::isfinite(lambda)))) return bad;
  for(double v:x)if(!std::isfinite(v))return bad;
  // The public location-only slice is strictly concave. Route it before
  // standardization so that overflow in an irrelevant sample variance cannot
  // spoil a representable conditional fit.
  if(std::isnan(mu) && !std::isnan(sigma) && !std::isnan(lambda)) {
    const auto c=emg_detail::location_certificate(x,sigma,lambda,tol,maxit);
    EmgResult r={c.mu,sigma,lambda,c.loglik,c.iterations,
      c.ok?EmgStatus::Ok:(c.arithmetic_failure?EmgStatus::NumericalFailure:EmgStatus::IterationFailure)};
    r.stop_detail=c.detail;
    if(!c.ok){if(r.sigma>0 && r.lambda>0)record_accuracy(r,tol,{r.mu,r.sigma,r.lambda},{std::isnan(mu)?std::max(std::abs(r.mu),r.sigma):0,std::isnan(sigma)?r.sigma:0,std::isnan(lambda)?r.lambda:0});else{r.stop_detail.accuracy_stop=AccuracyStop::None;r.stop_detail.attainable_tol=NAN;}}return r;
  }
  // Pointwise domination: for x <= mu and t >= 0,
  // phi_sigma(x-mu-t) <= phi_sigma(x-mu). This excludes every finite
  // rate, uniformly over sigma, when the location is held above all data.
  // Constant samples with held sigma use the universal normal peak bound.
  const double maximum=*std::max_element(x.begin(),x.end());
  const double smallest=*std::min_element(x.begin(),x.end());
  // Constancy is an exact data comparison: sample variance can round away
  // from zero on ties, or underflow to zero on distinct observations.
  if(smallest==maximum && std::isnan(sigma) && std::isnan(lambda) &&
      (std::isnan(mu)||mu==smallest)) {bad.status=EmgStatus::Unbounded;return bad;}
  if(smallest==maximum && std::isnan(sigma) && !std::isnan(lambda) &&
      (std::isnan(mu)||mu==smallest)) {
    // With a free endpoint, the exponential kernel bounds the density by
    // lambda; a vanishing scale with (x-mu)/sigma -> +infinity attains
    // that supremum. A held endpoint has exp(k^2/2)*Phi(-k) < 1/2.
    const double ll=x.size()*(std::log(lambda)-(std::isnan(mu)?0:std::log(2.0)));
    EmgResult r={smallest,0,lambda,ll,0,EmgStatus::TranslatedExponentialLimit};
    r.stop_detail=limit_detail({smallest,lambda},ll,StopModel::TranslatedExp);return r;
  }
  if(std::isnan(lambda) && ((!std::isnan(mu)&&mu>=maximum) ||
      (std::isnan(mu)&&!std::isnan(sigma)&&smallest==maximum))) {
    const double m=std::isnan(mu)?smallest:mu;
    const auto gaussian=norm_mle(x,m,sigma);
    const double s=gaussian.sd;
    if(gaussian.status!=NormStatus::Ok) {bad.status=EmgStatus::NumericalFailure;return bad;}
    CPPUNIV_EXTENDED_REAL ll=0;
    if(!(s>0&&std::isfinite(s))||(std::isnan(sigma)&&s<std::numeric_limits<double>::min())) {bad.status=EmgStatus::NumericalFailure;return bad;}
    for(double v:x) {const CPPUNIV_EXTENDED_REAL z=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m)/s;ll+=emg_detail::logphi(z)-std::log(s);}
    if(!std::isfinite(static_cast<double>(ll))) {bad.status=EmgStatus::NumericalFailure;return bad;}
    EmgResult r={m,s,INFINITY,static_cast<double>(ll),0,EmgStatus::NormalLimit};
    r.stop_detail=limit_detail({m,s},r.logLik,StopModel::Norm);return r;
  }
  if(!std::isnan(mu)&&!std::isnan(sigma)&&!std::isnan(lambda)) {
    const auto candidate=emg_detail::cert_finite_witness(x,mu,sigma,lambda);
    if(!emg_detail::cert_witness_valid(candidate)){bad.status=EmgStatus::NumericalFailure;return bad;}
    return {mu,sigma,lambda,(candidate.loglik_lower+candidate.loglik_upper)/2,0,EmgStatus::Ok};
  }
  return emg_detail::outer_certificate(x,tol,maxit,mu,sigma,lambda);
}
}

#include "../../core/likelihood_polish.hpp"
namespace cppunivariate {
inline EmgResult emg_mle(span x,double tol=1e-10,int maxit=500,double mu=NAN,double sigma=NAN,double lambda=NAN) {
  // Retain the established global enclosure budget; the joint local solve
  // below supplies the requested, finer coefficient accuracy.
  const bool location_only=std::isnan(mu)&&!std::isnan(sigma)&&!std::isnan(lambda);
  const double search_tol=!location_only&&tol>=4*std::numeric_limits<double>::epsilon()?std::max(tol,1e-8):tol;
  auto r=emg_accuracy_search(x,search_tol,maxit,mu,sigma,lambda);
  if(r.status!=EmgStatus::Ok||location_only)return r;
  const double center=r.mu,unit=r.sigma;
  std::array<double,3> q{{0,0,std::log(r.lambda*unit)}};
  const std::array<bool,3> free{{std::isnan(mu),std::isnan(sigma),std::isnan(lambda)}};
  const auto objective=[&](const std::array<accuracy_detail::Jet,3>& z) {
    const auto s=accuracy_detail::exp(z[1]),l=accuracy_detail::exp(z[2]),k=l*s;
    accuracy_detail::JetSum f;
    for(double v:x){const auto t=(accuracy_detail::Jet((static_cast<ExtendedReal>(v)-center)/unit)-z[0])/s;
      if((t-k).v < -10) {
        const auto a=t-k;accuracy_detail::Jet fraction(0);
        for(int j=48;j>=1;--j)fraction=accuracy_detail::Jet(j)/(-a+fraction);
        f.add(z[2]-accuracy_detail::Jet(.5)*t*t-accuracy_detail::Jet(.91893853320467274178)-accuracy_detail::log(-a+fraction));
      } else f.add(z[2]+accuracy_detail::Jet(.5)*k*k-k*t+accuracy_detail::logcdf(t-k));}
    return f.value;
  };
  if(!accuracy_detail::polish(q,free,tol,maxit,objective,r.stop_detail)){r.status=EmgStatus::NumericalFailure;{if(r.sigma>0 && r.lambda>0)record_accuracy(r,tol,{r.mu,r.sigma,r.lambda},{std::isnan(mu)?std::max(std::abs(r.mu),r.sigma):0,std::isnan(sigma)?r.sigma:0,std::isnan(lambda)?r.lambda:0});else{r.stop_detail.accuracy_stop=AccuracyStop::None;r.stop_detail.attainable_tol=NAN;}}return r;}
  if(free[0])r.mu=std::fma(unit,q[0],center);
  if(free[1])r.sigma=unit*std::exp(q[1]);
  if(free[2])r.lambda=std::exp(q[2])/unit;
  if((free[0]&&!accuracy_detail::location_projection(r.mu,center,unit,q[0],r.sigma,tol))||
     (free[1]&&!accuracy_detail::scale_projection(r.sigma,unit,q[1],tol))||
     (free[2]&&!accuracy_detail::scale_projection(r.lambda,unit,q[2],tol,true))) {
    {r.status=EmgStatus::AccuracyFailure;{if(r.sigma>0 && r.lambda>0)record_accuracy(r,tol,{r.mu,r.sigma,r.lambda},{std::isnan(mu)?std::max(std::abs(r.mu),r.sigma):0,std::isnan(sigma)?r.sigma:0,std::isnan(lambda)?r.lambda:0});else{r.stop_detail.accuracy_stop=AccuracyStop::None;r.stop_detail.attainable_tol=NAN;}}}return r;
  }
  r.logLik=0;for(double v:x)r.logLik+=distributions::demg(v,r.mu,r.sigma,r.lambda,true);
  return r;
}
}

namespace cppunivariate {
inline FitOutcome outcome(const EmgResult& r) {
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  if (r.stop_detail.comparison_tie && r.status == EmgStatus::IterationFailure) return FitOutcome::estimate(false);
  switch (r.status) {
    case EmgStatus::Ok: return FitOutcome::estimate(true);
    case EmgStatus::NormalLimit: return FitOutcome::limit();
    case EmgStatus::TranslatedExponentialLimit: return FitOutcome::limit();
    case EmgStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case EmgStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case EmgStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case EmgStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
    case EmgStatus::BudgetExhausted: return FitOutcome::budget();
    case EmgStatus::Unresolved: return FitOutcome::failure(OutcomeReason::Unresolved);
    case EmgStatus::Tie: return FitOutcome::estimate(false);
    case EmgStatus::AccuracyFailure: return FitOutcome::failure(OutcomeReason::Tolerance);
  }
  return {};
}
} // namespace cppunivariate
