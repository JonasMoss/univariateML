#pragma once
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
inline EmgResult emg_mle(span x,double tol=1e-8,int maxit=500,
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
    r.stop_detail=c.detail;return r;
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
    CPPUNIV_EXTENDED_REAL square=0;
    for(double v:x) {const CPPUNIV_EXTENDED_REAL d=static_cast<CPPUNIV_EXTENDED_REAL>(v)-m;square+=d*d/x.size();}
    const double s=std::isnan(sigma)?static_cast<double>(std::sqrt(square)):sigma;
    if(square==0 && std::isnan(sigma)) {bad.status=EmgStatus::NumericalFailure;return bad;}
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
