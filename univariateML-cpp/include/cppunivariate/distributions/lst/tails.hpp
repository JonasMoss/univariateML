#pragma once
#include "../../special/beta_tails.hpp"
#include "../../special/tails.hpp"
namespace cppunivariate { namespace student_detail {
inline double softplus(double x) { return missing_detail::softplus(x); }
inline double logmode(double df) {
  return special::log_t_mode(df);
}
inline double logdensity_log(double logabsx,double df) {
  const double t=2*logabsx-std::log(df);
  // Avoid a subnormal softplus which is subsequently multiplied by df.
  const double kernel=t < -36 ? std::exp(2*logabsx-std::log(2.0))*(1+1/df)
      : 0.5*(df+1)*softplus(t);
  return logmode(df)-kernel;
}
inline double probability_log(double logabsx,bool negative,double df,bool lower,bool logp) {
  if(std::isnan(logabsx)) return special::kNaN;
  if(logabsx==special::kNegInf) return logp ? -std::log(2.0) : 0.5;
  if(df>=1e8 && logabsx<=std::log(std::numeric_limits<double>::max())) return special::fs_t_p(std::exp(logabsx)*(negative?-1:1),df,lower,logp);
  const double t=2*logabsx-std::log(df);
  const double lp=-std::log(2.0)+missing_detail::beta_log(
      -softplus(t),-softplus(-t),df/2,0.5,true);
  return special::tail_probability(special::tails_from_log(lp,negative),lower,logp);
}
inline double quantile(double p,double df,bool lower,bool logp,double logscale=0) {
  const auto tails=special::quantile_log_tails(p,lower,logp);
  if(!tails.valid) return special::kNaN;
  if(!logp && p==0.5) return 0;
  // Exact log(2) is the rounded constant plus this residual. Retain the
  // nonzero quantile of a binary64 logarithmic input near the median.
  const double h=logp ? (p+std::log(2.0))+2.3190468138462995584e-17 : 0;
  if((logp && std::abs(h)<1e-8) || (!logp && std::abs(p-0.5)<1e-8)) {
    const double delta=logp ? 0.5*std::expm1(h) : p-0.5;
    return ((delta>0)==lower?1:-1)*std::exp(std::log(std::abs(delta))+logscale-logmode(df));
  }
  const bool left=tails.lower<tails.upper;
  const double lp=left?tails.lower:tails.upper;
  if(lp==special::kNegInf) return left?special::kNegInf:special::kPosInf;
  if(df>=1e8) {
    const double q=special::fs_t_q(lp,df,true);
    if(!std::isfinite(q)) return special::kNaN;
    const double check=special::fs_t_p(q,df,true,true);
    if(!std::isfinite(check) || std::abs(check-lp)>2e-12*std::max(1.0,std::abs(lp)))
      return special::kNaN;
    const double magnitude=std::exp(logscale+std::log(-q));
    return left?-magnitude:magnitude;
  }
  const double t=missing_detail::beta_logit_q(lp+std::log(2.0),df/2,0.5,true);
  const double magnitude=std::exp(logscale+0.5*(std::log(df)-t));
  return left ? -magnitude : magnitude;
}
} }
