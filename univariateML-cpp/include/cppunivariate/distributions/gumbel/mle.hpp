// Gumbel MLE: monotone scale-score bracket; mu profiles analytically.
//
// Used directly by Weibull and Inverse Weibull (via log-of-log transforms),
// so the helper fit_gumbel_newton is part of the public cppunivariate API for
// downstream estimators.

#pragma once
#include "../../core/outcome.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/work_budget.hpp"
#include "cppunivariate/core/constants.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"
#include "../norm/mle.hpp"
#include <vector>

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

// NumericalFailure: the log likelihood at the returned (or held)
// coefficients lies outside binary64; the coefficients are kept.
enum class GumbelStatus { Ok, InvalidInput, Unbounded, IterationFailure, AccuracyFailure, NumericalFailure };

struct GumbelResult {
  double mu;
  double sigma;
  double logLik;
  int iterations;
  GumbelStatus status = GumbelStatus::Ok;
 StopDetail stop_detail{}; };

inline GumbelResult fit_gumbel_newton(span x, double tol, int maxit,
                                      double theta0 = std::numeric_limits<double>::quiet_NaN(),
                                      bool exponential_location = false) {
  maxit = static_cast<int>(solver_budget(maxit));
  const std::size_t n = x.size();
  if (n == 0 || !(tol > 0 && std::isfinite(tol)) || maxit < 1)
    return {NAN,NAN,NAN,0,GumbelStatus::InvalidInput};
  for (double v : x) if (!std::isfinite(v)) return {NAN,NAN,NAN,0,GumbelStatus::InvalidInput};
  const double dn = static_cast<double>(n);

  const auto moments = norm_detail::moments(x);
  const double xbar = moments.mean;
  // Two-pass variance: the one-pass mean(x^2) - xbar^2 cancels to a negative
  // number for data with a large offset (NaN start at x + 1e8).
  double var0 = 0.0;
  for (auto v : x) var0 += (v - xbar) * (v - xbar);
  var0 /= dn;
  bool all_equal = true;
  for (auto v : x) all_equal = all_equal && v == x[0];
  if (all_equal) {
    // Constant data: no MLE (the likelihood is unbounded as sigma -> 0).
    const double nan = std::numeric_limits<double>::quiet_NaN();
    return {nan, nan, nan, 0, GumbelStatus::Unbounded};
  }

  // Solve in dimensionless coordinates when moments or Newton products
  // would overflow/underflow, or a large offset hides the spread.
  if (moments.sd > 0.0 && std::isfinite(moments.sd) &&
      (!std::isfinite(var0) || var0 == 0.0 || moments.sd < 1e-100 ||
       moments.sd > 1e100 || std::abs(moments.mean) > 1e6 * moments.sd)) {
    std::vector<double> z(n);
    for (std::size_t i = 0; i < n; ++i) z[i] = (x[i] - moments.mean) / moments.sd;
    const double start = std::isnan(theta0) ? theta0 : theta0 - std::log(moments.sd);
    const auto r = fit_gumbel_newton(span{z.data(), n}, tol, maxit, start, exponential_location);
    const double mu=moments.mean+moments.sd*r.mu,sigma=moments.sd*r.sigma;
    auto status=r.status;
    if(status==GumbelStatus::Ok && (!(sigma>0 && std::isfinite(sigma)) ||
       std::abs(sigma/moments.sd-r.sigma)>.25*tol*r.sigma ||
       std::abs((mu-moments.mean)/moments.sd-r.mu)>
         .25*tol*std::max(std::abs(mu)/moments.sd,r.sigma)))
      status=GumbelStatus::AccuracyFailure;
    GumbelResult out{mu,sigma,r.logLik-dn*std::log(moments.sd),r.iterations,status};
    if(status==GumbelStatus::AccuracyFailure){if(out.sigma>0)record_accuracy(out,tol,{mu,sigma},{std::max(std::abs(mu),sigma),sigma});else{out.stop_detail.accuracy_stop=AccuracyStop::None;out.stop_detail.attainable_tol=NAN;}}
    return out;
  }

  tol=std::min(tol,1e-10);
  // The profiled scale equation F(s)=s+E_s[X-xbar] has
  // F'(s)=1+Var_s(X)/s^2 > 0. Shift exponentials by the minimum.
  double minimum=x[0];
  for(double v:x)minimum=std::min(minimum,v);
  CPPUNIV_EXTENDED_REAL shifted_mean=0,mean_correction=0;
  for(double v:x) {
    const CPPUNIV_EXTENDED_REAL d=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-minimum)-mean_correction;
    const CPPUNIV_EXTENDED_REAL t=shifted_mean+d;mean_correction=(t-shifted_mean)-d;shifted_mean=t;
  }
  shifted_mean/=dn;
  const auto equation=[&](double scale,double& location) {
    CPPUNIV_EXTENDED_REAL w=0,first=0;
    for(double v:x) {
      const CPPUNIV_EXTENDED_REAL d=static_cast<CPPUNIV_EXTENDED_REAL>(v)-minimum;
      const CPPUNIV_EXTENDED_REAL e=std::exp(-d/scale);
      w+=e;first+=d*e;
    }
    location=minimum-scale*std::log(static_cast<double>(w/dn));
    return static_cast<double>(static_cast<CPPUNIV_EXTENDED_REAL>(scale)-shifted_mean+first/w);
  };
  double lo=0,hi=std::sqrt(var0),dummy=0;
  int iter=0;
  for(;iter<maxit && equation(hi,dummy)<=0;++iter)hi*=2;
  if(!(hi>0 && std::isfinite(hi)) || equation(hi,dummy)<=0)
    return {NAN,NAN,NAN,iter,GumbelStatus::IterationFailure};
  const double accuracy=std::min(.01*tol,std::max(.0001*tol,4*std::numeric_limits<double>::epsilon()));
  bool converged=false;
  for(;iter<maxit;++iter) {
    const double mid=lo+(hi-lo)/2;
    if(equation(mid,dummy)>0)hi=mid;else lo=mid;
    double ml=0,mh=0;equation(lo,ml);equation(hi,mh);
    // Both public coefficients, plus exp(-mu) in the Weibull delegate.
    if(lo>0 && hi-lo<=accuracy*lo &&
       std::abs(mh-ml)<=accuracy*(exponential_location?std::min(1.0,lo):std::max(lo,std::abs(ml)))) {converged=true;break;}
  }
  const double sigma=lo+(hi-lo)/2;
  const double invsF = -1.0 / sigma;
  double sum_e = 0.0;
  double sum_z = 0.0;
  double sum_exp_neg_z = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    sum_e += std::exp((x[i] - xbar) * invsF);
  }
  const double mu = -sigma * std::log(sum_e / dn) + xbar;
  for (std::size_t i = 0; i < n; ++i) {
    const double z = (x[i] - mu) / sigma;
    sum_z += z;
    sum_exp_neg_z += std::exp(-z);
  }
  const double logLik =
      -dn * std::log(sigma) - sum_z - sum_exp_neg_z;
  if (converged && !std::isfinite(logLik)) return {mu, sigma, NAN, iter, GumbelStatus::NumericalFailure};
  return {mu, sigma, logLik, iter,
      converged ? GumbelStatus::Ok : GumbelStatus::IterationFailure};
}


inline GumbelResult fit_gumbel_held(span x,double tol,int maxit,double mu,double sigma) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(std::isnan(mu)&&std::isnan(sigma))return fit_gumbel_newton(x,tol,maxit);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.empty()||!(tol>0 && std::isfinite(tol))||maxit<1)return {nan,nan,nan,0,GumbelStatus::InvalidInput};
  for (double v : x) if (!std::isfinite(v)) return {nan,nan,nan,0,GumbelStatus::InvalidInput};
  if ((!std::isnan(mu) && !std::isfinite(mu)) ||
      (!std::isnan(sigma) && !(sigma > 0 && std::isfinite(sigma))))
    return {nan,nan,nan,0,GumbelStatus::InvalidInput};
  tol=std::min(tol,1e-10);
  double m=mu,s=sigma;int it=0;
  if(std::isnan(m)) {
    double low=x[0];for(double v:x)low=std::min(low,v);
    CPPUNIV_EXTENDED_REAL sum=0,correction=0;
    for(double v:x) {
      const CPPUNIV_EXTENDED_REAL d=static_cast<CPPUNIV_EXTENDED_REAL>(v)-low;
      const CPPUNIV_EXTENDED_REAL ratio=std::isfinite(d)?d/s:v/s-low/s;
      const CPPUNIV_EXTENDED_REAL y=std::exp(-ratio)-correction,t=sum+y;
      correction=(t-sum)-y;sum=t;
    }
    const double eta=-std::log(static_cast<double>(sum/x.size()));
    m=low+s*eta;
    if(!std::isfinite(m))m=s*(low/s+eta);
    const double difference=m-low;
    const double restored=std::isfinite(difference)?difference/s:m/s-low/s;
    if(!std::isfinite(m) || std::abs(restored-eta)>.25*tol*std::max(std::abs(m)/s,1.))
      {GumbelResult out{m,s,nan,it,GumbelStatus::AccuracyFailure};
      if(std::isfinite(m)){double ll=0;for(double v:x){double z=(v-m)/s;ll-=std::log(s)+z+std::exp(-z);}out.logLik=ll;}
      {if(out.sigma>0)record_accuracy(out,tol,{m,s},{std::max(std::abs(m),s),0});else{out.stop_detail.accuracy_stop=AccuracyStop::None;out.stop_detail.attainable_tol=NAN;}}return out;}
  } else if(std::isnan(s)) {
    double radius=0;for(double v:x)radius=std::max(radius,std::abs(v-m));
    if(!(radius>0))return {nan,nan,nan,0,GumbelStatus::Unbounded};
    // The likelihood is strictly concave in inverse scale: its derivative
    // n/t - sum d*(1-exp(-t*d)) is strictly decreasing.
    double lo=0,hi=1/radius;
    auto score=[&](double t) {
      CPPUNIV_EXTENDED_REAL g=static_cast<CPPUNIV_EXTENDED_REAL>(x.size())/t,c=0;
      if(!std::isfinite(g))return g;
      for(double v:x) {
        const CPPUNIV_EXTENDED_REAL d=static_cast<CPPUNIV_EXTENDED_REAL>(v)-m;
        const CPPUNIV_EXTENDED_REAL term=-d*(-std::expm1(-t*d));
        if(!std::isfinite(term))return -std::numeric_limits<CPPUNIV_EXTENDED_REAL>::infinity();
        const CPPUNIV_EXTENDED_REAL y=term-c,u=g+y;c=(u-g)-y;g=u;
      }
      return g;
    };
    for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (score(hi)>0 && std::isfinite(hi*2)); ++bracket_pass)hi*=2;
    if(!(score(hi)<=0))return {nan,nan,nan,it,GumbelStatus::IterationFailure};
    for(;it<maxit;++it){const double t=lo/2+hi/2;if(score(t)>0)lo=t;else hi=t;if(hi-lo<.01*tol*hi)break;}
    s=1/(lo/2+hi/2);
  }
  double ll=0;for(double v:x){const double z=(v-m)/s;ll-=std::log(s)+z+std::exp(-z);}
  if(it==maxit)return {nan,nan,nan,it,GumbelStatus::IterationFailure};
  // exp(-z) can exceed binary64 at held coefficients (archive case 3008,
  // data near 1e150: logLik about -10^(3.66e149)): unrepresentable.
  if(!std::isfinite(ll))return {m,s,nan,it,GumbelStatus::NumericalFailure};
  return {m,s,ll,it};
}


inline FitOutcome outcome(const GumbelResult& r) {
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  switch (r.status) {
    case GumbelStatus::Ok: return FitOutcome::estimate(true);
    case GumbelStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case GumbelStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case GumbelStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case GumbelStatus::AccuracyFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
    case GumbelStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}


}  // namespace cppunivariate
