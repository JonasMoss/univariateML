#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include <limits>
namespace umvl_binding {
inline FitResult fit_bnbinom(cppunivariate::ispan x, Held size_arg = {}, Held alpha_arg = {}, Held beta_arg = {}, double gradtol=1e-10, int iterlim=600) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held size = size_arg;
  const Held alpha = alpha_arg;
  const Held beta = beta_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  for (const auto &parameter : {size,alpha,beta}) {
    if (parameter.supplied) {
      const double value=parameter.value;
      if (!(value>0) || !std::isfinite(value))
        return fail("Fixed parameters must be positive finite numbers.");
    }
  }
  const auto fit=cppunivariate::bnbinom_mle(x,gradtol,iterlim,
    size.supplied?size.value:nan,
    alpha.supplied?alpha.value:nan,
    beta.supplied?beta.value:nan);
  if(fit.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return stop_failure(failure("bnbinom numerical accuracy failure: requested coefficient accuracy cannot be established."),fit.stop_detail,"numerical",{"size","alpha","beta"});
  if(fit.accuracy_failure)return stop_failure(fail("bnbinom numerical accuracy failure: final Newton refinement did not converge to the requested coefficient tolerance; increase iterlim or inspect conditioning."),fit.stop_detail,"numerical",{"size","alpha","beta"});
  std::string stop_kind;
  switch(fit.status) {
    case cppunivariate::BnbinomStatus::NegativeBinomialLimit: stop_kind="limit";break;
    case cppunivariate::BnbinomStatus::PoissonLimit: stop_kind="limit";break;
    case cppunivariate::BnbinomStatus::PointMass: stop_kind="limit";break;
    case cppunivariate::BnbinomStatus::IterationFailure: if(fit.stop_detail.budget_exhausted)stop_kind="budget";break;
    case cppunivariate::BnbinomStatus::NumericalFailure: stop_kind="numerical";break;
    case cppunivariate::BnbinomStatus::ComparisonTie: stop_kind="tie";break;
    default:break;
  }
  if(fit.stop_detail.budget_exhausted)stop_kind="budget";
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    const std::string qualification=stop_kind=="limit"?(fit.certified?"Certified global comparison: ":"Uncertified legacy comparison: "):"";
    auto out=fail(qualification+binding_message(format,arguments...));
    out.extra.emplace_back("certified",fit.certified?1.:0.);
    return stop_kind.empty()?out:stop_failure(out,fit.stop_detail,stop_kind,{"size","alpha","beta"});
  };

  switch (fit.status) {
    case cppunivariate::BnbinomStatus::Ok: break;
    case cppunivariate::BnbinomStatus::NegativeBinomialLimit:
      return stopped_fail("The negative binomial face wins (including likelihood ties). Use `mlnbinom`; hold its size at the supplied `size` or `beta`, if either was held.");
    case cppunivariate::BnbinomStatus::PoissonLimit:
      return stopped_fail("The Poisson face wins (including likelihood ties). Use `mlpois` instead.");
    case cppunivariate::BnbinomStatus::PointMass:
      return stopped_fail("The maximum likelihood estimator does not exist: all observations are zero (point mass). Use `mlpois` instead.");
    case cppunivariate::BnbinomStatus::NumericalFailure:
      return stopped_fail("Beta negative binomial certificate failed: unresolved global or score comparisons, coefficient enclosure, or floating-point range.");
    case cppunivariate::BnbinomStatus::ComparisonTie:
      return stopped_fail("Beta negative binomial certificate cannot separate competing identified maxima within the floating-point margin.");
    case cppunivariate::BnbinomStatus::InvalidInput:
      return stopped_fail("Beta negative binomial fitting requires nonempty nonnegative counts, positive finite fixed parameters and gradtol, and positive iterlim.");
    case cppunivariate::BnbinomStatus::IterationFailure:
      if(fit.stop_detail.budget_exhausted)
        return stopped_fail("Beta negative binomial certificate budget exhausted; try increasing iterlim. Raising iterlim may resolve this stop.");
      return stopped_fail("Beta negative binomial search failed: unresolved stationary point or search range; try increasing iterlim. Raising iterlim may resolve a delegate budget stop.");
  }
  std::vector<std::string> fixed;
  if (size.supplied) fixed.push_back("size");
  if (alpha.supplied) fixed.push_back("alpha");
  if (beta.supplied) fixed.push_back("beta");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"size",fit.size},{"alpha",fit.alpha},{"beta",fit.beta}}; out.logLik = fit.logLik; out.iterations = fit.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true;
    out.extra.emplace_back("certified",fit.certified?1.:0.);
    if(std::isfinite(fit.global_interval[0])) {
      out.extra_vectors.emplace_back("global_loglik_interval",std::vector<double>{fit.global_interval[0],fit.global_interval[1]});
      const double radius=fit.global_coefficient_radius;
      const auto& center=fit.global_coefficient_center;
      out.extra_vectors.emplace_back("global_coefficient_lower",std::vector<double>{center[0]*(1-radius),center[1]*(1-radius),center[2]*(1-radius)});
      out.extra_vectors.emplace_back("global_coefficient_upper",std::vector<double>{center[0]*(1+radius),center[1]*(1+radius),center[2]*(1+radius)});
    }
    if(std::isfinite(fit.profile_alpha[0])) {
      out.extra_vectors.emplace_back("profile_alpha_interval",std::vector<double>{fit.profile_alpha[0],fit.profile_alpha[1]});
      out.extra_vectors.emplace_back("profile_mode_lower",std::vector<double>{fit.profile_mode[0],fit.profile_alpha[0],fit.profile_mode[2]});
      out.extra_vectors.emplace_back("profile_mode_upper",std::vector<double>{fit.profile_mode[1],fit.profile_alpha[1],fit.profile_mode[3]});
    }
    if(!size.supplied && !beta.supplied) {
      out.extra_vectors.emplace_back("swap_maximum",std::vector<double>{fit.beta,fit.alpha,fit.size});
      for(const auto &candidate:fit.stop_detail.candidates)
        if(candidate.coef.size()==3 && candidate.coef[0]==candidate.coef[2]) {
          out.extra_vectors.emplace_back("diagonal_witness",candidate.coef);break;
        }
    }
    return out; }();

}
}
