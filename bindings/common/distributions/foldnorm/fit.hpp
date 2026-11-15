#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/foldnorm/mle.hpp"
namespace umvl_binding {
inline FitResult fit_foldnorm(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, double reltol=1e-10, int iterlim=200) {

  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;

  const double m=mu.supplied ? mu.value : NAN;
  const double s=sigma.supplied ? sigma.value : NAN;
  if (mu.supplied && !(std::isfinite(m) && m>=0)) return invalid_argument(fail("fixed `mu` must be finite and nonnegative."));
  if (sigma.supplied && !(std::isfinite(s) && s>0)) return invalid_argument(fail("fixed `sigma` must be finite and positive."));
  const auto r=cppunivariate::foldnorm_mle(x,reltol,iterlim,m,s);
  const auto o = cppunivariate::outcome(r);
  const auto done = [&](FitResult out) {
    return with_outcome(std::move(out), o, static_cast<int>(r.status),{{"mu",mu_arg.supplied},{"sigma",sigma_arg.supplied}});
  };
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return stop_failure(failure(binding_message(format, arguments...), warnings), {}, outcome_payload(o), {}); };
  if(r.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("foldnorm numerical accuracy failure: requested coefficient accuracy cannot be established."),r.stop_detail,outcome_payload(o),{"mu","sigma"}));
  if(r.stop_detail.budget_exhausted)return done(stop_failure(failure("Folded-normal profile search did not converge; certificate work budget exhausted. Raising iterlim may resolve this stop."),r.stop_detail,outcome_payload(o),{"mu","sigma"}));
  if (r.status==cppunivariate::FoldnormStatus::AccuracyFailure)
    return done(stop_failure(fail("Foldnorm could not verify the requested coefficient accuracy in binary64."),r.stop_detail,outcome_payload(o),{"mu","sigma"}));
  if (r.status==cppunivariate::FoldnormStatus::Unbounded) return done(nomle("The folded-normal likelihood is unbounded as sigma tends to zero for these constant data."));
  if (r.status==cppunivariate::FoldnormStatus::InvalidInput) return done(fail("Folded normal requires finite nonnegative data, positive reltol and positive iterlim."));
  if (r.status!=cppunivariate::FoldnormStatus::Ok) return done(stop_failure(fail("Folded-normal profile search did not converge; increase iterlim."),r.stop_detail,outcome_payload(o),{"mu","sigma"}));
  std::vector<std::string> fixed;
  if (mu.supplied) fixed.push_back("mu");
  if (sigma.supplied) fixed.push_back("sigma");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",r.mu},{"sigma",r.sigma}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
