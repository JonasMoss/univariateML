#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/dblpareto/mle.hpp"

namespace umvl_binding {

inline FitResult fit_dblpareto(cppunivariate::span x, Held scale = {}, Held left_rate = {},
                              Held right_rate = {}) {
  std::vector<std::string> fixed;
  const std::vector<std::pair<std::string, Held>> arguments{
      {"scale", scale}, {"left_rate", left_rate}, {"right_rate", right_rate}};
  for (const auto& arg : arguments) {
    if (!arg.second.supplied) continue;
    if (!(arg.second.value > 0) || !std::isfinite(arg.second.value))
      return invalid_argument(failure("Held double Pareto parameters must be positive and finite."));
    fixed.push_back(arg.first);
  }

  const auto r = cppunivariate::dblpareto_mle(x, scale.supplied ? scale.value : NAN,
      left_rate.supplied ? left_rate.value : NAN, right_rate.supplied ? right_rate.value : NAN);
  const auto o = cppunivariate::outcome(r);
  const auto done = [&](FitResult out) {
    return with_outcome(std::move(out), o, static_cast<int>(r.status),
        {{"scale", scale.supplied}, {"left_rate", left_rate.supplied},
         {"right_rate", right_rate.supplied}});
  };
  if (o.kind == cppunivariate::OutcomeKind::Limit) {
    const bool pareto = r.status == cppunivariate::DblparetoStatus::ParetoLimit;
    std::string message = pareto
        ? "The double Pareto likelihood selects the Pareto limit. Use `mlpareto`."
        : "The double Pareto likelihood selects the power limit. Use `mlpower`.";
    if (scale.supplied || (pareto ? right_rate.supplied : left_rate.supplied))
      message += " Retain the held endpoint and rate from the limit coefficients where applicable.";
    return done(stop_failure(failure(message), r.stop_detail, outcome_payload(o), {}));
  }
  if (r.status == cppunivariate::DblparetoStatus::Unbounded)
    return done(stop_failure(failure("The double Pareto likelihood is unbounded for this constant sample."),
                             {}, outcome_payload(o), {}));
  if (r.status == cppunivariate::DblparetoStatus::InvalidInput)
    return done(failure("Double Pareto fitting requires nonempty, positive, finite observations."));
  if (r.status != cppunivariate::DblparetoStatus::Ok)
    return done(failure("The double Pareto fit could not be represented to the required accuracy."));
  FitResult out;
  out.estimates = {{"scale", r.scale}, {"left_rate", r.left_rate}, {"right_rate", r.right_rate}};
  out.logLik = r.logLik;
  out.fixed = fixed;
  out.has_fixed = true;
  if (r.maximizers.size)
    attach_maximizer_set(out, r.maximizers, "dblpareto", [&](const auto& c) {
      return cppunivariate::dblpareto_detail::log_likelihood(
          x, c[0], c[1], c[2]);
    });
  return done(std::move(out));
}

} // namespace umvl_binding
