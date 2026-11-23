#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/hyper/mle.hpp"
#include <cmath>
#include <iomanip>
#include <limits>
#include <sstream>
#include <string>
namespace umvl_binding {
inline FitResult fit_hyper(cppunivariate::span x, Held m_arg = {}, Held N_arg = {}, Held k_arg = {}) {
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...)); };
  const Held m = m_arg;
  const Held N = N_arg;
  const Held k = k_arg;

  for (const auto& named : {std::make_pair(std::string("k"), k),
                            std::make_pair(std::string("N"), N),
                            std::make_pair(std::string("m"), m)}) {
    if (named.second.supplied && std::isnan(named.second.value))
      return invalid_argument(fail("`%s` must be a whole number or NULL, not NA.", named.first));
  }
  const auto held_value = [](const Held& value, const std::string&) {
    return value.supplied ? value.value : std::numeric_limits<double>::quiet_NaN();
  };
  const auto whole = [](double v) {
    std::ostringstream out;
    out << std::fixed << std::setprecision(0) << v;
    return out.str();
  };
  const double design=held_value(k, "k");
  const double N_held = held_value(N, "N"), m_held = held_value(m, "m");
  const auto res = cppunivariate::hyper_mle(x, design, N_held, m_held);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"m",m_arg.supplied},{"N",N_arg.supplied},{"k",k_arg.supplied}});
  };
  PayloadKind stop_kind=outcome_payload(o);
  switch(res.status) {
    case cppunivariate::HyperStatus::FaceTie: stop_kind=PayloadKind::Comparison;break;
    case cppunivariate::HyperStatus::FaceUnresolved: stop_kind=outcome_payload(o);break;
    case cppunivariate::HyperStatus::AllZero: stop_kind=outcome_payload(o);break;
    case cppunivariate::HyperStatus::Infeasible: stop_kind=outcome_payload(o);break;
    default:break;
  }
  if(res.stop_detail.budget_exhausted) stop_kind=outcome_payload(o);
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    if(stop_kind==PayloadKind::None)return out;
    return comparison_payload(stop_failure(out,res.stop_detail,stop_kind,{"m","N","k"}),o,
      cppunivariate::comparison_choice(res),{{"m",m_arg},{"N",N_arg},{"k",k_arg}});
  };

  const std::string estimated = !k.supplied ? "k" : (!N.supplied ? "N" : "m");
  switch (res.status) {
    case cppunivariate::HyperStatus::FaceUnresolved:
      if (!N.supplied && !m.supplied && !k.supplied)
        return done(res.tie == 0 ? stopped_fail("No finite maximum was found in the exploratory search; a larger finite maximum is not excluded. The Poisson face may dominate; use mlpois for the limit model.") :
          stopped_fail("No finite maximum was found in the exploratory search; a larger finite maximum is not excluded. The binomial face of size %s may dominate; use mlbinom for the limit model.", whole(res.tie)));
      return done(stopped_fail("No finite maximum was found in the exploratory search; a larger finite maximum is not excluded. The binomial limit is available: use mlbinom(x, size = held count)."));
    case cppunivariate::HyperStatus::FaceTie:
      if (!N.supplied && !m.supplied && !k.supplied)
        return done(res.tie == 0 ? stopped_fail("A finite likelihood matched the Poisson face within numerical tolerance; a larger finite maximum is not excluded. Use mlpois for the limit model.") :
          stopped_fail("A finite likelihood matched the binomial face of size %s within numerical tolerance; a larger finite maximum is not excluded. Use mlbinom for the limit model.", whole(res.tie)));
      return done(stopped_fail("A finite likelihood matched the binomial face within numerical tolerance; a larger finite maximum is not excluded. Use mlbinom for the limit model."));
    case cppunivariate::HyperStatus::NumericalFailure:
      return done(stopped_fail("Finite-grid numerical failure: 1e7 candidate evaluation budget or representability/nonfinite likelihood limit exceeded."));
    case cppunivariate::HyperStatus::BothFree:
      return done(stopped_fail("Supply the population size `N` or the number of successes `m` "
                 "as well as `k`: estimating both is not supported. As `N` grows "
                 "with `m / N` fixed the model tends to the binomial; use "
                 "`mlbinom(x, size = k)`."));
    case cppunivariate::HyperStatus::InvalidInput:
      return done(stopped_fail("`k`, `N` and `m` must be whole numbers from 0 to 2^53 - 1, with "
                 "`k <= N` and `m <= N`; so must the observations."));
    case cppunivariate::HyperStatus::Infeasible:
      return done(stopped_fail("No admissible value of `%s` gives every observation positive "
                 "probability: observations must lie between max(0, k - (N - m)) "
                 "and min(k, m).", estimated));
    case cppunivariate::HyperStatus::AllZero:
      return done(stopped_fail("All observations are 0: with `m` and `k` positive the likelihood "
                 "increases toward a point mass at zero as `N` grows, so the "
                 "maximum likelihood estimator of `N` does not exist."));
    case cppunivariate::HyperStatus::TooLarge:
      return done(stopped_fail("The required population size or likelihood maximiser exceeds "
                 "2^53 - 1; double precision does not represent every integer "
                 "beyond that, so the "
                 "estimate of `N` cannot be computed exactly."));
    case cppunivariate::HyperStatus::Tie:
    case cppunivariate::HyperStatus::Unidentified:
    case cppunivariate::HyperStatus::Ok:
      break;
  }
  std::vector<std::string> fixed;
  if (m.supplied) fixed.push_back("m");
  if (N.supplied) fixed.push_back("N");
  if (k.supplied) fixed.push_back("k");
  FitResult out;
  out.estimates = {{"m",res.m},{"N",res.N},{"k",res.k}};
  out.logLik = res.logLik;
  out.iterations = res.iterations;
  out.has_iterations = true;
  out.fixed = fixed;
  out.has_fixed = true;
  if (res.maximizers.size) {
    attach_maximizer_set(out, res.maximizers, "hyper", [&](const auto&) { return res.logLik; });
    return done(std::move(out));
  }
  if (!o.uniqueness_known) {
    out.candidates.push_back({out.estimates, out.logLik, out.logLik, "hyper"});
    for (const auto& candidate : res.stop_detail.candidates)
      out.candidates.push_back({named_coefficients(candidate.coef, {"m","N","k"}),
          candidate.loglik_lower, candidate.loglik_upper, "hyper"});
    if (!res.stop_detail.candidates.empty())
      out.error = "The hypergeometric design comparison leaves multiple count orbits unresolved.";
    if (res.status == cppunivariate::HyperStatus::Tie) {
      const double chosen = !k.supplied ? res.k : (!N.supplied ? res.N : res.m);
      const double other = res.tie;
      if (other != chosen && cppunivariate::hyper_detail::is_count(other)) {
        const double m_value = estimated == "m" ? other : res.m;
        const double N_value = estimated == "N" ? other : res.N;
        const double k_value = estimated == "k" ? other : res.k;
        cppunivariate::hyper_detail::Sum sum;
        for (double v : x) sum.add(cppunivariate::hyper_log_pmf(v, N_value, m_value, k_value));
        const double ll = static_cast<double>(sum.value());
        if (std::isfinite(ll))
          out.candidates.push_back({{{"m",m_value},{"N",N_value},{"k",k_value}},ll,ll,"hyper"});
      }
      out.error = binding_message(
          "The likelihood comparison between `%s` = %s and %s could not be resolved.",
          estimated, whole(chosen), whole(other));
    } else if (res.non_unique_reason == 1) {
      out.candidates.push_back({{{"m",res.k},{"N",res.N},{"k",res.m}},
                                out.logLik,out.logLik,"hyper"});
      if (out.error.empty()) out.error = binding_message(
          "The likelihood is symmetric in `m` and `k`: (%s, %s) and (%s, %s) have equal likelihood.",
          whole(res.m), whole(res.k), whole(res.k), whole(res.m));
    }
  }
  return done(std::move(out));
}
}
