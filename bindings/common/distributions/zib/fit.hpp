#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/zib/mle.hpp"
namespace umvl_binding {
inline FitResult fit_zib(cppunivariate::ispan x, Held size_arg = {}, Held prob_arg = {}, Held pi_arg = {}, double reltol = 1e-10, int iterlim = 200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held size = size_arg;
  const Held prob = prob_arg;
  const Held pi = pi_arg;

  const double fixed_size=size.supplied?size.value:std::numeric_limits<double>::quiet_NaN();
  if(size.supplied && !std::isfinite(fixed_size))return fail("`size` must be finite and in its parameter domain.");
  const double held_prob=prob.supplied?prob.value:std::numeric_limits<double>::quiet_NaN();
  if(prob.supplied && (!std::isfinite(held_prob)||held_prob<0||held_prob>1))return fail("`prob` must be finite and in [0, 1].");
  const double held_pi=pi.supplied?pi.value:std::numeric_limits<double>::quiet_NaN();
  if(pi.supplied && (!std::isfinite(held_pi)||held_pi<0||held_pi>1))return fail("`pi` must be finite and in [0, 1].");
  const auto res=cppunivariate::zib_mle(x, reltol, iterlim, fixed_size, held_prob, held_pi);
  if(res.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return stop_failure(failure("zib numerical accuracy failure: requested coefficient accuracy cannot be established."),res.stop_detail,"numerical",{"size","prob","pi"});
  if(res.accuracy_failure)return stop_failure(fail("zib numerical accuracy failure: the returned coefficients cannot be resolved to the requested tolerance in the local refinement."),res.stop_detail,"numerical",{"size","prob","pi"});
  std::string stop_kind;
  switch(res.status) {
    case cppunivariate::ZibStatus::ZipLimit: stop_kind="limit";break;
    case cppunivariate::ZibStatus::FaceTie: stop_kind="tie";break;
    case cppunivariate::ZibStatus::IterationFailure: stop_kind="numerical";break;
    default:break;
  }
  if(res.stop_detail.budget_exhausted)stop_kind="budget";
  if(res.stop_detail.comparison_tie)stop_kind="unresolved";
  if(res.stop_detail.budget_exhausted)stop_kind="budget";
  if(stop_kind.empty() && !res.stop_detail.candidates.empty())stop_kind="numerical";
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(stop_kind=="unresolved" ? "Finite-size versus limit comparison is unresolved; no maximizer is certified. Use `mlzip` only to fit the comparison face." : binding_message(format,arguments...));
    return stop_kind.empty()?out:stop_failure(out,res.stop_detail,stop_kind,{"size","prob","pi"});
  };

  if(res.status==cppunivariate::ZibStatus::InvalidInput)return stopped_fail("requires nonempty nonnegative integer count data and valid controls; held parameters must give positive likelihood");
  if(res.status==cppunivariate::ZibStatus::InvalidSize)return stopped_fail("`size` must be a nonnegative integer at least max(x).");
  if(res.status==cppunivariate::ZibStatus::ZipLimit)return stopped_fail("The maximum likelihood estimator escapes to infinite size. Use `mlzip`.");
  if(res.status==cppunivariate::ZibStatus::FaceTie)return stopped_fail("No finite-size maximizer exists; the winning ZIP face has rate maxima that cannot be ordered at the comparison margin. Use `mlzip` and inspect the retained face candidates.");
  if(res.status==cppunivariate::ZibStatus::IterationFailure)return stopped_fail("Profile optimizer IterationFailure: iteration limit, floating-point range or precision exhausted; increase iterlim only for work-limit failures. No capped size estimate is returned.");
  if(!std::isfinite(res.logLik))return stopped_fail("MLE cannot be resolved at floating-point precision.");
  std::vector<std::string> fixed;
  if(size.supplied)fixed.push_back("size");
  if(prob.supplied)fixed.push_back("prob");
  if(pi.supplied)fixed.push_back("pi");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"size",res.size},{"prob",res.prob},{"pi",res.pi}}; out.extra.push_back({"certified", res.certified ? 1.0 : 0.0}); out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
