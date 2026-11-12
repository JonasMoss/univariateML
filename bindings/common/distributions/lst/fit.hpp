#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/lst/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_lst(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, Held df_arg = {}, double gradtol = 1.220703e-4, int iterlim = 200) {
  std::vector<std::string> warnings;
  bool face_flag=false;double certified=0;
  const auto fail = [&](const std::string& format, const auto&... arguments) {
    auto out=failure(binding_message(format, arguments...), warnings);
    if(face_flag)out.extra.emplace_back("certified",certified);
    return out;
  };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;
  const Held df = df_arg;

  if(df.supplied&&!std::isfinite(df.value))return fail("`df` must be finite and positive.");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mu=mu.supplied?mu.value:nan;
  if(mu.supplied && (!std::isfinite(held_mu)))return fail("Invalid held `mu`.");
  const double held_sigma=sigma.supplied?sigma.value:nan;
  if(sigma.supplied && (!std::isfinite(held_sigma) || !(held_sigma>0)))return fail("Invalid held `sigma`.");
  std::vector<std::string> fixed;
  if(mu.supplied)fixed.push_back("mu");
  if(sigma.supplied)fixed.push_back("sigma");
  if(df.supplied)fixed.push_back("df");
  auto r=cppunivariate::lst_mle(x,gradtol,iterlim,held_mu,held_sigma,df.supplied?df.value:nan);
  if(r.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return stop_failure(failure("lst numerical accuracy failure: requested coefficient accuracy cannot be established."),r.stop_detail,"numerical",{"mu","sigma","df"});
  certified=r.certified?1.:0.;
  if(r.accuracy_failure)return stop_failure(fail("Student-t numerical accuracy failure: observed-information Newton refinement could not justify the requested coefficient tolerance; increase iterlim or inspect conditioning."),r.stop_detail,"numerical",{"mu","sigma","df"});
  face_flag=r.status==cppunivariate::LstStatus::NormalBoundary||r.status==cppunivariate::LstStatus::ScaleBoundary;
  if(r.status==cppunivariate::LstStatus::CertificateBudget)return stop_failure(fail("Student-t global certificate budget exhausted; no verified MLE. Raising iterlim may resolve this stop."),r.stop_detail,"budget",{"mu","sigma","df"});
  if(r.status==cppunivariate::LstStatus::NonUniqueSymmetry)return stop_failure(fail("At this held shape, symmetry and a strict likelihood comparison prove distinct global location-scale t maximizers. Candidates are finite likelihood witnesses."),r.stop_detail,"tie",{"mu","sigma","df"});
  if(r.status==cppunivariate::LstStatus::CertificateTie)return stop_failure(fail("Student-t global certificate unresolved tie or sign-uncertain seam; no verified MLE."),r.stop_detail,"tie",{"mu","sigma","df"});
  if(r.status==cppunivariate::LstStatus::NumericalFailure && df.supplied && df.value<1 && !mu.supplied && !sigma.supplied)
    return stop_failure(fail("Location-scale t numerical failure: joint location/scale at df < 1 has unresolved global nuisance bounds; no verified MLE."),r.stop_detail,"numerical",{"mu","sigma","df"});
  if(r.status==cppunivariate::LstStatus::NumericalFailure && !df.supplied && (mu.supplied || sigma.supplied))
    return stop_failure(fail("Student-t held-subset global certificate unresolved: normal (`mlnorm`) or lower-shape comparisons, finite roots or arithmetic bounds remain unresolved; no verified MLE."),r.stop_detail,"numerical",{"mu","sigma","df"});
  if(r.status==cppunivariate::LstStatus::NumericalFailure)return stop_failure(fail("Student-t global certificate numerical failure: score/root, enclosure, domain or linear solve unresolved; no verified MLE."),r.stop_detail,"numerical",{"mu","sigma","df"});
  if (r.status == cppunivariate::LstStatus::InvalidDf)
    return fail("`df` must be finite and positive.");
  if (r.status == cppunivariate::LstStatus::Unbounded)
    return stop_failure(fail("Likelihood is unbounded through scale collapse: excessive ties."),r.stop_detail,"nomle",{"mu","sigma","df"});
  if (r.status == cppunivariate::LstStatus::ScaleBoundary)
    return stop_failure(fail("Tie equality scale-collapse boundary; a regular MLE is not certified."),r.stop_detail,"nomle",{"mu","sigma","df"},(!mu.supplied&&!sigma.supplied&&!df.supplied)?"unavailable:t-scale-collapse":"");
  if (r.status == cppunivariate::LstStatus::IterationFailure)
    return stop_failure(fail("Location-scale t solve could not establish a representable score bracket or converge to the score tolerance; increase iterlim."),r.stop_detail,"numerical",{"mu","sigma","df"});
  if (r.status == cppunivariate::LstStatus::NoMLE)
    return fail("Location-scale t MLE on df >= 1 requires at least 3 observations and no value repeated n/2 or more times; scale collapse otherwise prevents a regular MLE.");
  if (r.status == cppunivariate::LstStatus::NormalBoundary)
    return stop_failure(fail("The location-scale t likelihood is maximised as df -> Inf. Use `mlnorm` to fit the normal distribution."),r.stop_detail,"limit",{"mu","sigma","df"},"norm");
  if(r.status==cppunivariate::LstStatus::BfgsFailure) return fail("Location-scale t optimizer failed its score check; increase iterlim.");
  if (!std::isfinite(r.mu) || !std::isfinite(r.sigma) || !std::isfinite(r.df))
    return fail("Location-scale t MLE requires finite data with positive spread.");
  std::vector<std::pair<std::string,std::string>> restricted;
  if(r.restricted & 1u) restricted.emplace_back("mu","lower");
  if(r.restricted & 2u) restricted.emplace_back("mu","upper");
  if(r.restricted & 4u) restricted.emplace_back("sigma","lower");
  if(r.restricted & 8u) restricted.emplace_back("sigma","upper");
  if(r.restricted & 16u) restricted.emplace_back("df","lower");
  if(r.restricted & 32u) restricted.emplace_back("df","upper");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",r.mu},{"sigma",r.sigma},{"df",r.df}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.restricted = restricted; out.has_restricted = true; out.fixed = fixed; out.has_fixed = true; out.extra.emplace_back("certified",r.certified?1.:0.); return out; }();

}
}
