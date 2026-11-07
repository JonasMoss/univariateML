#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/cmb/mle.hpp"
#include <limits>
namespace umvl_binding {
inline FitResult fit_cmb(cppunivariate::ispan x, Held size_arg = {}, Held prob_arg = {}, Held nu_arg = {}, double reltol=1e-10, int iterlim=2000) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held size = size_arg;
  const Held prob = prob_arg;
  const Held nu = nu_arg;

  double trials=size.supplied ? size.value : 0;
  if (size.supplied && (!std::isfinite(trials) || trials<0 || trials>1e7 || trials!=std::floor(trials)))
    return fail("`size` must be a nonnegative integer at most 1e7.");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double p=prob.supplied ? prob.value : nan;
  const double v=nu.supplied ? nu.value : nan;
  if (prob.supplied && !(std::isfinite(p) && p>=0 && p<=1))
    return fail("fixed `prob` must be between zero and one inclusive.");
  if (nu.supplied && !std::isfinite(v)) return fail("fixed `nu` must be finite.");
  cppunivariate::CmbResult res;
  if(size.supplied)res=cppunivariate::cmb_mle(x,static_cast<int>(trials),reltol,iterlim,p,v);

  else {
    const auto profile=!nu.supplied ? cppunivariate::cmb_size_free_nu_mle(x,p,reltol,iterlim) :
      (prob.supplied ? cppunivariate::cmb_size_mle(x,p,v,reltol,iterlim) :
      cppunivariate::cmb_size_fixed_nu_mle(x,v,reltol,iterlim));
    trials=profile.size;res=profile.fit;
  }
  std::string stop_kind;
  switch(res.status) {
    case cppunivariate::CmbStatus::PointMass: stop_kind="limit";break;
    case cppunivariate::CmbStatus::AdjacentLimit: stop_kind="limit";break;
    case cppunivariate::CmbStatus::EndpointLimit: stop_kind="limit";break;
    case cppunivariate::CmbStatus::FaceTie: stop_kind="tie";break;
    case cppunivariate::CmbStatus::FaceUnresolved: stop_kind="unresolved";break;
    default:break;
  }
  if(res.stop_detail.budget_exhausted) stop_kind="budget";
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    return stop_kind.empty()?out:stop_failure(out,res.stop_detail,stop_kind,{"size","prob","nu"});
  };

  switch (res.status) {
    case cppunivariate::CmbStatus::Ok: break;
    case cppunivariate::CmbStatus::PointMass:
      return stopped_fail("The MLE does not exist: the likelihood approaches a point mass.");
    case cppunivariate::CmbStatus::AdjacentLimit:
      return stopped_fail("The MLE does not exist: the likelihood approaches an adjacent two-point law as nu tends to positive infinity.");
    case cppunivariate::CmbStatus::EndpointLimit:
      return stopped_fail("The MLE does not exist: the likelihood approaches an endpoint two-point law as nu tends to negative infinity.");
    case cppunivariate::CmbStatus::Unidentified:
      return stopped_fail("With size = 0 both parameters are unidentified.");
    case cppunivariate::CmbStatus::InvalidInput:
      return stopped_fail("CMB requires counts between zero and size, positive reltol and positive iterlim.");
    case cppunivariate::CmbStatus::FaceUnresolved:
      if (!nu.supplied)
        return stopped_fail("No finite maximum was certified above the limiting faces; a larger finite maximum is not excluded.");
      return stopped_fail("No finite maximum was found in the exploratory search; a larger finite maximum is not excluded. The Conway-Maxwell-Poisson limit is available: use mlcmp with nu held fixed.");
    case cppunivariate::CmbStatus::FaceTie:
      if (!nu.supplied && !prob.supplied) {
        bool adjacent_zero=!x.empty();
        for(int value:x)adjacent_zero=adjacent_zero && (value==0 || value==1);
        if(adjacent_zero)
          return stopped_fail("The attained size-one Bernoulli fit matched a limiting face: the adjacent Conway-Maxwell-Poisson limit has the same likelihood.");
      }
      if (!nu.supplied)
        return stopped_fail("A finite likelihood matched a limiting face within numerical tolerance; a larger finite maximum is not excluded.");
      return stopped_fail("A finite likelihood matched the Conway-Maxwell-Poisson face within numerical tolerance; a larger finite maximum is not excluded. Use mlcmp with nu held fixed for the limit model.");
    case cppunivariate::CmbStatus::NotImplemented:
      return stopped_fail("Free size with free dispersion is not implemented for this sample within the supported size range.");
    default:
      if (!size.supplied && !prob.supplied && nu.supplied && v>0)
        return stopped_fail("CMB MLE failed to converge or exhausted its normalizer work budget; a larger finite maximum is not excluded. The Conway-Maxwell-Poisson limit is available: use mlcmp with nu held fixed. Raising iterlim may resolve this stop.");
      return stopped_fail("CMB MLE failed to converge or exhausted its normalizer work budget. Raising iterlim may resolve this stop.");
  }
  std::vector<std::string> fixed;
  if(size.supplied)fixed.push_back("size");
  if (prob.supplied) fixed.push_back("prob");
  if (nu.supplied || trials==1) fixed.push_back("nu");
  if (trials==1 && !nu.supplied && res.prob>0 && res.prob<1) warnings.push_back(binding_message("nu is not identified with size = 1; returning nu = 1."));
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"size",trials},{"prob",res.prob},{"nu",res.nu}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
