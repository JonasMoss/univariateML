#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/sgomp/mle.hpp"
namespace umvl_binding {
inline FitResult fit_sgomp(cppunivariate::span x, Held b_arg = {}, Held eta_arg = {}, double reltol=1e-10, int iterlim=200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held b = b_arg;
  const Held eta = eta_arg;

  const double bv=b.supplied ? b.value : cppunivariate::sgomp_detail::nan();
  const double ev=eta.supplied ? eta.value : cppunivariate::sgomp_detail::nan();
  if (b.supplied && !(bv>0 && std::isfinite(bv))) return fail("fixed `b` must be positive and finite.");
  if (eta.supplied && !(ev>=0 && std::isfinite(ev))) return fail("fixed `eta` must be nonnegative and finite.");
  const auto f=cppunivariate::sgomp_mle(x,reltol,iterlim,bv,ev);
  if (f.status==cppunivariate::SgompStatus::Collapse) return fail("The shifted Gompertz MLE does not exist: constant data have unbounded likelihood.");
  if (f.status==cppunivariate::SgompStatus::InvalidInput) return fail("Shifted Gompertz requires finite nonnegative data, positive reltol and positive iterlim.");
  if (f.status!=cppunivariate::SgompStatus::Ok) return fail("Shifted Gompertz MLE failed to converge; increase iterlim.");
  std::vector<std::string> fixed;
  if (b.supplied) fixed.push_back("b");
  if (eta.supplied) fixed.push_back("eta");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"b",f.b},{"eta",f.eta}}; out.logLik = f.logLik; out.iterations = f.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
