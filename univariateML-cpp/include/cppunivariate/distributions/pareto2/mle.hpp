#pragma once
#include "../../core/work_budget.hpp"
#include "../burr/mle.hpp"
#include "../lomax/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
struct Pareto2Result { double shape,scale,min,logLik; int iterations; LomaxStatus status; StopDetail stop_detail{}; LomaxCertificateFailure certificate_failure = LomaxCertificateFailure::None; };

inline Pareto2Result pareto2_mle(span x, double min, double tol, int maxit, double shape=std::numeric_limits<double>::quiet_NaN(), double scale=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min);
  for(double v:x) valid=valid && std::isfinite(v) && v>min;
  if(!valid) y.clear();
  const auto r=lomax_mle(span{y.data(),y.size()},tol,maxit,NAN,
    std::isnan(scale)?scale:1/scale,shape);
  auto detail=r.stop_detail;
  for(auto& c:detail.candidates) if(c.model==StopModel::None && c.coef.size()==2)
    c.coef={c.coef[1],std::isnan(scale)?1/c.coef[0]:scale,min};
  const double fitted_scale=std::isnan(scale)?1/r.lambda:scale;
  if((r.status==LomaxStatus::Ok || r.status==LomaxStatus::AccuracyFailure) &&
      (!(r.kappa>0 && fitted_scale>0) || !std::isfinite(r.kappa) || !std::isfinite(fitted_scale))) {
    detail.accuracy_stop=AccuracyStop::None;detail.attainable_tol=NAN;detail.conditioning_bound=NAN;
    detail.candidates.erase(std::remove_if(detail.candidates.begin(),detail.candidates.end(),
      [](const FitCandidate& c){return c.model==StopModel::None &&
        (c.coef.size()!=3 || !(c.coef[0]>0 && c.coef[1]>0) ||
         !std::isfinite(c.coef[0]) || !std::isfinite(c.coef[1]) || !std::isfinite(c.coef[2]));}),detail.candidates.end());
    return {r.kappa,fitted_scale,min,r.logLik,r.iterations,LomaxStatus::IterationFailure,detail,LomaxCertificateFailure::Arithmetic};
  }
  Pareto2Result out{r.kappa,fitted_scale,min,r.logLik,r.iterations,r.status,detail,r.certificate_failure};
  if(out.stop_detail.accuracy_stop!=AccuracyStop::None && out.shape>0 && out.scale>0)
    record_accuracy(out,tol,{out.shape,out.scale,min},{std::isnan(shape)?out.shape:0,std::isnan(scale)?out.scale:0,0});
  return out;
}
}
