#pragma once
#include "../burr/mle.hpp"
#include "../lomax/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
struct Pareto2Result { double shape,scale,min,logLik; int iterations; LomaxStatus status; StopDetail stop_detail{}; LomaxCertificateFailure certificate_failure = LomaxCertificateFailure::None; };

inline Pareto2Result pareto2_mle(span x, double min, double tol, int maxit, double shape=std::numeric_limits<double>::quiet_NaN(), double scale=std::numeric_limits<double>::quiet_NaN()) {
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min);
  for(double v:x) valid=valid && std::isfinite(v) && v>min;
  if(!valid) y.clear();
  const auto r=lomax_mle(span{y.data(),y.size()},tol,maxit,NAN,
    std::isnan(scale)?scale:1/scale,shape);
  auto detail=r.stop_detail;
  for(auto& c:detail.candidates) if(c.model==StopModel::None && c.coef.size()==2)
    c.coef={c.coef[1],1/c.coef[0],min};
  return {r.kappa,std::isnan(scale)?1/r.lambda:scale,min,r.logLik,r.iterations,r.status,detail,r.certificate_failure};
}
}
