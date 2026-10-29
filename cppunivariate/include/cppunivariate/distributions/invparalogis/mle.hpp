// Actuar scale is the reciprocal of the paralogistic scale on 1/x.
// The fixed-scale path preserves the supplied value exactly.
#pragma once
#include "../paralogis/mle.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
struct InvParalogisResult { double shape, scale; double logLik; int iterations; ParalogisStatus status; };
inline InvParalogisResult invparalogis_mle(span x, double tol, int maxit, double shape0 = std::numeric_limits<double>::quiet_NaN()) {
  for (double v : x) {
    if (!(v > 0) || !std::isfinite(v) || !std::isfinite(1/v)) {
      const double nan=std::numeric_limits<double>::quiet_NaN();
      return {nan,nan,nan,0,ParalogisStatus::InvalidStart};
    }
  }
  std::vector<double> y(x.size());
  double jacobian = 0;
  for (std::size_t i=0; i<x.size(); ++i) { y[i]=1.0/x[i]; jacobian -= 2*std::log(x[i]); }
  auto r = paralogis_mle(span{y.data(), y.size()}, tol, maxit, shape0);
  return {r.shape, 1.0/r.scale, r.logLik+jacobian, r.iterations, r.status};
}
inline InvParalogisResult invparalogis_mle_fixed_scale(span x, double scale,
    double tol=1e-10, int maxit=100,
    double shape0=std::numeric_limits<double>::quiet_NaN()) {
  if (!(scale > 0) || !std::isfinite(scale)) {
    const double nan=std::numeric_limits<double>::quiet_NaN();
    return {nan,scale,nan,0,ParalogisStatus::InvalidStart};
  }
  std::vector<double> y(x.size());
  double jacobian=0;
  for (std::size_t i=0; i<x.size(); ++i) { y[i]=1/x[i]; jacobian-=2*std::log(x[i]); }
  auto r=paralogis_mle_fixed_scale(span{y.data(),y.size()},1/scale,tol,maxit,shape0);
  return {r.shape,scale,r.logLik+jacobian,r.iterations,r.status};
}
} // namespace cppunivariate

namespace cppunivariate {

inline InvParalogisResult invparalogis_mle_holds(span x,double shape,double scale=std::numeric_limits<double>::quiet_NaN()){
 std::vector<double> y;double jac=0;for(double v:x){y.push_back(1/v);jac-=2*std::log(v);}
 auto r=paralogis_mle_holds(span{y.data(),y.size()},shape,std::isnan(scale)?scale:1/scale);
 return {shape,std::isnan(scale)?1/r.scale:scale,r.logLik+jac,r.iterations,r.status};
}

}
