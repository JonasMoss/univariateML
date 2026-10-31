// Analytic README-coordinate derivatives; see core/inference.hpp for the contract.
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace inference_detail {
// Extend the held-shape fitter's digamma-increment expansion through z^-8.
// 1-q^n is evaluated as (1-q)*sum(q^k), preserving small increments.
// These are asymptotic numerical formulas, not certified uniform error bounds.
inline double beta_power_difference(double q,double d,unsigned n) {
  double sum=1,term=1;
  for(unsigned k=1;k<n;++k) {term*=q;sum+=term;}
  return d*sum;
}
inline double beta_psi_increment(double z,double h) {
  if(z<20) {
    if(h>=.01*z) return special::digamma(z+h)-special::digamma(z);
    // Exact digamma recurrence transports a small increment to z >= 20.
    double correction=0;
    while(z<20) {correction+=(h/(z+h))/z;z+=1;}
    return correction+beta_psi_increment(z,h);
  }
  const double r=h/z, q=1/(1+r), inv=1/z, d=r/(1+r), v=inv*inv;
  return std::log1p(r)+.5*inv*d
    +v*beta_power_difference(q,d,2)/12
    -v*v*beta_power_difference(q,d,4)/120
    +v*v*v*beta_power_difference(q,d,6)/252
    -v*v*v*v*beta_power_difference(q,d,8)/240;
}
inline double beta_tri_difference(double z,double h) {
  if(z<20) {
    if(h>=.01*z) return special::trigamma(z)-special::trigamma(z+h);
    double correction=0;
    while(z<20) {
      correction+=(h/(z+h))/z*(1/z+1/(z+h));z+=1;
    }
    return correction+beta_tri_difference(z,h);
  }
  const double r=h/z, q=1/(1+r), inv=1/z, d=r/(1+r), v=inv*inv;
  return inv*d+.5*v*beta_power_difference(q,d,2)
    +inv*v*beta_power_difference(q,d,3)/6
    -inv*v*v*beta_power_difference(q,d,5)/30
    +inv*v*v*v*beta_power_difference(q,d,7)/42
    -inv*v*v*v*v*beta_power_difference(q,d,9)/30;
}
struct BetaModel {
  static constexpr std::size_t count=2;
  double shape1, shape2;
  std::array<std::string,2> names() const { return {"shape1","shape2"}; }
  std::string conditions() const {return "shape1, shape2 > 0 finite; 0 < x < 1; independent data";}
  bool valid() const {return shape1>0 && shape2>0 && std::isfinite(shape1) && std::isfinite(shape2);}
  bool support(double x) const {return x>0 && x<1 && std::isfinite(x);}
  std::array<double,4> fisher() const { const double t=special::trigamma(shape1+shape2);
    return {beta_tri_difference(shape1,shape2),-t,-t,beta_tri_difference(shape2,shape1)}; }
  void observation(double x, std::array<double,2>& s, std::array<double,4>& h) const {
    s={std::log(x)+beta_psi_increment(shape1,shape2),std::log1p(-x)+beta_psi_increment(shape2,shape1)};
    h=fisher();
  }
};
} // namespace inference_detail
// Callback receives each free-coordinate score; default discards rows.
template<class Emit = inference_detail::Discard>
inline InferenceResult beta_inference(span x, double shape1, double shape2,
    unsigned free_mask=3, Emit emit={}) {
  return inference_detail::accumulate(x,inference_detail::BetaModel{shape1, shape2},free_mask,emit);
}
} // namespace cppunivariate
