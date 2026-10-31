// Analytic README-coordinate derivatives; see core/inference.hpp for the contract.
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace inference_detail {
struct BetaModel {
  static constexpr std::size_t count=2;
  double shape1, shape2;
  std::array<std::string,2> names() const { return {"shape1","shape2"}; }
  std::string conditions() const {return "shape1, shape2 > 0 finite; 0 < x < 1; independent data";}
  bool valid() const {return shape1>0 && shape2>0 && std::isfinite(shape1) && std::isfinite(shape2);}
  bool support(double x) const {return x>0 && x<1 && std::isfinite(x);}
  std::array<double,4> fisher() const { const double t=special::trigamma(shape1+shape2);
    return {special::trigamma(shape1)-t,-t,-t,special::trigamma(shape2)-t}; }
  void observation(double x, std::array<double,2>& s, std::array<double,4>& h) const {
    const double d=special::digamma(shape1+shape2);
    s={std::log(x)-special::digamma(shape1)+d,std::log1p(-x)-special::digamma(shape2)+d};
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
