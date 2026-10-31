// Analytic README-coordinate derivatives; see core/inference.hpp for the contract.
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace inference_detail {
struct GammaModel {
  static constexpr std::size_t count=2;
  double shape, scale;
  std::array<std::string,2> names() const { return {"shape","scale"}; }
  std::string conditions() const {return "shape, scale > 0 finite; x > 0 finite; independent data";}
  bool valid() const {return shape>0 && scale>0 && std::isfinite(shape) && std::isfinite(scale);}
  bool support(double x) const {return x>0 && std::isfinite(x);}
  std::array<double,4> fisher() const { return {special::trigamma(shape),1/scale,1/scale,shape/scale/scale}; }
  void observation(double x, std::array<double,2>& s, std::array<double,4>& h) const {
    const double inv=1/scale;
    s={std::log(x)-std::log(scale)-special::digamma(shape),(x/scale-shape)*inv};
    h={special::trigamma(shape),inv,inv,(2*x/scale-shape)*inv*inv};
  }
};
} // namespace inference_detail
// Callback receives each free-coordinate score; default discards rows.
template<class Emit = inference_detail::Discard>
inline InferenceResult gamma_inference(span x, double shape, double scale,
    unsigned free_mask=3, Emit emit={}) {
  return inference_detail::accumulate(x,inference_detail::GammaModel{shape, scale},free_mask,emit);
}
} // namespace cppunivariate
