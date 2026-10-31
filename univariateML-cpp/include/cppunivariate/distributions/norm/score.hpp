// Analytic README-coordinate derivatives; see core/inference.hpp for the contract.
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace inference_detail {
struct NormModel {
  static constexpr std::size_t count=2;
  double mean, sd;
  std::array<std::string,2> names() const { return {"mean","sd"}; }
  std::string conditions() const {return "finite mean; sd > 0 finite; finite real observations; independent data";}
  bool valid() const {return std::isfinite(mean) && sd>0 && std::isfinite(sd);}
  bool support(double x) const {return std::isfinite(x);}
  std::array<double,4> fisher() const {
    const double v=1/sd/sd;
    // Positive mathematical information underflow is an arithmetic failure.
    return {v==0 ? NAN : v,0,0,v==0 ? NAN : 2/sd/sd};
  }
  void observation(double x, std::array<double,2>& s, std::array<double,4>& h) const {
    const double z=(x-mean)/sd, inv=1/sd;
    s={z*inv,(z*z-1)*inv};
    h={inv*inv,2*z*inv*inv,2*z*inv*inv,(3*z*z-1)*inv*inv};
  }
};
} // namespace inference_detail
// Callback receives each free-coordinate score; default discards rows.
template<class Emit = inference_detail::Discard>
inline InferenceResult norm_inference(span x, double mean, double sd,
    unsigned free_mask=3, Emit emit={}) {
  return inference_detail::accumulate(x,inference_detail::NormModel{mean, sd},free_mask,emit);
}
} // namespace cppunivariate
