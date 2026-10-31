// Analytic README-coordinate derivatives; see core/inference.hpp for the contract.
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace inference_detail {
struct PoisModel {
  static constexpr std::size_t count=1;
  double lambda;
  std::array<std::string,2> names() const { return {"lambda"}; }
  std::string conditions() const {return "lambda > 0 finite (zero-rate boundary excluded); nonnegative integer x; independent data";}
  bool valid() const {return lambda>0 && std::isfinite(lambda);}
  bool support(double x) const {return x>=0 && std::isfinite(x) && std::floor(x)==x;}
  std::array<double,4> fisher() const { return {1/lambda,0,0,0}; }
  void observation(double x, std::array<double,2>& s, std::array<double,4>& h) const {
    s={x/lambda-1,0}; h={x/lambda/lambda,0,0,0};
  }
};
} // namespace inference_detail
// Callback receives each free-coordinate score; default discards rows.
template<class Emit = inference_detail::Discard>
inline InferenceResult pois_inference(span x, double lambda,
    unsigned free_mask=1, Emit emit={}) {
  return inference_detail::accumulate(x,inference_detail::PoisModel{lambda},free_mask,emit);
}
// Integer-span counterpart matching the Poisson fitter's input boundary.
template<class Emit = inference_detail::Discard>
inline InferenceResult pois_inference(ispan x, double lambda,
    unsigned free_mask=1, Emit emit={}) {
  return inference_detail::accumulate(x,inference_detail::PoisModel{lambda},free_mask,emit);
}
} // namespace cppunivariate
