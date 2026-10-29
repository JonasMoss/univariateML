#pragma once
#include "../ht/mle.hpp"
namespace cppunivariate {
enum class HcauchyStatus { Ok, InvalidData, Unbounded, ScaleBoundary, SearchFailure };
struct HcauchyResult { double sigma, logLik; int iterations; HcauchyStatus status; };
inline HcauchyResult hcauchy_mle(span x, double held_sigma = std::numeric_limits<double>::quiet_NaN()) {
  const auto r=ht_mle(x,1.0,held_sigma);
  HcauchyStatus s=HcauchyStatus::SearchFailure;
  if(r.status==HtStatus::Ok) s=HcauchyStatus::Ok;
  if(r.status==HtStatus::InvalidData) s=HcauchyStatus::InvalidData;
  if(r.status==HtStatus::Unbounded) s=HcauchyStatus::Unbounded;
  if(r.status==HtStatus::ScaleBoundary) s=HcauchyStatus::ScaleBoundary;
  return {r.sigma,r.logLik,r.iterations,s};
}
} // namespace cppunivariate
