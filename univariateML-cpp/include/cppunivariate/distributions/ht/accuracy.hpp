#pragma once
#include "../../core/likelihood_polish.hpp"
namespace cppunivariate {
namespace ht_detail {
// Half-step gamma ratio with its common log(nu)/2 removed before
// differentiation. The recurrence is exact; above 128 the omitted term
// is bounded by 691/(88 nu^11), below 5.2e-23.
inline accuracy_detail::Jet mode_correction(accuracy_detail::Jet nu) {
  if(nu.v<1)return accuracy_detail::lgamma((nu+accuracy_detail::Jet(1))/accuracy_detail::Jet(2))-accuracy_detail::lgamma(nu/accuracy_detail::Jet(2))-accuracy_detail::Jet(.5)*accuracy_detail::log(nu/accuracy_detail::Jet(2));
  accuracy_detail::JetSum shift;
  for(int j=0;j<64&&nu.v<128;++j) {
    const auto t=accuracy_detail::reciprocal(nu);
    shift.add(-accuracy_detail::log1pmx(t)+accuracy_detail::Jet(.5)*accuracy_detail::log1pmx(accuracy_detail::Jet(2)*t));
    nu=nu+accuracy_detail::Jet(2);
  }
  const auto t=accuracy_detail::reciprocal(nu),v=t*t;
  return shift.value+t*(accuracy_detail::Jet(-.25)+v*(accuracy_detail::Jet(1./24)+v*(accuracy_detail::Jet(-1./20)+v*(accuracy_detail::Jet(17./112)-v*accuracy_detail::Jet(31./36)))));
}
}
}
