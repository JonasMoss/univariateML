#pragma once
#include "../../special/cmp.hpp"
#include "dpqr.hpp"

namespace cppunivariate {
// Compatibility family entry point; the shared public solver owns closure.
inline CmpResult cmp_domain_mle(span x, const CmpFitControl& ctl = {}) {
  return cmp_mle(x,ctl);
}
}
