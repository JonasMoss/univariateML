#pragma once
#include "../../special/cmp.hpp"
#include "dpqr.hpp"

namespace cppunivariate {
// Family entry point adds the finite boundary members to the legacy solver
// in special/cmp.hpp. Keep that shared solver unchanged while the shared owner updates its public entry point.
inline CmpResult cmp_domain_mle(span x, const CmpFitControl& ctl = {}) {
  CmpResult out;
  const bool fl=!std::isnan(ctl.fixed_lambda), fn=!std::isnan(ctl.fixed_nu);
  if (x.empty() || !(ctl.tol>0 && ctl.tol<1) || ctl.maxit<1 || ctl.max_terms<2 ||
      (fl && (!(ctl.fixed_lambda>=0)||!std::isfinite(ctl.fixed_lambda))) ||
      (fn && (!(ctl.fixed_nu>=0)||!std::isfinite(ctl.fixed_nu))) ||
      (fl && fn && ctl.fixed_nu==0 && ctl.fixed_lambda>=1)) return out;
  bool zero=true;
  for (double v:x) {
    if (!std::isfinite(v) || v<0 || v!=std::floor(v) || v>2147483647.0) return out;
    zero=zero && v==0;
  }
  if (fl && ctl.fixed_lambda==0 && !zero) return out;
  if (zero && (!fl || ctl.fixed_lambda==0)) {
    out.eta=-special::kPosInf; out.nu=fn?ctl.fixed_nu:1;
    out.logLik=0; out.mean=0; out.mean_h=0; out.status=CmpStatus::Ok;
    return out;
  }
  out=cmp_mle(x,ctl);
  // The legacy solver has already checked the one-sided geometric score.
  if (out.status==CmpStatus::GeometricLimit) out.status=CmpStatus::Ok;
  return out;
}
}
