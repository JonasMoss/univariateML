// Sufficient global certificate using the two transformed-gamma side covers.
#pragma once
#include "shape_candidate.hpp"
#include "../../core/work_budget.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
namespace gengamma_fit_detail {

// Only a separated, matching finite winner is certified here. A parent face
// result is used as an upper bound on a losing side, never as proof that a
// small nonzero shape can be rounded to zero or that a face strictly wins.
inline bool certify_free_candidate(span y, const ShapePoint &winner, double tol, int maxit,
                                   double data_error=0) {
  if (!winner.valid || winner.q==0 || y.size()<2 || !(tol>=1e-10) || maxit<1 ||
      !(data_error>=0) || !std::isfinite(data_error)) return false;
  std::vector<double> reflected;
  reflected.reserve(y.size());
  CPPUNIV_EXTENDED_REAL sum=0;
  for (double v:y) {
    if (!std::isfinite(v)) return false;
    reflected.push_back(-v);sum+=v;
  }
  bool matched=false;
  const std::int64_t per_side=certificate_budget(maxit)/2;
  const double dn=static_cast<double>(y.size());
  const double jacobian=static_cast<double>(2*sum/dn);
  for (int side=0;side<2;++side) {
    trgamma_detail::LogData d;
    const span data=side==0 ? y : span{reflected.data(),reflected.size()};
    if (!trgamma_detail::standardize(data,d)) return false;
    const double data_scale=std::min(d.sd,d.sd*d.umax);
    if (!(data_scale>0) || !std::isfinite(data_scale)) return false;
    const double pad=(512*std::numeric_limits<double>::epsilon()*(1+y.size())+16*data_error/data_scale)*
        (1+std::abs(winner.mean_loglik)+std::abs(winner.objective));
    if (!std::isfinite(pad)) return false;
    const auto fit=trgamma_detail::fit_free(d,tol,maxit,per_side);
    const double shift=side==0 ? 0 : -jacobian;
    if (fit.status==TrGammaStatus::Ok) {
      const double upper=fit.logLik/dn+shift;
      if (!std::isfinite(upper) || upper>winner.mean_loglik+pad) return false;
      const double q=(side==0 ? 1 : -1)/std::sqrt(fit.shape1);
      const double sigma=std::abs(q)/fit.shape2;
      const double t=fit.shape2*d.sd;
      const auto c=trgamma_detail::cgf(d,t);
      const double mu=(side==0 ? 1 : -1)*(d.mean+d.sd*c.K/t);
      const bool same=std::isfinite(mu) && sigma>0 && std::isfinite(sigma) && std::isfinite(q) &&
        std::abs(q-winner.q)<=0.5*tol*std::max(1.0,std::abs(winner.q)) &&
        std::abs(sigma-winner.sigma)<=0.5*tol*winner.sigma &&
        std::abs(mu-winner.mu)<=0.5*tol*std::max(winner.sigma,std::abs(winner.mu));
      if (same) matched=true;
      else return false;
    } else if (fit.status==TrGammaStatus::LognormalBoundary || fit.status==TrGammaStatus::PowerBoundary) {
      const double upper=fit.stop_detail.loglik_upper/dn+shift;
      if (!(std::isfinite(upper) && upper<winner.mean_loglik-pad)) return false;
    } else return false;
  }
  return matched;
}

}  // namespace gengamma_fit_detail
}  // namespace cppunivariate
