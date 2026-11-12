// Local signed-profile root inclusion. Global winner comparison is separate.
#pragma once
#include "../trgamma/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>

namespace cppunivariate {
namespace gengamma_fit_detail {

struct ProfileRoot {
  // delta is a proposal diagnostic; cancellation near t=0 prevents using
  // it alone to compare a shallow candidate with the lognormal member.
  double mu, sigma, q, t, delta;
  int evaluations;
  bool ok;
};

// A positive left score and negative right score imply that the maximum of
// the continuous profile on [left,right] is attained in its interior. This
// requires neither global nor local uniqueness. K(t)/t increases, and the
// inner gamma shape decreases, so endpoint bounds enclose the reconstructed
// location, shape and scale of every point in the remaining root interval.
inline ProfileRoot profile_root(const trgamma_detail::LogData &d,
                                double left, double right, double tol, int maxit) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  ProfileRoot bad{nan,nan,nan,nan,nan,0,false};
  if (!(left > 0 && right > left && tol > 0 && std::isfinite(tol)) || maxit < 1)
    return bad;
  if (tol < 128*std::numeric_limits<double>::epsilon()) return bad;
  int evaluations=0;
  auto pl=trgamma_detail::profile(d,left,evaluations,maxit);
  auto pr=trgamma_detail::profile(d,right,evaluations,maxit);
  if (!(std::isfinite(pl.slope) && std::isfinite(pr.slope) && pl.slope > 0 && pr.slope < 0))
    return bad;
  const int budget=static_cast<int>(solver_budget(maxit));
  for (int i=0;i<budget;++i) {
    const double mid=left+(right-left)/2;
    if (!(mid>left && mid<right)) break;
    const auto p=trgamma_detail::profile(d,mid,evaluations,maxit);
    if (!(std::isfinite(p.slope) && p.shape>0 && std::isfinite(p.shape))) break;
    if (p.slope>0) {left=mid;pl=p;}
    else if (p.slope<0) {right=mid;pr=p;}
    else {
      const double a=left+(mid-left)/2, b=mid+(right-mid)/2;
      const auto pa=trgamma_detail::profile(d,a,evaluations,maxit);
      const auto pb=trgamma_detail::profile(d,b,evaluations,maxit);
      if (!(pa.slope>0 && pb.slope<0)) break;
      left=a;right=b;pl=pa;pr=pb;
    }
    // The margin surrounds the computed inner shape, and the two signs
    // below check that it brackets the exact inner inverse-digamma root.
    double margin=64*std::numeric_limits<double>::epsilon();
    double alo=nan, ahi=nan;
    bool enclosed=false;
    for (int j=0;j<budget && margin<0.5;++j) {
      alo=pr.shape*(1-margin); ahi=pl.shape*(1+margin);
      const double low_value=trgamma_detail::log_minus_digamma(alo);
      const double high_value=trgamma_detail::log_minus_digamma(ahi);
      const double comparison_pad=64*std::numeric_limits<double>::epsilon()*(1+d.n);
      if (low_value>pr.K+comparison_pad*std::max(std::abs(low_value),std::abs(pr.K)) &&
          high_value+comparison_pad*std::max(std::abs(high_value),std::abs(pl.K))<pl.K) { enclosed=true; break; }
      margin*=2;
    }
    if (!enclosed) continue;
    const double qlo=1/std::sqrt(ahi), qhi=1/std::sqrt(alo);
    const double slo=d.sd*qlo/right, shi=d.sd*qhi/left;
    const double mlo=d.mean+d.sd*pl.K/left, mhi=d.mean+d.sd*pr.K/right;
    if (!(qhi>=qlo && shi>=slo && mhi>=mlo)) continue;
    const double q=1/std::sqrt(p.shape), sigma=d.sd*q/mid;
    const double mu=d.mean+d.sd*p.K/mid;
    const double pad=64*std::numeric_limits<double>::epsilon()*(1+d.n);
    if (std::isfinite(mu) && sigma>0 && std::isfinite(sigma) && std::isfinite(q) &&
        qhi-qlo+pad*std::max(1.0,q) <= tol*std::max(1.0,q) &&
        shi-slo+pad*sigma <= tol*sigma &&
        mhi-mlo+pad*std::max(sigma,std::abs(mu)) <= tol*std::max(sigma,std::abs(mu)))
      return {mu,sigma,q,mid,p.delta,evaluations,true};
  }
  bad.evaluations=evaluations;
  return bad;
}

}  // namespace gengamma_fit_detail
}  // namespace cppunivariate
