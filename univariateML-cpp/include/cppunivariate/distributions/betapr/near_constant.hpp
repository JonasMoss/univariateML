// Beta prime on a near-constant sample: the supremum is a gamma or inverse-gamma face.
//
// For x_i = x0 (1 + v_i), max |v_i| <= 1e-6, the log likelihood of every
// beta-prime member with finite shapes stays below the gamma face (q -> infinity)
// or the inverse-gamma face (p -> infinity). Both face shapes are about
// 1 / (2 c) with c = log(mean) - mean(log) of x, respectively of 1/x, so they
// are 1e12 to 1e32 and the logs of the observations carry no information.
// Everything below works on the offsets v_i, which are exact to rounding.
//
// Which face is higher follows the skewness of the sample. With u the centered
// relative offsets, c_gamma = s2/2 - s3/3 and c_invgamma = s2/2 - 2 s3/3 to
// leading order (s2, s3 the second and third moments of u), so
//   L_invgamma - L_gamma = -(n/2) log(c_invgamma / c_gamma) = n s3 / (3 s2) (1 + O(rho)),
// positive for a right-skewed sample (two low, one high). The difference is far
// below the resolution of a double log likelihood (about 1e-17 relative to 100
// for n = 3). The sign of the leading term decides; the constant 64 in the
// validity test below is not proved. The other model members (finite shapes) are
// not excluded by a bound: the outcome is a margin-decided limit.
#pragma once
#include "../gamma/relative.hpp"
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate { namespace betapr_detail {

inline bool near_constant_limit(span x, BetaPrimeResult& out) {
  const std::size_t count = x.size();
  if (count < 3) return false;
  double lo = std::numeric_limits<double>::infinity(), hi = -lo;
  for (double v : x) {
    if (!(v > 0) || !std::isfinite(v)) return false;
    lo = std::min(lo, v); hi = std::max(hi, v);
  }
  if (!(hi > lo)) return false;
  // hi < 2 lo, so hi - lo is exact.
  if (!((hi - lo) / lo <= 1e-6)) return false;
  const double n = static_cast<double>(count);
  std::vector<double> e(count), v(count), w(count);
  relative_gamma::Sum esum;
  for (std::size_t i = 0; i < count; ++i) { e[i] = (x[i] - lo) / lo; esum.add(e[i]); }
  const double ebar = esum.value() / n, s = 1 + ebar;
  relative_gamma::Sum vsum, wsum, logsum;
  double vmax = 0;
  for (std::size_t i = 0; i < count; ++i) {
    v[i] = (e[i] - ebar) / s;
    w[i] = -v[i] / (1 + v[i]);
    vsum.add(v[i]); wsum.add(w[i]); logsum.add(std::log1p(v[i]));
    vmax = std::max(vmax, std::abs(v[i]));
  }
  const double vbar = vsum.value() / n, wbar = wsum.value() / n;
  relative_gamma::Sum m2, m3;
  for (double t : v) { const double d = t - vbar; m2.add(d * d); m3.add(d * d * d); }
  const double s2 = m2.value() / n, s3 = m3.value() / n;
  // Skewness must beat the fourth-order terms, which are at most 64 vmax^2 s2.
  if (!(s2 > 0) || !(std::abs(s3) > 64 * vmax * vmax * s2)) return false;
  const double cg = relative_gamma::log_ratio(v), ci = relative_gamma::log_ratio(w);
  if (!(cg > 0) || !(ci > 0) || !(cg < 1e-3) || !(ci < 1e-3)) return false;
  const double ag = relative_gamma::shape_from_ratio(cg), ai = relative_gamma::shape_from_ratio(ci);
  const double logxt = std::log(lo) + std::log1p(ebar);
  const double scale_g = lo * s * (1 + vbar) / ag, scale_i = ai * lo * s / (1 + wbar);
  // The log likelihood at the returned (rounded) coefficients: the supremum of each face minus the loss
  // from the rounding of the shape and scale, which is of order 1 at these shapes.
  const auto mean_x = relative_gamma::dd_mean(x.data(), count, false);
  const auto mean_y = relative_gamma::dd_mean(x.data(), count, true);
  const double llg = n * (relative_gamma::loglik_core(ag, cg) - logxt - std::log1p(vbar)
                          + relative_gamma::scale_loss(mean_x, ag, scale_g));
  // Inverse gamma: y = 1/x is gamma with rate scale_i.
  const double lli = n * (relative_gamma::loglik_core(ai, ci) - logxt - std::log1p(wbar)
                          + relative_gamma::scale_loss(mean_y, ai, scale_i, true)) - 2 * logsum.value();
  if (!std::isfinite(llg) || !std::isfinite(lli)) return false;
  const bool inverse = s3 > 0;
  const std::vector<double> coef_g{ag, scale_g}, coef_i{ai, scale_i};
  if (!valid_stop_coefficients(StopModel::Gamma, coef_g) || !valid_stop_coefficients(StopModel::InvGamma, coef_i)) return false;
  const double err = 512 * std::numeric_limits<double>::epsilon() * (1 + std::abs(llg) + n * (1 + std::abs(logxt)));
  const double win = inverse ? lli : llg, lose = inverse ? llg : lli;
  StopDetail d;
  d.limit_model = inverse ? StopModel::InvGamma : StopModel::Gamma;
  d.limit_coef = inverse ? coef_i : coef_g;
  d.limit_loglik = win;
  d.candidates.push_back({d.limit_coef, win - err, win + err, d.limit_model});
  d.candidates.push_back({inverse ? coef_g : coef_i, lose - err, lose + err, inverse ? StopModel::Gamma : StopModel::InvGamma});
  d.loglik_lower = win - err; d.loglik_upper = std::numeric_limits<double>::infinity();
  out.stop_detail = d;
  out.status = inverse ? BetaPrimeStatus::InvGammaBoundary : BetaPrimeStatus::GammaBoundary;
  out.alpha = inverse ? std::numeric_limits<double>::infinity() : ag;
  out.beta = inverse ? ai : std::numeric_limits<double>::infinity();
  out.scale = inverse ? 0 : std::numeric_limits<double>::infinity();
  out.logLik = win;
  out.certified = false; out.margin_decided = true; out.search_failed = false;
  out.certificate_failure = LomaxCertificateFailure::None;
  return true;
}

}}  // namespace cppunivariate::betapr_detail
