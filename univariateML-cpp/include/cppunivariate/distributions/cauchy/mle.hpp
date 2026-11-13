// Cauchy MLE: bivariate Newton on (mu, log-scale).
//
// Initialization: median and MAD. Safeguarded Newton steps with a
// scale-preconditioned score fallback when the observed Hessian is indefinite.
// Nonexistent MLEs and numerical failures are reported through CauchyStatus.

#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "cppunivariate/core/constants.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class CauchyStatus { Ok, NoMLE, InvalidInput, IterationFailure, AmbiguousMaximum };

struct CauchyResult {
  double mu;
  double scale;
  double logLik;
  int iterations;
  CauchyStatus status;
  StopDetail stop_detail{};
};

// Negative log-likelihood of Cauchy(mu, scale=s):
//   log f = -log(pi) - log(s) - log(1 + ((x-mu)/s)^2)
//   nll = -sum log f.
// Per-obs Hessian (z = (x-mu)/s):
//   H_mm = 2(1-z^2) / (s^2 (1+z^2)^2)
//   H_ms = 4z       / (s^2 (1+z^2)^2)
//   H_ss = (z^4 + 4z^2 - 1) / (s^2 (1+z^2)^2)
struct CauchyCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double mu = theta[0];
    const double s = theta[1];
    if (!(s > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double sum = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / s;
      sum += std::log(1.0 + z * z);
    }
    return n * (std::log(cppunivariate::pi) + std::log(s)) + sum;
  }
  void gradient(const double *theta, double *g) const {
    const double mu = theta[0];
    const double s = theta[1];
    const double n = static_cast<double>(x.size());
    double dnll_dmu = 0.0, dnll_ds = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / s;
      const double w = 1.0 / (1.0 + z * z);
      // d log f / dmu = 2z / (s(1+z^2)) = 2z*w/s
      // d log f / ds  = -1/s + 2z^2/(s(1+z^2)) = (2z^2*w - 1)/s
      dnll_dmu += -2.0 * z * w / s;
      dnll_ds += -(2.0 * z * z * w - 1.0) / s;
    }
    g[0] = dnll_dmu;
    g[1] = dnll_ds;
    (void)n;
  }
  void hessian(const double *theta, double *H) const {
    const double mu = theta[0];
    const double s = theta[1];
    const double s2 = s * s;
    double H_mm = 0.0, H_ms = 0.0, H_ss = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / s;
      const double zz = z * z;
      const double w = 1.0 / (1.0 + zz);
      const double w2 = w * w;
      H_mm += 2.0 * (1.0 - zz) * w2;
      H_ms += 4.0 * z * w2;
      H_ss += (zz * zz + 4.0 * zz - 1.0) * w2;
    }
    H[0] = H_mm / s2;
    H[1] = H_ms / s2;
    H[2] = H[1];
    H[3] = H_ss / s2;
  }
};

inline CauchyResult fit_cauchy(span x, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  constexpr double kEps = std::numeric_limits<double>::epsilon();
  const std::size_t n = x.size();
  // An empty sample is invalid input; one finite observation is an atom
  // carrying all the mass, so the likelihood is unbounded.
  if (n == 0) return {kNaN, kNaN, kNaN, 0, CauchyStatus::InvalidInput};
  for (auto v : x) {
    if (!std::isfinite(v)) return {kNaN, kNaN, kNaN, 0, CauchyStatus::InvalidInput};
  }
  if (n < 2) return {kNaN, kNaN, kNaN, 0, CauchyStatus::NoMLE};

  std::vector<double> data(x.begin(), x.end());
  std::sort(data.begin(), data.end());
  // Two equally weighted support points have a continuum of attained
  // maxima (including location midpoint, scale half their separation).
  if (n % 2 == 0 && data.front() != data.back() &&
      data[n / 2 - 1] == data.front() && data[n / 2] == data.back()) {
    const double mu = data.front() / 2.0 + data.back() / 2.0;
    const double scale = data.back() / 2.0 - data.front() / 2.0;
    return {mu, scale, -static_cast<double>(n) *
            (std::log(2.0 * cppunivariate::pi) + std::log(scale)), 0, CauchyStatus::Ok};
  }
  std::size_t run = 1;
  for (std::size_t i = 1; i < n; ++i) {
    run = data[i] == data[i - 1] ? run + 1 : 1;
    if (2 * run >= n) return {kNaN, kNaN, kNaN, 0, CauchyStatus::NoMLE};
  }
  const double span_scale = data.back() / 2.0 - data.front() / 2.0;
  const double centre = data.front() / 2.0 + data.back() / 2.0;
  if (span_scale > 0.0 && std::isfinite(span_scale) &&
      (span_scale < 1e-100 || span_scale > 1e100 || std::abs(centre) > 1e6 * span_scale)) {
    for (std::size_t i = 0; i < n; ++i) data[i] = static_cast<double>((static_cast<CPPUNIV_EXTENDED_REAL>(x[i]) - centre) / span_scale);
    const auto r = fit_cauchy(span{data.data(), n}, tol, maxit);
    return {centre + span_scale * r.mu, span_scale * r.scale,
            r.logLik - static_cast<double>(n) * std::log(span_scale), r.iterations, r.status};
  }
  double mu = data[n / 2];

  for (std::size_t i = 0; i < n; ++i) data[i] = std::fabs(data[i] - mu);
  std::nth_element(data.begin(), data.begin() + n / 2, data.end());
  double theta = std::log(data[n / 2] + kEps);

  double dmu = 0.0, dth = 0.0;
  int iter = 1;
  for (; iter <= maxit; ++iter) {
    const double inv_s = std::exp(-theta);
    const double inv_s2 = inv_s * inv_s;

    double sum1 = 0.0, sum2 = 0.0, sum3 = 0.0, sum4 = 0.0, sum5 = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
      const double z = (x[i] - mu) * inv_s;
      const double z2 = z * z;
      const double d1 = 1.0 + z2;
      const double i1 = 1.0 / d1;
      const double i2 = i1 * i1;
      sum1 += z * i1;
      sum2 += (z2 - 1) * i1;
      sum3 += (1 - z2) * i2;
      sum4 += z * i2;
      sum5 += z2 * i2;
    }

    const double U_mu = 2 * inv_s * sum1;
    const double U_th = sum2;
    const double H_mm = 2 * inv_s2 * sum3;
    const double H_mth = 4 * inv_s * sum4;
    const double H_thth = 4 * sum5;

    const double det = H_mm * H_thth - H_mth * H_mth;
    dmu = (H_thth * U_mu - H_mth * U_th) / det;
    dth = (-H_mth * U_mu + H_mm * U_th) / det;

    // An indefinite observed Hessian can point away from the maximum.
    // Fall back to a scale-preconditioned score and backtrack in log scale.
    if (!(H_mm > 0.0 && det > 0.0) || !std::isfinite(dmu + dth)) {
      dmu = U_mu * std::exp(2.0 * theta) / static_cast<double>(n);
      dth = U_th / static_cast<double>(n);
    }
    const CauchyCost cost{x};
    const double current[] = {mu, std::exp(theta)};
    const double old = cost(current);
    double step = 1.0;
    bool accepted = false;
    for (int ls = 0; ls < maxit; ++ls) {
      const double candidate[] = {mu + step * dmu, std::exp(theta + step * dth)};
      if (std::isfinite(candidate[0]) && std::isfinite(candidate[1]) &&
          candidate[1] > 0.0 && std::isfinite(cost(candidate)) && cost(candidate) <= old) {
        accepted = true;
        break;
      }
      step *= 0.5;
    }
    if (!accepted) return {mu, std::exp(theta), -old, iter, CauchyStatus::IterationFailure};
    dmu *= step;
    dth *= step;
    mu += dmu;
    theta += dth;
    if (std::max(std::fabs(dmu), std::fabs(dth)) < tol) break;
  }

  const double scale = std::exp(theta);
  double sum_log1p_z2 = 0.0;
  double score_location = 0.0, score_log_scale = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    const double z = (x[i] - mu) / scale;
    sum_log1p_z2 += std::log1p(z * z);
    const double w = 1.0 / (1.0 + z * z);
    score_location += 2.0 * z * w;
    score_log_scale += (z * z - 1.0) * w;
  }
  const double logLik =
      -static_cast<double>(n) * std::log(cppunivariate::pi * scale) - sum_log1p_z2;
  const bool finite = std::isfinite(mu) && std::isfinite(scale) && scale > 0.0 && std::isfinite(logLik);
  // Total log-likelihood score in (mu, log(scale)), measured in the
  // hyperbolic orthonormal frame: (scale * U_mu, U_log_scale).
  // This dimensionless gate is invariant under location/scale changes.
  // It is a practical stationarity margin, not a likelihood-gap bound.
  const bool stationary = std::isfinite(score_location) && std::isfinite(score_log_scale) &&
      std::hypot(score_location, score_log_scale) < 1e-3;
  return {mu, scale, logLik, iter, finite && stationary && iter <= maxit ? CauchyStatus::Ok : CauchyStatus::IterationFailure};
}


// Held-scale certificate; formulas and exact-arithmetic proof in README.md.
namespace cauchy_location_detail {
struct Sum {
  double value = 0, correction = 0;
  void add(double x) {
    const double y = x - correction, t = value + y;
    correction = (t - value) - y;
    value = t;
  }
};
inline double score_term(double z) {
  if (std::abs(z) > 1) { const double r = 1/z; return 2*r/(1+r*r); }
  return 2*z/(1+z*z);
}
inline double curvature_term(double z) {
  const double r = 1/std::hypot(1.0,z), q = z*r;
  return 2*(q*q-r*r)*r*r;
}
struct Bounds { double lower_score, upper_score, upper_curvature, upper_ll; };
inline Bounds bounds(const std::vector<double>& y, double a, double b) {
  Sum lower, upper, curvature, likelihood;
  constexpr double root3 = 1.7320508075688772935;
  for (double v : y) {
    const double l = v-b, h = v-a;
    double gl = std::min(score_term(l),score_term(h));
    double gh = std::max(score_term(l),score_term(h));
    if (l <= -1 && h >= -1) gl = -1;
    if (l <= 1 && h >= 1) gh = 1;
    lower.add(gl); upper.add(gh);
    double ch = std::max(curvature_term(l),curvature_term(h));
    if ((l <= -root3 && h >= -root3) || (l <= root3 && h >= root3)) ch = .25;
    curvature.add(ch);
    const double d = l > 0 ? l : (h < 0 ? h : 0);
    likelihood.add(-2*std::log(std::hypot(1.0,d)));
  }
  return {lower.value,upper.value,curvature.value,likelihood.value};
}
inline double score(const std::vector<double>& y, double t) {
  Sum sum; for (double v : y) sum.add(score_term(v-t)); return sum.value;
}
inline double likelihood(const std::vector<double>& y, double t) {
  Sum sum; for (double v : y) sum.add(-2*std::log(std::hypot(1.0,v-t))); return sum.value;
}
} // namespace cauchy_location_detail

inline CauchyResult fit_cauchy_location(const std::vector<double>& data, double scale, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan = std::numeric_limits<double>::quiet_NaN();
  int evaluations = 0;
  const auto failure = [&]() { return CauchyResult{nan,scale,nan,evaluations,CauchyStatus::IterationFailure}; };
  if (data.empty() || !(scale > 0 && std::isfinite(scale)) || maxit < 1) return failure();
  const double centre = data[data.size()/2];
  std::vector<double> y;
  y.reserve(data.size());
  for (double v : data) {
    const double z = static_cast<double>((static_cast<CPPUNIV_EXTENDED_REAL>(v)-centre)/scale);
    // Keep all interval differences finite, including on double-width platforms.
    if (!std::isfinite(z) || std::abs(z) > 1e150) return failure();
    y.push_back(z);
  }
  const double n = static_cast<double>(y.size());
  const double eps = std::numeric_limits<double>::epsilon();
  const double score_margin = 128*eps*n;
  // Compensated sums; allowance includes elementary-function and coordinate rounding.
  const double ll_margin = 512*eps*n*(1+2*std::log(std::hypot(1.0,y.back()-y.front())));
  struct Box { double a, b; int depth; };
  struct Root { double a, b, t, ll; };
  std::vector<Box> pending{{y.front(),y.back(),0}};
  std::vector<Root> roots;
  double incumbent = cauchy_location_detail::likelihood(y,0);
  const std::int64_t budget = certificate_budget(maxit);
  while (!pending.empty()) {
    if (evaluations >= budget) { auto out=failure(); out.stop_detail.budget_exhausted=true;
      const double ll=cauchy_location_detail::likelihood(y,0)-n*std::log(std::acos(-1.0)*scale);
      out.stop_detail.candidates.push_back({{centre,scale},ll,ll});
      out.stop_detail.loglik_lower=ll;out.stop_detail.loglik_upper=INFINITY;return out; }
    const Box box = pending.back(); pending.pop_back(); ++evaluations;
    const auto bound = cauchy_location_detail::bounds(y,box.a,box.b);
    if (bound.upper_ll < incumbent-ll_margin || bound.lower_score > score_margin ||
        bound.upper_score < -score_margin) continue;
    if (bound.upper_curvature < -score_margin) {
      // Expand slightly so a root at a partition endpoint has strict signs.
      const double pad = 1e-7*std::max(1.0,std::max(std::abs(box.a),std::abs(box.b)));
      double a = box.a-pad, b = box.b+pad;
      const auto extended = cauchy_location_detail::bounds(y,a,b);
      const double ga = cauchy_location_detail::score(y,a), gb = cauchy_location_detail::score(y,b);
      if (extended.upper_curvature < -score_margin &&
          (ga < -score_margin || gb > score_margin)) continue;
      if (extended.upper_curvature < -score_margin && ga > score_margin && gb < -score_margin) {
        bool converged = false;
        for (int j = 0; j < maxit; ++j) {
          const double t = a/2+b/2, g = cauchy_location_detail::score(y,t);
          // A small score alone is not a bracket; keep both endpoint signs.
          if (g > 0) a = t; else if (g < 0) b = t;
          else {
            const double d = 32*eps*std::max(1.0,std::abs(t));
            const double left = std::max(a,t-d), right = std::min(b,t+d);
            if (cauchy_location_detail::score(y,left) > 0 && cauchy_location_detail::score(y,right) < 0) {
              a = left; b = right;
            }
          }
          if (b-a <= 1e-10 && std::abs(cauchy_location_detail::score(y,a/2+b/2)) < 1e-7) {
            converged = true; break;
          }
        }
        if (!converged) return failure();
        const double t = a/2+b/2, f = cauchy_location_detail::likelihood(y,t);
        bool duplicate = false;
        for (const auto& root : roots) {
          // Two roots in a strictly concave union must be the same root.
          const auto hull = cauchy_location_detail::bounds(y,std::min(a,root.a),std::max(b,root.b));
          if (hull.upper_curvature < -score_margin) { duplicate = true; break; }
        }
        if (!duplicate) roots.push_back({a,b,t,f});
        incumbent = std::max(incumbent,f);
        continue;
      }
    }

    const double mid = box.a/2+box.b/2;
    if (!(mid > box.a && mid < box.b)) return failure();
    // Visit the interval nearer the median first, improving the likelihood bound.
    if (mid > 0) {
      pending.push_back({mid,box.b,box.depth+1}); pending.push_back({box.a,mid,box.depth+1});
    } else {
      pending.push_back({box.a,mid,box.depth+1}); pending.push_back({mid,box.b,box.depth+1});
    }
  }
  if (roots.empty()) return failure();
  std::size_t winner = 0;
  for (std::size_t j = 1; j < roots.size(); ++j) if (roots[j].ll > roots[winner].ll) winner = j;
  const auto& best = roots[winner];
  for (std::size_t j = 0; j < roots.size(); ++j) {
    if (j == winner) continue;
    const double width = roots[j].b-roots[j].a;
    // |ell''| <= 2n and |score| <= 1e-7 at a candidate.
    const double upper = roots[j].ll + 1e-7*width+n*width*width;
    if (!(best.ll > upper+ll_margin)) {
      CauchyResult out{nan,scale,nan,evaluations,CauchyStatus::AmbiguousMaximum};
      const double offset=n*(std::log(cppunivariate::pi)+std::log(scale));
      for (const auto& r : roots) {
        const double width=r.b-r.a;
        const double bound=r.ll+1e-7*width+n*width*width+ll_margin;
        if (bound >= best.ll) out.stop_detail.candidates.push_back({
          {static_cast<double>(static_cast<CPPUNIV_EXTENDED_REAL>(centre)+
             static_cast<CPPUNIV_EXTENDED_REAL>(scale)*r.t),scale},
          r.ll-offset-ll_margin,bound-offset});
      }
      return out;
    }
  }
  const double location = static_cast<double>(static_cast<CPPUNIV_EXTENDED_REAL>(centre)+
      static_cast<CPPUNIV_EXTENDED_REAL>(scale)*best.t);
  const double returned_t = static_cast<double>((static_cast<CPPUNIV_EXTENDED_REAL>(location)-centre)/scale);
  if (!std::isfinite(location) || std::abs(cauchy_location_detail::score(y,returned_t)) >= 1e-7 ||
      returned_t < best.a-1e-10 || returned_t > best.b+1e-10) return failure();
  const double ll = cauchy_location_detail::likelihood(y,returned_t)-n*(std::log(cppunivariate::pi)+std::log(scale));
  if (!std::isfinite(ll)) return failure();
  return {location,scale,ll,evaluations,CauchyStatus::Ok};
}


inline CauchyResult fit_cauchy(span x,double tol,int maxit,double location,double scale) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(std::isnan(location)&&std::isnan(scale))return fit_cauchy(x,tol,maxit);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CauchyResult bad{nan,nan,nan,0,CauchyStatus::InvalidInput};
  if(x.empty()||!(tol>0)||maxit<1)return bad;
  std::vector<double> y(x.begin(),x.end());for(double v:y)if(!std::isfinite(v))return bad;std::sort(y.begin(),y.end());
  double m=location,s=scale;int it=0;
  auto ll=[&](double a,double b){double f=0;for(double v:y)f-=std::log(cppunivariate::pi)+std::log(b)+2*std::log(std::hypot(1.0,(v-a)/b));return f;};
  if(std::isnan(s)) {
    if (!std::isfinite(m)) return bad;
    std::size_t k = 0;
    std::vector<double> log_residuals;
    for (double v : y) {
      if (v == m) { ++k; continue; }
      const double residual = std::abs(v - m);
      // Opposite finite extremes can have an unrepresentable difference.
      log_residuals.push_back(std::isfinite(residual) ? std::log(residual) :
          std::log(std::abs(v / 2 - m / 2)) + std::log(2.0));
    }
    if (2*k >= y.size()) { bad.status=CauchyStatus::NoMLE; return bad; }
    const auto failure = [&]() {
      return CauchyResult{m, nan, nan, it, CauchyStatus::IterationFailure};
    };
    const auto score = [&](double t) {
      double g = -static_cast<double>(k);
      for (double r : log_residuals) g += std::tanh(r - t);
      return g;
    };
    const auto bounds = std::minmax_element(log_residuals.begin(), log_residuals.end());
    const double floor = std::log(std::numeric_limits<double>::denorm_min());
    const double ceiling = std::log(std::numeric_limits<double>::max());
    double lo = std::max(floor, *bounds.first), hi = std::min(ceiling, *bounds.second);
    double step = 1;
    for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (!(score(lo) > 0) && lo > floor); ++bracket_pass) {
      lo = std::max(floor, lo - step); step *= 2;
    }
    step = 1;
    for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (!(score(hi) < 0) && hi < ceiling); ++bracket_pass) {
      hi = std::min(ceiling, hi + step); step *= 2;
    }
    if (!(score(lo) > 0 && score(hi) < 0)) return failure();
    bool converged = false;
    for (; it < maxit; ++it) {
      const double t = lo/2 + hi/2;
      const double g = score(t);
      if (!std::isfinite(g)) return failure();
      if (g > 0) lo = t; else if (g < 0) hi = t;
      else { lo = t; hi = t; }
      if (hi-lo < 1e-12) { converged = true; break; }
    }
    if (!converged) return failure();
    const double t = lo/2 + hi/2;
    s = std::exp(t);
    if (!(s > 0 && std::isfinite(s))) return failure();
    // Rounding to a subnormal scale may destroy log-coordinate accuracy.
    if (std::abs(score(std::log(s))) > 1e-10 * y.size()) return failure();
    double logLik = -static_cast<double>(y.size()) * (std::log(cppunivariate::pi) + std::log(s));
    for (double r : log_residuals) {
      const double d = r - std::log(s);
      logLik -= 2*std::max(d, 0.0) + std::log1p(std::exp(-2*std::abs(d)));
    }
    if (!std::isfinite(logLik)) return failure();
    return {m, s, logLik, it, CauchyStatus::Ok};
  } else if(std::isnan(m)) {
    return fit_cauchy_location(y,s,maxit);
  }
  return {m,s,ll(m,s),it,it<maxit?CauchyStatus::Ok:CauchyStatus::IterationFailure};
}


inline FitOutcome outcome(const CauchyResult& r) {
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  if (r.stop_detail.budget_exhausted) return FitOutcome::budget();
  switch (r.status) {
    case CauchyStatus::Ok: return FitOutcome::estimate(true);
    case CauchyStatus::NoMLE: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case CauchyStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case CauchyStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case CauchyStatus::AmbiguousMaximum: return FitOutcome::estimate(false);
  }
  return {};
}


}  // namespace cppunivariate
