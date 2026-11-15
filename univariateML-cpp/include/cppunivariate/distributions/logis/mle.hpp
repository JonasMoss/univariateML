// Logistic MLE: bivariate Newton on (mu, log-scale).
//
// Initialization via Welford one-pass mean/variance; theta seeded so
// scale ≈ sqrt(var) * sqrt(3) / pi (matches Logistic distribution variance).
//
// Convergence is on the undamped Newton step, max(|dmu| / max(|mu|, scale),
// |dtheta|) <= tol -- a 2D Newton, so outside the scope of
// cppunivariate::newton_raphson_1d. The fit moves to standardized coordinates
// (x - mean)/sd when the data's magnitude would make rounding noise in the
// score comparable to the step target.

#pragma once
#include "dpqr.hpp"
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "cppunivariate/core/constants.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"
#include "../norm/mle.hpp"
#include <vector>

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

// Unbounded: the scale collapses at the common value of constant data (one
// observation included) or at a held location every observation equals.
enum class LogisticStatus { Ok, InvalidInput, IterationFailure, Unbounded, NumericalFailure };

struct LogisticResult {
  double mu;
  double scale;
  double logLik;
  int iterations;
  LogisticStatus status;
};

// Negative log-likelihood of Logistic(mu, scale=s):
//   log f = -z - log(s) - 2 log(1 + e^{-z}),  z = (x-mu)/s.
// Per-obs Hessian (P = 1/(1+e^{-z}), so 2 P (1-P) = sech^2(z/2)/2):
//   H_mm = 2 P(1-P) / s^2
//   H_ms = (2P - 1 + 2 P(1-P) z) / s^2
//   H_ss = (2(2P-1) z + 2 P(1-P) z^2 - 1) / s^2
struct LogisticCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double mu = theta[0];
    const double s = theta[1];
    if (!(s > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double acc = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / s;
      // numerically stable: log(1 + e^{-z}) = max(0,-z) + log1p(exp(-|z|))
      const double az = std::fabs(z);
      const double log1pe = std::log1p(std::exp(-az)) + (z < 0.0 ? -z : 0.0);
      acc += -z - 2.0 * log1pe;
    }
    return n * std::log(s) - acc;
  }
  void gradient(const double *theta, double *g) const {
    const double mu = theta[0];
    const double s = theta[1];
    double dnll_dmu = 0.0, dnll_ds = 0.0;
    const double n = static_cast<double>(x.size());
    for (auto v : x) {
      const double z = (v - mu) / s;
      // P = 1/(1+e^{-z}); stable for z in either sign.
      const double P = (z >= 0.0)
                           ? 1.0 / (1.0 + std::exp(-z))
                           : std::exp(z) / (1.0 + std::exp(z));
      // dlogf/dmu = (2P-1)/s; dlogf/ds = -[(1-2P) z + 1]/s.
      dnll_dmu += -(2.0 * P - 1.0) / s;
      dnll_ds += ((1.0 - 2.0 * P) * z + 1.0) / s;
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
      const double P = (z >= 0.0)
                           ? 1.0 / (1.0 + std::exp(-z))
                           : std::exp(z) / (1.0 + std::exp(z));
      const double PQ = P * (1.0 - P);
      const double two_Pm1 = 2.0 * P - 1.0;
      H_mm += 2.0 * PQ;
      H_ms += two_Pm1 + 2.0 * PQ * z;
      H_ss += 2.0 * two_Pm1 * z + 2.0 * PQ * z * z - 1.0;
    }
    H[0] = H_mm / s2;
    H[1] = H_ms / s2;
    H[2] = H[1];
    H[3] = H_ss / s2;
  }
};

namespace logistic_detail {
// Damped Newton on (mu, log scale) from a Welford start. The caller has
// checked n >= 2, finite and non-constant data.
inline LogisticResult newton(span x, double tol, int maxit) {
  const std::size_t n = x.size();
  const double nan = std::numeric_limits<double>::quiet_NaN();
  // One-pass Welford mean/variance for the seed.
  double mean = 0.0;
  double M2 = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    const double diff = x[i] - mean;
    mean += diff / static_cast<double>(i + 1);
    M2 += diff * (x[i] - mean);
  }

  double mu = mean;
  double sample_var = (n > 1) ? M2 / static_cast<double>(n - 1) : 0.0;
  if (!(sample_var > 0.0) || !std::isfinite(sample_var))
    return {nan, nan, nan, 0, LogisticStatus::InvalidInput};
  double theta = std::log(std::sqrt(sample_var) * std::sqrt(3.0) / cppunivariate::pi);

  int iter = 0;
  for (iter = 0; iter < maxit; ++iter) {
    const double inv_s = std::exp(-theta);
    const double inv_s2 = inv_s * inv_s;

    double U1 = 0.0, U2 = 0.0;
    double sum_c2 = 0.0, sum_tz = 0.0, sum_z2c2 = 0.0;
    for (auto xi : x) {
      const double z = (xi - mu) * inv_s;
      const double th = std::tanh(0.5 * z);
      const double c2 = 1.0 - th * th;
      U1 += th;
      U2 += z * th;
      sum_c2 += c2;
      sum_tz += th + 0.5 * z * c2;
      sum_z2c2 += z * z * c2;
    }
    U1 *= inv_s;
    U2 -= static_cast<double>(n);

    const double J11 = 0.5 * sum_c2 * inv_s2;
    const double J12 = inv_s * sum_tz;
    const double J22 = (U2 + static_cast<double>(n)) + 0.5 * sum_z2c2;

    const double det = J11 * J22 - J12 * J12;
    double dmu = (J22 * U1 - J12 * U2) / det;
    double dtheta = (-J12 * U1 + J11 * U2) / det;

    bool newton_direction = true;
    if (!(J11 > 0.0 && det > 0.0) || !std::isfinite(dmu + dtheta)) {
      newton_direction = false;
      dmu = U1 * std::exp(2.0 * theta) / static_cast<double>(n);
      dtheta = U2 / static_cast<double>(n);
    }
    const LogisticCost cost{x};
    const double current[] = {mu, std::exp(theta)};
    const double old = cost(current);
    // Convergence is judged on the undamped Newton step (see below), measured
    // before the line search shortens it.
    const double full = std::max(std::fabs(dmu) * inv_s / std::max(1.0, std::fabs(mu) * inv_s), std::fabs(dtheta));
    // A candidate is accepted when the cost does not rise by more than the
    // rounding noise of the sum (n terms of size about 1 each carry 1 eps):
    // near the optimum the true change of a full step is below that noise,
    // and a strict test would reject a good step and falsely damp it.
    const double allowance = 64 * std::numeric_limits<double>::epsilon() * std::max(std::fabs(old), static_cast<double>(n));
    double step = 1.0;
    bool accepted = false;
    for (int ls = 0; ls < maxit; ++ls) {
      const double candidate[] = {mu + step * dmu, std::exp(theta + step * dtheta)};
      if (std::isfinite(candidate[0]) && std::isfinite(candidate[1]) &&
          candidate[1] > 0.0 && std::isfinite(cost(candidate)) && cost(candidate) <= old + allowance) {
        accepted = true;
        break;
      }
      step *= 0.5;
    }
    if (!accepted) return {mu, std::exp(theta), -old, iter, LogisticStatus::IterationFailure};
    dmu *= step;
    dtheta *= step;
    mu += dmu;
    theta += dtheta;

    // Stop after applying an accepted undamped step whose size is at most
    // tol in the contract's units: the location step relative to
    // max(|mu|, scale) and the log-scale step (a relative scale step). The
    // observed information is positive definite when the Newton direction is
    // used (det > 0, J11 > 0), so H^-1 g is the Newton estimate of the error
    // of the iterate before the step, accurate to second order by quadratic
    // convergence; after the step the error is of order full^2. A damped
    // step says nothing about the distance to the optimum, so it never stops
    // the iteration.
    if (accepted && step == 1.0 && newton_direction && full <= tol) break;
  }

  const double scale = std::exp(theta);
  const double params[] = {mu, scale};
  const double logLik = -LogisticCost{x}(params);
  const bool finite = std::isfinite(mu) && std::isfinite(scale) && scale > 0.0 && std::isfinite(logLik);
  return {mu, scale, logLik, iter == maxit ? maxit : iter + 1,
          finite && iter < maxit ? LogisticStatus::Ok : LogisticStatus::IterationFailure};
}
}  // namespace logistic_detail

inline LogisticResult logistic_mle(span x, double tol, int maxit) {
  if(!(tol>0 && std::isfinite(tol)) || maxit<1 || x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return {NAN,NAN,NAN,0,LogisticStatus::InvalidInput};

  auto result = [&]() -> LogisticResult {
  maxit = static_cast<int>(solver_budget(maxit));
  const std::size_t n = x.size();

  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (n == 0) return {nan, nan, nan, 0, LogisticStatus::InvalidInput};
  for (auto v : x) {
    if (!std::isfinite(v)) return {nan, nan, nan, 0, LogisticStatus::InvalidInput};
  }
  // Constant data (one observation included): the likelihood is unbounded
  // as the scale tends to zero at the common value.
  if (std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
    return {nan, nan, nan, 0, LogisticStatus::Unbounded};
  const auto moments = norm_detail::moments(x);
  // Standardize when moments or Newton products would overflow/underflow,
  // when a large offset hides the spread, or when tol is within 2^12 binary64
  // spacings of the location scale max(|mean|, sd): there the rounding noise
  // of the Newton location step, a few spacings of that scale, can keep the
  // step above the target (TASK-214.3: (1e-8, 1, 1e8) at tol 1e-10). In
  // standardized coordinates the target is relative to sd.
  if (moments.sd > 0.0 && std::isfinite(moments.sd) &&
      (moments.sd < 1e-100 || moments.sd > 1e100 || std::abs(moments.mean) > 1e6 * moments.sd ||
       tol < 0x1p-40 * std::max(std::abs(moments.mean), moments.sd))) {
    std::vector<double> z(n);
    for (std::size_t i = 0; i < n; ++i)
      z[i] = static_cast<double>((static_cast<CPPUNIV_EXTENDED_REAL>(x[i]) - moments.mean) / moments.sd);
    const auto r = logistic_detail::newton(span{z.data(), n}, tol, maxit);
    return {moments.mean + moments.sd*r.mu, moments.sd*r.scale,
            r.logLik - static_cast<double>(n)*std::log(moments.sd), r.iterations, r.status};
  }
  return logistic_detail::newton(x, tol, maxit);

  }();
  if(result.status==LogisticStatus::Ok) {
    double sum=0,carry=0;
    for(double v:x) {const double term=distributions::dlogis(v,result.mu,result.scale,true)-carry;
      const double next=sum+term;carry=(next-sum)-term;sum=next;}
    result.logLik=sum;
    if(!std::isfinite(sum))result.status=LogisticStatus::NumericalFailure;
  }
  return result;
}


// Both conditional scores are monotone: bisect the unique root.
inline LogisticResult logistic_mle(span x,double tol,int maxit,double location,double scale) {
  if(!(tol>0 && std::isfinite(tol)) || maxit<1 || x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return {NAN,NAN,NAN,0,LogisticStatus::InvalidInput};

  auto result = [&]() -> LogisticResult {
  maxit = static_cast<int>(solver_budget(maxit));
  if(std::isnan(location)&&std::isnan(scale))return logistic_mle(x,tol,maxit);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  LogisticResult bad{nan,nan,nan,0,LogisticStatus::InvalidInput};
  if(x.empty()||!(tol>0)||maxit<1)return bad;
  double lo=x[0],hi=x[0],radius=0;
  for(double v:x){if(!std::isfinite(v))return bad;lo=std::min(lo,v);hi=std::max(hi,v);if(!std::isnan(location))radius=std::max(radius,std::abs(v-location));}
  double m=location,s=scale;int it=0;
  if(std::isnan(m)) {
    const auto location_score=[&](double c){double g=0;for(double v:x)g+=std::tanh((v-c)/(2*s));return g;};
    bool adjacent=false;
    for(;it<maxit;++it){m=lo/2+hi/2;double score=0;for(double v:x)score+=std::tanh((v-m)/(2*s));if(score>0)lo=m;else hi=m;if(hi-lo<=1e-12*s)break;
      // No double lies strictly between lo and hi: the absolute width
      // 1e-12*s is below the spacing of the location (|m| > ~4500 s).
      if(lo/2+hi/2==lo || lo/2+hi/2==hi){adjacent=true;break;}}
    m=lo/2+hi/2;
    if(adjacent) {
      // Accept the bracket only with signs above the score's rounding bound
      // (n terms, each within ~2 eps, summed): the root then lies in it, so
      // m is within a few spacings. A computed score that saturates to 0
      // (spread >> s, archive case 4718) never certifies and stays a failure.
      const double bound=4*static_cast<double>(x.size())*static_cast<double>(x.size())*std::numeric_limits<double>::epsilon();
      double a=lo,b=hi;
      for(int k=0;k<4 && !(location_score(a)>bound);++k)a=std::nextafter(a,-INFINITY);
      for(int k=0;k<4 && !(location_score(b)<-bound);++k)b=std::nextafter(b,INFINITY);
      if(location_score(a)>bound && location_score(b)<-bound)m=a/2+b/2;else it=maxit;
    }
  } else if(std::isnan(s)) {
    if(!(radius>0)) {bad.status=LogisticStatus::Unbounded;return bad;}  // collapse at the held location
    lo=std::max(std::log(radius)-350,std::log(std::numeric_limits<double>::denorm_min()));hi=std::log(radius)+2;
    for(;it<maxit;++it){const double t=lo/2+hi/2;s=std::exp(t);double score=-static_cast<double>(x.size());for(double v:x){const double z=(v-m)/s;score+=z*std::tanh(z/2);}if(score>0)lo=t;else hi=t;if(hi-lo<1e-12)break;}
    s=std::exp(lo/2+hi/2);
  }
  const double p[]={m,s};return {m,s,-LogisticCost{x}(p),it,it<maxit?LogisticStatus::Ok:LogisticStatus::IterationFailure};

  }();
  if(result.status==LogisticStatus::Ok) {
    double sum=0,carry=0;
    for(double v:x) {const double term=distributions::dlogis(v,result.mu,result.scale,true)-carry;
      const double next=sum+term;carry=(next-sum)-term;sum=next;}
    result.logLik=sum;
    if(!std::isfinite(sum))result.status=LogisticStatus::NumericalFailure;
  }
  return result;
}


inline FitOutcome outcome(const LogisticResult& r) {
  switch (r.status) {
    case LogisticStatus::NumericalFailure:return FitOutcome::failure(OutcomeReason::Evaluation);
    case LogisticStatus::Ok: return FitOutcome::estimate(true);
    case LogisticStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case LogisticStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case LogisticStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
  }
  return {};
}


}  // namespace cppunivariate
