// Lomax profile search on the dimensionless bounded existence interval.

#pragma once
#include "../../core/precision.hpp"

#include "../../core/reductions.hpp"
#include "../../core/span.hpp"

#include <cmath>
#include <algorithm>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class LomaxStatus {
  Ok = 0,
  ExponentialLimit,
  InvalidStart,
  IterationFailure
};

struct LomaxResult {
  double lambda;
  double kappa;
  double logLik;
  int iterations;
  LomaxStatus status;
};

namespace lomax_detail {
// Profile log-likelihood in lambda (kappa at its closed-form maximiser),
// up to a constant: n log(lambda) - n log(S) - S, S = sum log1p(lambda x).
inline double profile_loglik(span x, double lambda) {
  double S = 0.0;
  for (auto v : x) S += std::log1p(lambda * v);
  const double n = static_cast<double>(x.size());
  return n * std::log(lambda) - n * std::log(S) - S;
}
}  // namespace lomax_detail

// lambda0: optional start for lambda; NaN (the default) or a non-positive
// value means the method-of-moments start.
inline LomaxResult lomax_newton_candidate(
    span x, double tol, int maxit,
    double lambda0 = std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0 || maxit<1) return {0.0, 0.0, kNaN, 0, LomaxStatus::InvalidStart};

  CPPUNIV_EXTENDED_REAL total = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (auto v : x) total += v;
  const double unit = static_cast<double>(total / static_cast<CPPUNIV_EXTENDED_REAL>(n));
  if (unit > 0.0 && std::isfinite(unit) && (unit < 1e-3 || unit > 1e3)) {
    std::vector<double> z(n);
    for (std::size_t i = 0; i < n; ++i) z[i] = x[i] / unit;
    const double start = std::isfinite(lambda0) && lambda0 > 0.0 ? lambda0 * unit : lambda0;
    const auto r = lomax_newton_candidate(span{z.data(), n}, tol, maxit, start);
    return {r.lambda/unit, r.kappa, r.logLik-static_cast<double>(n)*std::log(unit), r.iterations, r.status};
  }

  const double m = cppunivariate::mean(x);
  double sum_xsq = 0.0;
  for (auto v : x) sum_xsq += v * v;
  const double s = sum_xsq / static_cast<double>(n);
  const double m2 = m * m;

  const double alpha0 = 2.0 * (s - m2) / (s - 2.0 * m2);
  double lambda = (std::isfinite(lambda0) && lambda0 > 0.0)
                      ? lambda0
                      : 1.0 / (m * (alpha0 - 1.0));
  if (std::isfinite(lambda0) && lambda0>0.0 &&
      (lambda0*m < 1e-8 || lambda0*m > 1e8))
    return lomax_newton_candidate(x,tol,maxit); // A starting hint outside resolvable score arithmetic.
  if (lambda <= 0.0 || !std::isfinite(lambda)) {
    return {kNaN, kNaN, kNaN, 0, LomaxStatus::InvalidStart};
  }

  int iter = 1;
  for (; iter <= maxit; ++iter) {
    double S = 0.0, S1 = 0.0, S2 = 0.0;
    for (auto v : x) {
      const double xb = 1.0 + lambda * v;
      S += lambda*v < 1e-8 ? std::log1p(lambda*v) : std::log(xb);
      const double xd = v / xb;
      S1 += xd;
      S2 += xd * xd;
    }
    const double dn = static_cast<double>(n);
    S /= dn;
    S1 /= dn;
    S2 /= dn;

    const double psi = S1 / S;
    const double f = 1.0 / lambda - psi - S1;
    const double df = psi * psi + S2 / S + S2 - 1.0 / (lambda * lambda);

    // f is the profile score (per observation) and df its derivative.
    // Newton is an ascent step only where the profile is concave (df < 0);
    // elsewhere (possible from a user-supplied lambda0 far from the MLE)
    // move geometrically uphill. Every step is halved until the profile
    // log-likelihood does not decrease, and convergence is only declared
    // on a full Newton step, so a shrunken step cannot fake convergence.
    // From the moment start the plain Newton step is normally accepted.
    const bool newton = df < 0.0;
    double step = newton ? f / df : (f > 0.0 ? -lambda : 0.5 * lambda);
    if (!std::isfinite(step)) {
      return {kNaN, kNaN, kNaN, iter, LomaxStatus::IterationFailure};
    }
    if (lambda - step <= 0.0) step = 0.5 * lambda;
    const double lp_old = lomax_detail::profile_loglik(x, lambda);
    int halvings = 0;
    for (; halvings < 60; ++halvings) {
      const double lp_new = lomax_detail::profile_loglik(x, lambda - step);
      if (lp_new >= lp_old - 1e-12 * (std::abs(lp_old) + 1.0)) break;
      step *= 0.5;
    }
    if (halvings == 60)
      return {kNaN,kNaN,kNaN,iter,LomaxStatus::IterationFailure};
    const double lambda_new = lambda - step;
    if (newton && halvings == 0 &&
        std::abs(step) < (std::abs(lambda) * tol + tol)) {
      lambda = lambda_new;
      break;
    }
    lambda = lambda_new;
  }

  double sum_log1p = 0.0;
  for (auto v : x) sum_log1p += std::log1p(lambda * v);
  const double dn = static_cast<double>(n);
  const double kappa = dn / sum_log1p;
  const double logLik =
      dn * (std::log(kappa) + std::log(lambda) - 1.0 - 1.0 / kappa);
  if (iter>maxit) {
    double prob_sum=0;
    for (double v:x) {
      const double z=std::log(lambda)+std::log(v);
      prob_sum+=z>=0 ? 1/(1+std::exp(-z)) : std::exp(z)/(1+std::exp(z));
    }
    const double score=dn-(kappa+1)*prob_sum;
    if (!(std::isfinite(score) && std::abs(score)<1e-3))
      return {lambda,kappa,logLik,iter,LomaxStatus::IterationFailure};
  }
  return {lambda, kappa, logLik, iter, LomaxStatus::Ok};
}

namespace lomax_detail {
struct WeightedValue { double y, log_y, weight; };
// t = log u; log q = log u + u avoids forming q at the far endpoint.
inline double gap(const std::vector<WeightedValue>& sample, double t,
                  double &A) {
  const double logq = t + std::exp(t);
  if (!std::isfinite(logq)) { A = INFINITY; return -INFINITY; }
  double sum = 0.0;
  if (logq > 32.0) {
    // A = logq + mean(log y) + mean(softplus(-logq-log y)).
    // Subtract logq analytically rather than cancelling two large numbers.
    double correction = 0.0;
    for (const auto &v : sample) {
      const double z = -logq-v.log_y;
      correction += v.weight*(v.log_y+std::max(z,0.0)+std::log1p(std::exp(-std::abs(z))));
    }
    A = logq+correction;
    return -correction-std::log(A);
  }
  if (logq < -12.0) {
    // Accumulate A/q - 1 directly, avoiding cancellation at q=0.
    const double q = std::exp(logq);
    double delta = 0.0;
    for (const auto &v : sample) {
      const double z = q * v.y;
      const double ratio = z < 1e-4
          ? z*(-0.5 + z*(1.0/3.0 + z*(-0.25 + z*0.2)))
          : std::log1p(z)/z - 1.0;
      delta += v.weight * v.y * ratio;
      sum += v.weight * v.y;
    }
    delta += sum - 1.0;
    A = q*(1.0+delta);
    return -std::log1p(delta)-A;
  }
  for (const auto &v : sample) {
    const double z = logq + v.log_y;
    sum += v.weight * (std::max(z,0.0)+std::log1p(std::exp(-std::abs(z))));
  }
  A = sum;
  return logq - std::log(A) - A;
}
}

inline LomaxResult lomax_mle(span x, double tol, int maxit,
    double lambda0 = std::numeric_limits<double>::quiet_NaN()) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0 || maxit<1 || !(tol>0) || !std::isfinite(tol)) return {nan,nan,nan,0,LomaxStatus::InvalidStart};
  CPPUNIV_EXTENDED_REAL total = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (double v : x) {
    if (!(v > 0.0) || !std::isfinite(v))
      return {nan,nan,nan,0,LomaxStatus::InvalidStart};
    total += v;
  }
  const double m = static_cast<double>(total/n), dn = static_cast<double>(n);
  std::vector<double> y(n);
  std::vector<lomax_detail::WeightedValue> exact;
  exact.reserve(n);
  double logubar = 0.0, second = 0.0, minlog = INFINITY, maxlog = -INFINITY;
  for (std::size_t i=0; i<n; ++i) {
    y[i] = x[i]/m;
    const double ly = std::log(x[i])-std::log(m);
    exact.push_back({y[i],ly,1.0/dn});
    logubar -= ly/dn;
    second += y[i]*y[i]/dn;
    minlog = std::min(minlog,ly); maxlog = std::max(maxlog,ly);
  }
  if (maxlog == minlog)
    return {nan,nan,nan,0,LomaxStatus::ExponentialLimit};
  // Log bins preserve weights, arithmetic means and geometric means.
  // Compression is for nominating brackets only; all final fits use all data.
  std::vector<lomax_detail::WeightedValue> scan;
  if (n > 4096 && maxlog > minlog) {
    constexpr int bins = 2048;
    std::vector<double> sy(bins,0.0), sl(bins,0.0), count(bins,0.0);
    for (const auto &v : exact) {
      const int j = std::min(bins-1,static_cast<int>((v.log_y-minlog)/(maxlog-minlog)*bins));
      sy[j]+=v.y; sl[j]+=v.log_y; count[j]+=1.0;
    }
    for (int j=0;j<bins;++j) if (count[j]>0.0)
      scan.push_back({sy[j]/count[j],sl[j]/count[j],count[j]/dn});
  } else scan = exact;
  std::vector<double> grid;
  const double upper = std::max(0.0,logubar);
  for (double t=-36.0;t<upper;t+=0.125) grid.push_back(t);
  grid.push_back(upper);
  if (upper < std::log(1e4)) {
    const double ubar = std::exp(upper);
    for (int j=1;j<257;++j) grid.push_back(std::log(ubar*j/256.0));
  }
  std::sort(grid.begin(),grid.end());
  grid.erase(std::unique(grid.begin(),grid.end()),grid.end());
  double best=0.0, best_t=-36.0, best_A=0.0;
  int iterations=0;
  bool newton_wins=false;
  LomaxResult retained{nan,nan,nan,0,LomaxStatus::InvalidStart};
  // Preserve the established Newton solution as an additional candidate.
  if (second > 2.0 && std::isfinite(second)) {
    // The Newton candidate must use the same dimensionless tolerance as
    // the global comparison, including for ordinary-scale observations.
    auto r=lomax_newton_candidate(span{y.data(),n},tol,maxit,
        std::isfinite(lambda0) ? lambda0*m : lambda0);
    r.lambda/=m; r.logLik-=dn*std::log(m);
    if (r.status==LomaxStatus::Ok && r.lambda>0.0) {
      const double logq=std::log(r.lambda)+std::log(m);
      double u=std::max(1e-16, logq>1.0 ? logq-std::log(logq) : std::exp(logq));
      for (int j=0;j<30;++j) u *= (1.0+logq-std::log(u))/(1.0+u);
      double A=0.0;
      const double g=lomax_detail::gap(exact,std::log(u),A);
      if (g>best) { best=g;best_t=std::log(u);best_A=A;retained=r;newton_wins=true; }
      iterations=r.iterations;
    }
  }
  std::vector<double> values;
  for (double t:grid) { double A=0.0; values.push_back(lomax_detail::gap(scan,t,A)); }
  for (std::size_t j=1;j+1<grid.size();++j) {
    if (!(values[j]>1e-10 && values[j]>=values[j-1] && values[j]>=values[j+1])) continue;
    double lo=grid[j-1],hi=grid[j+1];
    constexpr double ratio=0.6180339887498948482;
    double a=hi-ratio*(hi-lo),b=lo+ratio*(hi-lo),A=0.0;
    const auto &refine = n>4096 ? scan : exact;
    double fa=lomax_detail::gap(refine,a,A),fb=lomax_detail::gap(refine,b,A);
    for (int k=0;k<std::min(maxit,55) && hi-lo>1e-10;++k) {
      if (fa>fb) {hi=b;b=a;fb=fa;a=hi-ratio*(hi-lo);fa=lomax_detail::gap(refine,a,A);}
      else {lo=a;a=b;fa=fb;b=lo+ratio*(hi-lo);fb=lomax_detail::gap(refine,b,A);}
    }
    if (hi-lo>1e-10) return {nan,nan,nan,iterations,LomaxStatus::IterationFailure};
    double t=fa>fb?a:b;
    if (n>4096) {
      // Refine the compressed nomination by the full-sample profile score
      // in z=log q. Safeguard Newton within the nominated grid bracket.
      double zl=grid[j-1]+std::exp(grid[j-1]);
      double zh=grid[j+1]+std::exp(grid[j+1]);
      double z=t+std::exp(t);
      for (int k=0;k<30;++k) {
        double av=0.0,bv=0.0,cv=0.0;
        for (const auto &v:exact) {
          const double w=z+v.log_y;
          const double prob=1.0/(1.0+std::exp(-w));
          av+=v.weight*(std::max(w,0.0)+std::log1p(std::exp(-std::abs(w))));
          bv+=v.weight*prob;
          cv+=v.weight*prob*(1.0-prob);
        }
        const double score=1.0-bv/av-bv;
        const double slope=bv*bv/(av*av)-cv/av-cv;
        if (score>0.0) zl=z; else zh=z;
        double next=z-score/slope;
        if (std::abs(score)<1e-12) break;
        if (!(next>zl && next<zh)) next=0.5*(zl+zh);
        if (std::abs(next-z)<1e-10) {z=next;break;}
        z=next;
      }
      for (int k=0;k<20;++k) t-=(t+std::exp(t)-z)/(1.0+std::exp(t));
      // A failed full-sample Newton solve (or its coordinate conversion)
      // may survive only when the total profiled log-lambda score passes.
      double av=0,bv=0;
      const double logq=t+std::exp(t);
      for (const auto &v:exact) {
        const double w=logq+v.log_y;
        av+=v.weight*(std::max(w,0.0)+std::log1p(std::exp(-std::abs(w))));
        bv+=v.weight*(w>=0 ? 1/(1+std::exp(-w)) : std::exp(w)/(1+std::exp(w)));
      }
      const double residual=dn*(1-bv/av-bv);
      if (!(std::isfinite(residual) && std::abs(residual)<1e-3))
        return {nan,nan,nan,iterations,LomaxStatus::IterationFailure};
    }
    const double g=lomax_detail::gap(exact,t,A);
    if (g>best+1e-13) {best=g;best_t=t;best_A=A;newton_wins=false;}
  }
  if (best<=1e-8) return {nan,nan,nan,iterations,LomaxStatus::ExponentialLimit};
  if (newton_wins) return retained;
  const double logq=best_t+std::exp(best_t);
  return {std::exp(logq-std::log(m)),1.0/best_A,
      dn*(best-std::log(m)-1.0),iterations,LomaxStatus::Ok};
}

inline LomaxResult lomax_mle(span x,double tol,int maxit,double lambda0,double lambda,double kappa) {
  if(std::isnan(lambda) && std::isnan(kappa))return lomax_mle(x,tol,maxit,lambda0);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.size()==0 || !(tol>0 && std::isfinite(tol)) || maxit<=0 || (!std::isnan(lambda) && !(lambda>0 && std::isfinite(lambda))) ||
     (!std::isnan(kappa) && !(kappa>0 && std::isfinite(kappa))))return {nan,nan,nan,0,LomaxStatus::InvalidStart};
  for(double v:x)if(!(v>0 && std::isfinite(v)))return {nan,nan,nan,0,LomaxStatus::InvalidStart};
  const double n=static_cast<double>(x.size());int iterations=0;
  if(std::isnan(lambda)) {
    double lo=-745,hi=710;
    for(int j=0;j<maxit && hi-lo>1e-12;++j){const double mid=(lo+hi)/2;CPPUNIV_EXTENDED_REAL score=n;for(double v:x){const double z=mid+std::log(v);score-=(kappa+1)/(1+std::exp(-z));}if(score>0)lo=mid;else hi=mid;++iterations;}
    if(hi-lo>1e-12)return {nan,kappa,nan,iterations,LomaxStatus::IterationFailure};
    lambda=std::exp((lo+hi)/2);
  }
  double s=0;for(double v:x){const double z=std::log(lambda)+std::log(v);s+=std::max(z,0.0)+std::log1p(std::exp(-std::abs(z)));}
  if(std::isnan(kappa))kappa=n/s;
  const double ll=n*(std::log(lambda)+std::log(kappa))-(kappa+1)*s;
  return {lambda,kappa,ll,iterations,LomaxStatus::Ok};
}

}  // namespace cppunivariate
