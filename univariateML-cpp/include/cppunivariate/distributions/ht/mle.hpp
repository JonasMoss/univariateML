#pragma once
#include "../../core/stop_detail.hpp"
#include "../../core/span.hpp"
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
inline constexpr double kHtNuLower = 1.0;

enum class HtStatus { Ok, InvalidData, InvalidParameter, Unbounded, ScaleBoundary, NormalLimit, SearchFailure, IterationFailure };
// `mu` defaults to NaN: `ht_mle_zero` and the search helpers below never set
// it (they operate on already-translated data); the public `ht_mle` entry
// point at the bottom of this file always fills it in before returning.
struct HtResult { double nu, sigma, logLik; int iterations; HtStatus status; unsigned restricted = 0; double mu = std::numeric_limits<double>::quiet_NaN();  StopDetail stop_detail{}; bool budget_exhausted=false; };
namespace ht_detail {
constexpr double inf = std::numeric_limits<double>::infinity();
inline double log_mode(double nu) {
  if (nu > 1e4) return -0.5*std::log(2*std::acos(-1.0))-0.25/nu+1/(24*nu*nu*nu);
  return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));
}
inline double softplus(double z) { return std::max(z,0.0)+std::log1p(std::exp(-std::abs(z))); }
inline double logistic(double z) { return z >= 0 ? 1/(1+std::exp(-z)) : std::exp(z)/(1+std::exp(z)); }
// All scale computations use log distances: finite data need not have finite squares.
inline double profile(span x, double nu, double &ls, bool fixed_scale) {
  const double n = static_cast<double>(x.size());
  const double ln = std::log(nu);
  if (!fixed_scale) {
    double lo = inf, hi = -inf;
    for (double v : x) if (v > 0) { lo = std::min(lo,std::log(v)); hi = std::max(hi,std::log(v)); }
    auto score = [&](double u) {
      double h = -n;
      for (double v : x) if (v > 0) h += (nu+1)*logistic(2*(std::log(v)-u)-ln);
      return h;
    };
    lo -= 1+0.5*std::abs(ln); hi += 1+0.5*std::abs(ln);
    for (int i=0; i<1024 && score(lo)<=0; ++i) lo -= 2;
    for (int i=0; i<1024 && score(hi)>=0; ++i) hi += 2;
    for (int i=0; i<120; ++i) {
      const double mid = (lo+hi)/2;
      if (score(mid)>0) lo=mid; else hi=mid;
      if (hi-lo < 2e-14*(1+std::abs(mid))) break;
    }
    ls = (lo+hi)/2;
  }
  double ll = n*(std::log(2.0)+log_mode(nu)-ls);
  for (double v : x) if (v>0) ll -= 0.5*(nu+1)*softplus(2*(std::log(v)-ls)-ln);
  return ll;
}
inline double normal(span x, double &ls, bool fixed_scale) {
  double m=0; for (double v:x) m=std::max(m,v);
  if (!fixed_scale) {
    double ss=0; for(double v:x) ss+=(v/m)*(v/m);
    ls=std::log(m)+0.5*std::log(ss/static_cast<double>(x.size()));
  }
  double ll=static_cast<double>(x.size())*(0.5*std::log(2/std::acos(-1.0))-ls);
  for(double v:x) if(v>0) ll-=0.5*std::exp(2*(std::log(v)-ls));
  return ll;
}
} // namespace ht_detail

// Fixed-endpoint engine, also used when nu itself is held. A held mu
// exempts the free shape from the translated-model restriction. Exact
// ties still need the sample-dependent collapse diagnostics below.
namespace ht_detail {
inline HtResult ht_mle_zero_legacy(span x, double held_nu = std::numeric_limits<double>::quiet_NaN(),
                       double held_sigma = std::numeric_limits<double>::quiet_NaN()) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const bool fn=!std::isnan(held_nu), fs=!std::isnan(held_sigma);
  if ((fn && (!(held_nu>0)||!std::isfinite(held_nu))) ||
      (fs && (!(held_sigma>0)||!std::isfinite(held_sigma))))
    return {nan,nan,nan,0,HtStatus::InvalidParameter};
  if(x.size()==0) return {nan,nan,nan,0,HtStatus::InvalidData};
  double k=0;
  for(double v:x) { if(!std::isfinite(v)||v<0) return {nan,nan,nan,0,HtStatus::InvalidData}; if(v==0) ++k; }
  const double n=static_cast<double>(x.size());
  if(!fs && k>0) {
    if(!fn || held_nu*(n-k)<k) return {nan,nan,nan,0,HtStatus::Unbounded};
    if(held_nu*(n-k)==k) return {held_nu,0,nan,0,HtStatus::ScaleBoundary};
  }
  double ls=fs ? std::log(held_sigma) : 0;
  if(fn) {
    const double ll=ht_detail::profile(x,held_nu,ls,fs);
    const double sigma=fs ? held_sigma : std::exp(ls);
    return {held_nu,sigma,ll,0, std::isfinite(ll)&&sigma>0&&std::isfinite(sigma) ? HtStatus::Ok : HtStatus::SearchFailure};
  }
  const double normal_ll=ht_detail::normal(x,ls,fs);
  const double normal_ls=ls;
  double best=-ht_detail::inf, best_t=0, best_ls=ls;
  auto value = [&](double t) {
    double u=fs ? std::log(held_sigma) : 0;
    const double ll=ht_detail::profile(x,std::exp(t),u,fs);
    if(ll>best) {best=ll;best_t=t;best_ls=u;}
    return ll;
  };
  // nu -> 0 cannot win: for x>0, f(x) <= exp(log_mode(nu))*2/x *
  // (1+1/nu)^(-(nu+1)/2). At zero with fixed scale use its mode.
  // These pointwise maxima bound every scale profile, and vanish as nu -> 0.
  const double benchmark=std::max(normal_ll,value(0.0));
  double lower=-4;
  for(int j=0;j<180;++j) {
    const double nu=std::exp(lower);
    double bound=0;
    for(double v:x) bound+= v>0 ? std::log(2.0)+ht_detail::log_mode(nu)-std::log(v)
        -0.5*(nu+1)*std::log1p(1/nu) : std::log(2.0)+ht_detail::log_mode(nu)-std::log(held_sigma);
    if(bound<benchmark-1) break;
    lower-=4;
  }
  // Global grid in log nu (equivalently log eta), refining EVERY local maximum.
  // Large-nu values are compared with the exact normal face, never returned as a cap.
  const double upper=std::log(1e8), step=0.125;
  const int count=static_cast<int>(std::ceil((upper-lower)/step));
  const double delta=(upper-lower)/count;
  std::vector<double> grid(count+1);
  for(int j=0;j<=count;++j) grid[j]=value(lower+j*delta);
  const double ratio=(std::sqrt(5.0)-1)/2;
  for(int j=1;j<count;++j) if(grid[j]>=grid[j-1] && grid[j]>=grid[j+1]) {
    double a=lower+(j-1)*delta,b=lower+(j+1)*delta;
    double l=b-ratio*(b-a),r=a+ratio*(b-a),fl=value(l),fr=value(r);
    for(int it=0;it<70;++it) {
      if(fl>fr) {b=r;r=l;fr=fl;l=b-ratio*(b-a);fl=value(l);}
      else {a=l;l=r;fl=fr;r=a+ratio*(b-a);fr=value(r);}
    }
  }
  if(normal_ll>=best-1e-8*n) return {ht_detail::inf,std::exp(normal_ls),normal_ll,count,HtStatus::NormalLimit};
  if(best_t>=upper-delta || best_t<=lower+delta || !std::isfinite(best) ||
      !(std::exp(best_ls)>0) || !std::isfinite(std::exp(best_ls)))
    return {std::exp(best_t),std::exp(best_ls),best,count,HtStatus::SearchFailure};
  return {std::exp(best_t),fs?held_sigma:std::exp(best_ls),best,count,HtStatus::Ok};
}
} // namespace ht_detail

namespace ht_detail {
// Free endpoint: search the free shape on [1, infinity), comparing the
// exact half-normal face and (for a free scale at 2k=n) the scale-zero face.
inline HtResult free_threshold_search(span y, double held_sigma) {
  const bool fs = !std::isnan(held_sigma);
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double n = static_cast<double>(y.size());
  double k = 0.0;
  for (double v : y) if (v == 0.0) ++k;
  if (!fs && 2.0*k > n) return {nan, nan, nan, 0, HtStatus::Unbounded};
  const bool tie = !fs && (2.0*k == n);
  double normal_ls = fs ? std::log(held_sigma) : 0.0;
  const double normal_ll = normal(y, normal_ls, fs);
  double boundary_ll = -inf;
  if (tie) {
    double sum_log_pos = 0.0;
    for (double v : y) if (v > 0.0) sum_log_pos += std::log(v);
    // nu = 1 (half-Cauchy), sigma -> 0 limit at the exact break-even tie.
    boundary_ll = n*std::log(2.0/std::acos(-1.0)) - 2.0*sum_log_pos;
  }
  double best=-inf, best_t=0.0, best_ls=0.0;
  auto value = [&](double t) {
    double u = fs ? std::log(held_sigma) : 0.0;
    const double ll = profile(y, std::exp(t), u, fs);
    if (ll > best) { best = ll; best_t = t; best_ls = u; }
    return ll;
  };
  // Away from the exact tie, nu = 1 itself is an ordinary, safely evaluable
  // search point (existence holds strictly above the break-even nu*). At
  // the tie, nudge above 1 and rely on the explicit boundary value instead.
  const double lower = tie ? std::log1p(1e-9) : 0.0;
  const double upper=std::log(1e8), step=0.125;
  const int count=static_cast<int>(std::ceil((upper-lower)/step));
  const double delta=(upper-lower)/count;
  std::vector<double> grid(count+1);
  for(int j=0;j<=count;++j) grid[j]=value(lower+j*delta);
  const double ratio=(std::sqrt(5.0)-1)/2;
  for(int j=1;j<count;++j) if(grid[j]>=grid[j-1] && grid[j]>=grid[j+1]) {
    double a=lower+(j-1)*delta,b=lower+(j+1)*delta;
    double l=b-ratio*(b-a),r=a+ratio*(b-a),fl=value(l),fr=value(r);
    for(int it=0;it<70;++it) {
      if(fl>fr) {b=r;r=l;fr=fl;l=b-ratio*(b-a);fl=value(l);}
      else {a=l;l=r;fl=fr;r=a+ratio*(b-a);fr=value(r);}
    }
  }
  const double tol=1e-8*n;
  const double challenger=std::max(best,boundary_ll);
  if(normal_ll>=challenger-tol) return {inf,std::exp(normal_ls),normal_ll,count,HtStatus::NormalLimit};
  if(tie && boundary_ll>=best-tol) return {1.0,0.0,nan,count,HtStatus::ScaleBoundary};
  if(best_t>=upper-delta || !std::isfinite(best) || !(std::exp(best_ls)>0) || !std::isfinite(std::exp(best_ls)))
    return {std::exp(best_t),std::exp(best_ls),best,count,HtStatus::SearchFailure};
  HtResult r{std::exp(best_t),fs ? held_sigma : std::exp(best_ls),best,count,HtStatus::Ok};
  // Landing exactly on the restriction's lower edge (only possible when not
  // at the tie, whose interior search starts strictly above 1) means the
  // restriction, not an interior score, picked this nu.
  if(!tie && best_t==lower) r.restricted|=4u;
  if(r.status==HtStatus::NormalLimit)r.stop_detail=limit_detail({std::isfinite(r.mu)?r.mu:0,r.sigma},r.logLik,StopModel::Hnorm);
  return r;
}
} // namespace ht_detail

#include "cert.hpp"

inline HtResult ht_mle_zero(span x, double held_nu = std::numeric_limits<double>::quiet_NaN(),
                            double held_sigma = std::numeric_limits<double>::quiet_NaN()) {
  HtResult r = ht_detail::ht_mle_zero_legacy(x, held_nu, held_sigma);
  if (std::isnan(held_nu) && (r.status == HtStatus::Ok || r.status == HtStatus::NormalLimit ||
                              r.status == HtStatus::ScaleBoundary || r.status == HtStatus::SearchFailure))
    r = ht_detail::certified_search(x, held_sigma, false, r);
  if(r.status==HtStatus::NormalLimit)r.stop_detail=limit_detail({std::isfinite(r.mu)?r.mu:0,r.sigma},r.logLik,StopModel::Hnorm);
  return r;
}

// NaN requests estimation. The free endpoint is exactly min(x); its
// free shape is restricted to nu>=1. Held nu values are preserved, while
// holding mu exempts free nu from the restriction.
inline HtResult ht_mle(span x, double mu, double nu, double sigma) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (x.size() == 0) return {nan,nan,nan,0,HtStatus::InvalidData,0,nan};
  for (double v : x) if (!std::isfinite(v)) return {nan,nan,nan,0,HtStatus::InvalidData,0,nan};
  if (!std::isnan(sigma) && (!(sigma > 0) || !std::isfinite(sigma)))
    return {nan,nan,nan,0,HtStatus::InvalidParameter};
  const bool mu_free = std::isnan(mu);
  double mu_hat;
  if (mu_free) {
    mu_hat = *std::min_element(x.begin(), x.end());
  } else {
    if (!std::isfinite(mu)) return {nan,nan,nan,0,HtStatus::InvalidParameter,0,mu};
    mu_hat = mu;
    for (double v : x) if (v < mu_hat) return {nan,nan,nan,0,HtStatus::InvalidData,0,mu_hat};
  }
  std::vector<double> y(x.size());
  for (std::size_t i = 0; i < x.size(); ++i) y[i] = x[i] - mu_hat;
  HtResult r = (!mu_free || !std::isnan(nu))
      ? ht_detail::ht_mle_zero_legacy(span{y.data(), y.size()}, nu, sigma)
      : ht_detail::free_threshold_search(span{y.data(), y.size()}, sigma);
  if (std::isnan(nu) && (r.status == HtStatus::Ok || r.status == HtStatus::NormalLimit ||
                         r.status == HtStatus::ScaleBoundary || r.status == HtStatus::SearchFailure))
    r = ht_detail::certified_search(span{y.data(), y.size()}, sigma, mu_free, r);
  r.mu = mu_hat;
  for(auto& c:r.stop_detail.candidates)if(!c.coef.empty())c.coef[0]=mu_hat;
  if(r.status==HtStatus::NormalLimit)r.stop_detail=limit_detail({std::isfinite(r.mu)?r.mu:0,r.sigma},r.logLik,StopModel::Hnorm);
  return r;
}

} // namespace cppunivariate
