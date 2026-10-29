// Gumbel MLE: 1D Newton on log-scale; mu computed analytically given sigma.
//
// Used directly by Weibull and Inverse Weibull (via log-of-log transforms),
// so the helper fit_gumbel_newton is part of the public cppunivariate API for
// downstream estimators.

#pragma once

#include "../../core/span.hpp"
#include "../norm/mle.hpp"
#include <vector>

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class GumbelStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct GumbelResult {
  double mu;
  double sigma;
  double logLik;
  int iterations;
  GumbelStatus status = GumbelStatus::Ok;
};

inline GumbelResult fit_gumbel_newton(span x, double tol, int maxit,
                                      double theta0 = std::numeric_limits<double>::quiet_NaN()) {
  const std::size_t n = x.size();
  const double dn = static_cast<double>(n);

  double sum_x = 0.0;
  for (auto v : x) sum_x += v;
  const double xbar = sum_x / dn;
  // Two-pass variance: the one-pass mean(x^2) - xbar^2 cancels to a negative
  // number for data with a large offset (NaN start at x + 1e8).
  double var0 = 0.0;
  for (auto v : x) var0 += (v - xbar) * (v - xbar);
  var0 /= dn;
  bool all_equal = true;
  for (auto v : x) all_equal = all_equal && v == x[0];
  if (all_equal) {
    // Constant data: no MLE (the likelihood is unbounded as sigma -> 0).
    const double nan = std::numeric_limits<double>::quiet_NaN();
    return {nan, nan, nan, 0, GumbelStatus::Unbounded};
  }

  // Solve in dimensionless coordinates when moments or Newton products
  // would overflow/underflow, or a large offset hides the spread.
  const auto moments = norm_detail::moments(x);
  if (moments.sd > 0.0 && std::isfinite(moments.sd) &&
      (!std::isfinite(var0) || var0 == 0.0 || moments.sd < 1e-100 ||
       moments.sd > 1e100 || std::abs(moments.mean) > 1e6 * moments.sd)) {
    std::vector<double> z(n);
    for (std::size_t i = 0; i < n; ++i) z[i] = (x[i] - moments.mean) / moments.sd;
    const double start = std::isnan(theta0) ? theta0 : theta0 - std::log(moments.sd);
    const auto r = fit_gumbel_newton(span{z.data(), n}, tol, maxit, start);
    return {moments.mean + moments.sd * r.mu, moments.sd * r.sigma,
            r.logLik - dn * std::log(moments.sd), r.iterations};
  }

  double theta = std::isnan(theta0)
                     ? std::log(std::sqrt(var0) * 2.45 / M_PI)
                     : theta0;
  int iter = 1;
  for (; iter <= maxit; ++iter) {
    const double sigma = std::exp(theta);
    const double invs = 1.0 / sigma;
    const double negs = -invs;

    double psi0 = 0.0, psi1 = 0.0, psi2 = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
      const double v = x[i] - xbar;
      const double e = std::exp(v * negs);
      psi0 += e;
      psi1 += v * e;
      psi2 += v * v * e;
    }
    psi1 /= psi0;
    psi2 = psi2 / psi0 - psi1 * psi1;

    const double f = sigma + psi1;
    const double df = 1.0 + psi2 * (invs * invs);
    const double dth = f / (df * sigma);

    theta -= dth;
    if (std::abs(dth) < std::abs(theta) * tol) break;
  }

  const double sigma = std::exp(theta);
  const double invsF = -1.0 / sigma;
  double sum_e = 0.0;
  double sum_z = 0.0;
  double sum_exp_neg_z = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    sum_e += std::exp((x[i] - xbar) * invsF);
  }
  const double mu = -sigma * std::log(sum_e / dn) + xbar;
  for (std::size_t i = 0; i < n; ++i) {
    const double z = (x[i] - mu) / sigma;
    sum_z += z;
    sum_exp_neg_z += std::exp(-z);
  }
  const double logLik =
      -dn * std::log(sigma) - sum_z - sum_exp_neg_z;
  return {mu, sigma, logLik, iter};
}


inline GumbelResult fit_gumbel_held(span x,double tol,int maxit,double mu,double sigma) {
  if(std::isnan(mu)&&std::isnan(sigma))return fit_gumbel_newton(x,tol,maxit);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.empty()||!(tol>0)||maxit<1)return {nan,nan,nan,0,GumbelStatus::InvalidInput};
  double m=mu,s=sigma;int it=0;
  if(std::isnan(m)) {
    double low=x[0];for(double v:x)low=std::min(low,v);
    double sum=0;for(double v:x)sum+=std::exp(-(v-low)/s);
    m=low-s*std::log(sum/x.size());
  } else if(std::isnan(s)) {
    double radius=0;for(double v:x)radius=std::max(radius,std::abs(v-m));
    if(!(radius>0))return {nan,nan,nan,0,GumbelStatus::Unbounded};
    // The likelihood is strictly concave in inverse scale: its derivative
    // n/t - sum d*(1-exp(-t*d)) is strictly decreasing.
    double lo=0,hi=1/radius;
    auto score=[&](double t){long double g=x.size()/t;for(double v:x){const long double d=static_cast<long double>(v)-m;g-=d*(-std::expm1(-t*d));}return g;};
    while(score(hi)>0 && std::isfinite(hi*2))hi*=2;
    for(;it<maxit;++it){const double t=lo/2+hi/2;if(score(t)>0)lo=t;else hi=t;if(hi-lo<1e-12*hi)break;}
    s=1/(lo/2+hi/2);
  }
  double ll=0;for(double v:x){const double z=(v-m)/s;ll-=std::log(s)+z+std::exp(-z);}
  if(it==maxit)return {nan,nan,nan,it,GumbelStatus::IterationFailure};
  return {m,s,ll,it};
}

}  // namespace cppunivariate
