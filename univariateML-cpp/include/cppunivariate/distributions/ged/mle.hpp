// Generalized Error Distribution (GED, symmetric) MLE on (mu, sigma, nu).
//
// Fitting reparam phi = (mu, log sigma, log(nu - 1)). Pure-libm log-density via
// log_dged_unit, so the standalone fit doesn't need Rmath.
//
// Density at (mu, sigma, nu): f(x) = (1/sigma) * f_unit((x-mu)/sigma; nu)
// where f_unit is the standardised GED used by dged().

#pragma once
#include "../../core/multistart.hpp"

#include "../../core/bfgs.hpp"
#include "../../core/finite_diff.hpp"
#include "../../core/reductions.hpp"
#include "../../core/reparam.hpp"
#include "../../core/span.hpp"
#include "../norm/mle.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <vector>
#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {
inline constexpr double kGedNuLower = 1.0;


enum class GedStatus { Ok = 0, BfgsFailure, InvalidStart, UniformBoundary, InvalidShape };

struct GedResult {
  double mu;
  double sigma;
  double nu;
  double logLik;
  int iterations;
  GedStatus status; unsigned restricted = 0;
};

// Per-obs nll in phi-space (with a = x - mu, w = |a|/(sigma * lambda),
// W = 0.5 * w^nu, K = log 2 - psi(1/nu)/2 + 3 psi(3/nu)/2,
// u = nu * log w - K):
//   nll_per = phi[1] + W + C(nu),  C(nu) = -log nu + 1.5 lgamma(1/nu)
//                                          - 0.5 lgamma(3/nu) + log 2.
// Per-observation Hessian (sympy-derived; verified vs numerical diff):
//   H_per[0][0] = nu (nu - 1) W / a^2
//   H_per[0][1] = nu^2 W / a
//   H_per[0][2] = -nu W (u + 1) / a
//   H_per[1][1] = nu^2 W
//   H_per[1][2] = -nu W (u + 1)
//   H_per[2][2] = d^2 C / d ln^2 + W (u^2 + u)
//                 - (W / (2 nu)) (trigamma(1/nu) - 9 trigamma(3/nu))
// where d^2 C / d ln^2 = (3/(2 nu)) (psi(1/nu) - psi(3/nu))
//                       + (3/(2 nu^2)) (trigamma(1/nu) - 3 trigamma(3/nu)).
struct GedCost {
  span x;
  int dim() const { return 3; }
  double operator()(const double *phi) const {
    const double mu = phi[0];
    const double sigma = cppunivariate::from_log(phi[1]);
    const double nu = cppunivariate::from_log(phi[2]);
    if (!std::isfinite(sigma) || !std::isfinite(nu) || sigma <= 0.0 ||
        nu <= 0.0) {
      return std::numeric_limits<double>::infinity();
    }
    const double log_sigma = std::log(sigma);
    double neg = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / sigma;
      neg += log_sigma - cppunivariate::dens::log_dged_unit(z, nu);
    }
    return neg;
  }
  void hessian(const double *phi, double *H) const {
    const double mu = phi[0];
    const double sigma = cppunivariate::from_log(phi[1]);
    const double nu = cppunivariate::from_log(phi[2]);
    if (!std::isfinite(sigma) || !std::isfinite(nu) || sigma <= 0.0 ||
        nu <= 0.0) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      for (int i = 0; i < 9; ++i) H[i] = nan;
      return;
    }
    const double lam = cppunivariate::dens::ged_lambda(nu);
    const double log_lam = std::log(lam);
    const double psi_1 = cppunivariate::special::digamma(1.0 / nu);
    const double psi_3 = cppunivariate::special::digamma(3.0 / nu);
    const double tri_1 = cppunivariate::special::trigamma(1.0 / nu);
    const double tri_3 = cppunivariate::special::trigamma(3.0 / nu);
    const double K = std::log(2.0) - 0.5 * psi_1 + 1.5 * psi_3;
    const double d2C_dln2 = 1.5 / nu * (psi_1 - psi_3) +
                            1.5 / (nu * nu) * (tri_1 - 3.0 * tri_3);
    double S_W_a2 = 0.0, S_W_a = 0.0, S_W = 0.0;
    double S_W_a_up1 = 0.0, S_W_up1 = 0.0, S_W_lin = 0.0;
    for (auto v : x) {
      const double a = v - mu;
      if (a == 0.0) continue;
      const double az = std::fabs(a);
      const double log_w = std::log(az) - phi[1] - log_lam;
      const double W = 0.5 * std::exp(nu * log_w);
      const double u = nu * log_w - K;
      const double up1 = u + 1.0;
      S_W_a2 += W / (a * a);
      S_W_a += W / a;
      S_W += W;
      S_W_a_up1 += W * up1 / a;
      S_W_up1 += W * up1;
      S_W_lin += W * (u * u + u);
    }
    const double dn = static_cast<double>(x.size());
    H[0] = nu * (nu - 1.0) * S_W_a2;
    H[1] = nu * nu * S_W_a;
    H[2] = -nu * S_W_a_up1;
    H[3] = H[1];
    H[4] = nu * nu * S_W;
    H[5] = -nu * S_W_up1;
    H[6] = H[2];
    H[7] = H[5];
    H[8] = dn * d2C_dln2 + S_W_lin -
           (1.0 / (2.0 * nu)) * (tri_1 - 9.0 * tri_3) * S_W;
  }
};

// Keep the unconstrained Cost/Hessian coordinates for density diagnostics.
// Fitting uses log(nu - 1), with the attained face compared separately.
struct GedFitCost {
  span x;
  double divisor = 1.0;
  int dim() const { return 3; }
  double operator()(const double *phi) const {
    double q[3];
    for (int i = 0; i < 3; ++i) q[i] = phi[i];
    q[2] = std::log1p(std::exp(phi[2]));
    return GedCost{x}(q)/divisor;
  }
};

inline GedResult ged_mle_impl(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n < 2) return {kNaN, kNaN, kNaN, kNaN, 0, GedStatus::InvalidStart};

  const double mean_x = cppunivariate::mean(x);
  double var_x = 0.0;
  for (auto v : x) {
    const double d = v - mean_x;
    var_x += d * d;
  }
  var_x /= static_cast<double>(n - 1);
  double sd_x = std::sqrt(var_x);
  if (!(sd_x > 0.0) || !std::isfinite(sd_x))
    sd_x = norm_detail::moments(x).sd * std::sqrt(static_cast<double>(n) / (n-1.0));
  if (!std::isfinite(sd_x) || sd_x <= 0.0) {
    return {kNaN, kNaN, kNaN, kNaN, 0, GedStatus::InvalidStart};
  }

  // Standardise before optimisation to make tolerances independent of units.
  std::vector<double> z;
  for (double v : x) z.push_back((v - mean_x) / sd_x);
  std::sort(z.begin(), z.end());
  const double dn = static_cast<double>(n);
  const double tie = 1e-8 * dn; // total log-likelihood tie; attained wins.
  const double c = z[n / 2];
  double a = 0.0;
  for (double v : z) a += std::abs(v - c) / dn;
  GedResult best{c, std::sqrt(2.0) * a, 1.0,
                 -dn * (std::log(2.0 * a) + 1.0), 0, GedStatus::Ok};
  double attempted=best.logLik;
  bool accepted=false;
  const double objective_divisor = n>=100000 ? dn : 1.0;
  GedFitCost cost{span{z.data(), z.size()},objective_divisor};
  finite_diff_cost<GedFitCost> fd{cost};
  BfgsControl ctl;
  ctl.grad_tol = tol;
  ctl.maxit = maxit;
  for (double nu0 : {1.5, 2.0, 4.0, 8.0}) {
    const double phi0[3] = {0.0, 0.0, std::log(nu0 - 1.0)};
    auto r = bfgs_minimize(fd, phi0, ctl);
    attempted=std::max(attempted,-r.f*objective_divisor);
    if (!bfgs_candidate_acceptable(r,objective_divisor)) continue;
    accepted=true;
    r.status=BfgsStatus::Ok;
    r.f *= objective_divisor;
    if (std::isfinite(r.f) && -r.f > best.logLik + tie) {
      best = {r.theta[0], std::exp(r.theta[1]), 1.0 + std::exp(r.theta[2]),
              -r.f, r.iterations,
              r.status == BfgsStatus::Ok ? GedStatus::Ok : GedStatus::BfgsFailure};
    }
  }
  if(!accepted && attempted>best.logLik+tie) best.status=GedStatus::BfgsFailure;
  const double uniform = -dn * std::log(z.back() - z.front());
  if (uniform > attempted + tie) {
    best.logLik = uniform;
    best.status = GedStatus::UniformBoundary;
  }
  best.mu = mean_x + sd_x * best.mu;
  best.sigma *= sd_x;
  best.logLik -= dn * std::log(sd_x);
  return best;
}

namespace ged_detail {
struct Maximum {double point;bool converged;};
template <typename F>
inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {
  double best=grid.front(),value=f(best);bool converged=grid.size()==1;
  for(std::size_t j=0;j<grid.size();++j) {
    const double v=f(grid[j]);if(v>value){value=v;best=grid[j];converged=false;}
    if((j&&v<f(grid[j-1]))||(j+1<grid.size()&&v<f(grid[j+1])))continue;
    double lo=grid[j?j-1:0],hi=grid[j+1<grid.size()?j+1:j];
    double a=hi-(hi-lo)*.6180339887498948482,b=lo+(hi-lo)*.6180339887498948482;
    double fa=f(a),fb=f(b);
    for(int it=0;it<maxit&&hi-lo>tol;++it) {
      if(fa>fb){hi=b;b=a;fb=fa;a=hi-(hi-lo)*.6180339887498948482;fa=f(a);}
      else{lo=a;a=b;fa=fb;b=lo+(hi-lo)*.6180339887498948482;fb=f(b);}
    }
    if(best==grid[j]&&hi-lo<=tol)converged=true;
    if(std::max(fa,fb)>value){value=std::max(fa,fb);best=fa>fb?a:b;converged=hi-lo<=tol;}
  }
  return {best,converged};
}

inline GedResult conditional(span x,double nu,double mean,double sd) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.empty())return {nan,nan,nu,nan,0,GedStatus::InvalidStart};
  std::vector<double> y(x.begin(),x.end());for(double v:y)if(!std::isfinite(v))return {nan,nan,nu,nan,0,GedStatus::InvalidStart};std::sort(y.begin(),y.end());
  double m=mean;
  if(std::isnan(m)) {
    if(nu<1) {
      double best=INFINITY;
      const double radius=y.back()-y.front();
      for(double c:y) {
        double value=0;
        for(double v:y) value+=std::pow(radius>0 ? std::abs(v-c)/radius : 0,nu);
        if(value<best){best=value;m=c;}
      }
    }
    else if(nu==1)m=y[y.size()/2];
    else if(nu==2)m=norm_detail::moments(x).mean;
    else {
      double lo=y.front(),hi=y.back();

      for(int it=0;it<100;++it){const double mid=lo/2+hi/2;const double unit=std::max(std::max(std::abs(y.front()-mid),std::abs(y.back()-mid)),std::numeric_limits<double>::denorm_min());long double score=0;for(double v:y){const double d=(v-mid)/unit;score+=(d>0?1:-1)*std::pow(std::abs(d),nu-1);}if(score>0)lo=mid;else hi=mid;}
      m=lo/2+hi/2;
    }
  }
  double s=sd;
  if(std::isnan(s)) {
    double radius=0;for(double v:y)radius=std::max(radius,std::abs(v-m));
    if(!(radius>0))return {m,0,nu,INFINITY,0,GedStatus::InvalidStart};
    double sum=0;for(double v:y)sum+=std::pow(std::abs(v-m)/radius,nu);
    s=radius*std::exp(std::log(nu*sum/(2*y.size()))/nu)/dens::ged_lambda(nu);
  }
  double ll=0;for(double v:y)ll-=std::log(s)-dens::log_dged_unit((v-m)/s,nu);
  return {m,s,nu,ll,0,GedStatus::Ok};
}
}
inline GedResult ged_mle_impl(span x,double tol,int maxit,double mean,double sd,double nu) {
  if(std::isnan(mean)&&std::isnan(sd)&&std::isnan(nu))return ged_mle_impl(x,tol,maxit);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(!std::isnan(nu)) {
    if(!std::isfinite(nu)||nu<=0)return {nan,nan,nu,nan,0,GedStatus::InvalidShape};
    return ged_detail::conditional(x,nu,mean,sd);
  }
  if(x.empty()||!(tol>0)||maxit<1)return {nan,nan,nan,nan,0,GedStatus::InvalidStart};
  std::vector<double> grid;for(double t=-12;t<=12;t+=.4)grid.push_back(t);
  auto profile=[&](double t){return ged_detail::conditional(x,1+std::exp(t),mean,sd);};
  const auto optimum=ged_detail::maximize([&](double a){return profile(a).logLik;},grid,1e-8,maxit);
  if(!optimum.converged)return {nan,nan,nan,nan,maxit,GedStatus::BfgsFailure};
  auto best=profile(optimum.point);const auto laplace=ged_detail::conditional(x,1,mean,sd);const double tie=1e-8*x.size();
  if(laplace.logLik>=best.logLik-tie)best=laplace;
  const auto mm=std::minmax_element(x.begin(),x.end());
  const double m=std::isnan(mean)?*mm.first/2+*mm.second/2:mean;
  const double radius=std::max(std::abs(*mm.first-m),std::abs(*mm.second-m));
  const double r=std::isnan(sd)?radius:std::sqrt(3.0)*sd;
  double uniform=-INFINITY;
  if(r>0 && radius<=r) {
    uniform=-static_cast<double>(x.size())*std::log(2*r);
    if(!std::isnan(sd)&&radius==r) {
      double left=0,right=0;for(double v:x){left+=v==m-r;right+=v==m+r;}
      uniform-=std::exp(-0.5772156649015328606)*(std::isnan(mean)?2*std::sqrt(left*right):left+right);
    }
  }
  if(uniform>best.logLik+tie){best.logLik=uniform;best.status=GedStatus::UniformBoundary;}
  return best;
}

inline GedResult ged_mle(span x,double tol,int maxit) {
  auto r=ged_mle_impl(x,tol,maxit);
  if(r.status==GedStatus::Ok && (true) && r.nu==kGedNuLower) r.restricted|=16u;
  return r;
}
inline GedResult ged_mle(span x,double tol,int maxit,double mean,double sd,double nu) {
  auto r=ged_mle_impl(x,tol,maxit,mean,sd,nu);
  if(r.status==GedStatus::Ok && (std::isnan(nu)) && r.nu==kGedNuLower) r.restricted|=16u;
  return r;
}
}  // namespace cppunivariate
