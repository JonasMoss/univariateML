// Student-t MLE on (mu, sigma, nu).
//
// Reparam phi = (mu, log sigma, log(nu - 2)) — keeps sigma > 0 and nu > 2
// without bounds. Pure-libm log-density via log_dstd_unit (lgamma-based),
// independent of the math backend.
//
// Density at (mu, sigma, nu): f(x) = (1/sigma) * f_unit((x-mu)/sigma; nu),
// where f_unit is the standardised t (mean 0, sd 1) used by dstd().

#pragma once

#include "../lst/mle.hpp"
#include "../../core/bfgs.hpp"
#include "../../core/finite_diff.hpp"
#include "../../core/reductions.hpp"
#include "../../core/reparam.hpp"
#include "../../core/span.hpp"
#include "../../core/tie_collapse.hpp"
#include "../norm/mle.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <vector>
#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class StudentTStatus { Ok = 0, BfgsFailure, InvalidStart, NormalBoundary, DfTwoBoundary, InvalidNu, Unbounded, ScaleBoundary, IterationFailure };

struct StudentTResult {
  double mu;
  double sigma;
  double nu;
  double logLik;
  int iterations;
  StudentTStatus status;
};

// Per-obs nll in phi-space (with z = (x - mu)/sigma, m = nu - 2,
// y = z^2/m, R = 1 + y, sigma = exp(phi[1]), m = exp(phi[2])):
//   nll_per = phi[1] - lgamma((nu+1)/2) + lgamma(nu/2) + (1/2) log(pi m)
//             + (nu+1)/2 * log(R).
// Hessian summed over observations:
//   H[0][0] = (nu+1)/(sigma^2 m) * sum (1 - y_i) / R_i^2
//   H[0][1] = (2(nu+1)/(sigma m)) * sum z_i / R_i^2
//   H[0][2] = (1/(sigma m)) * sum (3 - z_i^2) z_i / R_i^2
//   H[1][1] = 2(nu+1) * sum y_i / R_i^2
//   H[1][2] = (1/m) * sum z_i^2 (3 - z_i^2) / R_i^2
//   H[2][2] = (nu+1)/2 * sum y_i / R_i^2 - sum z_i^2 / R_i
//             + (m/2) * sum log R_i
//             - n * m^2/4 * (trigamma((nu+1)/2) - trigamma(nu/2))
//             - n * m/2 * (digamma((nu+1)/2) - digamma(nu/2)).
// Sympy-derived; verified vs numerical Hessian to machine precision.
struct StudentTCost {
  span x;
  double divisor = 1.0;
  int dim() const { return 3; }
  double operator()(const double *phi) const {
    const double mu = phi[0];
    const double sigma = cppunivariate::from_log(phi[1]);
    const double nu = cppunivariate::from_log_shift(phi[2], 2.0);
    if (!std::isfinite(sigma) || !std::isfinite(nu) || sigma <= 0.0 ||
        nu <= 2.0) {
      return std::numeric_limits<double>::infinity();
    }
    const double log_sigma = std::log(sigma);
    double neg = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / sigma;
      neg += log_sigma - cppunivariate::dens::log_dstd_unit(z, nu);
    }
    return neg/divisor;
  }
  void hessian(const double *phi, double *H) const {
    const double mu = phi[0];
    const double sigma = cppunivariate::from_log(phi[1]);
    const double m = cppunivariate::from_log(phi[2]);  // nu - 2
    const double nu = m + 2.0;
    if (!std::isfinite(sigma) || !std::isfinite(m) || sigma <= 0.0 ||
        m <= 0.0) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      for (int i = 0; i < 9; ++i) H[i] = nan;
      return;
    }
    double S_invR2 = 0.0;       // sum 1/R^2
    double S_y_invR2 = 0.0;     // sum y / R^2
    double S_z_invR2 = 0.0;     // sum z / R^2
    double S_z2_invR = 0.0;     // sum z^2 / R
    double S_z3_invR2 = 0.0;    // sum z^3 / R^2
    double S_z2_invR2 = 0.0;    // sum z^2 / R^2
    double S_z4_invR2 = 0.0;    // sum z^4 / R^2
    double S_logR = 0.0;        // sum log R
    for (auto v : x) {
      const double z = (v - mu) / sigma;
      const double z2 = z * z;
      const double y = z2 / m;
      const double R = 1.0 + y;
      const double inv_R = 1.0 / R;
      const double inv_R2 = inv_R * inv_R;
      S_invR2 += inv_R2;
      S_y_invR2 += y * inv_R2;
      S_z_invR2 += z * inv_R2;
      S_z2_invR += z2 * inv_R;
      S_z3_invR2 += z * z2 * inv_R2;
      S_z2_invR2 += z2 * inv_R2;
      S_z4_invR2 += z2 * z2 * inv_R2;
      S_logR += std::log(R);
    }
    const double dn = static_cast<double>(x.size());
    const double nu1 = nu + 1.0;
    const double sigma_m = sigma * m;
    const double sigma2_m = sigma * sigma_m;
    // sum (1 - y) / R^2 = sum 1/R^2 - sum y/R^2.
    H[0] = nu1 / sigma2_m * (S_invR2 - S_y_invR2);
    // sum z / R^2.
    H[1] = (2.0 * nu1 / sigma_m) * S_z_invR2;
    // sum (3 - z^2) z / R^2 = 3 sum z/R^2 - sum z^3/R^2.
    H[2] = (3.0 * S_z_invR2 - S_z3_invR2) / sigma_m;
    H[3] = H[1];
    H[4] = 2.0 * nu1 * S_y_invR2;
    // sum z^2 (3 - z^2) / R^2 = 3 sum z^2/R^2 - sum z^4/R^2.
    H[5] = (3.0 * S_z2_invR2 - S_z4_invR2) / m;
    H[6] = H[2];
    H[7] = H[5];
    const double dpsi = cppunivariate::special::digamma(0.5 * nu1) -
                        cppunivariate::special::digamma(0.5 * nu);
    const double dtri = cppunivariate::special::trigamma(0.5 * nu1) -
                        cppunivariate::special::trigamma(0.5 * nu);
    H[8] = 0.5 * nu1 * S_y_invR2 - S_z2_invR + 0.5 * m * S_logR -
           0.25 * m * m * dn * dtri - 0.5 * m * dn * dpsi;
    for (int i=0;i<9;++i) H[i] /= divisor;
  }
};

// Ordinary location-scale t2 in standardized data coordinates. Unlike the
// unit-variance parameterization this face has a finite scale.
struct StudentTTwoCost {
  span x;
  double center, unit;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double scale = std::exp(theta[1]);
    if (!(scale > 0.0) || !std::isfinite(scale))
      return std::numeric_limits<double>::infinity();
    double neg = 0.0;
    for (double v : x) {
      const double w = ((v - center) / unit - theta[0]) / scale;
      neg += std::log(unit) + theta[1] + 1.5 * std::log(2.0) +
             1.5 * std::log1p(w * w / 2.0);
    }
    return neg;
  }
  void gradient(const double *theta, double *g) const {
    const double scale = std::exp(theta[1]);
    g[0] = 0.0;
    g[1] = 0.0;
    for (double v : x) {
      const double w = ((v - center) / unit - theta[0]) / scale;
      g[0] -= 3.0 * w / (scale * (2.0 + w * w));
      g[1] += 1.0 - 3.0 * w * w / (2.0 + w * w);
    }
  }
};

inline StudentTResult student_t_mle_fixed_nu(span x, double nu, double tol, int maxit) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (!std::isfinite(nu) || nu <= 2.0)
    return {nan,nan,nu,nan,0,StudentTStatus::InvalidNu};
  const auto r = lst_mle_fixed_df(x,nu,tol,maxit);
  StudentTStatus status = StudentTStatus::InvalidStart;
  if (r.status == LstStatus::Ok) status = StudentTStatus::Ok;
  if (r.status == LstStatus::Unbounded) status = StudentTStatus::Unbounded;
  if (r.status == LstStatus::ScaleBoundary) status = StudentTStatus::ScaleBoundary;
  if (r.status == LstStatus::IterationFailure) status = StudentTStatus::IterationFailure;
  return {r.mu,r.sigma*std::sqrt(nu/(nu-2.0)),nu,r.logLik,r.iterations,status};
}

inline StudentTResult student_t_mle_impl(span x, double tol, int maxit, bool standardized) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n < 2) return {kNaN, kNaN, kNaN, kNaN, 0, StudentTStatus::InvalidStart};

  // For fixed skewness the two side scales are s*xi and s/xi;
  // constants change, but the collapse exponent remains nu*(n-k)-k.
  const std::size_t k = collapse_multiplicity(x);
  if (k > 2*(n-k))
    return {kNaN, kNaN, kNaN, kNaN, 0, StudentTStatus::Unbounded};
  if (k == 2*(n-k))
    return {kNaN, kNaN, kNaN, kNaN, 0, StudentTStatus::ScaleBoundary};

  // Use the same dimensionless optimisation problem in every system of units.
  const auto moments = norm_detail::moments(x);
  if (moments.sd > 0.0 && std::isfinite(moments.sd) &&
      !standardized) {
    std::vector<double> z(n);
    for (std::size_t i = 0; i < n; ++i)
      z[i] = static_cast<double>((static_cast<long double>(x[i]) - moments.mean) / moments.sd);
    auto r = student_t_mle_impl(span{z.data(), n}, tol, maxit, true);
    r.mu = moments.mean + moments.sd*r.mu;
    r.sigma *= moments.sd;
    r.logLik -= static_cast<double>(n)*std::log(moments.sd);
    return r;
  }

  const double mean_x = cppunivariate::mean(x);
  double var_x = 0.0;
  for (auto v : x) {
    const double d = v - mean_x;
    var_x += d * d;
  }
  var_x /= static_cast<double>(n - 1);
  const double sd_x = std::sqrt(var_x);
  if (!std::isfinite(sd_x) || sd_x <= 0.0) {
    return {kNaN, kNaN, kNaN, kNaN, 0, StudentTStatus::InvalidStart};
  }

  const double dn = static_cast<double>(n);
  const double objective_divisor = n>=100000 ? dn : 1.0;
  StudentTCost cost{x,objective_divisor};
  finite_diff_cost<StudentTCost> fd{cost};
  const double phi0[3] = {mean_x, cppunivariate::to_log(sd_x),
                          cppunivariate::to_log_shift(6.0, 2.0)};

  BfgsControl ctl;
  ctl.grad_tol = tol;
  ctl.maxit = maxit;
  auto r = bfgs_minimize(fd, phi0, ctl);
  r.f *= objective_divisor;
  const double normal_sd = sd_x * std::sqrt((dn - 1.0) / dn);
  double kurtosis = 0.0;
  for (double v : x) {
    const double z = (v - mean_x) / normal_sd;
    kurtosis += z * z * z * z / dn;
  }
  if (kurtosis < 3.0) {
    const double start[3] = {mean_x, std::log(normal_sd), std::log(1e4 - 2.0)};
    auto large = bfgs_minimize(fd, start, ctl);
    large.f *= objective_divisor;
    if (std::isfinite(large.f) && large.f < r.f) r = large;
  }

  StudentTTwoCost two{x, mean_x, normal_sd};
  BfgsResult face;
  face.f = std::numeric_limits<double>::infinity();
  // Mean and empirical quartile starts, each refined by t2 EM. EM supplies
  // a scale adapted to tight clusters before the two-dimensional BFGS fit.
  std::vector<double> sorted;
  for (double v : x) sorted.push_back((v - mean_x) / normal_sd);
  std::sort(sorted.begin(), sorted.end());
  for (double location : {0.0, sorted[n / 4], sorted[n / 2], sorted[3 * n / 4]}) {
    double scale = 1.0;
    for (int j = 0; j < maxit; ++j) {
      double weights = 0.0, weighted = 0.0;
      for (double v : sorted) {
        const double w = (v - location) / scale;
        const double weight = 3.0 / (2.0 + w * w);
        weights += weight;
        weighted += weight * v;
      }
      const double next = weighted / weights;
      double variance = 0.0;
      for (double v : sorted) {
        const double w = (v - location) / scale;
        variance += 3.0 / (2.0 + w * w) * (v - next) * (v - next);
      }
      location = next;
      scale = std::sqrt(variance / dn);
    }
    const double start[2] = {location, std::log(scale)};
    auto candidate = bfgs_minimize(two, start, ctl);
    if (std::isfinite(candidate.f) && candidate.f < face.f) face = candidate;
  }
  const double normal_ll = -dn * (std::log(normal_sd) +
      0.5 * (std::log(2.0 * M_PI) + 1.0));
  // Absolute log-likelihood tie tolerance, scaled per observation. Prefer
  // a face within 1e-8 per observation of the best numerical fit; normal
  // takes precedence if the two faces tie. This is a multistart numerical
  // comparison, not an exhaustive global stationary-point certificate.
  const double tie = 1e-8 * dn;
  const double interior_ll = std::isfinite(r.f) ? -r.f : -INFINITY;
  if (normal_ll >= interior_ll - tie && normal_ll >= -face.f - tie) {
    return {mean_x, normal_sd, INFINITY, normal_ll, r.iterations,
            StudentTStatus::NormalBoundary};
  }
  if (-face.f >= interior_ll - tie) {
    return {mean_x + normal_sd * face.theta[0], INFINITY, 2.0, -face.f,
            face.iterations, StudentTStatus::DfTwoBoundary};
  }

  const double mu = r.theta[0];
  const double sigma = cppunivariate::from_log(r.theta[1]);
  const double nu = cppunivariate::from_log_shift(r.theta[2], 2.0);
  const double logLik = -r.f;

  if (r.status != BfgsStatus::Ok) {
    return {mu, sigma, nu, logLik, r.iterations, StudentTStatus::BfgsFailure};
  }
  return {mu, sigma, nu, logLik, r.iterations, StudentTStatus::Ok};
}

inline StudentTResult student_t_mle(span x, double tol, int maxit) {
  return student_t_mle_impl(x, tol, maxit, false);
}


inline StudentTResult student_t_mle(span x,double tol,int maxit,double mean,double sd,double nu) {
  if(std::isnan(mean)&&std::isnan(sd))return std::isnan(nu)?student_t_mle(x,tol,maxit):student_t_mle_fixed_nu(x,nu,tol,maxit);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  auto profile=[&](double d){
    const double s=std::isnan(sd)?sd:sd*std::sqrt((d-2)/d);
    const auto r=lst_detail::conditional(x,d,mean,s,tol,maxit);
    StudentTStatus status=StudentTStatus::InvalidStart;
    if(r.status==LstStatus::Ok)status=StudentTStatus::Ok;
    if(r.status==LstStatus::Unbounded)status=StudentTStatus::Unbounded;
    if(r.status==LstStatus::ScaleBoundary)status=StudentTStatus::ScaleBoundary;
    if(r.status==LstStatus::IterationFailure)status=StudentTStatus::IterationFailure;
    return StudentTResult{r.mu,std::isnan(sd)?r.sigma*std::sqrt(d/(d-2)):sd,d,r.logLik,r.iterations,status};
  };
  if(!std::isnan(nu)) {
    if(!(nu>2)||!std::isfinite(nu))return {nan,nan,nu,nan,0,StudentTStatus::InvalidNu};
    return profile(nu);
  }
  if(x.empty())return {nan,nan,nan,nan,0,StudentTStatus::InvalidStart};
  const std::size_t k=std::isnan(mean)?collapse_multiplicity(x):static_cast<std::size_t>(std::count(x.begin(),x.end(),mean));
  if(k>2*(x.size()-k))return {nan,nan,nan,INFINITY,0,StudentTStatus::Unbounded};
  std::vector<double> grid;for(double t=-18;t<=16;t+=.5)grid.push_back(t);
  const auto optimum=lst_detail::maximize([&](double a){return profile(2+std::exp(a)).logLik;},grid,1e-8,maxit);
  if(!optimum.converged)return {nan,nan,nan,nan,maxit,StudentTStatus::IterationFailure};
  auto best=profile(2+std::exp(optimum.point));const double tie=1e-8*x.size();
  const auto normal=norm_mle(x,mean,sd);
  double face=-INFINITY;
  if(std::isnan(sd))face=lst_detail::conditional(x,2,mean,nan,tol,maxit).logLik;
  if(k==2*(x.size()-k)) {
    double atom=mean;
    if(std::isnan(atom)) {
      std::vector<double> y(x.begin(),x.end());std::sort(y.begin(),y.end());atom=y[y.size()/2];
    }
    face=-static_cast<double>(x.size())*std::log(2.0);
    for(double v:x)if(v!=atom)face-=3*std::log(std::abs(v-atom));
  }
  if(normal.logLik>=best.logLik-tie&&normal.logLik>=face-tie)return {normal.mean,normal.sd,INFINITY,normal.logLik,0,StudentTStatus::NormalBoundary};
  if(face>=best.logLik-tie)return {mean,INFINITY,2,face,0,k==2*(x.size()-k)?StudentTStatus::ScaleBoundary:StudentTStatus::DfTwoBoundary};
  return best;
}
}  // namespace cppunivariate
