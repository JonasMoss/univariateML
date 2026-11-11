// Package-defined, mean/standard-deviation parameterisation.
#pragma once
#include "../snorm/fs.hpp"
#include "../snorm/dpqr.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"
#include "../std/dpqr.hpp"
#include "../../special/skew_moments.hpp"
namespace cppunivariate {
namespace distributions {
inline double sstd_logradius_density(const snorm_detail::Coordinate &c,double nu,bool log) {
 if(!c.valid || !std::isfinite(c.log_value) || !std::isfinite(c.log_error))return special::kNaN;
 const double ld=c.log_normalizer+dens::log_dstd_unit_const(nu)
     -.5*(nu+1)*special::softplus(2*c.log_value-std::log(nu-2));
 const double margin=(nu+1)*c.log_error;
 if(std::isfinite(ld) && margin>1e-11*std::max(1.0,std::abs(ld)))return special::kNaN;
 return log?ld:std::exp(ld);
}
inline double sstd_logradius_probability(const snorm_detail::Coordinate &c,double nu,double xi,bool lower,bool log) {
 if(!c.valid || !std::isfinite(c.log_value) || !std::isfinite(c.log_error))return special::kNaN;
 const double lt=xi<=1?std::log(xi):-std::log(xi);
 const double base=student_detail::probability_log(c.log_value+.5*(std::log(nu)-std::log(nu-2)),true,nu,true,true);
 const double direct=std::log(2.0)-std::log1p(c.square)+(c.thin?2*lt:0)+base;
 const auto tails=special::tails_from_log(direct,c.negative);
 const double selected=lower?tails.lower:tails.upper;
 double margin=nu*c.log_error;
 if(selected>-.6931471805599453)margin=direct<-745?0:margin*std::exp(direct-selected);
 if(std::isfinite(selected) && selected!=0 && (log || selected>-700) && margin>(log?1e-11*std::max(1.0,std::abs(selected)):1e-11))return special::kNaN;
 return special::tail_probability(tails,lower,log);
}
inline double dsstd(double x, double mean, double sd, double nu, double xi, bool log) {
  if (!(fs::valid(mean, sd, nu, xi) && nu > 2.0) || std::isnan(x)) return cppunivariate::special::kNaN;
  if(xi==1)return dstd(x,mean,sd,nu,log);
  if(nu==5) {
    if(!std::isfinite(x))return log?special::kNegInf:0;
    const auto c=snorm_detail::coordinate_with_moment(x,mean,sd,xi,{0.7351051938957227,2.4362428464894153e-17},0);
    if(!c.valid)return special::kNaN;
    if(!std::isfinite(c.value))return sstd_logradius_density(c,nu,log);
    const double ld=c.log_normalizer+dens::log_dstd_unit_const(nu)-3*special::softplus(2*std::log(c.value)-std::log(3.0));
    const double relative=c.error/c.value;
    const double variation=relative<.5?6*relative/(1-relative):2*c.error;
    if(variation>1e-11*std::max(1.0,std::abs(ld)))return special::kNaN;
    return log?ld:std::exp(ld);
  }
  const auto moment=special::skew_moments::student(nu);
  if(moment.valid) {
    if(!std::isfinite(x))return log?special::kNegInf:0;
    const auto c=snorm_detail::coordinate_with_moment(x,mean,sd,xi,{moment.value.high,moment.value.low},0,moment.error);
    if(!c.valid)return special::kNaN;
    if(!std::isfinite(c.value))return sstd_logradius_density(c,nu,log);
    const double ld=c.log_normalizer+fs::std_logdensity(c.value,nu);
    const double low=std::max(0.0,c.value-c.error),high=c.value+c.error;
    const double change=fs::std_logdensity(low,nu)-fs::std_logdensity(high,nu)+4*moment.error;
    if(std::isfinite(ld) && (!(change>=0) || change>1e-11*std::max(1.0,std::abs(ld))))return special::kNaN;
    return log?ld:std::exp(ld);
  }
  double mu = 0.0;
  const double sigma = fs::sstd_sigma(nu, xi, mu);
  const double z = (x - mean)/sd*sigma + mu;
  if(std::isfinite(x) && !std::isfinite(z)) return special::kNaN;
  const double skew_scale=z<0?1/xi:xi, standardized=z/skew_scale;
  const double log_radius=std::log(std::abs(z))-std::log(skew_scale);
  const double base=std::isinf(standardized) && std::isfinite(z)
      ? cppunivariate::dens::log_dstd_unit_const(nu)-.5*(nu+1)*special::softplus(2*log_radius-std::log(nu-2))
      :fs::std_logdensity(standardized,nu);
  const double ld = std::log(2.0/(xi+1.0/xi))+base-std::log(sd)+std::log(sigma);
  return log ? ld : std::exp(ld);
}
inline double psstd(double q, double mean, double sd, double nu, double xi, bool lower_tail=true, bool log_p=false) {
  if (!(fs::valid(mean, sd, nu, xi) && nu > 2.0) || std::isnan(q)) return cppunivariate::special::kNaN;
  if(xi==1)return pstd(q,mean,sd,nu,lower_tail,log_p);
  if(nu==5) {
   const auto c=snorm_detail::coordinate_with_moment(q,mean,sd,xi,{0.7351051938957227,2.4362428464894153e-17},0);
   if(c.valid && !std::isfinite(c.value))return sstd_logradius_probability(c,nu,xi,lower_tail,log_p);
   return snorm_detail::probability_with_moment(q,mean,sd,xi,{0.7351051938957227,2.4362428464894153e-17},lower_tail,log_p,
    [](double v){return pstd(v,0,1,5,false,true);},
    [](double v){
      // On [0,v], 1-(1+t*t/3)^(-3) <= v*v bounds the relative
      // error of the central linear mass by v*v.
      if(v*v<=1e-12)return std::log(v)+-0.020059596611783474;
      if(v>1e150)return 0.0;
      const double square=v*v;
      return std::log(special::ibeta(.5,2.5,square/(3+square)));
    },
    [](double v,double error){
      v=std::max(0.0,v-error);
      // Integrating the t_5 density by parts gives hazard <= 5/v
      // for v>=1. Below one, S(v)>=f(2), so hazard < (7/3)^3<16.
      return v>=1?5/v:16;
    });
  }
  const auto moment=special::skew_moments::student(nu);
  if(moment.valid) {
   const auto coordinate=snorm_detail::coordinate_with_moment(q,mean,sd,xi,{moment.value.high,moment.value.low},0,moment.error);
   if(coordinate.valid && !std::isfinite(coordinate.value))return sstd_logradius_probability(coordinate,nu,xi,lower_tail,log_p);
   const double low=coordinate.value-coordinate.error;
   if(coordinate.valid && low>0) {
     const double logpenalty=std::log(nu)-.69314718055994530942+std::log(special::softplus(2*std::log(low)-std::log(nu-2)));
     if(logpenalty>std::log(std::numeric_limits<double>::max())+2)
       return special::tail_probability(special::tails_from_log(special::kNegInf,coordinate.negative),lower_tail,log_p);
   }
   return snorm_detail::probability_with_moment(q,mean,sd,xi,{moment.value.high,moment.value.low},lower_tail,log_p,
    [nu](double v){return pstd(v,0,1,nu,false,true);},
    [nu](double v){
      const double square=v*v/(nu-2);
      // Gamma log-convexity bounds the normalizer, and integrating
      // |d log(f_t/phi)/dv| bounds the entire central-mass ratio.
      const double normal_bound=(1+.5*v*v*(3+v*v))/(nu-2);
      if(normal_bound<=1e-12)return std::log(std::erf(v*.70710678118654752440));
      const double relative_bound=(nu+1)*square/6;
      if(relative_bound<=1e-12)return std::log(v)+std::log(2.0)+dens::log_dstd_unit_const(nu);
      const double lr=2*std::log(v)-std::log(nu-2);
      const double lz=-special::softplus(-lr),lzbar=-special::softplus(lr);
      return missing_detail::beta_log(lz,lzbar,.5,nu*.5,true);
    },
    [nu](double v,double error){
      const double low=std::max(0.0,v-error),high=v+error;
      const double density=fs::std_logdensity(low,nu),tail=pstd(high,0,1,nu,false,true);
      const double rounding=16*std::numeric_limits<double>::epsilon()*(1+std::abs(density)+std::abs(tail));
      return std::exp(density-tail+rounding);
    },moment.error);
  }
  double mu = 0.0;
  const double sigma = fs::sstd_sigma(nu, xi, mu);
  const double z = (q - mean)/sd*sigma + mu;
  if(std::isfinite(q) && !std::isfinite(z)) return special::kNaN;
  const double skew_scale=z<0?1/xi:xi;
  if(std::isinf(z/skew_scale) && std::isfinite(z)) {
    const double log_radius=std::log(std::abs(z))-std::log(skew_scale)+.5*(std::log(nu)-std::log(nu-2));
    const double base=student_detail::probability_log(log_radius,true,nu,true,true);
    const double small=std::log(2.0)-std::log(xi+1/xi)+std::log(skew_scale)+base;
    return special::tail_probability(special::tails_from_log(small,z<0),lower_tail,log_p);
  }
  return fs::skew_p(z, xi, lower_tail, log_p, [&](double v) { return pstd(v, 0.0, 1.0, nu, true, true); });
}
inline double qsstd(double p, double mean, double sd, double nu, double xi, bool lower_tail=true, bool log_p=false) {
  if (!(fs::valid(mean, sd, nu, xi) && nu > 2.0)) return cppunivariate::special::kNaN;
  // Exact symmetric member avoids rounded log-tail splitting at the median.
  if (xi==1) return qstd(p,mean,sd,nu,lower_tail,log_p);
  double mu = 0.0;
  const double sigma = fs::sstd_sigma(nu, xi, mu);
  const double z = fs::skew_q(p, xi, lower_tail, log_p, [&](double v) { return qstd(v, 0.0, 1.0, nu, true, true); });
  return mean + sd*(z-mu)/sigma;
}
template <typename Rng>
inline double rsstd(Rng &rng, double mean, double sd, double nu, double xi) {
  if (!(fs::valid(mean, sd, nu, xi) && nu > 2.0)) return cppunivariate::special::kNaN;
  double mu = 0.0;
  const double sigma = fs::sstd_sigma(nu, xi, mu);
  const double weight = xi / (xi + 1.0 / xi);
  const double z = rng.runif(-weight, 1.0 - weight);
  const double Xi = std::pow(xi, cppunivariate::dens::sign1(z));
  const double base = -std::fabs(cppunivariate::distributions::rstd(rng, 0.0, 1.0,
                                                             nu)) /
                      Xi * cppunivariate::dens::sign1(z);
  const double scaled = (base - mu) / sigma;
  return scaled * sd + mean;
}

} // namespace distributions
} // namespace cppunivariate
