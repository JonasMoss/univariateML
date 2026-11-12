// Package-defined, mean/standard-deviation parameterisation.
#pragma once
#include "../snorm/fs.hpp"
#include <limits>
#include "../../special/skew_coordinate.hpp"
#include "tails.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace ged_detail {
struct Radius { double value,error,scale; };
inline double zero_rounding_threshold() {
 return special::skew_probability::zero_rounding_threshold();
}
inline special::three_word::Number logscale_uncached(double nu) {
 if(nu>=1e30)return special::three_word::multiply({.5},special::three_word::logarithm({3}));
 const auto z=special::three_word::divide({1},{nu});
 return special::three_word::multiply(special::three_word::add(special::three_word::log_gamma(z),special::three_word::negative(special::three_word::log_gamma(special::three_word::multiply(z,{3})))),{.5});
}
inline special::three_word::Number logscale_precise(double nu) {
 static thread_local double last=0;
 static thread_local special::three_word::Number saved{0};
 if(nu!=last){saved=logscale_uncached(nu);last=nu;}
 return saved;
}
inline Radius radius_precise(special::three_word::Number v,double nu,double input_error=0) {
 const auto ls=logscale_precise(nu),lv=special::three_word::logarithm(v);
 const auto w=special::three_word::add(lv,special::three_word::negative(ls));
 const double e=special::three_word::margin(std::abs(ls.high)+std::abs(lv.high))+std::abs(w.low)+4*std::numeric_limits<double>::epsilon()*std::abs(w.high);
 return {w.high,e+(nu>=1e30?2/nu:0)+(v.high>input_error?input_error/(v.high-input_error):input_error==0?0:special::kPosInf),ls.high};
}
inline double density_precise(Radius r,double nu,double normalizer,bool log) {
 const double gamma=special::lgamma1p(1/nu);
 const double cost=std::exp(nu*r.value),ld=normalizer-std::log(2.)-r.scale-gamma-cost;
 if(!std::isfinite(cost)) {
  if(nu*(r.value-r.error)>std::log(std::numeric_limits<double>::max())+2)return log?special::kNegInf:0;
  return special::kNaN;
 }
 const double change=nu*r.error<.5?cost*std::expm1(nu*r.error)
   :std::max(std::exp(nu*(r.value+r.error))-cost,cost-std::exp(nu*(r.value-r.error)));
 const double margin=change+16*std::numeric_limits<double>::epsilon()*(1+cost+std::abs(ld)+std::abs(normalizer)+std::abs(r.scale)+std::abs(gamma));
 if(!log && ld+margin<zero_rounding_threshold())return 0;
 if(!(margin>=0) || margin>(log?1e-11*std::max(1.,std::abs(ld)):1e-11))return special::kNaN;
 return log?ld:std::exp(ld);
}
// Compare the selected tail itself at both uncertainty endpoints. A
// log-error multiplied by the rounded center tail is unsafe after that
// center has underflowed: an endpoint can still have appreciable mass.
inline bool probability_interval(double point,double left,double right,bool log) {
 return special::skew_probability::interval(point,left,right,log);
}
inline double gamma_log_margin(double value) {
 return std::isfinite(value)?2e-12+16*std::numeric_limits<double>::epsilon()*std::abs(value):0;
}
inline double probability_precise(Radius r,double nu,double logweight,bool negative,bool lower,bool log) {
 const double base=gamma_power_log(r.value,nu,false),direct=logweight+base;
 const auto tails=special::tails_from_log(direct,negative);
 const double selected=lower?tails.lower:tails.upper;
 const double left=gamma_power_log(r.value-r.error,nu,false),right=gamma_power_log(r.value+r.error,nu,false);
 const double weight_error=16*std::numeric_limits<double>::epsilon()*(1+std::abs(logweight));
 const auto low=special::tails_from_log(logweight+right-gamma_log_margin(right)-weight_error,negative);
 const auto high=special::tails_from_log(std::min(0.,logweight+left+gamma_log_margin(left)+weight_error),negative);
 if(!probability_interval(selected,lower?low.lower:low.upper,lower?high.lower:high.upper,log))return special::kNaN;
 return special::tail_probability(tails,lower,log);
}
inline Radius physical_radius(double x,double mean,double sd,double nu) {
 auto delta=special::three_word::add({x},{-mean});
 special::three_word::Number lv;
 if(std::isfinite(delta.high)) {
  if(delta.high<0)delta=special::three_word::negative(delta);
  lv=special::three_word::logarithm(delta);
 } else {
  delta=special::three_word::add({x*.5},{-mean*.5});
  if(delta.high<0)delta=special::three_word::negative(delta);
  lv=special::three_word::add(special::three_word::logarithm(delta),special::three_word::log_two());
 }
 const auto ls=logscale_precise(nu),logsd=special::three_word::logarithm({sd});
 const auto w=special::three_word::add(special::three_word::add(lv,special::three_word::negative(logsd)),special::three_word::negative(ls));
 const double error=special::three_word::margin(std::abs(lv.high)+std::abs(logsd.high)+std::abs(ls.high))+std::abs(w.low)+4*std::numeric_limits<double>::epsilon()*std::abs(w.high)+(nu>=1e30?2/nu:0);
 return {w.high,error,ls.high};
}
}
namespace distributions {
inline double dged(double x, double mean, double sd, double nu, bool log) {
  if (!(fs::valid(mean, sd, nu, 1.0)) || std::isnan(x)) return cppunivariate::special::kNaN;
  const double z = (x - mean)/sd;
  const double ls=fs::ged_logscale(nu);
  if(nu>=1e6 && std::isfinite(x) && x!=mean)
    return ged_detail::density_precise(ged_detail::physical_radius(x,mean,sd,nu),nu,-std::log(sd),log);
  const double ld = (std::isinf(z) || (x != mean && std::fabs(z) < std::numeric_limits<double>::min())) && std::isfinite(x)
      ? -std::log(2.0)-ls-std::lgamma(1+1/nu)-std::log(sd)
          -std::exp(nu*(special::log_abs_difference(x,mean)-std::log(sd)-ls))
      : fs::ged_logdensity(z, nu) - std::log(sd);
  return log ? ld : std::exp(ld);
}
inline double pged(double q, double mean, double sd, double nu, bool lower_tail, bool log_p) {
  if (!(fs::valid(mean, sd, nu, 1.0)) || std::isnan(q)) return cppunivariate::special::kNaN;
  double la;
  if(nu>=1e6 && std::isfinite(q) && q!=mean)
    return ged_detail::probability_precise(ged_detail::physical_radius(q,mean,sd,nu),nu,-std::log(2.),q<mean,lower_tail,log_p);
  la=special::log_abs_difference(q,mean)-std::log(sd)-fs::ged_logscale(nu);
  const double small = -std::log(2.0) + ged_detail::gamma_power_log(la,nu,false);
  const double result = ((q < mean) == lower_tail) ? small : fs::log1mexp(small);
  return log_p ? result : std::exp(result);
}
inline double qged(double p, double mean, double sd, double nu, bool lower_tail, bool log_p) {
  if (!(fs::valid(mean, sd, nu, 1.0))) return cppunivariate::special::kNaN;
  const double lp = fs::logprob(p, log_p);
  if (std::isnan(lp)) return lp;
  const double h=log_p ? (p+std::log(2.0))+2.3190468138462995584e-17 : 0;
  if ((!log_p && std::abs(p-0.5)<1e-8) || (log_p && std::abs(h)<1e-8)) {
    if(!log_p && p==0.5) return mean;
    const double mass=log_p ? std::expm1(h) : 2*(p-0.5);
    const double la=ged_detail::gamma_power_logq(std::log(std::abs(mass)),nu,true);
    const double magnitude=std::exp(std::log(sd)+fs::ged_logscale(nu)+la);
    return mean+((mass>0)==lower_tail?magnitude:-magnitude);
  }
  const bool small = lp <= -std::log(2.0);
  const double tail = (small ? lp : fs::log1mexp(lp)) + std::log(2.0);
  const double la = ged_detail::gamma_power_logq(tail,nu,false);
  const double magnitude = std::exp(std::log(sd)+fs::ged_logscale(nu)+la);
  return mean + ((small == lower_tail) ? -magnitude : magnitude);
}
template <typename Rng>
inline double rged(Rng &rng, double mean, double sd, double nu) {
  if (!(fs::valid(mean, sd, nu, 1.0))) return cppunivariate::special::kNaN;
  // Gamma augmentation: G_a = G_(a+1) U^(1/a). Apply the
  // GED power before exponentiation so U survives even for a near zero.
  const double a=1/nu;
  double logamplitude;
  if(nu>=16) {
    const double g=rng.rgamma(1+a,1);
    if(!(g>0) || !std::isnormal(g)) return special::kNaN;
    const double u=rng.runif(0,1);
    logamplitude=std::log(g)*a+std::log(u);
  } else {
    // Preserve the ordinary-shape primitive stream. A lost gamma draw is
    // a numerical failure, rather than a spurious atom at the center.
    const double r=rng.rgamma(a,1);
    if(!(r>0) || !std::isnormal(r)) return special::kNaN;
    logamplitude=std::log(r)/nu;
  }
  const double sign = cppunivariate::dens::sign1(rng.runif(0.0, 1.0) - 0.5);
  const double z = std::exp(fs::ged_logscale(nu) + logamplitude) * sign;
  return z * sd + mean;
}

} // namespace distributions
} // namespace cppunivariate
