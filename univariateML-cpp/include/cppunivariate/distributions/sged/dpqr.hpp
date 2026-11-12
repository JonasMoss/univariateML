// Package-defined, mean/standard-deviation parameterisation.
#pragma once
#include "../snorm/fs.hpp"
#include "../snorm/dpqr.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"
#include "../ged/dpqr.hpp"
#include "../../special/skew_moments.hpp"
#include <algorithm>
#include <utility>
namespace cppunivariate {
namespace distributions {
namespace sged_detail {
struct LogResidual { double magnitude; bool negative; };
// Signed logarithmic addition, used only when an intermediate leaves the
// normal range. Keeping the magnitude logarithmic also preserves |z|^nu
// for subunit nu when z itself cannot be represented.
inline LogResidual add(LogResidual a, LogResidual b) {
  if (a.magnitude < b.magnitude) std::swap(a,b);
  if (b.magnitude == special::kNegInf) return a;
  return {a.magnitude + (a.negative == b.negative
      ? std::log1p(std::exp(b.magnitude-a.magnitude))
      : fs::log1mexp(b.magnitude-a.magnitude)), a.negative};
}
inline LogResidual log_shift(double nu, double xi, double mu) {
  if (std::isnormal(mu) || xi==1) return {std::log(std::fabs(mu)),mu<0};
  // The centering moment may itself underflow for very small held shapes.
  return {std::lgamma(2.0/nu)-0.5*(std::lgamma(1.0/nu)+std::lgamma(3.0/nu))
          +std::log(std::fabs(xi-1.0/xi)),xi<1};
}
inline LogResidual residual(double x, double mean, double sd, double sigma, double mu,
                            double nu, double xi) {
  return add({special::log_abs_difference(x,mean)-std::log(sd)+std::log(sigma),x<mean},
             log_shift(nu,xi,mu));
}
inline bool needs_log(double x, double mean, double sd, double sigma, double z, double v, double mu, double xi) {
  const double t=(x-mean)/sd;
  const double tiny=std::numeric_limits<double>::min();
  return std::isfinite(x) && std::isfinite(sigma) && std::isfinite(mu) && ((!std::isnormal(mu) && xi!=1) || std::isinf(t) || std::isinf(z) || std::isinf(v)
      || (x!=mean && std::fabs(t)<tiny)
      || (t!=0 && std::fabs(t*sigma)<tiny)
      || (z!=0 && std::fabs(v)<tiny));
}
inline double input_error(double x,double mean,double sd,double sigma,double mu,
                          double xi,double z,double v) {
  if(!std::isfinite(x)) return 0;
  const double part=(x-mean)/sd*sigma;
  const double dz=32*std::numeric_limits<double>::epsilon()
      *(1+std::abs(part)+std::abs(mu)+(std::abs(x)+std::abs(mean))/sd*std::abs(sigma));
  const double scale=std::abs(z)>dz ? (z<0?1/xi:xi) : std::min(xi,1/xi);
  return dz/scale+32*std::numeric_limits<double>::epsilon()*std::abs(v);
}
} // namespace sged_detail
inline double sged_logradius_density(const snorm_detail::Coordinate &c,double nu,bool log) {
 if(!c.valid || !std::isfinite(c.log_value) || !std::isfinite(c.log_error))return special::kNaN;
 const double ls=fs::ged_logscale(nu),logcost=nu*(c.log_value-ls),cost=std::exp(logcost);
 const double ld=c.log_normalizer-.69314718055994530942-ls-special::lgamma1p(1/nu)-cost;
 if(!std::isfinite(cost)) {
   if(nu*(c.log_value-c.log_error-ls)>std::log(std::numeric_limits<double>::max())+2)return log?special::kNegInf:0;
   return special::kNaN;
 }
 const double margin=cost*std::expm1(nu*c.log_error);
 if(margin>1e-11*std::max(1.0,std::abs(ld)))return special::kNaN;
 return log?ld:std::exp(ld);
}
inline double sged_logradius_probability(const snorm_detail::Coordinate &c,double nu,double xi,bool lower,bool log) {
 if(!c.valid || !std::isfinite(c.log_value) || !std::isfinite(c.log_error))return special::kNaN;
 const double lt=xi<=1?std::log(xi):-std::log(xi),ls=fs::ged_logscale(nu);
 const double logcost=nu*(c.log_value-ls),cost=std::exp(logcost);
 if(!std::isfinite(cost)) {
   if(nu*(c.log_value-c.log_error-ls)>std::log(std::numeric_limits<double>::max())+2)return special::tail_probability(special::tails_from_log(special::kNegInf,c.negative),lower,log);
   return special::kNaN;
 }
 const double base=ged_detail::gamma_power_log(c.log_value-ls,nu,false);
 const double direct=-std::log1p(c.square)+(c.thin?2*lt:0)+base;
 const auto tails=special::tails_from_log(direct,c.negative);
 const double selected=lower?tails.lower:tails.upper;
 double margin=c.log_error*(nu*cost+std::max(0.0,nu-1));
 if(selected>-.6931471805599453)margin=direct<-745?0:margin*std::exp(direct-selected);
 if(std::isfinite(selected) && selected!=0 && (log || selected>-700) && margin>(log?1e-11*std::max(1.0,std::abs(selected)):1e-11))return special::kNaN;
 return special::tail_probability(tails,lower,log);
}
// Recompute the moment and the physical residual before rounding the radius.
inline double sged_precise(double x,double mean,double sd,double nu,double xi,bool density,bool lower,bool log) {
 if(!std::isfinite(x))return density?(log?special::kNegInf:0):special::tail_probability(special::tails_from_log(special::kNegInf,x<0),lower,log);
 const auto m=special::skew_coordinate::ged(nu);
 const auto c=special::skew_coordinate::coordinate(x,mean,sd,xi,m);
 if(!c.valid)return special::kNaN;
 if(!std::isfinite(c.value))return density?sged_logradius_density({c.value,c.log_normalizer,c.error,c.centering_error,c.square,c.log_value,c.log_error,c.thin,c.negative,c.valid},nu,log):sged_logradius_probability({c.value,c.log_normalizer,c.error,c.centering_error,c.square,c.log_value,c.log_error,c.thin,c.negative,c.valid},nu,xi,lower,log);
 const auto r=ged_detail::radius_precise(c.radius,nu,c.arithmetic_error);
 if(density)return ged_detail::density_precise(r,nu,c.log_normalizer,log);
 const double lt=xi<=1?std::log(xi):-std::log(xi);
 if(!c.thin) {
  const double central=ged_detail::gamma_power_log(r.value,nu,true);
  const double opposite=special::log_sum_exp(2*lt,central)-std::log1p(c.square);
  if(opposite<-.6931471805599453) {
   // For P(a,exp(nu*w)), a=1/nu, 0 <= d log(P)/dw <= 1:
   // integrate exp(-x*u) >= exp(-x) over u in [0,1]. Adding
   // the thin-side mass can only decrease this log sensitivity.
   const double margin=r.error+ged_detail::gamma_log_margin(central);
   const double weight_error=32*std::numeric_limits<double>::epsilon()*(1+std::abs(lt));
   const double left=special::log_sum_exp(2*lt,central-margin)-std::log1p(c.square)-weight_error;
   const double right=std::min(0.,special::log_sum_exp(2*lt,std::min(0.,central+margin))-std::log1p(c.square)+weight_error);
   const auto tails=special::tails_from_log(opposite,!c.negative);
   const auto low=special::tails_from_log(left,!c.negative),high=special::tails_from_log(right,!c.negative);
   const double selected=lower?tails.lower:tails.upper;
   if(!ged_detail::probability_interval(selected,lower?low.lower:low.upper,lower?high.lower:high.upper,log))return special::kNaN;
   return special::tail_probability(tails,lower,log);
  }
 }
 return ged_detail::probability_precise(r,nu,-std::log1p(c.square)+(c.thin?2*lt:0),c.negative,lower,log);
}
inline double sged_central_quantile(double p,double mean,double sd,double nu,double xi,bool lower,bool log) {
 if(nu<1e6 || !(log?p>-.92 && p<-.51:p>.4 && p<.6))return special::kNaN;
 const auto m=special::skew_coordinate::ged(nu);
 if(!m.valid)return special::kNaN;
 const auto t=xi<=1?special::three_word::Number{xi}:special::three_word::divide({1},{xi});
 const auto t2=special::three_word::multiply(t,t),m2=special::three_word::multiply(m.value,m.value);
 const auto sigma=special::three_word::root(special::three_word::add(special::three_word::multiply(special::three_word::add({1},special::three_word::negative(m2)),special::three_word::add({1},special::three_word::multiply(t2,t2))),special::three_word::multiply(special::three_word::add(special::three_word::multiply({2},m2),{-1}),t2)));
 auto probability=log?special::three_word::exponential({p}):special::three_word::Number{p};
 if(!lower)probability=special::three_word::add({1},special::three_word::negative(probability));
 const auto side=xi<=1?probability:special::three_word::add({1},special::three_word::negative(probability));
 const auto central=special::three_word::add({1},special::three_word::negative(special::three_word::multiply(side,special::three_word::add({1},t2))));
 if(!(central.high>0))return special::kNaN;
 const auto lc=special::three_word::logarithm(central);
 // P(a,y)=y^a/Gamma(1+a)*(1+O(y/(nu+1))). Here y is
 // below exp(-100), so this explicit inverse has negligible remainder.
 if(nu*lc.high>=-100)return special::kNaN;
 const auto a=special::three_word::divide({1},{nu});
 const auto scale=special::three_word::exponential(special::three_word::add(ged_detail::logscale_precise(nu),special::three_word::log_gamma(special::three_word::add({1},a))));
 const auto v=special::three_word::multiply(scale,central);
 auto offset=special::three_word::divide(special::three_word::add(special::three_word::multiply(m.value,special::three_word::add({1},special::three_word::negative(t2))),special::three_word::negative(v)),sigma);
 offset=special::three_word::multiply(offset,{(xi<=1?1.:-1.)*sd});
 const double q=special::three_word::add({mean},offset).high;
 const double eps=std::numeric_limits<double>::epsilon();
 const double unit_error=10*m.error+special::three_word::margin(64)+(nu>=1e30?4/nu:0)+scale.high*std::exp(nu*lc.high)/(nu+1);
 const double sum_error=32768*eps*eps*eps*(std::abs(mean)+std::abs(offset.high))+128*std::numeric_limits<double>::denorm_min();
 const double physical_error=sd*unit_error/sigma.high+sum_error+eps*std::abs(q);
 // Either relative quantile accuracy or the probability criterion closes
 // the near-zero cancellation. In this central region f(x) <= 2/sd.
 if(physical_error<=1e-10*std::abs(q) || 2*(unit_error/sigma.high+sum_error/sd+eps*(std::abs(q)/sd))<=1e-13)return q;
 return special::kNaN;
}
inline double dsged(double x, double mean, double sd, double nu, double xi, bool log) {
  if (!(fs::valid(mean, sd, nu, xi)) || std::isnan(x)) return cppunivariate::special::kNaN;
  if(xi==1)return dged(x,mean,sd,nu,log);
  if(nu!=2 && nu>=.05 && (nu>=1e6 || xi<1e-6 || xi>1e6))return sged_precise(x,mean,sd,nu,xi,true,true,log);
  if(nu==1) {
    if(!std::isfinite(x))return log?special::kNegInf:0;
    const auto c=snorm_detail::coordinate_with_moment(x,mean,sd,xi,{0.7071067811865476,-4.833646656726457e-17},0);
    if(!c.valid)return special::kNaN;
    if(!std::isfinite(c.value))return sged_logradius_density(c,nu,log);
    const double ld=c.log_normalizer-.34657359027997265471-1.4142135623730950488*c.value;
    if(1.4142135623730950488*c.error>1e-11*std::max(1.0,std::abs(ld)))return special::kNaN;
    return log?ld:std::exp(ld);
  }
  if(nu==2) return dsnorm(x,mean,sd,xi,log);
  const auto moment=special::skew_moments::ged(nu);
  if(moment.valid) {
    if(!std::isfinite(x))return log?special::kNegInf:0;
    const auto c=snorm_detail::coordinate_with_moment(x,mean,sd,xi,{moment.value.high,moment.value.low},0,moment.error);
    if(!c.valid)return special::kNaN;
    if(!std::isfinite(c.value))return sged_logradius_density(c,nu,log);
    const double ls=fs::ged_logscale(nu),lr=std::log(c.value)-ls;
    if(nu>=1e6 && !fs::ged_radius_precise(c.value,fs::ged_log_radius(c.value,nu,ls),nu,c.error))return special::kNaN;
    const double cost=std::exp(nu*lr),ld=c.log_normalizer+fs::ged_logdensity(c.value,nu);
    const double ratio=c.error/c.value;
    const double increment=nu*std::log1p(ratio);
    const double increase=ratio<1?std::exp(nu*lr+increment+special::log1mexp(-increment))
      :std::exp(nu*(std::log(c.value+c.error)-ls))-cost;
    const double decrease=ratio<1?-cost*std::expm1(nu*std::log1p(-ratio)):cost;
    const double margin=std::max(increase,decrease)+4*moment.error;
    if(std::isfinite(ld) && (!(margin>=0) || margin>1e-11*std::max(1.0,std::abs(ld))))return special::kNaN;
    return log?ld:std::exp(ld);
  }
  double mu = 0.0;
  const double sigma = fs::sged_sigma(nu, xi, mu);
  const double z = (x - mean)/sd*sigma + mu;
  const double v=z/(z<0.0 ? 1.0/xi : xi);
  if(nu>=1e6 && (sged_detail::needs_log(x,mean,sd,sigma,z,v,mu,xi) ||
      !fs::ged_radius_precise(std::abs(v),fs::ged_log_radius(v,nu,fs::ged_logscale(nu)),nu,
          sged_detail::input_error(x,mean,sd,sigma,mu,xi,z,v)))) return special::kNaN;
  double ld = (std::log(2.0/(xi+1.0/xi)) + fs::ged_logdensity(v, nu)) - std::log(sd) + std::log(sigma);
  if (sged_detail::needs_log(x,mean,sd,sigma,z,v,mu,xi)) {
    const auto r=sged_detail::residual(x,mean,sd,sigma,mu,nu,xi);
    const double ls=fs::ged_logscale(nu);
    const double lv=r.magnitude+(r.negative ? std::log(xi) : -std::log(xi));
    ld=std::log(2.0/(xi+1.0/xi))-std::log(2.0)-ls-std::lgamma(1+1/nu)
        -std::exp(nu*(lv-ls))-std::log(sd)+std::log(sigma);
  }
  return log ? ld : std::exp(ld);
}
inline double psged(double q, double mean, double sd, double nu, double xi, bool lower_tail=true, bool log_p=false) {
  if (!(fs::valid(mean, sd, nu, xi)) || std::isnan(q)) return cppunivariate::special::kNaN;
  if(xi==1)return pged(q,mean,sd,nu,lower_tail,log_p);
  if(nu!=2 && nu>=.05 && (nu>=1e6 || xi<1e-6 || xi>1e6))return sged_precise(q,mean,sd,nu,xi,false,lower_tail,log_p);
  if(nu==1) {
   const auto c=snorm_detail::coordinate_with_moment(q,mean,sd,xi,{0.7071067811865476,-4.833646656726457e-17},0);
   if(c.valid && !std::isfinite(c.value))return sged_logradius_probability(c,nu,xi,lower_tail,log_p);
   return snorm_detail::probability_with_moment(q,mean,sd,xi,{0.7071067811865476,-4.833646656726457e-17},lower_tail,log_p,
    [](double v){return -.69314718055994530942-1.4142135623730950488*v;},
    [](double v){return std::log(-std::expm1(-1.4142135623730950488*v));},
    [](double,double){return 1.4142135623730950488;});
  }
  if(nu==2) return psnorm(q,mean,sd,xi,lower_tail,log_p);
  const auto moment=special::skew_moments::ged(nu);
  if(moment.valid) {
    const double ls=fs::ged_logscale(nu);
    const auto coordinate=snorm_detail::coordinate_with_moment(q,mean,sd,xi,{moment.value.high,moment.value.low},0,moment.error);
    if(coordinate.valid && !std::isfinite(coordinate.value))return sged_logradius_probability(coordinate,nu,xi,lower_tail,log_p);
    const double low=coordinate.value-coordinate.error;
    if(coordinate.valid && low>0 && nu*(std::log(low)-ls)>std::log(std::numeric_limits<double>::max())+2 && 1/nu<std::numeric_limits<double>::max()/4)
      return special::tail_probability(special::tails_from_log(special::kNegInf,coordinate.negative),lower_tail,log_p);
    return snorm_detail::probability_with_moment(q,mean,sd,xi,{moment.value.high,moment.value.low},lower_tail,log_p,
      [nu](double v){return pged(v,0,1,nu,false,true);},
      [nu,ls](double v){return ged_detail::gamma_power_log(std::log(v)-ls,nu,true);},
      [nu,ls](double v,double e){
        const double low=std::max(0.0,v-e),high=v+e;
        if(nu<1) {
          const double at_zero=-ls-special::lgamma1p(1/nu);
          const double power=low>0?std::log(nu)+nu*(std::log(low)-ls)-std::log(low):special::kPosInf;
          return std::exp(std::min(at_zero,power));
        }
        if(nu>=1 && low>0 && std::log(low)>=ls)
          return std::exp(std::log(nu)+nu*(std::log(high)-ls)-std::log(high))+(nu-1)/low;
        const double density=fs::ged_logdensity(low,nu),tail=pged(high,0,1,nu,false,true);
        const double rounding=16*std::numeric_limits<double>::epsilon()*(1+std::abs(density)+std::abs(tail));
        return std::exp(density-tail+rounding);
      },moment.error);
  }
  double mu = 0.0;
  const double sigma = fs::sged_sigma(nu, xi, mu);
  const double z = (q - mean)/sd*sigma + mu;
  const double v=z/(z<0.0 ? 1.0/xi : xi);
  if(nu>=1e6 && (sged_detail::needs_log(q,mean,sd,sigma,z,v,mu,xi) ||
      !fs::ged_radius_precise(std::abs(v),fs::ged_log_radius(v,nu,fs::ged_logscale(nu)),nu,
          sged_detail::input_error(q,mean,sd,sigma,mu,xi,z,v)))) return special::kNaN;
  if (sged_detail::needs_log(q,mean,sd,sigma,z,v,mu,xi)) {
    const auto r=sged_detail::residual(q,mean,sd,sigma,mu,nu,xi);
    const double lscale=r.negative ? -std::log(xi) : std::log(xi);
    const double la=r.magnitude-lscale-fs::ged_logscale(nu);
    const double lp=std::log(2.0)-std::log(xi+1.0/xi)+lscale-std::log(2.0)
        +ged_detail::gamma_power_log(la,nu,false);
    const double result=(r.negative==lower_tail) ? lp : fs::log1mexp(lp);
    return log_p ? result : std::exp(result);
  }
  return fs::skew_p(z, xi, lower_tail, log_p, [&](double v) { return pged(v, 0.0, 1.0, nu, true, true); });
}
inline double qsged(double p, double mean, double sd, double nu, double xi, bool lower_tail=true, bool log_p=false) {
  if (!(fs::valid(mean, sd, nu, xi))) return cppunivariate::special::kNaN;
  // Exact symmetric member avoids rounded log-tail splitting at the median.
  if (xi==1) return qged(p,mean,sd,nu,lower_tail,log_p);
  if(nu>=1e6 && (xi<1e-6 || xi>1e6) && (log_p?p>-.92 && p<-.51:p>.4 && p<.6))
    return sged_central_quantile(p,mean,sd,nu,xi,lower_tail,log_p);
  double mu = 0.0;
  const double sigma = fs::sged_sigma(nu, xi, mu);
  const double z = fs::skew_q(p, xi, lower_tail, log_p, [&](double v) { return qged(v, 0.0, 1.0, nu, true, true); });
  const double offset=sd*(z-mu)/sigma;
  if (!std::isfinite(sigma) || !std::isfinite(mu)) return mean+offset;
  if (std::isnormal(mu) && std::isfinite(z) && std::isfinite(offset) &&
      (z==mu || std::isnormal(z))) return mean+offset;
  // Form the signed unit quantile and subtract its centering shift in logs
  // before scaling: either can underflow while the final offset is finite.
  const double lp=fs::logprob(p,log_p);
  if (std::isnan(lp)) return lp;
  const double lc=fs::log1mexp(lp);
  const double logg=std::log(2.0)-std::log(xi+1.0/xi);
  const double lleft=logg-std::log(xi)-std::log(2.0);
  const double lright=logg+std::log(xi)-std::log(2.0);
  const bool left=lower_tail ? lp<=lleft : lp>=lright;
  const double lscale=left ? -std::log(xi) : std::log(xi);
  const double side=(left==lower_tail) ? lp : lc;
  const double tail=std::min(0.0,side-logg-lscale+std::log(2.0));
  const double la=ged_detail::gamma_power_logq(tail,nu,false);
  auto shift=sged_detail::log_shift(nu,xi,mu);
  shift.negative=!shift.negative;
  const auto r=sged_detail::add({lscale+fs::ged_logscale(nu)+la,left},shift);
  return mean+std::copysign(std::exp(std::log(sd)-std::log(sigma)+r.magnitude),
                            r.negative ? -1.0 : 1.0);
}
template <typename Rng>
inline double rsged(Rng &rng, double mean, double sd, double nu, double xi) {
  if (!(fs::valid(mean, sd, nu, xi))) return cppunivariate::special::kNaN;
  double mu = 0.0;
  const double sigma = fs::sged_sigma(nu, xi, mu);
  const double weight = xi / (xi + 1.0 / xi);
  const double z = rng.runif(-weight, 1.0 - weight);
  const double Xi = std::pow(xi, cppunivariate::dens::sign1(z));
  const double base = -std::fabs(cppunivariate::distributions::rged(rng, 0.0, 1.0,
                                                             nu)) /
                      Xi * cppunivariate::dens::sign1(z);
  const double scaled = (base - mu) / sigma;
  return scaled * sd + mean;
}

} // namespace distributions
} // namespace cppunivariate
