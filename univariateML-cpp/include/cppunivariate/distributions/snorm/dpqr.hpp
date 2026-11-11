// Package-defined, mean/standard-deviation parameterisation.
#pragma once
#include "../snorm/fs.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"
#include "../../special/tails.hpp"
namespace cppunivariate {
namespace distributions {
namespace snorm_detail {
// Double-double arithmetic retains the normalized centering moment before
// division by xi^2. No extended-width floating-point type is required.
struct DD { double high,low; };
inline DD sum(double a,double b) {
  const double high=a+b;
  return {high,special::addition_residual(a,b,high)};
}
inline DD add(DD a,DD b) {
  const DD first=sum(a.high,b.high),last=sum(first.low,a.low+b.low);
  return sum(first.high,last.high+last.low);
}
inline DD negative(DD a) {return {-a.high,-a.low};}
inline DD multiply(DD a,DD b) {
  const double high=a.high*b.high;
  return sum(high,std::fma(a.high,b.high,-high)+a.high*b.low+a.low*b.high+a.low*b.low);
}
inline DD divide(DD a,DD b) {
  int exponent=0;
  std::frexp(b.high,&exponent);
  a={std::scalbn(a.high,-exponent),std::scalbn(a.low,-exponent)};
  b={std::scalbn(b.high,-exponent),std::scalbn(b.low,-exponent)};
  const double high=a.high/b.high;
  if(!std::isfinite(high)) return {high,0};
  const DD residual=add(a,negative(multiply({high,0},b)));
  return sum(high,(residual.high+residual.low)/b.high);
}
inline DD root(DD a) {
  const double high=std::sqrt(a.high);
  const DD residual=add(a,negative(multiply({high,0},{high,0})));
  return sum(high,(residual.high+residual.low)/(2*high));
}
struct Coordinate {double value,log_normalizer,error,centering_error,square,log_value,log_error;bool thin,negative,valid;};
inline Coordinate coordinate_with_moment(double x,double mean,double sd,double xi,DD moment,double log_base,double moment_error=0) {
  const double sign=xi<=1?1:-1;
  const DD t=xi<=1?DD{xi,0}:divide({1,0},{xi,0});
  const DD t2=multiply(t,t);
  const DD m2=multiply(moment,moment);
  const DD variance=add(multiply(add({1,0},negative(m2)),add({1,0},multiply(t2,t2))),
      multiply(add(multiply({2,0},m2),{-1,0}),t2));
  const DD sigma=root(variance);
  const DD difference=sum(x,-mean);
  const DD standardized_difference=std::isfinite(difference.high)?divide(difference,{sd,0})
    :(sd*.5>0?divide(sum(x*.5,-mean*.5),{sd*.5,0}):DD{special::kNaN,0});
  const DD w=multiply(standardized_difference,{sign,0});
  const DD n=add(multiply(w,sigma),multiply(moment,add(t2,{-1,0})));
  if(!std::isfinite(n.high)) return {0,0,0,0,0,0,0,false,false,false};
  const bool thin=n.high>0 || (n.high==0 && n.low>=0);
  const DD standardized=thin && t2.high>0?divide(n,t2):n;
  const double value=thin && t2.high==0?special::kPosInf:std::abs(standardized.high+standardized.low);
  const double eps=std::numeric_limits<double>::epsilon();
  // Practical error margin for this fixed sequence of compensated operations.
  const double centering=64*eps*eps*(std::abs(moment.high)+std::abs(w.high))+(1+2*std::abs(w.high))*moment_error;
  const double error=(thin?centering/t2.high:centering)+8*eps*value;
  const double ln=std::log(2.0)+std::log(sigma.high)+sigma.low/sigma.high
      -std::log1p(t2.high)-std::log(sd)+log_base;
  const double magnitude=std::abs(n.high+n.low);
  const double logarithm=std::log(magnitude)-(thin?2*(std::log(t.high)+t.low/t.high):0);
  const double relative_error=centering/magnitude;
  const double logarithm_error=relative_error<.5?2*relative_error+16*eps*(1+std::abs(logarithm)):special::kPosInf;
  return {value,ln,error,centering,t2.high,logarithm,logarithm_error,thin,sign*(thin?1:-1)<0,true};
}
inline Coordinate coordinate(double x,double mean,double sd,double xi) {
  return coordinate_with_moment(x,mean,sd,xi,{.7978845608028654,-4.98465440455546e-17},-.91893853320467274178);
}
// A symmetric variance-one base needs only its first absolute moment.
// Retain that moment's low word before subtracting the skew mean.
template <typename LogUpper,typename LogCentral,typename Hazard>
inline double probability_with_moment(double x,double mean,double sd,double xi,DD moment,
 bool lower,bool log,LogUpper upper,LogCentral central,Hazard hazard,double moment_error=0) {
 if(!std::isfinite(x))return special::tail_probability(special::tails_from_log(special::kNegInf,x<0),lower,log);
 const auto c=coordinate_with_moment(x,mean,sd,xi,moment,0,moment_error);
 if(!c.valid || !std::isfinite(c.value))return special::kNaN;
 const double lt=xi<=1?std::log(xi):-std::log(xi);
 const double direct=std::log(2.0)-std::log1p(c.square)+(c.thin?2*lt:0)+upper(c.value);
 auto tails=special::tails_from_log(direct,c.negative);
 // The DD quotient is rounded once after centering. Two epsilons cover
 // that final radius rounding; the absolute centering margin is separate.
 const double radius_error=(c.thin?c.centering_error/c.square:c.centering_error)
     +2*std::numeric_limits<double>::epsilon()*c.value;
 double margin=radius_error*hazard(c.value,radius_error)+2e-12+32*std::numeric_limits<double>::epsilon();
 if(!c.thin) {
   const double opposite=special::log_sum_exp(2*lt,central(c.value))-std::log1p(c.square);
   if(opposite<direct) {
     tails=special::tails_from_log(opposite,!c.negative);
     margin=16*c.centering_error/(c.square+c.value)+2e-12+32*std::numeric_limits<double>::epsilon();
   }
 }
 const double selected=lower?tails.lower:tails.upper;
 if(!std::isfinite(margin))return special::kNaN;
 if(log && selected>-.6931471805599453) {
   const double other=std::min(tails.lower,tails.upper);
   margin=other<-745?0:margin*std::exp(other-selected);
 }
 const double allowance=log?1e-11*std::max(1.0,std::abs(selected)):1e-11;
 if(std::isfinite(selected) && selected!=0 && (log || (selected>-700 && selected<-.01)) && margin>allowance)return special::kNaN;
 return special::tail_probability(tails,lower,log);
}
inline double density(double x,double mean,double sd,double xi,bool log) {
  if(!std::isfinite(x)) return log?special::kNegInf:0;
  const auto c=coordinate(x,mean,sd,xi);
  if(!c.valid) return special::kNaN;
  const double ld=c.log_normalizer-(.5*c.value)*c.value;
  if(std::isfinite(ld) && c.error*(c.value+c.error)>1e-11*std::max(1.0,std::abs(ld))) return special::kNaN;
  return log?ld:std::exp(ld);
}
inline double probability(double x,double mean,double sd,double xi,bool lower,bool log) {
  if(!std::isfinite(x)) return special::tail_probability(special::tails_from_log(special::kNegInf,x<0),lower,log);
  const auto c=coordinate(x,mean,sd,xi);
  if(!c.valid) return special::kNaN;
  const double lt=xi<=1?std::log(xi):-std::log(xi);
  const double base=special::normal_log_upper(c.value);
  const double direct=std::log(2.0)-std::log1p(c.square)+(c.thin?2*lt:0)+base;
  special::LogTails tails=special::tails_from_log(direct,c.negative);
  double margin=c.error*(1+c.value+c.error);
  if(!c.thin) {
    // Complement the broad side about the skew mode, retaining xi^2 even
    // when the normal CDF is indistinguishable from its rounded half.
    const double opposite=special::log_sum_exp(2*lt,std::log(std::erf(c.value*.70710678118654752440)))-std::log1p(c.square);
    if(opposite<direct) {
      tails=special::tails_from_log(opposite,!c.negative);
      margin=2*c.centering_error/(c.square+c.value)+16*std::numeric_limits<double>::epsilon();
    }
  }
  const double selected=lower?tails.lower:tails.upper;
  if(log && selected>-.6931471805599453) {
    const double other=std::min(tails.lower,tails.upper);
    margin=other<-745?0:margin*std::exp(other-selected);
  }
  const double allowance=log?1e-11*std::max(1.0,std::abs(selected)):1e-11;
  // A rounded zero/one on the natural scale needs no relative small-tail
  // comparison; finite logarithmic tails retain their forward-error gate.
  if(std::isfinite(selected) && selected!=0 && (log || (selected>-700 && selected<-.01)) && margin>allowance) return special::kNaN;
  return special::tail_probability(tails,lower,log);
}
}

inline double dsnorm(double x, double mean, double sd, double xi, bool log) {
  if (!(fs::valid(mean, sd, 1.0, xi)) || std::isnan(x)) return cppunivariate::special::kNaN;
  return snorm_detail::density(x,mean,sd,xi,log);
}
inline double psnorm(double q, double mean, double sd, double xi, bool lower_tail=true, bool log_p=false) {
  if (!(fs::valid(mean, sd, 1.0, xi)) || std::isnan(q)) return cppunivariate::special::kNaN;
  return snorm_detail::probability(q,mean,sd,xi,lower_tail,log_p);
}
inline double qsnorm(double p, double mean, double sd, double xi, bool lower_tail=true, bool log_p=false) {
  if (!(fs::valid(mean, sd, 1.0, xi))) return cppunivariate::special::kNaN;
  // Exact symmetric member avoids rounded log-tail splitting at the median.
  if (xi==1) return mean+sd*special::normal_quantile(p,lower_tail,log_p);
  double mu = 0.0;
  const double sigma = cppunivariate::dens::snorm_sigma(xi, mu);
  const double z = fs::skew_q(p, xi, lower_tail, log_p, [&](double v) { return cppunivariate::special::fs_normal_q(v, true); });
  return mean + sd*(z-mu)/sigma;
}
template <typename Rng>
inline double rsnorm(Rng &rng, double mean, double sd, double xi) {
  if (!(fs::valid(mean, sd, 1.0, xi))) return cppunivariate::special::kNaN;
  double mu = 0.0;
  const double sigma = cppunivariate::dens::snorm_sigma(xi, mu);
  const double weight = xi / (xi + 1.0 / xi);
  const double z = rng.runif(-weight, 1.0 - weight);
  const double Xi = std::pow(xi, cppunivariate::dens::sign1(z));
  const double base =
      -std::fabs(rng.rnorm()) / Xi * cppunivariate::dens::sign1(z);
  const double scaled = (base - mu) / sigma;
  return scaled * sd + mean;
}

} // namespace distributions
} // namespace cppunivariate
