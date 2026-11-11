#pragma once
#include "../beta/dpqr.hpp"
namespace cppunivariate { namespace distributions {
inline double dnsbeta(double x, double shape1, double shape2, double min, double max, bool log) {

 if(!(max>min) || !std::isfinite(min) || !std::isfinite(max) ||
    !(shape1>0) || !(shape2>0) || !std::isfinite(shape1) ||
    !std::isfinite(shape2) || std::isnan(x)) return special::kNaN;
 const double lw=special::log_positive_distance(max,min);
 double ld;
 if(x<min || x>max) ld=special::kNegInf;
 else if(x==min) ld=shape1==1 ? std::log(shape2)-lw :
     (shape1<1 ? special::kPosInf : special::kNegInf);
 else if(x==max) ld=shape2==1 ? std::log(shape1)-lw :
     (shape2<1 ? special::kPosInf : special::kNegInf);
 else ld=special::affine_beta_log_density_positive(x,shape1,shape2,min,max);
 return log ? ld : std::exp(ld);
}
} }

#include "../beta/dpqr.hpp"
namespace cppunivariate { namespace distributions {
inline double pnsbeta(double q,double shape1,double shape2,double min,double max,bool lower_tail,bool log_p) {
 if(!(std::isfinite(min) && std::isfinite(max) && max>min && shape1>0 && std::isfinite(shape1) && shape2>0 && std::isfinite(shape2))) return std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(q)) return std::numeric_limits<double>::quiet_NaN();
 if(q<=min) return log_p ? (lower_tail ? -std::numeric_limits<double>::infinity() : 0) : (lower_tail ? 0 : 1);
 if(q>=max) return log_p ? (lower_tail ? 0 : -std::numeric_limits<double>::infinity()) : (lower_tail ? 1 : 0);
 if(shape1==shape2) {
   int exponent=0;
   std::frexp(std::max(std::abs(q),std::max(std::abs(min),std::abs(max))),&exponent);
   const double x=std::scalbn(q,-exponent), a=std::scalbn(min,-exponent), b=std::scalbn(max,-exponent);
   const double sum=a+b, low=special::addition_residual(a,b,sum);
   const double centered=((x-sum*0.5)-low*0.5)/((b-a)*0.5);
   if(std::abs(centered)<=0.5) {
     const double lp=missing_detail::beta_log_from_logit(2*std::atanh(centered),shape1,shape2,lower_tail);
     return log_p?lp:std::exp(lp);
   }
 }
 const auto coordinates=special::interval_log_coordinates(q,min,max);
 const double lp=missing_detail::beta_log(coordinates.lower,coordinates.upper,shape1,shape2,lower_tail);
 return log_p ? lp : std::exp(lp);
}
inline double qnsbeta(double p,double shape1,double shape2,double min,double max,bool lower_tail,bool log_p) {
 if(!(std::isfinite(min) && std::isfinite(max) && max>min && shape1>0 && std::isfinite(shape1) && shape2>0 && std::isfinite(shape2))) return std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return std::numeric_limits<double>::quiet_NaN();
 if(p==(log_p ? -std::numeric_limits<double>::infinity() : 0)) return lower_tail ? min : max;
 if(p==(log_p ? 0 : 1)) return lower_tail ? max : min;
 double t;
 if(missing_detail::symmetric_beta_logit_quantile(p,shape1,shape2,lower_tail,log_p,t))
   return special::centered_interval_quantile(min,max,0.5*std::tanh(t*0.5));
 return min+(max-min)*qbeta(p,shape1,shape2,lower_tail,log_p);
}
template <typename Rng> inline double rnsbeta(Rng &rng,double shape1,double shape2,double min,double max) {
 if(!(std::isfinite(min) && std::isfinite(max) && max>min && shape1>0 && std::isfinite(shape1) && shape2>0 && std::isfinite(shape2))) return std::numeric_limits<double>::quiet_NaN();
 return min+(max-min)*rbeta(rng,shape1,shape2);
}
} }
