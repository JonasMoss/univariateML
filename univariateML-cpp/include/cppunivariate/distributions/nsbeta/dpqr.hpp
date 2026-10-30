#pragma once
#include "../beta/dpqr.hpp"
namespace cppunivariate { namespace distributions {
inline double dnsbeta(double x, double shape1, double shape2, double min, double max, bool log) {

 if(!(max>min) || !std::isfinite(min) || !std::isfinite(max) ||
    !(shape1>0) || !(shape2>0) || !std::isfinite(shape1) ||
    !std::isfinite(shape2) || std::isnan(x)) return special::kNaN;
 const double lw=std::log(max-min);
 double ld;
 if(x<min || x>max) ld=special::kNegInf;
 else if(x==min) ld=shape1==1 ? std::log(shape2)-lw :
     (shape1<1 ? special::kPosInf : special::kNegInf);
 else if(x==max) ld=shape2==1 ? std::log(shape1)-lw :
     (shape2<1 ? special::kPosInf : special::kNegInf);
 else ld=(shape1-1)*(std::log(x-min)-lw)
     +(shape2-1)*(std::log(max-x)-lw)-special::lbeta(shape1,shape2)-lw;
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
 const double width=max-min;
 const double lp=missing_detail::beta_log(std::log(q-min)-std::log(width),
     std::log(max-q)-std::log(width),shape1,shape2,lower_tail);
 return log_p ? lp : std::exp(lp);
}
inline double qnsbeta(double p,double shape1,double shape2,double min,double max,bool lower_tail,bool log_p) {
 if(!(std::isfinite(min) && std::isfinite(max) && max>min && shape1>0 && std::isfinite(shape1) && shape2>0 && std::isfinite(shape2))) return std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return std::numeric_limits<double>::quiet_NaN();
 if(p==(log_p ? -std::numeric_limits<double>::infinity() : 0)) return lower_tail ? min : max;
 if(p==(log_p ? 0 : 1)) return lower_tail ? max : min;
 return min+(max-min)*qbeta(p,shape1,shape2,lower_tail,log_p);
}
template <typename Rng> inline double rnsbeta(Rng &rng,double shape1,double shape2,double min,double max) {
 if(!(std::isfinite(min) && std::isfinite(max) && max>min && shape1>0 && std::isfinite(shape1) && shape2>0 && std::isfinite(shape2))) return std::numeric_limits<double>::quiet_NaN();
 return min+(max-min)*rbeta(rng,shape1,shape2);
}
} }
