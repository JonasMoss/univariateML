#pragma once
#include "special.hpp"
#include "tails.hpp"
namespace cppunivariate { namespace special { namespace skew_probability {
inline double zero_rounding_threshold() {
 return std::log(std::numeric_limits<double>::denorm_min())-std::log(2.)-1e-12;
}
// Inputs are logarithms of the requested probability. Comparing these
// endpoints also protects against a rounded-zero center hiding finite mass.
inline bool interval(double point,double left,double right,bool log) {
 if(std::isnan(point) || std::isnan(left) || std::isnan(right))return false;
 if(!log && std::max({point,left,right})<zero_rounding_threshold())return true;
 if(!std::isfinite(point))return point==left && point==right;
 if(!std::isfinite(left) || !std::isfinite(right))return false;
 double margin=std::max(std::abs(left-point),std::abs(right-point))
     +8*std::numeric_limits<double>::epsilon()*std::max(1.,std::abs(point));
 if(log)return margin<=1e-10*std::max(1.,std::abs(point));
 const double rounded=std::exp(point);
 if(!(rounded>0))return false;
 // A half-spacing bound would unnecessarily reject aligned subnormals.
 // Bound the actual rounding displacement using log(rounded), including
 // a practical reserve for that logarithm's own rounding.
 const double roundtrip=std::log(rounded);
 margin+=std::abs(roundtrip-point)
     +8*std::numeric_limits<double>::epsilon()*std::max(1.,std::abs(roundtrip));
 return std::expm1(margin)<=1e-10;
}
inline double bounds(double point,double left,double right,bool negative,bool lower,bool log) {
 if(std::isnan(point) || std::isnan(left) || std::isnan(right))return kNaN;
 const auto tails=tails_from_log(point,negative);
 const auto a=tails_from_log(std::min(0.,left),negative);
 const auto b=tails_from_log(std::min(0.,right),negative);
 if(!interval(lower?tails.lower:tails.upper,lower?a.lower:a.upper,lower?b.lower:b.upper,log))return kNaN;
 return tail_probability(tails,lower,log);
}
inline double enclose(double point,double error,bool negative,bool lower,bool log) {
 if(!(error>=0) || !std::isfinite(error))return kNaN;
 return bounds(point,point-error,point+error,negative,lower,log);
}
} } }
