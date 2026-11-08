#pragma once
#include "../../special/tails.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate { namespace distributions {
inline double ddlaplace(double x,double location,double scale,bool log=false){
 if(!(scale>0&&scale<1)||!std::isfinite(location)||location!=std::floor(location))return std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(x))return x;
 const double delta=x-location;
 const double tail=std::isfinite(x)&&!std::isfinite(delta)
     ? (std::abs(x*0.5-location*0.5)*std::log(scale))*2
     : std::abs(delta)*std::log(scale);
 double v=x!=std::floor(x)?-INFINITY:std::log1p(-scale)-std::log1p(scale)+tail;return log?v:std::exp(v);
}
inline double pdlaplace(double q,double location,double scale,bool lower_tail=true,bool log_p=false){
 if(!(scale>0&&scale<1)||!std::isfinite(location)||location!=std::floor(location))return std::numeric_limits<double>::quiet_NaN();
 double z=std::floor(q-location),l=-z*std::log(scale)-std::log1p(scale),u=(z+1)*std::log(scale)-std::log1p(scale);
 // Here the lost lattice unit is below binary64 resolution. Scale the
 // half-distance before restoring its factor of two, retaining finite logs.
 if(std::isfinite(q)&&!std::isfinite(z)) {
   const double tail=((q*0.5-location*0.5)*std::log(scale))*2;
   l=-tail-std::log1p(scale);u=tail-std::log1p(scale);
 }
 double v=lower_tail?(z<0?l:std::log(-std::expm1(u))):(z>=0?u:std::log(-std::expm1(l)));return log_p?v:std::exp(v);
}
inline double qdlaplace(double p,double location,double scale,bool lower_tail=true,bool log_p=false){
 double lp=log_p?p:std::log(p);
 if(std::isnan(lp)||lp>0||(!log_p&&p<0)||!(scale>0&&scale<1)||!std::isfinite(location)||location!=std::floor(location))return std::numeric_limits<double>::quiet_NaN();
 if(lp==-INFINITY)return lower_tail?-INFINITY:INFINITY;if(lp==0)return lower_tail?INFINITY:-INFINITY;
 return special::discrete_signed_search(location,1,[&](double q) {
   if (std::abs(q)>9007199254740992.0) {
     const double f=pdlaplace(q,location,scale,lower_tail,log_p);
     return std::isnan(f) ? -1 : int(lower_tail ? f>=p : f<=p);
   }
   const double v=pdlaplace(q,location,scale,lower_tail,true);
   return std::isnan(v) ? -1 : int(lower_tail ? v>=lp : v<=lp);
 });
}
template<typename Rng> inline double rdlaplace(Rng &rng,double location,double scale){return qdlaplace(rng.runif(0,1),location,scale);}
} }
