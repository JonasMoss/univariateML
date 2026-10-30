#pragma once
#include <cmath>
#include <limits>
namespace cppunivariate { namespace distributions {
inline double ddlaplace(double x,double location,double scale,bool log=false){
 if(!(scale>0&&scale<1)||!std::isfinite(location)||location!=std::floor(location))return std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(x))return x;
 double v=x!=std::floor(x)?-INFINITY:std::log1p(-scale)-std::log1p(scale)+std::abs(x-location)*std::log(scale);return log?v:std::exp(v);
}
inline double pdlaplace(double q,double location,double scale,bool lower_tail=true,bool log_p=false){
 if(!(scale>0&&scale<1)||!std::isfinite(location)||location!=std::floor(location))return std::numeric_limits<double>::quiet_NaN();
 double z=std::floor(q-location),l=-z*std::log(scale)-std::log1p(scale),u=(z+1)*std::log(scale)-std::log1p(scale);
 double v=lower_tail?(z<0?l:std::log(-std::expm1(u))):(z>=0?u:std::log(-std::expm1(l)));return log_p?v:std::exp(v);
}
inline double qdlaplace(double p,double location,double scale,bool lower_tail=true,bool log_p=false){
 double lp=log_p?p:std::log(p);
 if(std::isnan(lp)||lp>0||(!log_p&&p<0)||!(scale>0&&scale<1)||!std::isfinite(location)||location!=std::floor(location))return std::numeric_limits<double>::quiet_NaN();
 if(lp==-INFINITY)return lower_tail?-INFINITY:INFINITY;if(lp==0)return lower_tail?INFINITY:-INFINITY;
 auto reached=[&](double q){double v=pdlaplace(q,location,scale,lower_tail,true);return lower_tail?v>=lp:v<=lp;};
 double a=location-1,b=location,step=1;
 while(reached(a)){b=a;step*=2;a=location-step;if(step>9e15)return -INFINITY;}
 step=1;while(!reached(b)){a=b;step*=2;b=location+step;if(step>9e15)return INFINITY;}
 while(b-a>1){double m=std::floor(a+(b-a)/2);if(reached(m))b=m;else a=m;}return b;
}
template<typename Rng> inline double rdlaplace(Rng &rng,double location,double scale){return qdlaplace(rng.runif(0,1),location,scale);}
} }
