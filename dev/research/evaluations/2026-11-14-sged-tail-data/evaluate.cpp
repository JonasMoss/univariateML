// Evaluate psged/psnorm/psstd (both tails, log and natural) and the
// quantile used to place sweep points. Input lines:
//   p <family> mean sd nu xi x      (snorm ignores nu)
//   q <family> mean sd nu xi logp lower
//   g ged 0 1 nu 0 v                (pged(v,0,1,nu,upper,log): the baseline base-tail evaluator)
//   u ged 0 1 nu 0 v                (the fixed fast-path evaluator, two-word log(v/s); current tree)
//   m sged mean sd nu xi x          (fast-path coordinate and baseline margin terms)
// Output: one line per input with %.17g values.
#include <cstdio>
#include <cstring>
#include "cppunivariate/distributions/sged/dpqr.hpp"
#include "cppunivariate/distributions/sstd/dpqr.hpp"
namespace d=cppunivariate::distributions;
static double P(const char*f,double m,double s,double nu,double xi,double x,bool lower,bool log){
  if(!std::strcmp(f,"sged"))return d::psged(x,m,s,nu,xi,lower,log);
  if(!std::strcmp(f,"sstd"))return d::psstd(x,m,s,nu,xi,lower,log);
  return d::psnorm(x,m,s,xi,lower,log);
}
static double Q(const char*f,double m,double s,double nu,double xi,double p,bool lower){
  if(!std::strcmp(f,"sged"))return d::qsged(p,m,s,nu,xi,lower,true);
  if(!std::strcmp(f,"sstd"))return d::qsstd(p,m,s,nu,xi,lower,true);
  return d::qsnorm(p,m,s,xi,lower,true);
}
int main(){
  char op[8],f[16];double m,s,nu,xi,x;
  while(std::scanf("%7s %15s %lf %lf %lf %lf %lf",op,f,&m,&s,&nu,&xi,&x)==7){
    if(op[0]=='g'){std::printf("%.17g\n",d::pged(x,0,1,nu,false,true));continue;}
#ifdef SGED_TAIL_CURRENT
    if(op[0]=='u'){std::printf("%.17g\n",-.69314718055994530942+cppunivariate::ged_detail::gamma_power_log(d::sged_detail::log_scaled_radius(x,nu),nu,false));continue;}
#endif
    if(op[0]=='m'){
      // Recompute the terms of snorm_detail::probability_with_moment's
      // direct-tail margin with the sged hazard bound of psged.
      const auto moment=cppunivariate::special::skew_moments::ged(nu);
      const double ls=d::fs::ged_logscale(nu),eps=std::numeric_limits<double>::epsilon();
      const auto c=cppunivariate::distributions::snorm_detail::coordinate_with_moment(x,m,s,xi,{moment.value.high,moment.value.low},0,moment.error);
      const double centering=c.thin?c.centering_error/c.square:c.centering_error;
      const double radius_error=centering+2*eps*c.value;
      const double low=std::max(0.0,c.value-radius_error),high=c.value+radius_error;
      const double hazard=nu>=1 && low>0 && std::log(low)>=ls
        ?std::exp(std::log(nu)+nu*(std::log(high)-ls)-std::log(high))+(nu-1)/low:std::nan("");
      std::printf("%.17g %.17g %.17g %.17g %d\n",c.value,centering,radius_error,hazard,int(c.thin));
      continue;}
    if(op[0]=='q'){int lower;if(std::scanf("%d",&lower)!=1)return 1;
      std::printf("%.17g\n",Q(f,m,s,nu,xi,x,lower!=0));}
    else std::printf("%.17g %.17g %.17g %.17g\n",P(f,m,s,nu,xi,x,true,true),P(f,m,s,nu,xi,x,false,true),
                     P(f,m,s,nu,xi,x,true,false),P(f,m,s,nu,xi,x,false,false));
  }
}
