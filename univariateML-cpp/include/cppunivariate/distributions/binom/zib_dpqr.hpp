#pragma once
#include "../pois/ztpois_dpqr_core.hpp"
namespace cppunivariate { namespace distributions {
inline bool zib_valid(double size,double prob) {
  return std::isfinite(size) && size>=0 && size==std::floor(size) && prob>=0 && prob<=1;
}
inline double dzib(double x,double size,double prob,double pi,bool log=false) {
  if(!zib_valid(size,prob)||!(pi>=0 && pi<=1)||std::isnan(x))return std::numeric_limits<double>::quiet_NaN();
  double l=-std::numeric_limits<double>::infinity();
  if(size==0)l=x==0?0:-std::numeric_limits<double>::infinity();
  else if(x==0) {
    const double base=size*std::log1p(-prob);
    const double lo=special::log_sum_exp(std::log(pi),std::log1p(-pi)+base);
    const double up=std::log1p(-pi)+special::log1mexp(base);
    l=lo<up ? lo : special::log1mexp(up);
  }
  else if(x>0 && x<=size && x==std::floor(x))l=std::log1p(-pi)+dbinom(x,size,prob,true);
  return log?l:std::exp(l);
}
inline double pzib(double q,double size,double prob,double pi,bool lower_tail=true,bool log_p=false) {
  if(!zib_valid(size,prob)||!(pi>=0 && pi<=1)||std::isnan(q))return std::numeric_limits<double>::quiet_NaN();
  if(q<0) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  const auto t=binom_tails(q,size,prob);
  const double lo=special::log_sum_exp(std::log(pi),std::log1p(-pi)+t.lower);
  const double up=std::log1p(-pi)+t.upper;
  return special::tail_probability(lo<up ? special::tails_from_log(lo,true)
                                        : special::tails_from_log(up,false),lower_tail,log_p);
}
inline double zib_quantile(double p,double size,double prob,double pi,bool lower_tail,bool log_p,bool fuzz) {
  if(!zib_valid(size,prob)||!(pi>=0 && pi<=1)||std::isnan(p)||
      (log_p?p>0:(p<0||p>1)))return std::numeric_limits<double>::quiet_NaN();
  const double ls=special::discrete_log_tails(p,lower_tail,log_p).upper;
  if(ls==0)return 0;
  if(std::isinf(ls))return size;
  if(size==0||prob==0||pi==1)return 0;
  if(prob==1 && pi==0)return size;
  return special::discrete_search(0,size,[&](double x) {
    if(fuzz)return special::discrete_reached(pzib(x,size,prob,pi,lower_tail,log_p),p,lower_tail,log_p);
    if (x>9007199254740992.0) {
      const double f=pzib(x,size,prob,pi,lower_tail,log_p);
      return std::isnan(f) ? -1 : int(lower_tail ? f>=p : f<=p);
    }
    const double f=pzib(x,size,prob,pi,false,true);
    return std::isnan(f) ? -1 : int(f<=ls);
  });
}
inline double qzib(double p,double size,double prob,double pi,bool lower_tail=true,bool log_p=false) {
  return zib_quantile(p,size,prob,pi,lower_tail,log_p,true);
}
template <typename Rng> inline double rzib(Rng &rng,double size,double prob,double pi) {
  if (size==0 || prob==0 || pi==1 || (pi==0 && prob==1))
    return zib_quantile(.5,size,prob,pi,true,false,false);
  return zib_quantile(rng.runif(0,1),size,prob,pi,true,false,false);
}
} }
