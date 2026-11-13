#pragma once
#include "truncation.hpp"
#include "../../core/zero_hurdle.hpp"
#include "dpqr.hpp"
#include "ztnbinom_mle.hpp"
#include "zmnbinom_mle.hpp"
#include "zinb_mle.hpp"
namespace cppunivariate {
struct NbinomZeroTraits {
  static bool positive_ok(const ZtnbinomResult& r) {return r.status==ZtnbinomStatus::Ok;}
  static double log_pmf(double x,double size,double prob) {return distributions::dnbinom(x,size,prob,true);}
  static double log_zero(double size,double prob) {return distributions::dnbinom(0,size,prob,true);}
  template<class... Args> static auto truncated(ispan x,Args... args) {return ztnbinom_mle(x,args...);}
  template<class... Args> static auto modified(ispan x,Args... args) {return zmnbinom_mle(x,args...);}
  template<class... Args> static auto inflated(ispan x,Args... args) {return zinb_mle(x,args...);}
};
}
