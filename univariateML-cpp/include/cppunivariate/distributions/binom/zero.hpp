#pragma once
#include "truncation.hpp"
#include "../../core/zero_hurdle.hpp"
#include "dpqr.hpp"
#include "../ztbinom/mle.hpp"
#include "../zmbinom/mle.hpp"
#include "../zib/mle.hpp"
namespace cppunivariate {
struct BinomZeroTraits {
  static bool positive_ok(const ZtbinomResult& r) {return r.status==ZtbinomStatus::Ok;}
  static double log_pmf(double x,double size,double prob) {return distributions::dbinom(x,size,prob,true);}
  static double log_zero(double size,double prob) {return distributions::dbinom(0,size,prob,true);}
  template<class... Args> static auto truncated(ispan x,Args... args) {return ztbinom_mle(x,args...);}
  template<class... Args> static auto modified(ispan x,Args... args) {return zmbinom_mle(x,args...);}
  template<class... Args> static auto inflated(ispan x,Args... args) {return zib_mle(x,args...);}
};
}
