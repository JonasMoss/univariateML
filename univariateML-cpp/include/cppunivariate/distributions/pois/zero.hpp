#pragma once
#include "truncation.hpp"
#include "../../core/zero_hurdle.hpp"
#include "dpqr.hpp"
#include "ztpois_mle.hpp"
#include "zmpois_mle.hpp"
#include "zip_mle.hpp"
namespace cppunivariate {
struct PoisZeroTraits {
  static bool positive_ok(const ZtpoisResult& r) {return r.status==ZtpoisStatus::Ok;}
  static double log_pmf(double x,double lambda) {return distributions::dpois(x,lambda,true);}
  static double log_zero(double lambda) {return distributions::dpois(0,lambda,true);}
  template<class... Args> static auto truncated(ispan x,Args... args) {return ztpois_mle(x,args...);}
  template<class... Args> static auto modified(ispan x,Args... args) {return zmpois_mle(x,args...);}
  template<class... Args> static auto inflated(ispan x,Args... args) {return zip_mle(x,args...);}
};
}
