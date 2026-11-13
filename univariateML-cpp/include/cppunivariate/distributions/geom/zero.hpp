#pragma once
#include "truncation.hpp"
#include "../../core/zero_hurdle.hpp"
#include "dpqr.hpp"
#include "ztgeom_mle.hpp"
#include "zmgeom_mle.hpp"
#include "../nbinom/zinb_mle.hpp"
namespace cppunivariate {
struct GeomZeroTraits {
  static bool positive_ok(const ZtgeomResult& r) {return r.status==ZtgeomStatus::Ok;}
  static double log_pmf(double x,double prob) {return distributions::dgeom(x,prob,true);}
  static double log_zero(double prob) {return distributions::dgeom(0,prob,true);}
  template<class... Args> static auto truncated(ispan x,Args... args) {return ztgeom_mle(x,args...);}
  static ZinbResult inflated(ispan x,double tol=1e-10,int maxit=200,double prob=NAN,double pi=NAN) {
    return zinb_mle(x,tol,maxit,1,prob,pi);
  }
  template<class... Args> static auto modified(ispan x,Args... args) {return zmgeom_mle(x,args...);}
};
}
