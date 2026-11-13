#pragma once
#include "../../core/likelihood_polish.hpp"
#include "../../core/span.hpp"
namespace cppunivariate {
namespace accuracy_detail {
// The positive-count log likelihood in (log size, logit probability).
// Integer rising factorials give exact analytic derivatives without
// subtracting two nearly equal polygamma values near the Poisson face.
inline bool nb_polish(ispan x,double& size,double& prob,double& pi,
                      bool fs,bool fp,bool fi,bool truncated,double tol,int maxit,StopDetail& detail) {
  if(!(size>0&&prob>0&&prob<1))return true;
  std::array<double,3> q{{std::log(size),std::log(prob)-std::log1p(-prob),
    pi>0&&pi<1?std::log(pi)-std::log1p(-pi):0}};
  const std::array<bool,3> free{{fs,fp,fi&&pi>0&&pi<1}};
  const auto objective=[&](const std::array<Jet,3>& z) {
    const auto r=exp(z[0]),lp=-softplus(-z[1]),lq=-softplus(z[1]);
    const auto logzero=r*lp;
    const auto lpi=pi==0?Jet(-INFINITY):pi==1?Jet(0):-softplus(-z[2]);
    const auto lmpi=pi==0?Jet(0):pi==1?Jet(-INFINITY):-softplus(z[2]);
    JetSum f;
    for(int v:x) {
      if(!truncated&&v==0) {
        f.add(pi==0?logzero:lpi+softplus(lmpi+logzero-lpi));
      } else {
        JetSum rising;for(int j=0;j<v;++j)rising.add(log(r+Jet(j)));
        const auto term=rising.value+logzero+Jet(v)*lq;
        f.add(truncated?term-log(-expm1(logzero)):term+lmpi);
      }
    }
    return f.value;
  };
  if(!polish(q,free,tol,maxit,objective,detail))return false;
  if(fs)size=std::exp(q[0]);
  if(fp)prob=q[1]>0?1/(1+std::exp(-q[1])):std::exp(q[1])/(1+std::exp(q[1]));
  if(free[2])pi=q[2]>0?1/(1+std::exp(-q[2])):std::exp(q[2])/(1+std::exp(q[2]));
  return true;
}
inline bool nb_polish(ispan x,double& size,double& prob,double& pi,
                      bool fs,bool fp,bool fi,bool truncated,double tol,int maxit) {
  StopDetail detail;return nb_polish(x,size,prob,pi,fs,fp,fi,truncated,tol,maxit,detail);
}
}
}
