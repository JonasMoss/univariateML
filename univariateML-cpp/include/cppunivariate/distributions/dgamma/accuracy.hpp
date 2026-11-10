#pragma once
#include "../../core/likelihood_polish.hpp"
namespace cppunivariate {
namespace dgamma_detail {
// Differentiated incomplete-gamma series/continued fraction. The shape score
// needs the derivatives of the cell integral, not the density at a midpoint.
inline accuracy_detail::Jet gamma_tail(const accuracy_detail::Jet& a,const accuracy_detail::Jet& x,bool lower,int maxit) {
  if(x.v==0)return accuracy_detail::Jet(lower?0:1);
  const auto factor=accuracy_detail::exp(a*accuracy_detail::log(x)-x-accuracy_detail::lgamma(a));
  const ExtendedReal eps=std::numeric_limits<ExtendedReal>::epsilon()*.01;
  if(x.v<a.v+1) {
    accuracy_detail::Jet term(1),sum(1);bool done=false;
    for(std::int64_t k=1;k<=certificate_budget(maxit);++k) {
      term=term*x/(a+accuracy_detail::Jet(k));sum=sum+term;
      if(term.v<sum.v*eps){done=true;break;}
    }
    if(!done)return accuracy_detail::Jet(NAN);
    const auto p=factor*sum/a;return lower?p:accuracy_detail::Jet(1)-p;
  }
  auto b=x+accuracy_detail::Jet(1)-a;
  accuracy_detail::Jet c(1e100),d=accuracy_detail::reciprocal(b),h=d;bool done=false;
  for(std::int64_t k=1;k<=certificate_budget(maxit);++k) {
    const auto an=accuracy_detail::Jet(k)*(a-accuracy_detail::Jet(k));b=b+accuracy_detail::Jet(2);
    d=accuracy_detail::reciprocal(an*d+b);c=b+an/c;
    const auto delta=d*c;h=h*delta;
    if(std::abs(delta.v-1)<eps*16){done=true;break;}
  }
  if(!done)return accuracy_detail::Jet(NAN);
  const auto q=factor*h;return lower?accuracy_detail::Jet(1)-q:q;
}
}
}
