#pragma once
#include "../../core/likelihood_polish.hpp"
namespace cppunivariate {
namespace skellam_detail {
inline accuracy_detail::Jet differentiable_mass(double value,accuracy_detail::Jet a,accuracy_detail::Jet b,int maxit) {
  if(value<0){value=-value;std::swap(a,b);}
  const ExtendedReal n=value,product=a.v*b.v;
  const ExtendedReal mode=std::floor(2*product/(std::hypot(n,2*std::sqrt(product))+n));
  if(!(mode>=0)||mode>enumeration_budget(maxit))return accuracy_detail::Jet(NAN);
  const std::int64_t j=static_cast<std::int64_t>(mode);
  const auto ab=a*b;
  accuracy_detail::Jet total(1),term(1);
  const ExtendedReal eps=std::numeric_limits<ExtendedReal>::epsilon()*.01;
  for(std::int64_t h=j;h>0;--h) {term=term*accuracy_detail::Jet(ExtendedReal(h)*(h+n))/ab;total=total+term;if(term.v<total.v*eps)break;}
  term=accuracy_detail::Jet(1);bool done=false;
  for(std::int64_t k=1;k<=certificate_budget(maxit);++k) {
    const ExtendedReal h=mode+k;
    term=term*ab/accuracy_detail::Jet(h*(h+n));total=total+term;
    if(term.v<total.v*eps){done=true;break;}
  }
  if(!done)return accuracy_detail::Jet(NAN);
  return -a-b+accuracy_detail::Jet(mode+n)*accuracy_detail::log(a)+accuracy_detail::Jet(mode)*accuracy_detail::log(b)+accuracy_detail::log(total)-accuracy_detail::Jet(std::lgamma(mode+n+1)+std::lgamma(mode+1));
}
}
}
