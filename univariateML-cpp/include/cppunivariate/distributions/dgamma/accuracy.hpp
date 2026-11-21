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
// Integrate smooth positive cells directly, avoiding subtraction of two
// nearly equal tails and their derivatives.
inline accuracy_detail::Jet cell_logprob(const accuracy_detail::Jet& a,
    const accuracy_detail::Jet& rate,double cell,int maxit) {
  const auto l=accuracy_detail::Jet(cell)*rate,h=accuracy_detail::Jet(cell+1)*rate;
  const bool lower=(l.v+h.v)/2<a.v;
  // Above 4096, consecutive endpoints are within 2^-12 of each other.
  // Use the integral when the endpoint subtraction can lose significant
  // score bits and the log integrand varies gently across the cell.
  if(cell<4096 || std::abs((a.v-1)/(cell+.5)-rate.v)+std::abs(a.v-1)/(cell*(cell+1))>1) {
    const auto prob=lower?gamma_tail(a,h,true,maxit)-gamma_tail(a,l,true,maxit):
      gamma_tail(a,l,false,maxit)-gamma_tail(a,h,false,maxit);
    return accuracy_detail::log(prob);
  }
  const accuracy_detail::Jet midpoint(cell+.5),x=midpoint*rate;
  const auto loga=accuracy_detail::log(a),logx=accuracy_detail::log(x);
  auto density=a*logx-x-accuracy_detail::lgamma(a);
  if(a.v>1000 && std::abs(x.v-a.v)<a.v*.25) {
    const auto v=(x-a)/a,inverse=accuracy_detail::reciprocal(a);
    density=a*accuracy_detail::log1pmx(v)+
      accuracy_detail::Jet(.5)*(loga-accuracy_detail::Jet(1.83787706640934548356))-
      inverse*(accuracy_detail::Jet(1./12)-inverse*inverse*accuracy_detail::Jet(1./360));
  }
  constexpr double nodes[]={.1834346424956498,.5255324099163290,.7966664774136267,.9602898564975362};
  constexpr double weights[]={.3626837833783620,.3137066458778873,.2223810344533745,.1012285362903763};
  accuracy_detail::Jet sum(0);
  for(double half:{-.25,.25})for(int j=0;j<4;++j)for(double sign:{-1.,1.}) {
    const accuracy_detail::Jet offset(half+sign*.25*nodes[j]),fraction=offset/midpoint;
    const auto exponent=(a-accuracy_detail::Jet(1))*accuracy_detail::log1pmx(fraction)+
      ((a-accuracy_detail::Jet(1))/midpoint-rate)*offset;
    sum=sum+accuracy_detail::Jet(.25*weights[j])*accuracy_detail::exp(exponent);
  }
  return density-accuracy_detail::log(midpoint)+accuracy_detail::log(sum);
}

}
}
