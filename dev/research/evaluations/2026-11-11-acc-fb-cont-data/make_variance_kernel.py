from fractions import Fraction
from math import factorial
from pathlib import Path
import json
P=Path(__file__).resolve().parent
coefficient=[sum((Fraction(-1,6)**(m-j)/factorial(m-j)/factorial(2*j+1) for j in range(m+1)),Fraction(0)) for m in range(33)]
(P/'bhatt-hermite-coefficients.json').write_text(json.dumps([str(c) for c in coefficient],indent=2)+'\n')
body='''#pragma once
// Included inside bhatt_detail, after local_kernel.
// Coefficients of exp(-u^2/6) sinh(u)/u: the variance term is exactly zero.
inline accuracy_detail::Jet variance_remainder(const accuracy_detail::Jet& z,const accuracy_detail::Jet& eta) {
  constexpr std::array<double,33> coefficient{{COEFFICIENT}};
  const auto u=z*eta,q=eta*eta;
  accuracy_detail::Jet previous(1),current=u;accuracy_detail::JetSum sum;
  // P_n=eta^n H_n(z) avoids both huge Hermite values and tiny eta^n.
  // For eta<=1/4, |eta*z|<=1, Cauchy on |t|=3*eta bounds the
  // discarded value by 512*3^-66/(1-1/3), below 3e-29.
  for(int n=2;n<=64;++n){const auto next=u*current-accuracy_detail::Jet(n-1)*q*previous;
    if(n>=4&&n%2==0)sum.add(accuracy_detail::Jet(coefficient[n/2])*next);
    previous=current;current=next;}
  return accuracy_detail::log1p(sum.value);
}
inline bool variance_accuracy(BhattResult& r,span x,double mu,double tol,int maxit) {
  const double center=r.mu,unit=std::hypot(r.sigma,r.a/std::sqrt(3.)),location_unit=std::max(std::abs(r.mu),r.sigma);
  std::array<double,3> q{{0,0,std::log(r.a/unit)}};
  const std::array<bool,3> free{{std::isnan(mu),true,true}};
  const auto objective=[&](const std::array<accuracy_detail::Jet,3>& w) {
    const auto S=accuracy_detail::exp(w[1]),eta=accuracy_detail::exp(w[2]);
    const auto d=accuracy_detail::Jet(1)-eta*eta/accuracy_detail::Jet(3);
    if(!(d.v>0))return accuracy_detail::Jet(-INFINITY);
    const auto c=accuracy_detail::exp(accuracy_detail::log(d)/accuracy_detail::Jet(2));
    accuracy_detail::JetSum f;
    for(double v:x){const auto z=(accuracy_detail::Jet((static_cast<ExtendedReal>(v)-center)/unit)-w[0]*accuracy_detail::Jet(location_unit/unit))/S;
      if(eta.v<=.25&&eta.v*(1+std::abs(z.v))<=1)f.add(-w[1]-z*z/accuracy_detail::Jet(2)-accuracy_detail::Jet(.91893853320467274178)+variance_remainder(z,eta));
      else f.add(-w[1]-accuracy_detail::log(c)+local_kernel(z/c,eta/c));}
    return f.value;
  };
  if(!continuous_fallback_polish(q,free,tol,maxit,objective))return false;
  double eta=std::exp(q[2]),S=unit*std::exp(q[1]);
  const double location=static_cast<double>(static_cast<ExtendedReal>(center)+location_unit*static_cast<ExtendedReal>(q[0]));
  const double smoothing=S*std::sqrt(1-eta*eta/3);
  const double sensitivity=std::max(2.,std::max(1+eta*eta/(3-eta*eta),location_unit/std::max(std::abs(location),smoothing)));
  if(sensitivity>4&&!continuous_fallback_polish(q,free,tol*4/sensitivity,maxit,objective))return false;
  eta=std::exp(q[2]);S=unit*std::exp(q[1]);
  if(free[0])r.mu=static_cast<double>(static_cast<ExtendedReal>(center)+location_unit*static_cast<ExtendedReal>(q[0]));
  r.sigma=S*std::sqrt(1-eta*eta/3);r.a=S*eta;
  return r.sigma>0&&std::isfinite(r.sigma+r.a)&&
    (!free[0]||accuracy_detail::location_projection(r.mu,center,location_unit,q[0],r.sigma,tol))&&
    accuracy_detail::scale_projection(r.sigma,unit,q[1]+.5*std::log1p(-eta*eta/3),tol)&&
    accuracy_detail::scale_projection(r.a,unit,q[1]+q[2],tol);
}
'''.replace('COEFFICIENT',','.join(format(float(c),'.17g') for c in coefficient))
root=P.parents[3]
(root/'univariateML-cpp/include/cppunivariate/distributions/bhatt/variance_accuracy.hpp').write_text(body)
