#pragma once
// Included inside bhatt_detail, after local_kernel.
// Coefficients of exp(-u^2/6) sinh(u)/u: the variance term is exactly zero.
inline accuracy_detail::Jet variance_remainder(const accuracy_detail::Jet& z,const accuracy_detail::Jet& eta) {
  constexpr std::array<double,33> coefficient{{1,0,-0.0055555555555555558,0.00035273368606701942,-1.1022927689594357e-05,1.7814832629647445e-07,2.3791176054550539e-10,-1.0332167886547663e-10,3.4481655490827335e-12,-7.3977327435498954e-14,1.2014248185055961e-15,-1.5413625556691389e-17,1.5429143610697541e-19,-1.0769770881153958e-21,1.8769827092263714e-24,9.0996892827403273e-26,-1.9209199724493857e-27,2.5940218663200634e-29,-2.8118824114477339e-31,2.608820019798352e-33,-2.1248151989786644e-35,1.5323836949369032e-37,-9.7463059049513562e-40,5.3481399114516135e-42,-2.3689343587750833e-44,6.4243618379983745e-47,1.7804620519898854e-49,-4.4816324130964525e-51,4.4347072425656574e-53,-3.383006663395076e-55,2.2277340455522795e-57,-1.3190066281484694e-59,7.1553702221154561e-62}};
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
  if(pair_variance_accuracy(r,x,mu,tol,maxit))return true;
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
  const bool projected=r.sigma>0&&std::isfinite(r.sigma+r.a)&&
    (!free[0]||accuracy_detail::location_projection(r.mu,center,location_unit,q[0],r.sigma,tol))&&
    accuracy_detail::scale_projection(r.sigma,unit,q[1]+.5*std::log1p(-eta*eta/3),tol)&&
    accuracy_detail::scale_projection(r.a,unit,q[1]+q[2],tol);
  if(!projected)return false;
  if(r.a<.01*std::hypot(r.sigma,r.a/std::sqrt(3.)))return pair_variance_accuracy(r,x,mu,tol,maxit);
  return true;
}
