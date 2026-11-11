#pragma once
// Included inside cppunivariate. Local accuracy is independent of coverage.
namespace gev_accuracy_detail {
inline accuracy_detail::Jet ratio(const accuracy_detail::Jet& xi,const accuracy_detail::Jet& z,int maxit) {
  const auto v=xi*z;
  if(std::abs(v.v)>.125)return accuracy_detail::log1p(v)/xi;
  auto sum=z,term=z;
  const double r=static_cast<double>(std::abs(v.v));
  for(std::int64_t k=1;k<=solver_budget(maxit);++k) {
    term=term*(-v)*accuracy_detail::Jet(static_cast<ExtendedReal>(k)/(k+1));sum=sum+term;
    // Two derivatives lose at most two powers; bound their geometric tail.
    if(k>=4&&std::pow(r,static_cast<double>(k-2))*(k+1)*(k+1)<std::numeric_limits<ExtendedReal>::epsilon()*.01)return sum;
  }
  return accuracy_detail::Jet(NAN);
}
// Two-component arithmetic protects a small positive-shape score from
// cancellation. It is used only for the open Frechet/Gumbel seam; widening
// a rounded normalized sample cannot recover the original score.
struct Pair {
  double hi=0,lo=0;
  Pair()=default;Pair(double h):hi(h) {} Pair(double h,double l):hi(h),lo(l) {}
};
inline Pair renorm(double h,double l) {const double s=h+l;return {s,l-(s-h)};}
inline Pair operator+(Pair a,Pair b) {
  const double s=a.hi+b.hi,v=s-a.hi;
  return renorm(s,((a.hi-(s-v))+(b.hi-v))+a.lo+b.lo);
}
inline Pair operator-(Pair a) {return {-a.hi,-a.lo};}
inline Pair operator-(Pair a,Pair b) {return a+(-b);}
inline Pair operator*(Pair a,Pair b) {
  const double h=a.hi*b.hi;
  return renorm(h,std::fma(a.hi,b.hi,-h)+a.hi*b.lo+a.lo*b.hi+a.lo*b.lo);
}
inline Pair operator/(Pair a,Pair b) {
  const double q=a.hi/b.hi;const auto r=a-Pair(q)*b;
  return Pair(q)+Pair((r.hi+r.lo)/b.hi);
}
inline Pair exp_pair(Pair a,int maxit) {
  if(!std::isfinite(a.hi)||std::abs(a.hi)>700)return {NAN};
  const Pair ln2{.69314718055994528623,2.3190468138462995584e-17};
  const int k=static_cast<int>(std::nearbyint(a.hi/ln2.hi));
  const Pair r=a-Pair(k)*ln2;Pair term(1),sum(1);
  for(std::int64_t j=1;j<=solver_budget(maxit);++j) {
    term=term*r/Pair(static_cast<double>(j));sum=sum+term;
    if(std::abs(term.hi)<1e-33) return {std::ldexp(sum.hi,k),std::ldexp(sum.lo,k)};
  }
  return {NAN};
}
inline bool seam_score(span data,double center,double unit,double shape,const std::array<double,3>& q,
                       int maxit,std::array<ExtendedReal,3>& score) {
  const Pair mu=Pair(center)+Pair(unit)*Pair(q[0]);
  const Pair sigma=Pair(unit)*exp_pair(Pair(q[1]),maxit),xi=Pair(shape)*exp_pair(Pair(q[2]),maxit);
  std::array<Pair,3> sum{};
  for(double d:data) {
    const Pair z=(Pair(d)-mu)/sigma,v=xi*z,t=Pair(1)+v;
    if(!(t.hi>0)||std::abs(v.hi)>.125)return false;
    Pair L=z,term=z,Lx(0),derivative=-z*z/Pair(2);bool done=false;
    for(std::int64_t k=1;k<=solver_budget(maxit);++k) {
      term=term*(-v)*Pair(static_cast<double>(k))/Pair(static_cast<double>(k+1));L=L+term;
      Lx=Lx+derivative;
      derivative=derivative*(-v)*Pair(static_cast<double>((k+1)*(k+1)))/Pair(static_cast<double>(k*(k+2)));
      if(k>=4&&std::pow(std::abs(v.hi),static_cast<double>(k-2))*(k+1)*(k+1)<1e-33) {done=true;break;}
    }
    if(!done)return false;
    const Pair A=Pair(1)+xi-exp_pair(-L,maxit);
    sum[0]=sum[0]+A/(sigma*t)*Pair(unit);
    sum[1]=sum[1]-Pair(1)+A*z/t;
    sum[2]=sum[2]+(-L-A*Lx)*xi;
  }
  for(int i=0;i<3;++i) {const auto s=sum[i]/Pair(static_cast<double>(data.size()));score[i]=static_cast<ExtendedReal>(s.hi)+s.lo;}
  return true;
}
inline bool polish(span data,GevResult& fit,bool held_mu,bool held_sigma,double tol,int maxit,bool frechet=false) {
  if(fit.status!=GevStatus::Ok||!(fit.sigma>0)||!std::isfinite(fit.mu)||!std::isfinite(fit.xi))return false;
  // On this closed moving-support face the conditional scale and location
  // are analytic. No smooth derivative is taken through a zero residual.
  const double maximum=*std::max_element(data.begin(),data.end());
  if(fit.xi== -1&&((!held_mu&&fit.mu==maximum-fit.sigma)||
     (held_mu&&fit.sigma==maximum-fit.mu)))
    return std::isfinite(gev_loglik(data,fit.mu,fit.sigma,fit.xi));
  const double center=fit.mu,unit=fit.sigma,shape=fit.xi;
  const bool shape_edge=shape==1||shape== -1;
  std::array<double,3> q{0,0,frechet?0:shape};
  const std::array<bool,3> free{!held_mu,!held_sigma,!shape_edge};
  auto objective=[&](const std::array<accuracy_detail::Jet,3>& a) {
    const auto mu=accuracy_detail::Jet(center)+accuracy_detail::Jet(unit)*a[0];
    const auto sigma=accuracy_detail::Jet(unit)*accuracy_detail::exp(a[1]);
    const auto xi=frechet?accuracy_detail::Jet(shape)*accuracy_detail::exp(a[2]):a[2];
    if(xi.v< -1||xi.v>1||(frechet&&!(xi.v>0)))return accuracy_detail::Jet(NAN);
    accuracy_detail::JetSum sum;
    for(double d:data) {
      const auto z=(accuracy_detail::Jet(d)-mu)/sigma;
      if(!(1+xi.v*z.v>0))return accuracy_detail::Jet(NAN);
      const auto L=ratio(xi,z,maxit);
      sum.add(-accuracy_detail::log(sigma)-(accuracy_detail::Jet(1)+xi)*L-accuracy_detail::exp(-L));
    }
    auto value=sum.value/accuracy_detail::Jet(static_cast<ExtendedReal>(data.size()));
    if(frechet&&shape<1e-4) {
      std::array<double,3> point{};for(int i=0;i<3;++i)point[i]=static_cast<double>(a[i].v);
      std::array<ExtendedReal,3> score{};
      if(!seam_score(data,center,unit,shape,point,maxit,score))return accuracy_detail::Jet(NAN);
      for(int i=0;i<3;++i)value.g[i]=a[i].g[i]!=0?score[i]:0;
    }
    return value;
  };
  if(!accuracy_detail::polish(q,free,tol,maxit,objective))return false;
  if(shape_edge) {
    std::array<accuracy_detail::Jet,3> a;
    for(int i=0;i<3;++i)a[i]=accuracy_detail::variable(q[i],i,true);
    const auto score=objective(a).g[2];
    if(shape==1?!(score>=0):!(score<=0))return false;
  }
  const double mu=held_mu?center:center+unit*q[0],sigma=held_sigma?unit:unit*std::exp(q[1]);
  const double xi=frechet?shape*std::exp(q[2]):q[2];
  if(!accuracy_detail::location_projection(mu,center,unit,q[0],sigma,tol)||
     !accuracy_detail::scale_projection(sigma,unit,q[1],tol)||!std::isfinite(xi))return false;
  const double value=gev_loglik(data,mu,sigma,xi);
  if(!std::isfinite(value))return false;
  fit.mu=mu;fit.sigma=sigma;fit.xi=xi;fit.logLik=value;return true;
}
} // namespace gev_accuracy_detail
